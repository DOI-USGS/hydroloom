"""Network functions over hy frames."""

from __future__ import annotations

import warnings

import numpy as np
import pandas as pd  # type: ignore[import-untyped]

from hydroloom._docstrings import doc
from hydroloom._hy import (
    Representation,
    _as_hy,
    _classify,
    _hy_reverse,
    _join,
    _route,
    _true,
    requires,
)
from hydroloom._kernels import walk
from hydroloom.errors import (
    HydroloomError,
    HydroloomWarning,
    RepresentationError,
    _stack_level,
    msg,
)
from hydroloom.index_ids import IndexIds, _from_edges

_MODES = ("to", "from", "both")


def _main(df: pd.DataFrame, column: str, keep: np.ndarray) -> np.ndarray | None:
    if column not in df.columns:
        return None

    flags: np.ndarray = _true(df[column])[keep]

    return flags


def _make_index_ids_impl(
    net: pd.DataFrame, *, mode: str
) -> IndexIds | tuple[IndexIds, IndexIds]:
    """Build the index on a topo or flownetwork frame (make_index_ids.md).

    Identifiers are text after `_as_hy` (I-2), so no type check is needed, and
    a repeated (id, toid) pair has already raised there (S-6); one body serves
    both patterns. `make_index_ids` has checked ``mode``.
    """
    # Dense positions in first-appearance order; a toid that is no id is an
    # outlet and gives no edge (I-4).
    codes, uniques = pd.factorize(net["id"])
    target = uniques.get_indexer(net["toid"])
    keep = target >= 0
    source = codes[keep].astype(np.int64)
    target = target[keep].astype(np.int64)

    to = _from_edges(uniques, source, target, "to", _main(net, "downmain", keep))
    if walk.has_cycle(to.indptr, to.indices):
        raise HydroloomError(msg("network_cycle"))
    if mode == "to":
        return to

    # "from" edges are the "to" edges reversed; main is the upmain of the row
    # that defines the edge (D3-43).
    up = _from_edges(uniques, target, source, "from", _main(net, "upmain", keep))
    if mode == "from":
        return up

    return to, up


@requires(
    Representation.TOPO,
    Representation.FLOWNETWORK,
    via={Representation.NODE: "add_toids"},
)
@doc("make_index_ids")
def make_index_ids(
    network: pd.DataFrame, *, mode: str = "to"
) -> IndexIds | tuple[IndexIds, IndexIds]:
    """Make index ids.

    Examples
    --------
    >>> import pandas as pd
    >>> from hydroloom import make_index_ids
    >>> net = pd.DataFrame({"id": [30, 4, 100, 10, 7], "toid": [10, 10, 10, 7, 0]})
    >>> up = make_index_ids(net, mode="from")
    >>> up.uniques.tolist()
    ['30', '4', '100', '10', '7']
    >>> node = up.uniques.get_loc("10")
    >>> up.uniques[up.indices[up.indptr[node] : up.indptr[node + 1]]].tolist()
    ['100', '30', '4']
    """
    if mode not in _MODES:
        raise ValueError(msg("index_mode_invalid"))

    df, _ = _as_hy(network)
    native = {
        Representation.TOPO: _make_index_ids_impl,
        Representation.FLOWNETWORK: _make_index_ids_impl,
    }
    result: IndexIds | tuple[IndexIds, IndexIds]
    result = _route(df, make_index_ids, native, mode=mode)

    return result


def _eligible(net: pd.DataFrame) -> np.ndarray:
    """Check the divergence coding; return the rows that may be a target.

    add_toids.md Algorithm step 3: a missing divergence and a broken coding at
    a split stop before either tolerated-coding warning. A minor path at a
    split (divergence 2, its fromnode reached and shared) is not a target; a
    lone downstream flowline coded 2 stays one. The source attribute decides
    (D-9).
    """
    if "divergence" not in net.columns:
        raise HydroloomError(msg("divergence_required"))

    ids = net["id"]
    divergence = pd.to_numeric(net["divergence"]).to_numpy(
        dtype=np.float64, na_value=np.nan
    )
    missing = np.isnan(divergence)
    if missing.any():
        raise HydroloomError(
            msg("divergence_missing", rows=_join(ids[missing].tolist()))
        )

    fromnode = net["fromnode"]
    present = fromnode.notna().to_numpy()
    reached = present & fromnode.isin(net["tonode"].dropna()).to_numpy()
    counts = fromnode.map(fromnode.value_counts()).fillna(0).to_numpy()
    minor = divergence == 2
    split = reached & (counts >= 2)

    # Downstream flowlines not coded 2 at each node; one per split is valid.
    mains = (
        pd.Series(~minor, index=net.index)
        .groupby(fromnode, dropna=False)
        .transform("sum")
        .to_numpy()
    )
    broken = split & (mains != 1)
    if broken.any():
        raise HydroloomError(
            msg("divergence_coding_invalid", rows=_join(ids[broken].tolist()))
        )

    zero = split & (divergence == 0)
    if zero.any():
        warnings.warn(
            msg("divergence_zero_main_path", rows=_join(ids[zero].tolist())),
            HydroloomWarning,
            stacklevel=_stack_level(),
        )
    lone = reached & (counts == 1) & minor
    if lone.any():
        warnings.warn(
            msg("divergence_minor_only_path", rows=_join(ids[lone].tolist())),
            HydroloomWarning,
            stacklevel=_stack_level(),
        )

    eligible: np.ndarray = ~(minor & split)

    return eligible


def _add_toids_impl(net: pd.DataFrame, *, return_dendritic: bool) -> pd.DataFrame:
    """Join each tonode to the eligible fromnodes (add_toids.md steps 3 to 7).

    One output row per match, in the input order of the upstream row and then
    of the downstream row (step 7); a row with no match, or a missing tonode,
    keeps its input position with the outlet value "" (I-5).
    """
    n = len(net)
    position = np.arange(n, dtype=np.int64)
    if return_dendritic:
        eligible = _eligible(net)
    else:
        eligible = np.ones(n, dtype=bool)

    fromnode, tonode = net["fromnode"], net["tonode"]
    target = eligible & fromnode.notna().to_numpy()
    has_to = tonode.notna().to_numpy()
    down = pd.DataFrame({"node": fromnode[target], "d": position[target]})
    up = pd.DataFrame({"node": tonode[has_to], "u": position[has_to]})
    pairs = up.merge(down, on="node", how="left")

    terminal = position[~has_to]
    u = np.concatenate([pairs["u"].to_numpy(dtype=np.int64), terminal])
    d = np.concatenate(
        [pairs["d"].to_numpy(dtype=np.float64), np.full(len(terminal), np.nan)]
    )
    order = np.lexsort((d, u))
    u, d = u[order], d[order]

    hit = ~np.isnan(d)
    toid = np.full(len(u), "", dtype=object)
    toid[hit] = net["id"].to_numpy(dtype=object)[d[hit].astype(np.int64)]

    out = net.iloc[u].reset_index(drop=True)
    out.insert(1, "toid", pd.Series(toid, index=out.index, dtype=object))

    return out


@requires(Representation.NODE, via={Representation.TOPO: "make_node_topology"})
@doc("add_toids")
def add_toids(network: pd.DataFrame, *, return_dendritic: bool = True) -> pd.DataFrame:
    """Add toids.

    Examples
    --------
    >>> import pandas as pd
    >>> from hydroloom import add_toids
    >>> net = pd.DataFrame(
    ...     {
    ...         "id": [1, 2, 3, 4],
    ...         "fromnode": [10, 20, 20, 30],
    ...         "tonode": [20, 30, 40, 50],
    ...         "divergence": [0, 1, 2, 0],
    ...     }
    ... )
    >>> add_toids(net)["toid"].tolist()
    ['2', '4', '', '']
    """
    if not return_dendritic:
        # Before every other check (add_toids.md Preconditions, A-11).
        warnings.warn(
            msg("return_dendritic_deprecated"),
            FutureWarning,
            stacklevel=_stack_level(),
        )

    df, names = _as_hy(network)
    if "toid" in df.columns:
        current = _classify(df).value
        raise RepresentationError(msg("toid_already_present", current=current))

    native = {Representation.NODE: _add_toids_impl}
    out: pd.DataFrame = _route(df, add_toids, native, return_dendritic=return_dendritic)

    return _hy_reverse(out, names)
