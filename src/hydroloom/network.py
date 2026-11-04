"""Network functions over hy frames."""

from __future__ import annotations

import warnings
from collections.abc import Iterable, Sequence
from typing import Any, Literal, cast

import geopandas as gpd  # type: ignore[import-untyped]
import numpy as np
import pandas as pd  # type: ignore[import-untyped]
from numpy.typing import NDArray

from hydroloom._docstrings import doc
from hydroloom._hy import (
    Representation,
    _as_hy,
    _classify,
    _hy_reverse,
    _identifier_values,
    _join,
    _missing,
    _route,
    _true,
    requires,
)
from hydroloom._kernels import sequential, walk
from hydroloom.errors import (
    HydroloomError,
    HydroloomWarning,
    RepresentationError,
    _stack_level,
    msg,
)
from hydroloom.index_ids import IndexIds, _byte_rank, _from_edges

_MODES = ("to", "from", "both")


def _main(
    df: pd.DataFrame, column: str, keep: NDArray[np.bool_]
) -> NDArray[np.bool_] | None:
    if column not in df.columns:
        return None

    flags: NDArray[np.bool_] = _true(df[column])[keep]

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


def _eligible(net: pd.DataFrame) -> NDArray[np.bool_]:
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

    eligible: NDArray[np.bool_] = ~(minor & split)

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

    return _insert_computed(out, "toid", pd.Series(toid, index=out.index, dtype=object))


@requires(Representation.NODE)
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


def _node_to_topo(net: pd.DataFrame) -> pd.DataFrame:
    # The node route of sort_network and add_topo_sort: add_toids with its
    # default return_dendritic = True, silently (A-3, A-12).
    return _add_toids_impl(net, return_dendritic=True)


def _insert_computed(out: pd.DataFrame, name: str, values: Any) -> pd.DataFrame:
    """Add a computed column after the inputs, geometry last (D-14).

    An input column of the same hy name is replaced (B-4).
    """
    out = out.drop(columns=name, errors="ignore")
    at = len(out.columns)
    if isinstance(out, gpd.GeoDataFrame):
        at = out.columns.get_loc(out.geometry.name)
    out.insert(at, name, values)

    return out


def _outlet_values(outlets: Any) -> list[str | None]:
    # One identifier, or a sequence of them, typed as id is on ingest (I-2).
    if isinstance(outlets, str | int | float | np.integer | np.floating):
        outlets = [outlets]

    return _identifier_values(pd.Series(list(outlets), dtype=object), "outlets")


def _sort_network_impl(net: pd.DataFrame, *, split: bool, outlets: Any) -> pd.DataFrame:
    """Sort a topo or flownetwork frame upstream to downstream (sort_network.md).

    The index build runs the D-8 cycle check (steps 2 and 3). Dense positions
    follow first appearance of each id; byte ranks give the D-4 tie rules.
    """
    up = cast(IndexIds, _make_index_ids_impl(net, mode="from"))
    uniques = up.uniques
    rank = _byte_rank(uniques)
    codes = uniques.get_indexer(net["id"])

    # Starts (step 4): an id with a row whose toid is no id (I-4), or the
    # named outlets, each once; order of `outlets` is not read (D3-50).
    if outlets is None:
        terminal = uniques.get_indexer(net["toid"]) < 0
        candidates = np.unique(codes[terminal])
    else:
        values = _outlet_values(outlets)
        found = uniques.get_indexer(pd.Index(values, dtype=object))
        absent = [v for v, f in zip(values, found, strict=True) if f < 0]
        if absent:
            names = _join(list(dict.fromkeys(absent)))
            raise HydroloomError(msg("outlet_not_in_network", outlets=names))
        candidates = np.unique(found)
    # Step 6: starts in descending byte order.
    starts = candidates[np.argsort(-rank[candidates], kind="stable")]

    # Step 5: one countdown per distinct (id, toid) pair, an outside toid
    # included (D3-155); as_hy has raised on a repeated pair, so a row is one.
    countdown = np.bincount(codes, minlength=len(uniques)).astype(np.int64)
    visits, walks = walk.topo_walk(up.indptr, up.indices, countdown, starts)

    # Step 7: each id at its last visit, latest first; rows of one id by toid
    # byte order, then input order (D-3).
    last = np.full(len(uniques), -1, dtype=np.int64)
    np.maximum.at(last, visits, np.arange(len(visits), dtype=np.int64))
    rows = np.flatnonzero(last[codes] >= 0)
    toid_rank = _byte_rank(net["toid"].to_numpy(dtype=object))
    order = rows[np.lexsort((toid_rank[rows], -last[codes[rows]]))]
    out = net.iloc[order].reset_index(drop=True)
    if not split:
        return out

    # Step 9: the start of the walk that visited each flowline.
    if len(np.unique(visits)) < len(visits):
        raise HydroloomError(msg("split_outlets_overlap"))
    start_of = np.full(len(uniques), -1, dtype=np.int64)
    start_of[visits] = starts[walks]
    ids = uniques.to_numpy(dtype=object)[start_of[codes[order]]]

    return _insert_computed(
        out, "terminal_id", pd.Series(ids, index=out.index, dtype=object)
    )


@requires(
    Representation.TOPO,
    Representation.FLOWNETWORK,
    convert={Representation.NODE: _node_to_topo},
    via={Representation.NODE: "add_toids"},
)
@doc("sort_network")
def sort_network(
    network: pd.DataFrame,
    *,
    split: bool = False,
    outlets: str | int | Sequence[str | int] | None = None,
) -> pd.DataFrame:
    """Sort a network upstream to downstream.

    Examples
    --------
    >>> import pandas as pd
    >>> from hydroloom import sort_network
    >>> net = pd.DataFrame({"id": [30, 2, 10, 4, 5], "toid": [4, 4, 4, 5, 0]})
    >>> sort_network(net)["id"].tolist()
    ['10', '2', '30', '4', '5']
    >>> sort_network(net, split=True)["terminal_id"].tolist()
    ['5', '5', '5', '5', '5']
    """
    df, names = _as_hy(network)
    native = {
        Representation.TOPO: _sort_network_impl,
        Representation.FLOWNETWORK: _sort_network_impl,
    }
    out: pd.DataFrame = _route(df, sort_network, native, split=split, outlets=outlets)

    return _hy_reverse(out, names)


def _add_topo_sort_impl(net: pd.DataFrame, *, outlets: Any) -> pd.DataFrame:
    """Give each sorted id its value, k down to 1 (add_topo_sort.md step 7)."""
    out = _sort_network_impl(net, split=False, outlets=outlets)
    # Distinct ids in output order; repeated-id rows share one value.
    codes, uniques = pd.factorize(out["id"])
    topo = (len(uniques) - codes).astype(np.int32)

    return _insert_computed(
        out, "topo_sort", pd.Series(topo, index=out.index, dtype=np.int32)
    )


@requires(
    Representation.TOPO,
    Representation.FLOWNETWORK,
    convert={Representation.NODE: _node_to_topo},
    via={Representation.NODE: "add_toids"},
)
@doc("add_topo_sort")
def add_topo_sort(
    network: pd.DataFrame, *, outlets: str | int | Sequence[str | int] | None = None
) -> pd.DataFrame:
    """Add a topological sort column.

    Examples
    --------
    >>> import pandas as pd
    >>> from hydroloom import add_topo_sort
    >>> net = pd.DataFrame(
    ...     {"id": [10, 9, 2, 3, 5, 7], "toid": [2, 2, 3, 0, 3, 0]}
    ... )
    >>> out = add_topo_sort(net)
    >>> out["id"].tolist()
    ['10', '9', '2', '5', '3', '7']
    >>> out["topo_sort"].tolist()
    [6, 5, 4, 3, 2, 1]
    """
    df, names = _as_hy(network)
    native = {
        Representation.TOPO: _add_topo_sort_impl,
        Representation.FLOWNETWORK: _add_topo_sort_impl,
    }
    out: pd.DataFrame = _route(df, add_topo_sort, native, outlets=outlets)

    return _hy_reverse(out, names)


_DIRECTIONS = ("down", "up", "downmain", "upmain")


def _start_values(starts: Any) -> list[str | None]:
    """Return each start as text, ``None`` where it cannot be an id.

    A start compares with ``id`` as text (navigate_network_dfs.md Algorithm
    step 3, I-1), so a whole number matches its decimal string; a missing,
    fractional, or non-finite value matches nothing.
    """
    if isinstance(starts, str | int | float | np.integer | np.floating):
        starts = [starts]
    out: list[str | None] = []
    for value in starts:
        if _missing(value) or isinstance(value, bool | np.bool_):
            out.append(None)
        elif isinstance(value, str):
            out.append(value)
        elif isinstance(value, int | np.integer):
            out.append(str(int(value)))
        elif isinstance(value, float | np.floating) and float(value).is_integer():
            out.append(str(int(value)))
        else:
            out.append(None)

    return out


def _navigate_dfs_index(
    index: IndexIds, starts: Any, *, direction: str, reset: bool
) -> list[list[pd.Index]]:
    """Walk an index whose direction and main flags are checked (steps 3 to 7)."""
    uniques = pd.Index(index.uniques.astype(str), dtype=object)
    values = _start_values(starts)
    found = uniques.get_indexer(pd.Index(values, dtype=object))
    if (found < 0).any():
        raise HydroloomError(msg("start_not_in_network"))
    # Unique starts in first-appearance order (step 3).
    unique = np.asarray(list(dict.fromkeys(found.tolist())), dtype=np.int64)

    main = index.main if direction.endswith("main") else None
    visit, path, start_ptr = sequential.dfs_paths(
        index.indptr, index.indices, unique, main, reset
    )

    # Step 7: identifiers by dense position, split by path.
    ids = uniques.to_numpy(dtype=object)[visit]
    result: list[list[pd.Index]] = []
    for lo, hi in zip(start_ptr[:-1].tolist(), start_ptr[1:].tolist(), strict=True):
        cuts = np.flatnonzero(np.diff(path[lo:hi])) + 1
        result.append(
            [pd.Index(part, dtype=object) for part in np.split(ids[lo:hi], cuts)]
            if hi > lo
            else []
        )

    return result


@requires(
    Representation.TOPO,
    Representation.FLOWNETWORK,
    via={Representation.NODE: "add_toids"},
)
@doc("navigate_network_dfs")
def navigate_network_dfs(
    network: pd.DataFrame | IndexIds,
    starts: str | int | Iterable[str | int | float],
    *,
    direction: Literal["down", "up", "downmain", "upmain"] = "down",
    reset: bool = False,
) -> list[list[pd.Index]]:
    """Navigate all paths depth first.

    Examples
    --------
    >>> import pandas as pd
    >>> from hydroloom import navigate_network_dfs
    >>> net = pd.DataFrame({"id": [1, 1, 10, 9, 4], "toid": [10, 9, 4, 4, 0]})
    >>> [path.tolist() for path in navigate_network_dfs(net, ["1"])[0]]
    [['1', '10', '4'], ['9']]
    >>> up = navigate_network_dfs(net, ["4"], direction="up")
    >>> [path.tolist() for path in up[0]]
    [['4', '10', '1'], ['9']]
    """
    if direction not in _DIRECTIONS:
        raise ValueError(msg("direction_invalid", direction=direction))
    mode = "from" if direction.startswith("up") else "to"
    is_main = direction.endswith("main")

    if isinstance(network, IndexIds):
        # Checked against direction and used as is (Algorithm step 1).
        if network.mode != mode:
            needed = "down" if network.mode == "to" else "up"
            raise HydroloomError(
                msg("index_ids_direction_mismatch", needed=needed, mode=network.mode)
            )
        if is_main and network.main is None:
            raise HydroloomError(msg("index_ids_missing_main", direction=direction))
        return _navigate_dfs_index(network, starts, direction=direction, reset=reset)
    if not isinstance(network, pd.DataFrame):
        raise HydroloomError(msg("network_not_frame_or_index_ids"))

    df, _ = _as_hy(network)
    # A node table walked down or up converts silently with every connection
    # kept (Preconditions). The main directions route through to_flownetwork,
    # which Python does not have yet (Remedies, node_main_needs_flownetwork).
    if _classify(df) is Representation.NODE:
        if is_main:
            raise RepresentationError(
                msg(
                    "node_main_needs_flownetwork",
                    function="navigate_network_dfs",
                    direction=direction,
                )
            )
        df = _add_toids_impl(df, return_dendritic=False)
    native = {
        Representation.TOPO: _make_index_ids_impl,
        Representation.FLOWNETWORK: _make_index_ids_impl,
    }
    index: IndexIds = _route(df, navigate_network_dfs, native, mode=mode)
    if is_main and index.main is None:
        raise HydroloomError(msg("main_path_required", direction=direction))

    return _navigate_dfs_index(index, starts, direction=direction, reset=reset)
