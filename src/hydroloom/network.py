"""Network functions over hy frames."""

from __future__ import annotations

import numpy as np
import pandas as pd  # type: ignore[import-untyped]

from hydroloom._docstrings import doc
from hydroloom._hy import Representation, _as_hy, _route, _true, requires
from hydroloom._kernels import walk
from hydroloom.errors import HydroloomError, msg
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
    both patterns.
    """
    if mode not in _MODES:
        raise ValueError(msg("index_mode_invalid"))

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
    df, _ = _as_hy(network)
    native = {
        Representation.TOPO: _make_index_ids_impl,
        Representation.FLOWNETWORK: _make_index_ids_impl,
    }
    result: IndexIds | tuple[IndexIds, IndexIds]
    result = _route(df, make_index_ids, native, mode=mode)

    return result
