"""Graph walks over CSR: strongly connected components (SPEC D-8).

Language-local kernel below the frame boundary (K-1, K-9). Nodes are dense
positions ``0 .. n - 1``; identifiers never enter.
"""

from __future__ import annotations

import numpy as np
from numpy.typing import NDArray
from scipy.sparse import csr_array  # type: ignore[import-untyped]
from scipy.sparse.csgraph import (  # type: ignore[import-untyped]
    connected_components,
)

__all__ = ["has_cycle", "strong_components"]


def strong_components(
    indptr: NDArray[np.int64], indices: NDArray[np.int64]
) -> NDArray[np.int64]:
    """Label the strongly connected components of a directed CSR graph.

    Parameters
    ----------
    indptr, indices : NDArray[np.int64]
        The CSR graph.

    Returns
    -------
    NDArray[np.int64]
        Length n, the component label of each node, ``0 .. k - 1``.
    """
    n = len(indptr) - 1
    if n == 0:
        return np.zeros(0, dtype=np.int64)

    graph = csr_array(
        (np.ones(len(indices), dtype=np.int8), indices, indptr), shape=(n, n)
    )
    _, labels = connected_components(graph, directed=True, connection="strong")

    return np.asarray(labels, dtype=np.int64)


def has_cycle(indptr: NDArray[np.int64], indices: NDArray[np.int64]) -> bool:
    """Return whether a directed CSR graph holds a cycle.

    A cycle is a strongly connected component larger than one node or a
    self-loop (D-8).

    Parameters
    ----------
    indptr, indices : NDArray[np.int64]
        The CSR graph.

    Returns
    -------
    bool
        ``True`` when the graph holds a cycle.
    """
    indptr = np.asarray(indptr, dtype=np.int64)
    indices = np.asarray(indices, dtype=np.int64)
    n = len(indptr) - 1
    owner = np.repeat(np.arange(n, dtype=np.int64), np.diff(indptr))
    if bool(np.any(owner == indices)):
        return True

    sizes = np.bincount(strong_components(indptr, indices))

    return bool(np.any(sizes > 1))
