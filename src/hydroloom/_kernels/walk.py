"""Graph walks over CSR: strongly connected components (SPEC D-8), topological walk.

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

__all__ = ["has_cycle", "strong_components", "topo_walk"]


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


def topo_walk(
    indptr: NDArray[np.int64],
    indices: NDArray[np.int64],
    countdown: NDArray[np.int64],
    starts: NDArray[np.int64],
) -> tuple[NDArray[np.int64], NDArray[np.int64]]:
    """Walk upstream from each start, depth first (sort_network.md step 5).

    Each start is walked in two passes. The first finds the nodes the walk
    visits, each once, with the countdowns carried from earlier walks: a
    neighbor whose countdown is 1 is pushed, any other is decremented. The
    second visits those nodes depth first with each countdown set to the
    node's downstream connections among them, so a node an earlier walk
    visited comes once, after its downstream nodes in this walk. The graph
    must be acyclic.

    Parameters
    ----------
    indptr, indices : NDArray[np.int64]
        The upstream CSR: the neighbors of node i are the nodes that drain to
        it, in the order they are taken.
    countdown : NDArray[np.int64]
        Length n, each node's number of downstream connections; not modified.
    starts : NDArray[np.int64]
        Dense positions, in the order they are walked.

    Returns
    -------
    visits : NDArray[np.int64]
        Dense positions in visit order; a node visited from two walks appears
        twice, and never twice within one walk.
    walks : NDArray[np.int64]
        Aligned with ``visits``, the position in ``starts`` of the walk that
        made each visit.
    """
    ptr = np.asarray(indptr, dtype=np.int64).tolist()
    up = np.asarray(indices, dtype=np.int64).tolist()
    left = np.asarray(countdown, dtype=np.int64).tolist()
    member = [False] * len(left)
    local = [0] * len(left)

    visits: list[int] = []
    walks: list[int] = []
    for walk, start in enumerate(np.asarray(starts, dtype=np.int64).tolist()):
        # Pass 1: the nodes this walk visits, carrying the countdowns.
        # LIFO stacks throughout (K-8: sequential and deterministic).
        found: list[int] = []
        stack = [start]
        while stack:
            node = stack.pop()
            if member[node]:
                continue
            member[node] = True
            found.append(node)
            for neighbor in up[ptr[node] : ptr[node + 1]]:
                if left[neighbor] == 1:
                    stack.append(neighbor)
                else:
                    left[neighbor] -= 1

        # Pass 2: each found node's downstream connections among the found.
        for node in found:
            for neighbor in up[ptr[node] : ptr[node + 1]]:
                if member[neighbor]:
                    local[neighbor] += 1
        stack = [start]
        while stack:
            node = stack.pop()
            visits.append(node)
            walks.append(walk)
            for neighbor in up[ptr[node] : ptr[node + 1]]:
                if not member[neighbor]:
                    continue
                if local[neighbor] == 1:
                    stack.append(neighbor)
                else:
                    local[neighbor] -= 1

        for node in found:
            member[node] = False
            local[node] = 0

    return np.asarray(visits, dtype=np.int64), np.asarray(walks, dtype=np.int64)
