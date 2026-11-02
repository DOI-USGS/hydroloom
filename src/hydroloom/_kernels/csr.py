"""Compressed sparse row build, transpose, and ELL view (SPEC K-2).

Language-local kernel below the frame boundary (K-1, K-9). Nodes are dense
positions ``0 .. n - 1``; identifiers never enter. Neighbor order comes from an
integer ``rank`` per node that the caller computes above the boundary, the
identifier byte-order rank in ``make_index_ids`` (D-5). Per-edge payload such
as ``main`` or ``edge_id`` follows its edge through the returned ``order``:
``payload[order]`` is aligned with ``indices``.
"""

from __future__ import annotations

import numpy as np
from numpy.typing import NDArray

__all__ = ["ell", "from_edges", "transpose"]


def from_edges(
    source: NDArray[np.int64],
    target: NDArray[np.int64],
    n: int,
    rank: NDArray[np.int64],
) -> tuple[NDArray[np.int64], NDArray[np.int64], NDArray[np.int64]]:
    """Build CSR from an edge list.

    Parameters
    ----------
    source, target : NDArray[np.int64]
        Dense positions in ``[0, n)`` of each edge's owning node and neighbor,
        length m.
    n : int
        Number of nodes.
    rank : NDArray[np.int64]
        Sort key per node, length n. Each neighbor list is in ``rank`` order;
        edges with equal neighbor rank keep their input order (D-3).

    Returns
    -------
    indptr : NDArray[np.int64]
        Length n + 1; the neighbors of node i are ``indices[indptr[i]:indptr[i + 1]]``.
    indices : NDArray[np.int64]
        Length m, neighbor positions.
    order : NDArray[np.int64]
        Length m, the input edge in each CSR slot; ``payload[order]`` aligns a
        per-edge array with ``indices``.
    """
    source = np.asarray(source, dtype=np.int64)
    target = np.asarray(target, dtype=np.int64)
    rank = np.asarray(rank, dtype=np.int64)

    # One stable argsort on (owning node, neighbor rank) (K-2, D-3, D-5).
    key = source * np.int64(max(n, 1)) + rank[target]
    order = np.argsort(key, kind="stable").astype(np.int64)

    counts = np.bincount(source, minlength=n)
    indptr = np.zeros(n + 1, dtype=np.int64)
    np.cumsum(counts, out=indptr[1:])

    return indptr, target[order], order


def transpose(
    indptr: NDArray[np.int64],
    indices: NDArray[np.int64],
    rank: NDArray[np.int64],
) -> tuple[NDArray[np.int64], NDArray[np.int64], NDArray[np.int64]]:
    """Build the CSR of the reversed graph.

    Parameters
    ----------
    indptr, indices : NDArray[np.int64]
        The CSR to reverse.
    rank : NDArray[np.int64]
        Sort key per node, as in `from_edges`.

    Returns
    -------
    indptr, indices : NDArray[np.int64]
        The reversed graph, each neighbor list in ``rank`` order.
    order : NDArray[np.int64]
        Length m, the slot of the input CSR in each output slot;
        ``payload[order]`` carries a payload aligned with the input ``indices``.
    """
    indptr = np.asarray(indptr, dtype=np.int64)
    n = len(indptr) - 1
    owner = np.repeat(np.arange(n, dtype=np.int64), np.diff(indptr))

    return from_edges(np.asarray(indices, dtype=np.int64), owner, n, rank)


def ell(
    indptr: NDArray[np.int64], indices: NDArray[np.int64]
) -> tuple[NDArray[np.int64], NDArray[np.int64]]:
    """Return the padded ELLPACK view of a CSR.

    Parameters
    ----------
    indptr, indices : NDArray[np.int64]
        The CSR.

    Returns
    -------
    matrix : NDArray[np.int64]
        Shape (n, k) for the largest neighbor count k; row i holds node i's
        neighbors in CSR order, padded with -1 (K-4).
    lengths : NDArray[np.int64]
        Length n, neighbors per node.
    """
    indptr = np.asarray(indptr, dtype=np.int64)
    indices = np.asarray(indices, dtype=np.int64)
    n = len(indptr) - 1
    lengths = np.diff(indptr)
    width = int(lengths.max()) if n else 0

    rows = np.repeat(np.arange(n, dtype=np.int64), lengths)
    cols = np.arange(len(indices), dtype=np.int64) - np.repeat(indptr[:-1], lengths)
    matrix = np.full((n, width), -1, dtype=np.int64)
    matrix[rows, cols] = indices

    return matrix, lengths
