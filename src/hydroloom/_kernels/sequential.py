"""Named sequential kernels (SPEC K-6, K-7): the many-start DFS.

Below the frame boundary (K-1, K-9). Each kernel here carries the signature
its algorithm description states, the same in both languages (K-6, K-10).
Nodes are dense positions ``0 .. n - 1``; identifiers never enter. Kernels are
plain loops over arrays (K-7), single-threaded and deterministic (K-8).
"""

from __future__ import annotations

import numpy as np
from numpy.typing import NDArray

__all__ = ["dfs_paths"]

# The stack entry that marks a break (navigate_network_dfs.md Algorithm step 6,
# K-4: -1, never 0).
_BREAK = -1


def dfs_paths(
    indptr: NDArray[np.int64],
    indices: NDArray[np.int64],
    starts: NDArray[np.int64],
    main: NDArray[np.bool_] | None,
    reset: bool,
) -> tuple[NDArray[np.int64], NDArray[np.int64], NDArray[np.int64]]:
    """Walk depth first from each start, splitting each walk into paths.

    navigate_network_dfs.md Signature and Algorithm step 4. A node is
    visited the first time it is popped. Expanding a node with no neighbor
    pushes one break; expanding any other node pushes, for each neighbor slot
    in reverse CSR order, a break if the slot is already followed, else the
    neighbor, then marks every slot of the node followed. With ``main``, only
    unfollowed slots whose flag is true are pushed, and no break. A popped
    break ends the current path at the next popped node. With ``reset``,
    visited nodes and followed slots are cleared before each start.

    Parameters
    ----------
    indptr, indices : NDArray[np.int64]
        The CSR graph in the walk direction, each neighbor list in the order
        siblings are taken (identifier byte order, D-5, D-6).
    starts : NDArray[np.int64]
        Dense positions, unique, in the order they are walked.
    main : NDArray[np.bool_] or None
        Length m, aligned with ``indices``; when given, only slots flagged
        true are followed.
    reset : bool
        Clear visited nodes and followed slots before each start.

    Returns
    -------
    visit : NDArray[np.int64]
        Dense positions in visit order, all starts concatenated.
    path : NDArray[np.int64]
        Aligned with ``visit``, the 0-based path within its start.
    start_ptr : NDArray[np.int64]
        Length k + 1; the visits of start j are
        ``visit[start_ptr[j]:start_ptr[j + 1]]``.
    """
    ptr = np.asarray(indptr, dtype=np.int64).tolist()
    nbr = np.asarray(indices, dtype=np.int64).tolist()
    flags = None if main is None else np.asarray(main, dtype=bool).tolist()
    n = len(ptr) - 1
    m = len(nbr)

    visited = [False] * n
    followed = [False] * m
    visit: list[int] = []
    path: list[int] = []
    start_ptr = [0]
    for start in np.asarray(starts, dtype=np.int64).tolist():
        if reset:
            visited = [False] * n
            followed = [False] * m

        # LIFO stack (K-8); the break flag and current path length decide
        # where a path closes.
        stack = [start]
        broken = False
        current = 0
        number = 0
        while stack:
            node = stack.pop()
            if node == _BREAK:
                broken = True
                continue
            if broken:
                if current:
                    number += 1
                    current = 0
                broken = False
            if not visited[node]:
                visited[node] = True
                visit.append(node)
                path.append(number)
                current += 1

            lo, hi = ptr[node], ptr[node + 1]
            if lo == hi:
                # A terminal pushes one break on every arrival (D3-156).
                if flags is None:
                    stack.append(_BREAK)
                continue
            for slot in range(hi - 1, lo - 1, -1):
                if flags is None:
                    stack.append(_BREAK if followed[slot] else nbr[slot])
                elif flags[slot] and not followed[slot]:
                    stack.append(nbr[slot])
                followed[slot] = True

        start_ptr.append(len(visit))

    return (
        np.asarray(visit, dtype=np.int64),
        np.asarray(path, dtype=np.int64),
        np.asarray(start_ptr, dtype=np.int64),
    )
