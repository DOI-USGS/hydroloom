"""Dense index construction for hydroloom networks (SPEC K-2, K-3).

`IndexIds` is the CSR graph `make_index_ids` returns and the one object that
crosses the kernel boundary as-is. The identifier byte-order rank that orders
each neighbor list (D-5) is computed here, above the boundary.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

import numpy as np
import pandas as pd  # type: ignore[import-untyped]
from numpy.typing import NDArray

from hydroloom._kernels import csr

__all__ = ["IndexIds"]


def _byte_rank(uniques: Any) -> NDArray[np.int64]:
    """Return the rank of each identifier in byte order (D-1, D-5).

    Python compares ``str`` by code point, which is UTF-8 byte order.
    """
    values = np.asarray(uniques, dtype=object)
    order = np.argsort(values, kind="stable")
    rank = np.empty(len(values), dtype=np.int64)
    rank[order] = np.arange(len(values), dtype=np.int64)

    return rank


@dataclass(frozen=True, eq=False)
class IndexIds:
    """A network graph in compressed sparse row form (SPEC K-3).

    The neighbors of dense node ``i`` are ``indices[indptr[i]:indptr[i + 1]]``,
    in identifier byte order (D-5). ``uniques[i]`` is the ``id`` of node ``i``.
    In mode ``"to"`` the neighbors are the downstream flowlines, in mode
    ``"from"`` the upstream ones. A terminal has an empty range (K-4).
    Equality and hashing are by object identity, since the fields are arrays.

    Attributes
    ----------
    indptr : NDArray[np.int64]
        Length n + 1.
    indices : NDArray[np.int64]
        Length m, dense positions.
    uniques : pandas.Index
        Length n, the ``id`` of each dense position.
    mode : str
        ``"to"`` or ``"from"``.
    main : NDArray[np.bool_] or None
        Length m, the main-path flag of each connection, aligned with
        ``indices``.
    edge_id : NDArray[np.object_] or None
        Length m, an identifier per connection; always ``None`` for now.
    """

    indptr: NDArray[np.int64]
    indices: NDArray[np.int64]
    uniques: pd.Index
    mode: str
    main: NDArray[np.bool_] | None = None
    edge_id: NDArray[np.object_] | None = None

    def transpose(self) -> IndexIds:
        """Return the graph with every connection reversed.

        ``main`` and ``edge_id`` follow their connection.

        Returns
        -------
        IndexIds
            The other mode of the same graph.

        Examples
        --------
        >>> import numpy as np
        >>> import pandas as pd
        >>> index = IndexIds(
        ...     np.array([0, 1, 1]), np.array([1]), pd.Index(["a", "b"]), "to"
        ... )
        >>> index.transpose().indptr.tolist()
        [0, 0, 1]
        """
        indptr, indices, order = csr.transpose(
            self.indptr, self.indices, _byte_rank(self.uniques)
        )
        mode = "from" if self.mode == "to" else "to"
        main = None if self.main is None else self.main[order]
        edge_id = None if self.edge_id is None else self.edge_id[order]

        return IndexIds(indptr, indices, self.uniques, mode, main, edge_id)

    def ell(self) -> tuple[NDArray[np.int64], NDArray[np.int64]]:
        """Return the padded view and the neighbor count of each node (K-2).

        Returns
        -------
        matrix : NDArray[np.int64]
            Shape (n, k); row i holds node i's neighbors, padded with -1.
        lengths : NDArray[np.int64]
            Length n.
        """
        return csr.ell(self.indptr, self.indices)

    def to_scipy(self) -> Any:
        """Return the graph as a ``scipy.sparse.csr_array``.

        scipy may copy ``indptr`` and ``indices`` to a narrower index dtype.

        Returns
        -------
        scipy.sparse.csr_array
            Shape (n, n), one stored ``True`` per connection.
        """
        from scipy.sparse import csr_array  # type: ignore[import-untyped]

        n = len(self.uniques)
        data = np.ones(len(self.indices), dtype=bool)

        return csr_array((data, self.indices, self.indptr), shape=(n, n))


def _from_edges(
    uniques: Any,
    source: NDArray[np.int64],
    target: NDArray[np.int64],
    mode: str,
    main: NDArray[np.bool_] | None = None,
) -> IndexIds:
    """Build an `IndexIds` from dense edge positions.

    ``source`` is the node that lists ``target`` as a neighbor; ``main`` is
    aligned with the edges as given.
    """
    indptr, indices, order = csr.from_edges(
        source, target, len(uniques), _byte_rank(uniques)
    )
    flags = None if main is None else np.asarray(main, dtype=bool)[order]

    return IndexIds(indptr, indices, pd.Index(uniques), mode, flags, None)
