---
inclusion: fileMatch
fileMatchPattern: ["src/hydroloom/_kernels/**", "src/hydroloom/index_ids.py"]
---
# Kernel rules (SPEC K, D-3, D-5, A-14)

- Below the boundary: `indptr`, `indices`, attribute arrays, scalars. Nothing else crosses (K-1).
- CSR is canonical: `indptr` length n+1, `indices` length m; the ELLPACK view is derived by
  repeat and reshape (K-2). `IndexIds` is the one object that crosses as-is and may carry
  optional `main` and `edge_id` arrays aligned with `indices` (K-3).
- Sentinel: CSR has none; a terminal is an empty range. In dense per-node arrays the
  sentinel is −1. Never 0 (K-4).
- A kernel never reaches upward: no identifiers, no column names, no vocabulary, no domain
  errors (K-5).
- Index arrays are `int64` dense positions 0..n−1, never identifier values; weights are
  `float64` (K-6).
- Single-threaded and deterministic (K-8). No numba, no compiled code (K-7).
- `hydroloom._kernels` imports neither pandas, geopandas, shapely, nor `hydroloom._hy`;
  import-linter enforces it (K-9).
- Every sort is stable: `np.argsort(kind="stable")` (D-3). Neighbor lists are in identifier
  byte order (D-5).
- mypy strict here; signatures use `NDArray[np.int64]` and `NDArray[np.float64]` (A-14).
- Named kernels (`sequential.py`, `geom.py`, `string.py`) carry the cross-language signature
  their description states. `csr.py`, `walk.py`, `accumulate.py` are language-local.
