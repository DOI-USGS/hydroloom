# Pattern files — read before writing

Name the file you will copy from before you write a line. One pattern per concern.

<!-- Packages, not file paths, until the exemplar exists (Q7, 2026-10-25): replace with paths in the commit that merges M5's first PR. -->

- CSR construction and kernel shape: pyflwdir's core flow-direction and path kernels —
  how a kernel takes `indptr`/`indices` and attribute arrays and returns arrays.
- Public signatures: pynhd's public functions — frame in, frame out, keyword-only
  options, no classes for stateless operations.
- Harness primitives: `geopandas.testing.assert_geodataframe_equal` and
  `pandas.testing.assert_frame_equal` with `check_dtype=True`.
- Argument conventions in this community: gdptools' public module.
- Routing and representation: the hydroloom exemplar — `src/hydroloom/_hy.py`,
  `network.py`, `index_ids.py`, `_kernels/csr.py`. Once it exists it outranks the
  external files above.
