# tests/python — agent notes

This tree holds pytest for the Python package: the conformance harness and the language-local interface tests. Rules for every agent are in `AGENTS.md`; these notes add what binds a session working here.

`conformance.py` reads `inst/extdata/conformance/manifest.yaml` (SPEC S-4) and holds the converters and the comparison. `test_conformance.py` runs one item per manifest case, with the case id as the pytest id, and skips a case whose function is not in `hydroloom.__all__` or whose `supported_by` lacks `python`. `test_harness.py` tests the converters and the comparison on the stored expectation layers. `test_api.py` asserts `hydroloom.__all__` equals the manifest functions whose `supported_by` includes `python` (A-18).

The harness compares against `inst/extdata/conformance/expected/<function>.gpkg`, one layer per case. It writes a layer only when `HYDROLOOM_WRITE_EXPECTED=1` and the case's `generator` lists `python` (S-7). An expectation about a computed value lives in the manifest, never inline in a test (S-7). Comparison follows S-7 and VERIFICATION §2: each column is coerced to the dtype its vocabulary type declares, identifiers compare as text, a `flag` column compares in its stored type, a `count` or `code` outside the 32-bit range compares as double (I-15). `compare: tolerance` applies the D-13 `atol` per type with `rtol = 0`; row order binds only under `order: exact`, and graph and traversal layers are sorted on the manifest `key` (D-14).

Interface tests (argument validation, error classes, ingest dtypes) live here; semantics live in the manifest. Property tests come from a description's Postconditions and run with `derandomize=True`; a shrunk failure becomes a synthetic fixture and a manifest case, never a tolerance change (VERIFICATION §4).

A Python-side session never opens `R/` or `tests/testthat/` (AGENTS rule 1). Error text checked by a case comes from `inst/extdata/remedies.json` by its `raises_key` (A-10). Package code under `src/hydroloom/_kernels/` imports no pandas, geopandas, shapely, or `hydroloom._hy` (K-9).

Done means `pixi run lint` and `pixi run test` both pass. Run them yourself; hooks do not fire in sub-agents.
