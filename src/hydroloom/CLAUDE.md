# src/hydroloom — agent notes

This tree is the Python package in the standard src layout (SPEC K-9). Rules for every agent are in `AGENTS.md`; these notes add what binds a session working here.

The contract for a public function is its algorithm description, `inst/extdata/conformance/algorithms/<function>.md`, and the SPEC anchors it cites; the expectation GeoPackage `inst/extdata/conformance/expected/<function>.gpkg` is the oracle. A Python-side session never opens `R/` or `tests/testthat/`.

`_kernels/` holds the array kernels below the frame boundary. A kernel imports neither pandas, geopandas, shapely, nor `hydroloom._hy`; the import-linter contract in `pyproject.toml` enforces it (K-9). A kernel the description names binds by its signature (K-6); one it does not name is the author's to shape. mypy is strict on `_kernels` and `index_ids.py` and standard elsewhere, with no `pandas-stubs` (A-14).

Error text comes from `inst/extdata/remedies.json` by key with `str.format` placeholders, never typed at the raise site (A-10). The vocabulary, abbreviations, and remedies are read through `importlib.resources.files("hydroloom.data")`, never a path built from `__file__` (VERIFICATION §1.3); `hydroloom/data/` holds copies of the `inst/extdata/` files, so edit the originals and run `pixi run install`. Docstrings are composed at import from `docstrings.json` (A-13); do not write a Parameters section by hand.

`hydroloom.__all__` holds exactly the manifest functions whose `supported_by` includes `python`; `tests/python/test_api.py` asserts it (A-18).

Done means `pixi run lint` (ruff check, ruff format check, import-linter, mypy) and `pixi run test` (pytest over `tests/python/`) both pass. Run them yourself; hooks do not fire in sub-agents.
