# PR checklist — what no linter sees

Mechanical gates (ruff `PD NPY B UP I D`, mypy strict on kernels, import-linter, pytest
with manifest ids) run first. This list is the rest.

- A-8 — never mutates; no `inplace=`; same class returned; geometry never dropped or added
  as a side effect.
- I-1, I-2 — every identifier column is `str` on ingest and on return; nothing widens or
  casts it back to a number.
- I-9 — no nullable extension dtype for a computed integer attribute, which is complete
  `int32`; a flag column with a missing value is nullable `boolean`.
- D-3 — `kind="stable"` on every `sort_values` and `argsort`.
- K-5 — no kernel reaches upward.
- A-9 — function and argument names copied from R; data first, then `*`; first argument is
  `network` (phase 1–2) or the operand noun (phase 3).
- A-10 — error text from `remedies.json` by key; an argument naming an absent column raises
  `KeyError(column)` with no key.
- A-11 — every `warnings.warn` uses the first frame outside hydroloom as its stack
  level, not a fixed number (REVIEW_11022026 item 5); the package warning class.
- A-13 — docstring composed from `docstrings.json`; hand-written doctest present; no R
  phrasing left (`data.frame`, `logical`, `sf`); roxygen R-only facts in their own "In R,"
  sentence (no "e.g."/"i.e.") or "In R," paragraph, every other sentence true in both
  languages (REVIEW_11022026 item 8).
- A-18 — `__all__` updated; `test_api.py` passes.
- Helper reuse — no new private helper without checking `_hy.py` and `_kernels/csr.py`
  for an existing one.
- `changelog.d/` fragment present.
- The merge question — would a pandas or geopandas maintainer merge this as written.
  Answer it in one sentence at the end of the findings.
