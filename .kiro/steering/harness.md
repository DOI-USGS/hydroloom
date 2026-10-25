---
inclusion: fileMatch
fileMatchPattern: "tests/python/**"
---
# Harness rules (SPEC S-7, D-7, D-13, D-14, A-18; VERIFICATION §2, §4)

- Conformance tests are driven by `inst/extdata/conformance/manifest.yaml`; case ids are
  pytest ids. The harness only compares; it writes expectations only for a function whose
  manifest entry names Python as co-generator.
- An expectation about a computed value lives in the manifest, never inline in a test (S-7).
- Compare after coercing each column to the dtype the vocabulary type declares; `atol` per
  type, `rtol = 0` (D-13); row order only where the manifest says `order: exact` (D-14);
  long-table returns are sorted on the manifest `key` before comparison.
- Property tests come from the description's Postconditions; permutation invariance (D-7)
  runs on every manifest function; `derandomize=True` in CI; a shrunk failure becomes a
  synthetic fixture and a manifest case, never a tolerance change.
- `test_api.py` asserts `hydroloom.__all__` equals the manifest functions whose
  `supported_by` includes `python` (A-18).
- Every public function has a hand-written doctest; `--doctest-modules` runs it.
- Interface tests (argument validation, error classes, ingest dtypes) are language-local
  and live here; semantics live in the manifest.
