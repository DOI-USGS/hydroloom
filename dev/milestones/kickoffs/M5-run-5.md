# Kickoff: M5, run 5

Base `ee5aee3` on `python-port`. Effort medium. Model claude-opus-5.5.

Written by the orchestrator from the `REVIEW_11042026` dispositions (both filled and
transcribed to `dev/plan/RULES.md` in `ee5aee3`, P-5 and B-4) and the open BACKLOG M5 rows
(P-5). Item 1 needs no unit: the `python_only` column and the S-4 sentence stand, and the
four `[python]` cases in the manifest are all B-5 raises with no anchor field. Item 2 is the
run: `capabilities` gets its R-only marks, its Python cases, and its export. Effort is
medium because the run carries no kernel and one function.

Who does what. The orchestrator builds nothing (P-5). Unit 1 is description, manifest, SPEC
and R test-helper work: `general-task-execution`, reviewed by `hl-style-reviewer` on the
unit's diff, one fix round; `hl-hydro-reviewer` reads the binding diff of
`hy_capabilities.md` since `d1d6317`, and its findings are `record` (frozen, P-4) unless
one shows a sentence the disposition added to be false. Unit 2 is a `port-function` run:
`hl-implementer`, then `hl-spec-reviewer` and `hl-style-reviewer` on the diff, one fix
round; the parity regeneration runs in R and goes to `general-task-execution` inside the
unit. Each unit is one Log entry in `dev/milestones/M5.md`, written by `hl-aggregator` in
mode `record`.

## Units, in dependency order

1. `capabilities` description and cases (description, cases, plan) — REVIEW_11042026
   item 2, B-4. The `hy_capabilities.md` Postcondition "in code-table order" states each
   language's order: R's code-table order, and in Python function-name order; the
   description's Changes line cites the disposition. The 14 shared `hy_capabilities` cases
   take `r_only: hy_capabilities.md §Algorithm`. Each gets a Python case on the same
   fixture and `args`, `supported_by: [python]`, `generator: [python]`, with an id ending
   `_python`; its comment cites B-4 and REVIEW_11042026 item 2, not B-5, so the case is
   not read as a B-5 raise. The parity generator
   (`tests/testthat/helper-conformance-parity.R`) reads a function whose every case is
   counted apart, with cases on both sides (`r_only` and `[python]`), as `r, python`, under
   the same `__all__` condition the all-`r_only` rule uses; the `parity.md` header sentence
   and SPEC S-4 say so (P-2). R conformance stays green; `parity.md` regenerates
   (`hy_capabilities` still reads `r`, since `capabilities` is not yet in `__all__`).
2. `capabilities` (Python) — the Python cases pass, with their expectation layers written
   by the Python generator into `conformance/expected/hy_capabilities.gpkg` beside the R
   layers; rows from the `@requires` registry in function-name order (B-4).
   `capabilities` joins `__all__` (A-18, B-4), and the changelog fragment relands with the
   unit 7 U7-03 wording ("giving the one conversion", "one row per hydroloom function").
   `parity.md` regenerates and `hy_capabilities` reads `r, python`.

## BACKLOG rows pulled in

- none. Not pulled in: the Linux `R CMD build .` check (no Linux environment on this
  machine: WSL is not installed and there is no Docker), and the `py-v0.1.0` release setup
  (David). Both stay in BACKLOG M5.

## Last logged read per description

| description | read SHA |
| --- | --- |
| hy_capabilities.md | `d1d6317`; frozen (P-4); unit 1 changes a Postcondition by disposition (P-1) |

## Done-when items the script does not check

- Every phase-1 manifest case passes under `pixi run test` on win-64. Three platforms, both
  Python versions, and `py-min-versions` need a push; report as not met and name the
  workflow files (`.github/workflows/py-test.yaml`, `.github/workflows/py-min-versions.yaml`).
- The parity table: report which phase-1 functions read `r, python`, and which still carry
  `[r]` cases with no `r_only` and why (the M6 exceptions for `to_flownetwork` cases).
- Permutation invariance holds on every fixture: rerun the property with `capabilities`
  added if it takes a table, and report any violation's BACKLOG row.
- `R CMD check --as-cran` on the tarball: report the result and which notes come from the
  environment.
- Not in this run, so report each as not met: the `hydroloom` name on PyPI and TestPyPI, the
  wheel installing clean in the release job, the Linux `R CMD build` check, and the release.
