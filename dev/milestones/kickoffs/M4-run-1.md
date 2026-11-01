# Kickoff: M4, run 1

Base `0ac648d860355d0ccf5c35a17c389fe47a067f8f` on `python-port`. Effort medium. Model claude-opus-5.5.

Who does what. The orchestrator builds nothing. Work outside `src/hydroloom/` and `tests/python/` (packaging, pixi, R and `dev/` scripts, CI, pre-commit, agent configuration) goes to `general-task-execution` and is reviewed by `hl-style-reviewer` on the unit's diff, one fix round. Python development goes to `hl-implementer`; units 2 and 4 have no description, so `hl-style-reviewer` alone reviews them, and unit 7 is the full `port-function` sequence. Every unit is one Log entry in `dev/milestones/M4.md` written by `hl-aggregator` in mode `record`.

## Units, in dependency order

1. Packaging and environments — `pyproject.toml` (`[project]`, hatchling per VERIFICATION §1.3, the pixi workspace and environments per §1.5, tasks `validate`, `lint`, `test`, `build`, `install`, `r-check`; `validate` wraps the M2 R-side checks), `pixi.lock`, the ruff rule set (§6), the import-linter contract (K-9), `.Rbuildignore` (§1.2), `.gitignore` and `.gitattributes` (§1.4), and the empty modules `src/hydroloom/__init__.py`, `_kernels/__init__.py`, `index_ids.py`, `_hy.py`, `network.py`. Starts by recording the `R CMD check --as-cran` result at the base so the Done-when comparison has a baseline.
2. Conformance harness — `tests/python/` reading the manifest and skipping every case with no Python implementation, `test_api.py` (A-18), `src/hydroloom/CLAUDE.md`, `tests/python/CLAUDE.md` (Python, `hl-implementer`).
3. Docstrings generator — the `dev/` script over `man/*.Rd` (A-13), `inst/extdata/docstrings.json`, a pixi task that regenerates it.
4. A-13 substitution table — the module that composes a numpydoc docstring from `docstrings.json` with the R-phrasing substitution table (Python, `hl-implementer`). Its own Log entry: `hl-modeler-reader` reads the table and its composed output against `.kiro/context/prose.md`, after `hl-style-reviewer`.
5. CI and pre-commit — `.pre-commit-config.yaml` with the schema validators (VERIFICATION §3), the path-filtered GitHub workflows that start and short-circuit including the three-platform `py-test` and `docstrings-diff` jobs, `.github/PULL_REQUEST_TEMPLATE.md`.
6. Implementer allowlist — narrow the `hl-implementer.json` shell allowlist to the pixi tasks units 1 to 5 define and the git commands the implementer needs, and correct `dev/FIXTURE_ARCHITECTURE.md` § Two sessions and a record to match.
7. `_kernels/csr.py` — `port-function` run: description `make_index_ids.md`, the `make_index_ids` cases, the K-2 construction with its hypothesis properties.

## BACKLOG rows pulled in

- Write `dev/milestones/kickoffs/M4-run-1.md` (BACKLOG M4; §4 row not needed) — this file.
- Narrow the `hl-implementer.json` shell allowlist (BACKLOG M4; §4 row not needed) — unit 6.

## Last logged read per description

| description | read SHA |
| --- | --- |
| make_index_ids.md | `7e21308`; frozen (P-4), no reader round in this run |

## Done-when items the script does not check

- `R CMD check --as-cran` on the tarball is as clean as at the base; `pixi run r-check` reproduces it.
- `pixi run test` runs the harness against zero implemented functions and reports every case skipped.
- `csr.py`'s properties pass.
- `test_api.py` passes against `supported_by`.
- `docstrings.json` regenerates with no diff.
- The A-13 substitution table is its own Log entry, read against prose.md.
- The stub is on PyPI. Not run work: the M4 Deliverable puts the PyPI name, accounts, and tokens outside M4 and BACKLOG M5 holds the release setup; reported as not met.
- A Windows, macOS, and Linux `py-test` job goes green on the empty package. Needs a push, which a run never makes; reported as not met with the workflow file named.
