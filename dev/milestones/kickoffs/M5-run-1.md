# Kickoff: M5, run 1

Base `7ffe18d77d85533fe3ef8b04c5188301e351efd0` on `python-port`. Effort high. Model claude-opus-5.5.

Who does what. The orchestrator builds nothing (P-5). Units 1 and 2 are description-side: they read `R/`, go to `general-task-execution`, and `hl-style-reviewer` reviews the unit's diff, one fix round. Unit 3 has no description, so `hl-implementer` writes it and `hl-style-reviewer` reviews it alone. Unit 4 is the exemplar slice, one `port-function` run with three descriptions. It is reviewed disproportionately (ROADMAP §1 M5): `hl-spec-reviewer` and `hl-style-reviewer` on the diff, then `hl-modeler-reader` on the composed docstrings of `as_hy`, `representation`, and `make_index_ids` against `.kiro/context/prose.md`. The Log flags it as an exemplar item for the second review. Every unit is one Log entry in `dev/milestones/M5.md`, written by `hl-aggregator` in mode `record`; the first entry creates the file.

## Units, in dependency order

1. Reference hydrofabric subset — a hydrogeofetch call fetches the sample, a block in `inst/extdata/conformance/fixtures/make_fixtures.R` writes the fixture and its index entry, and an `hy` manifest case on it with its expectation layer exercises the hydrofabric alias set (cases and expectations; S-2, D2-15).
2. `make_index_ids` toid-outside case — a flowline with one `toid` in the table and one outside it, as `make_index_ids.md` §Edge cases states from an R probe: a manifest case and an expectation layer (cases and expectations; P-3, M4 unit 7 SP-03).
3. `errors.py` — the three classes and `HydroloomWarning` (A-10, A-11) and the `remedies.json` lookup the raise sites use, nothing else (Python).
4. Exemplar slice — `as_hy` at the A-16 scope (vocabulary-alias renaming, I-2, I-3, I-5, I-12, G-14, the private `_as_hy` and `_hy_reverse`, A-17); `representation` (A-2); `_route` and the `@requires` registry (A-3, A-17); `make_index_ids` returning `IndexIds` with optional `main` and `edge_id` (K-3, A-16) on `_kernels/csr.py`; the A-13 per-function pass on the composed `make_index_ids` docstring; `__all__` and `supported_by: [r, python]` for the three functions (A-15, A-18) (Python).

## BACKLOG rows pulled in

- Reference hydrofabric subset fixture and an `hy` alias case (BACKLOG M5; §4 row not needed) — unit 1.
- `make_index_ids` case for a flowline with one `toid` in the table and one outside it (BACKLOG M5; §4 row not needed) — unit 2.
- A-13 language-neutral pass on the composed `make_index_ids` docstring (BACKLOG M5; §4 row not needed) — unit 4. The `navigate_network_dfs` half stays in the row for its port.

## Last logged read per description

| description | read SHA |
| --- | --- |
| hy.md | `7e21308`; frozen (P-4), no reader round in this run |
| hy_network_type.md | `9cfc1da`; frozen (P-4), no reader round in this run |
| make_index_ids.md | `7e21308`; frozen (P-4), no reader round in this run |
| hy_reverse.md | `7e21308`; frozen (P-4), read for `_hy_reverse` only; the public `hy_reverse` is the naming block |

## Done-when items the script does not check

- The phase-1 cases for `as_hy`, `representation`, and `make_index_ids` pass under `pixi run test` on win-64. All three platforms and both Python versions need a push, which a run never makes, so report this as not met and name the workflow file.
- `py-min-versions` at the floors: not run work. It needs a push; report as not met.
- The parity table shows `hy`, `hy_network_type`, and `make_index_ids` as `r, python`.
- The exemplar Log entry has the exemplar flag and every disposition cell filled.
- Not in this run, so report each as not met: permutation invariance (D-7), the naming block, `add_toids`, `sort_network` and `add_topo_sort`, `navigate_network_dfs`, `capabilities`, `is_dendritic`, the towncrier fragment, hatch-vcs, the Linux `R CMD build` check, and the release.
