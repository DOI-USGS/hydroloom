# hydroloom Python port — BACKLOG

Work found during a run that lands outside the run's units (`.kiro/context/orchestrator.md`, bin 2). One row per item under the milestone it blocks; a milestone starts with the maintainer reading its section and moving each row into the kickoff, into ROADMAP §4 where the `§4` column says so, or down to a later milestone. An item no milestone blocks sits under "Any time". A row leaves this file in the commit that does the work, with the row's source cited in the commit message. ROADMAP §3 rows closed before 2026-10-30 stay in git history only.

Columns: Item (what, with the fixed point it waits for); Source (the disposition, finding, or review that put it here); Rule (what decides it, or `—` for the maintainer); §4 (`yes` where it is a user-visible R change needing a ROADMAP §4 row before it lands); Owner.

## M4

| Item | Source | Rule | §4 | Owner |
| --- | --- | --- | --- | --- |
| Write `dev/milestones/kickoffs/M4-run-1.md` from `.kiro/context/kickoff-template.md`: the M4 units in order, base SHA, effort, no BACKLOG rows, no description reads. First run under the three bins, P-5, and `dev/done_check.R`; its `REVIEW_<date>.md` length is the test of the 2026-10-30 process changes (`dev/three_bins_plan.md`, `dev/orchestrator_rules_2_plan.md`, gitignored). | 2026-10-30 process read | P-5 | no | David |

## M5

| Item | Source | Rule | §4 | Owner |
| --- | --- | --- | --- | --- |
| Reference hydrofabric subset fixture and an `hy` alias case for the hydrofabric alias set, fetched by a hydrogeofetch call before the exemplar run; nothing else exercises those aliases. | D2-15 | S-2 | no | M5 run |
| `supported_by: [python]` raise cases on `sort_network_dups.gpkg` (repeated `id`/`toid` pair) and the `check_hy_graph` Python-only case, added when `generator: [python]` has something to run; keeps the M2 parity invariant until then. | D3-217, D3-210, D3-166 | B-5 | no | M5 run |
| `register_names()` tie rule when several aliases map to one hydroloom name: which alias wins, whether Python raises. Lands with the naming block. | navigate_network_dfs reader H-02 (2026-10-29), D3-15 | — | no | maintainer at M5 start |
| Release setup for `py-v0.1.0`: PyPI and TestPyPI accounts, project-scoped token, `py-v*` protected tag, code.usgs.gov token variables and egress, DOI certificate as a file-type variable. Steps in VERIFICATION §7. Outside the agent process. | ROADMAP §3 (deferred 2026-10-25) | — | no | David |

## M6

| Item | Source | Rule | §4 | Owner |
| --- | --- | --- | --- | --- |
| `is_dendritic()` on a node table with divergences shown only by a repeated `fromnode` and no `divergence` column returns `TRUE`; detection lands with `add_divergence`, which needs it. Python aligns names first (D3-29, D3-197); whether R should align names too is open. | D3-34, D3-197 | B-1, B-4 | yes | M6 run; maintainer on R alignment |
| R side of the repeated (`id`, `toid`) pair raise at ingest, where R still warns or waits. | D3-57, M3 Summary | B-1, B-5 | yes | M6 run |
| A-4 code table rows corrected as each description lands and the A-4 test reaches it: `add_pathlength` gains `length_km`, the flownetwork `make_node_topology` via key, `add_pfafstetter` drainage-area columns, `get_confluences` `aggregate_id` and `length_km`. | D3-70, hy_capabilities H-08 and H-09 (2026-10-26) | A-4 | no | M6 run |
| Eight 3DHP Flowline fields aliased as the description that reads each lands: `dnlevelpath`, `uplevelpath`, `arbolatesum`, `terminalpath`, `streamorder`, `streamcalculator`, `terminalflag`, `streamlevel`. | D3-205 | B-4 | no | M6 run |
| `remedies.json` read by both readers, in the milestone where R starts reading the catalog: the five agent-drafted keys (`flag_value_not_binary`, `divergence_zero_main_path`, `divergence_minor_only_path`, `identifier_not_finite`, `id_toid_pair_repeated`) with the `NaN` choice; the disagreements D3-153 lists (`no_id_column`, `outlet_fill_type_mismatch`, `not_hy_object`, `needs_topo`, `divergence_required`, `raised_by` lists); reader findings held on it (hy_reverse H-04, H-12; hy_network_type H-04; add_toids H-03; make_index_ids H-01; sort_network H-05, H-06; check_hy_graph H-03; I9-3). Then the description Remedies tables are aligned by an agent (D3-146). | D3-178, D3-153, D3-180, D3-191, D3-142 | P-4 | no | David writes, readers read |
| WBD dissolve benchmark, run before the `dissolve_polygons` implementation is chosen. | ROADMAP §3 | — | no | M6 run |

## M7

| Item | Source | Rule | §4 | Owner |
| --- | --- | --- | --- | --- |
| `vignettes/articles/flow-table.Rmd` around line 70 still describes `loop_check`, removed by D-8. | D3-88 | P-2 | no | David |

## Any time

| Item | Source | Rule | §4 | Owner |
| --- | --- | --- | --- | --- |
| `definition` edits in `hy_vocab.json` from the M1 findings marked "deferred to maintainer" (M1.md Log, H-04 to M-38), the hydrology reader's corrections (D3-66, `.kiro/workflow-output/M2-M3/hydroloom_name_definitions/findings-description.md` H-04 to H-13), and removal of `DRAFT:` from each accepted `plain` sentence; then the `hydroloom_name_definitions` expectation is regenerated. | REVIEW_10272026 part 8 | P-4 | no | David |
| Roxygen beyond correctness for `hy_network_type`, `navigate_network_dfs`, `is_dendritic`, `is.hy`. | D3-68 | P-4 | no | David |
| References citations the readers could not source: the 3DHP field names and outlet convention; NHDPlusHR's `Divergence` coding beside NHDPlusV2's; Kahn 1962 beside `Hydroseq` in add_topo_sort; 3DHP and Mainstems sources in hydroloom_names. | hy H-08, add_toids H-09, sort_network H-09, hydroloom_names H-02, add_topo_sort H-08 (2026-10-29) | P-4 | no | David |
| Co-maintainer from the gdptools side: identified, not formally involved. Exemplar and named-kernel Log items wait for that second review. | ROADMAP §3 | — | no | David |
