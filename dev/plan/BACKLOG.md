# hydroloom Python port — BACKLOG

Work found during a run that lands outside the run's units (`.kiro/context/orchestrator.md`, bin 2). One row per item under the milestone it blocks; a milestone starts with the maintainer reading its section and moving each row into the kickoff, into ROADMAP §4 where the `§4` column says so, or down to a later milestone. An item no milestone blocks sits under "Any time". A row leaves this file in the commit that does the work, with the row's source cited in the commit message. ROADMAP §3 rows closed before 2026-10-30 stay in git history only.

Columns: Item (what, with the fixed point it waits for); Source (the disposition, finding, or review that put it here); Rule (what decides it, or `—` for the maintainer); §4 (`yes` where it is a user-visible R change needing a ROADMAP §4 row before it lands); Owner.

## M4

| Item | Source | Rule | §4 | Owner |
| --- | --- | --- | --- | --- |

## M5

| Item | Source | Rule | §4 | Owner |
| --- | --- | --- | --- | --- |
| Reference hydrofabric subset fixture and an `hy` alias case for the hydrofabric alias set, fetched by a hydrogeofetch call before the exemplar run; nothing else exercises those aliases. | D2-15 | S-2 | no | M5 run |
| `supported_by: [python]` raise cases on `sort_network_dups.gpkg` (repeated `id`/`toid` pair) and the `check_hy_graph` Python-only case, added when `generator: [python]` has something to run; keeps the M2 parity invariant until then. | D3-217, D3-210, D3-166 | B-5 | no | M5 run |
| `register_names()` tie rule when several aliases map to one hydroloom name: which alias wins, whether Python raises. Lands with the naming block. | navigate_network_dfs reader H-02 (2026-10-29), D3-15 | — | no | maintainer at M5 start |
| `R CMD build .` on Linux from a checkout with `src/` (the `.gitlab-ci.yml` `rc/*` tarball job) relies on the no-op `src/Makefile` and `src/install.libs.R`, which have been run only on win-64, and stamps `NeedsCompilation: yes` while `pixi run r-check` checks a pkgbuild-copy tarball stamped `no`. Build once on Linux before the next `rc/*` branch, record the value, and either accept the difference or make the release job build the way `r-check` does. | M4 unit 1 S-03 | AGENTS § CI and release | no | M5 run |
| Release setup for `py-v0.1.0`: PyPI and TestPyPI accounts, project-scoped token, `py-v*` protected tag, code.usgs.gov token variables and egress, DOI certificate as a file-type variable. Steps in VERIFICATION §7. Outside the agent process. | ROADMAP §3 (deferred 2026-10-25) | — | no | David |
| A-13 language-neutral pass on the composed `navigate_network_dfs` docstring, in its port: bare `x` in running text for `network`; "the `main` element must be included" names an R list element, not the `IndexIds` attribute. The `make_index_ids` half landed in M5 run 1 unit 4. | M4 unit 4 modeler M-06, M-16 | A-13 | no | M5 run |
| towncrier setup (`changelog.d/`, the pixi task) and the first fragment, covering `hydroloom.errors` and the exemplar slice's public functions; until then a Python diff has no fragment to add. | M5 run 1 unit 3 S-03; M4 run 1 unit 4 S-08 | ROADMAP §1 M5 | no | M5 run |
| Exemplar follow-ups the second code review left as `should`, before a later port copies them: a function registered with no needs whose `native` has no body for the input's pattern raises a bare `KeyError` where A-3 names `RepresentationError` (fall back to the `hy` body); `IndexIds` is `@dataclass(frozen=True)` over numpy arrays, so `==`, `hash()`, and `in` raise (`eq=False`); `make_index_ids` checks `mode` after ingest and routing, so a bad `mode` on a node table raises `RepresentationError` instead of `ValueError` (check first). | M5 run 1 unit 4 style round 2 S-16, S-17, S-18 | A-3; checklist The merge question | no | M5 run |
| Shared composer text in public docstrings, before `py-v0.1.0`: the `network` parameter text names `register_names`, which Python lacks until the naming block, and implies a raw NHDPlus table must be registered first; See Also points to the R function under its R name (`hy`, `hy_network_type`), which a Python reader cannot follow. | M5 run 1 unit 4 modeler M-05, M-08; style round 2 S-19 | A-13 | no | M5 run |

## M6

| Item | Source | Rule | §4 | Owner |
| --- | --- | --- | --- | --- |
| `is_dendritic()` on a node table with divergences shown only by a repeated `fromnode` and no `divergence` column returns `TRUE`; detection lands with `add_divergence`, which needs it. Python aligns names first (D3-29, D3-197); whether R should align names too is open. | D3-34, D3-197 | B-1, B-4 | yes | M6 run; maintainer on R alignment |
| R side of the repeated (`id`, `toid`) pair raise at ingest, where R still warns or waits. | D3-57, M3 Summary | B-1, B-5 | yes | M6 run |
| A-4 code table rows corrected as each description lands and the A-4 test reaches it: `add_pathlength` gains `length_km`, the flownetwork `make_node_topology` via key, `add_pfafstetter` drainage-area columns, `get_confluences` `aggregate_id` and `length_km`. | D3-70, hy_capabilities H-08 and H-09 (2026-10-26) | A-4 | no | M6 run |
| Eight 3DHP Flowline fields aliased as the description that reads each lands: `dnlevelpath`, `uplevelpath`, `arbolatesum`, `terminalpath`, `streamorder`, `streamcalculator`, `terminalflag`, `streamlevel`. | D3-205 | B-4 | no | M6 run |
| `remedies.json` read by both readers, in the milestone where R starts reading the catalog: the five agent-drafted keys (`flag_value_not_binary`, `divergence_zero_main_path`, `divergence_minor_only_path`, `identifier_not_finite`, `id_toid_pair_repeated`) with the `NaN` choice; the disagreements D3-153 lists (`no_id_column`, `outlet_fill_type_mismatch`, `not_hy_object`, `needs_topo`, `divergence_required`, `raised_by` lists); reader findings held on it (hy_reverse H-04, H-12; hy_network_type H-04; add_toids H-03; make_index_ids H-01; sort_network H-05, H-06; check_hy_graph H-03; I9-3). Then the description Remedies tables are aligned by an agent (D3-146). | D3-178, D3-153, D3-180, D3-191, D3-142 | P-4 | no | David writes, readers read |
| WBD dissolve benchmark, run before the `dissolve_polygons` implementation is chosen. | ROADMAP §3 | — | no | M6 run |
| A-13 language-neutral pass on the composed `index_points_to_lines` and `accumulate_downstream` docstrings, in each function's port: bare `x` for `network`; Note 4 points to `sf::st_segmentize()` and `search_radius` to RANN nn2, both R-only; `search_radius` is a bare float in Python, so "conversion to the linear unit ... is attempted" no longer holds and the unit is not stated; `point_id` is "the row or list element", an R sfc reading; the `accumulate_downstream` description says "See details for required columns" and the details no longer carry that line (A-13 drops it). | M4 unit 4 modeler M-06, M-18 to M-21, M-27 | A-13 | no | M6 run |
| `to_flownetwork` roxygen puts `Required attributes:` on its own line with the attribute list in the paragraphs after it, so `dev/docstrings.R` drops only the heading and the `details` field opens with an orphaned list; reshape to the one-line form the other 20 Rd files use (keeping the `divergence` and `levelpath` definitions as prose), then `pixi run docstrings`. Before the `to_flownetwork` Python port. | M4 unit 3 S-02 | A-13, P-4 | no | David |

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
| DESIGN §7 "Tolerances anchored on meter-scale source precision" says hydrographic coordinates are good to about a meter, which does not hold as positional accuracy for NHDPlusV2 (1:100k) or NHDPlus HR (1:24k); the reason `dev/FIXTURE_ARCHITECTURE.md` gives is that both implementations compute from the same coordinates (D-13). | M3-2 H-24 | P-2 | no | David |
| SPEC I-4 calls the row a terminal ("A terminal is a row whose `toid` is not present in `id`") where RULES B-4 gives "terminal" to the `toid` outside the table and "outlet" to the feature; one wording for both files. | M3-2 H-03 | B-4 | no | David |
| Roxygen beyond correctness the A-13 modeler read found, for a Python reader who meets these sentences in the composed docstrings: `make_index_ids` `mode` ("character indicating the mode of the graph" reads as a fragment once the type word is removed) and its description's graph vocabulary ("row number and id are equal", "adjacency matrix", "node") with no flowline terms; `navigate_network_dfs` summary "Given a starting node" (`starts` takes flowline ids), the upmain/downmain sentence that names no columns, the index-ids sentence with no main verb, and Returns "dfs result"; `index_points_to_lines` summary terms `aggregate_id`, reachcode, and aggregate id measure with no gloss, and Note 5's run-on; `accumulate_downstream` `quiet` ("Quiet messages?"). Proposed wordings in the M4 Log, unit 4 rows M-08, M-12 to M-15, M-17, M-23, M-24. | M4 unit 4 modeler | P-4 | no | David |
| Roxygen beyond correctness the M5 exemplar modeler read found in the first public Python docstrings: `hy_network_type` lists the patterns as "enriched edge list", "self-referencing edge list", "dendritic", "junction table", "name-aligned base object" with no river terms and no word on what each layout lets the user do next; `make_index_ids` title "Make Index ids.", the circular "Makes index ids for the provided network table", the mode paragraph's "directed from the `id` to the `toid`" (downstream and upstream unsaid), and the `mode` fragment "Indicating the mode of the graph." Proposed wordings in `dev/milestones/M5.md`, exemplar entry, rows M-10, M-11, M-14 to M-16, M-18. | M5 run 1 unit 4 modeler | P-4 | no | David |
