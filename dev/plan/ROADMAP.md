# hydroloom Python port — ROADMAP

Milestones in the order chosen so that each is useful if the next never happens. Each brief is what a person or agent starting that milestone reads, together with the SPEC anchors it names. Rules are in SPEC.md, reasoning in DESIGN.md, harness and gates in VERIFICATION.md.

## Unit of work

Inside every milestone that touches a function, the unit of work and of review is one function in dependency order:

1. Its algorithm description (SPEC S-8) and conformance cases (S-4, S-5, S-6), written in a description-side session that may read `R/`.
2. The R-generated expectations (S-7); where the function carries an intended change (§4), the R edit, its new cases, and a NEWS entry in the same step (D-16). No R kernel extraction (K-10).
3. The Python function against the same cases and the same signature (A-16), written in a Python-side session that never opens `R/`, from the description and the SPEC anchors it cites alone, so the Python reads as Python rather than as a transliteration. Exemplar and named-kernel (K-6) changes are flagged in the Log for a second human review when a co-maintainer joins (§3).

One function is one logged review round and one commit set, committed directly to the `python-port` branch; there are no milestone branches and no per-milestone pull requests. `python-port` reaches `main` by a merge commit when the maintainer chooses. Each milestone's record is `dev/milestones/M<n>.md`, created when the milestone starts: Summary (written at close), Plan (one line per run), Log (one entry per run with the reviewed diff's base and head SHAs, the findings table, the implementer's responses, and a disposition column only the maintainer writes). The two sides are independent after M3: phase-2 descriptions can be written while phase 1 is ported.

## 1. Milestones

Two gates hold inside every milestone from M3 on. The per-function gate, checked before any Python is written: the description passes the template validator; every case is in the manifest with `fixture`, `expected`, `compare`, `order`, `supported_by`, and `generator`; `expected/<function>.gpkg` has a layer per case; the last R conformance run on the function is green. The kernel gate, for a function whose description names a kernel (K-6): the description's Postconditions and Algorithm are drafted and the signature is stated before the implementer starts, since the signature binds.

### M1. Vocabulary out of the R package

- Deliverable: `inst/extdata/hy_vocab.json` and `abbreviations.json` with their JSON Schemas, `abbreviations.json` a schema-valid stub until G-16 fills it from hydrolink; the R package reads them at runtime (S-2, S-3); the hard-coded mapping in the `hy` layer removed, with the `outletID` alias lowercased so it matches; the exported `hydroloom_name_definitions` object (kept an object, assigned in `.onLoad`) and `hydroloom_names()` served from the file; the `tier` and `plain` fields filled for every term: 46 as of 1.3.0, the five added there (`hydro_location_id`, `distance_km`, `past_end_km`, `id_from_measure`, `id_to_measure`) included, each `plain` sentence agent-drafted and marked `DRAFT:` until the maintainer reviews it. jsonlite to Imports. R-only; lands on `python-port`.
- Done when: every existing testthat test passes unchanged; the vocabulary schema test (S-10; jsonvalidate in Suggests) passes; the `plain` and `definition` fields have a `review-description`-style read by the hydrology and modeler readers dispositioned in the Log (prose.md); no alias mapping or definition text remains in `R/` outside the vocabulary loader, and `good_names` comes from the file (the attribute-name constants in `00_hydroloom.R` and the literals inside function bodies stay, since they serve tidyselect and `.data` masking); `R CMD check --as-cran` is clean.
- Load: SPEC S-1, S-2, S-3, I-15.
- Do not touch: any test expectation, any function body beyond the name lookup, any Python file.
- Record: `dev/milestones/M1.md`, light form (one Log entry per change a reviewer looked at).
- Blockers: none.

### M2. Conformance manifest and R harness

- Deliverable: `manifest.yaml` with its schema, the fixture set (S-5), the R harness that writes expectations under `HYDROLOOM_WRITE_EXPECTED=1` and compares otherwise (VERIFICATION §2); existing testthat value assertions migrated into cases with generated expectations; the non-frame converters (graph, traversal, scalar) and the `returns` and `key` fields in the manifest (S-4, S-7), since the migration meets `make_index_ids` here; the CRAN-shipped collation and vocabulary tests (D-1, S-10); the manifest schema check and the description template validator (S-8: front matter and every heading present) as conformance tests in the same R harness, so M3 runs them with no Python tooling; `pixi run validate` wraps them from M4. `inst/extdata/new_hope_mainstems.csv` is the `aggregate_id` source for the New Hope fixture and `tests/testthat/helper-mainstems.R` the assembly to migrate. R-only; improves the R package's test discipline regardless of the port.
- Done when: every migrated assertion is a manifest case and its testthat original is deleted; `devtools::test()` from the checkout runs conformance and the tarball check skips it cleanly; the parity table generates from `supported_by` with every function `r` only.
- Load: SPEC S-4 to S-7, S-10, I (all), D-13, D-14.
- Do not touch: function bodies; the intended changes (that is M3).
- Record: `dev/milestones/M2.md`, light form.
- Blockers: none.

### M3. Descriptions, cases, and intended changes for the phase-1 functions

- Deliverable: for the phase-1 functions (§2), the algorithm description with its YAML `requires` front matter, the boundary and tie cases, and the R-generated expectations; `remedies.json` with the keys those descriptions cite (A-10); the intended changes that land on these functions (§4: sibling order, SCC cycle check, `hy_capabilities()` shape, identifier ingest, the one condition class) as edits to the existing bodies, new R code written to K-4 and K-6, with their NEWS entries and new expectations. Each description runs through `review-description` (the hydrology reader) before its expectations are generated. On `python-port`.
- Done when: every phase-1 function has a description passing the template validator (M2's R conformance test) and a `review-description` round dispositioned in the Log; every D rule the description cites has a fixture that exercises it; conformance passes before and after each intended change except where its NEWS entry says otherwise; `hy_capabilities()` returns the (function, available, via) frame from a code table that a testthat test checks against the manifest's `requires`; `remedies.json` text has a read by both readers dispositioned in the Log (prose.md).
- Load: SPEC K-1 to K-11, D (all), I (all), A-2 to A-4, A-10, S-6, S-8.
- Do not touch: functions outside phase 1; any Python file; existing R bodies beyond the intended changes.
- Record: `dev/milestones/M3.md`; one Log entry per description review and per intended change.
- Blockers: `sort_network` calls `check_hy_graph` first (D-8), so the SCC change to `check_hy_graph` lands here although the function is phase 2. The `.kiro/` files (hydrology reviewer and `review-description` at least) are in the tree.

### M4. Python skeleton

- Deliverable: `pyproject.toml` with `[project]` and the pixi workspace (tasks `validate`, `lint`, `test` at least; `validate` wraps M2's R-side checks), `.pre-commit-config.yaml` with the schema validators (VERIFICATION §3), `.Rbuildignore` additions, `src/hydroloom/` with `_kernels/`, `index_ids.py`, `_hy.py`, `network.py` as empty modules with the import-linter contract, `_kernels/csr.py` as the one real module (language-local, oracled by `make_index_ids` cases) with its hypothesis properties, which is also the first `port-function` run; `tests/python/` with the conformance harness reading the manifest and the `test_api.py` assertion; `src/hydroloom/CLAUDE.md`, `tests/python/CLAUDE.md`, `.github/PULL_REQUEST_TEMPLATE.md`; the ruff rule set (VERIFICATION §6); the `docstrings.json` generator in `dev/` and its diff job; the path-filtered CI workflows that start and short-circuit. Release jobs, the PyPI name, accounts, and tokens are not M4 work: David sets them up when the first release is ready, before `py-v0.1.0` (§3).
- Done when: `R CMD check --as-cran` on the tarball is as clean as before M4; `pixi run r-check` reproduces it; `pixi run test` runs the harness against zero implemented functions and reports every case skipped; `csr.py`'s properties pass; `test_api.py` passes against `supported_by`; `docstrings.json` regenerates with no diff; the A-13 substitution table is its own Log entry, read against prose.md; the stub is on PyPI; a Windows, macOS, and Linux `py-test` job goes green on the empty package.
- Load: SPEC K-2, K-9, A-13, A-18; VERIFICATION §1, §3, §6, §7 (the L rules on layout, packaging, and dependencies live there).
- Do not touch: towncrier, Renovate, asv, pooch, the scheduled jobs. Each arrives in M5 or M6 with the first function that needs it. hypothesis arrives here, for `csr.py`.
- Record: `dev/milestones/M4.md`; the `csr.py` run is the first full `port-function` Log entry.
- Blockers: none; the `.gitignore` negations and the `AGENTS.md` merge closed 2026-10-25 (§3).

### M5. First Python functions, py-v0.1.0

- Deliverable, in this order of `port-function` runs: (1) the exemplar slice, one run and the first PR-sized commit set: `as_hy` at the A-16 scope (vocabulary-alias renaming, I-2, I-3, I-5, I-12, G-14, the private `_hy_reverse`), `representation`, `_route` and the `@requires` registry, `make_index_ids` returning `IndexIds` (with optional `main`, `edge_id`), `_kernels/csr.py` from M4 as its kernel; reviewed disproportionately because it sets the pattern everything else copies; flagged in the Log as an exemplar item. (2) The naming block: `hy_reverse`, `is_hy`, `register_names`, `name_definitions`, `align_names`, one run; the effort sweep (medium against high) runs here or on (3). (3) `add_toids`. (4) `sort_network` and `add_topo_sort`, with the SCC kernel in `_kernels/walk.py` (D-8; the public `check_hy_graph` wraps it in phase 2). (5) `navigate_network_dfs`, a named kernel (many-start DFS, K-6); flagged in the Log as a named-kernel item. Also `capabilities`, `is_dendritic`, `errors.py`, the first towncrier fragment, hatch-vcs, the permutation-invariance property (D-7) over every manifest function. Release setup is David's, done when this release is ready and outside the agent-based process (§3; the steps are in VERIFICATION §7): `.gitlab-ci.yml` release jobs, the PyPI and TestPyPI accounts, tokens, the protected tag. Then `rc/py-0.1.0` to TestPyPI and `py-v0.1.0` to PyPI.
- Done when: every phase-1 manifest case passes on all three platforms and both Python versions; `py-min-versions` passes at the floors; permutation invariance holds on every fixture; the wheel installs clean and passes conformance in the release job; the parity table shows the phase-1 functions as `r, python`; every Log entry has its dispositions filled.
- Load: SPEC K (all), I (all), D-1, D-3 to D-8, D-13 to D-16, A (all), S-6, S-7; the phase-1 descriptions.
- Do not touch: phase-2 functions; the accessor (A-7, later); anything geometric; public `hy_reverse(source=)` beyond the private helper until the naming block.
- Record: `dev/milestones/M5.md`; items flagged for a later second review: the exemplar slice, `navigate_network_dfs`.
- Blockers: M3 and M4 complete.

### M6. Port in dependency order

- Deliverable: phase 2 then phase 3 from §2, each function by the unit of work above. Arrivals with the function that first needs them: asv and the scale harness with `add_levelpaths` (the first sequential kernel, VERIFICATION §5); pooch and the domain fixture with the scale harness; the WBD benchmark before `dissolve_polygons`; the units conformance case with `navigate_hydro_network` (G-3); the state-plane case with `index_points_to_lines` (G-4). A `py-v0.x` release when a group lands. 1.0 is parity with hydroloom 1.3.0, phases 1 to 3 (DESIGN §9; 2026-10-25).
- Done when, per function: its description, cases, R-generated expectations, and Python function are one logged round on `python-port` and the parity table row reads `r, python`; per release: `pixi run scale` and `pixi run bench` on record for that tag (VERIFICATION §5).
- Load: per function, the SPEC anchors its description cites; for phase 3, G (all).
- Do not touch: a function whose upstream in the dependency order is not yet at parity.
- Record: `dev/milestones/M6<letter>.md`, one per function group; items flagged for a later second review: every named kernel (`add_levelpaths`, `add_pfafstetter`, `get_bridge_flowlines`, `navigate_hydro_network`, `index_points_to_lines`' `project_point_to_line`, `disambiguate_indexes`' Levenshtein).
- Blockers: which NHDPlus product supplies HU04 0204 for the scale fixture (§3); the WBD benchmark result for the dissolve implementation.

### M7. Documentation with two tabs

- Deliverable: the pipeline article "From a file to a model-ready network" as the first page and the end-to-end test (VERIFICATION §5); the generated representation map; one existing vignette converted to a Quarto article with `{.panel-tabset}`; tabs added to the rest as functions reach parity; quartodoc reference under `python/`; the README first screen and the three orientation paragraphs (DESIGN §8). Started once M5 ships, grown through M6.
- Done when: every code tab runs at build time in the `docs` environment; the pipeline article passes as the end-to-end test in both languages; an article without a Python tab says so in one line.
- Load: DESIGN §8; SPEC A-13; VERIFICATION §6.
- Do not touch: the R API reference build (pkgdown as today).
- Record: `dev/milestones/M7.md`; the `milestone-docs` report dispositioned per page.
- Blockers: M5.

## 2. Function inventory

Every hydroloom 1.3.0 export and its treatment. Phase 1 is M5; phases 2 and 3 are M6 in dependency order; wrapper is a one-to-three-line shapely or geopandas call kept for name parity. The generated parity table replaces this once the manifest carries `supported_by`.

| R export | Group | Phase | Python | Note |
| --- | --- | --- | --- | --- |
| `hy`, `hy_reverse`, `is.hy` | naming | 1 | `as_hy`, `hy_reverse`, `is_hy` | I-14 |
| `hydroloom_names`, `hydroloom_name_definitions`, `align_names` | naming | 1 | `register_names`, `name_definitions`, `align_names` | read the vocabulary |
| `hy_network_type`, `hy_capabilities`, `is_dendritic` | representation | 1 | `representation`, `capabilities`, `is_dendritic` | A-2, A-4; as of 1.3.0 R's table has a `has_aggregate_flowpath` predicate (`aggregate_id` and `length_km` present, topo or node) gating the aggregate-flowpath group |
| `hy_topo`, `hy_leveled`, `hy_node`, `hy_flownetwork` | representation | 1 | pattern names from `representation()` | doc pages only in R |
| `add_toids` | topology | 1 | same | ingest normalization and I-5 fill live here |
| `make_index_ids` | topology | 1 | returns `IndexIds` | K-2, K-3; `IndexIds` carries optional `main` and `edge_id`; R already has `mode = "to" | "from" | "both"`, no intended change |
| `sort_network`, `add_topo_sort` | topology | 1 | same | D-4; output order is contract |
| `navigate_network_dfs` | navigation | 1 | same | frame or `IndexIds`; D-6 |
| `check_hy_graph` | validation | 2 (lands in M3) | same | D-8 |
| `make_node_topology` | topology | 2 | same | includes the non-dendritic case |
| `to_flownetwork`, `subset_network` | topology | 2 | same | |
| `get_bridge_flowlines` | topology | 2 | same | Tarjan-style sequential kernel |
| `add_levelpaths`, `add_pathlength`, `add_streamorder`, `add_streamlevel` | attributes | 2 | same | in that order; D-11, D-12 |
| `add_divergence`, `add_return_divergence` | attributes | 2 | same | need the node pattern; D-9 |
| `add_pfafstetter` | attributes | 2 | same | needs the leveled pattern; sequential kernel |
| `accumulate_downstream` | attributes | 2 | same | triangular solve on dendritic; bridge-aware on non-dendritic |
| `navigate_hydro_network` | navigation | 2 | same | UT / UM / DM / DD with distance limits; G-3 |
| `make_attribute_topology` | topology | 3 | same | endpoint kd-tree with the D-10 rule |
| `index_points_to_lines` | indexing | 3 | same | G-6 to G-8; `aggregate_ids` constrains each point to the flowlines carrying its `aggregate_id`, exclusive with `ids` (1.3.0); `id_measure` returned on every call |
| `index_points_to_waterbodies` | indexing | 3 | same | point-in-polygon: `STRtree` with a predicate |
| `disambiguate_indexes` | indexing | 3 | same | G-16 |
| `get_hydro_location` | linear referencing | 3 | same | `line_interpolate_point` on the fraction from `frommeas`/`tomeas` |
| `add_measures`, `rescale_measures` | linear referencing | 3 | same | arithmetic over `frommeas`/`tomeas`; aggregate-id measures from lengths along a reachcode or mainstem |
| `get_partial_length`, `navigate_connected_paths` | linear referencing | 3 | same | one shapely primitive each; network distance between indexed points |
| `get_confluences`, `add_hydro_location_distance`, `index_hydro_location_distance` | aggregate-flowpath referencing | 3 | same | 1.3.0; the aggregate flowpath axis (cumulative `length_km` per `aggregate_id`, bottom first) plus one join each; no geometry; G-9 placement and sign rules, D-17 tie |
| `get_subpath` | aggregate-flowpath referencing | 3 | same | 1.3.0; same axis; `snap_m` in meters (G-2); optional geometry cut is `shapely.ops.substring` (G-11), R uses `lwgeom::st_linesubstring()` |
| `dissolve_polygons` | geometry | 3 | same | after the WBD benchmark; `coverage_union_all` with `union_all` fallback; `get_interiors` / `Polygon` rebuild for holes; `buffer(+d).buffer(−d)` for gaps; `get_parts` + `area` for largest part; `groupby` for summaries; G-5 |
| `check_valid` | geometry | wrapper | same | `make_valid`, `get_parts` filtered by type, `to_crs` |
| `fix_flowdir` | geometry | wrapper | same | `shapely.reverse` |
| `get_node` | geometry | wrapper | same | `get_point(0)` / `get_point(−1)` |
| `rename_geometry` | geometry | wrapper | same | `GeoDataFrame.rename_geometry`; exists for name parity only |
| `st_compatibalize` | geometry | wrapper | same | `to_crs` + `rename_geometry` |
| `format_index_ids`, `make_fromids` | deprecated | not ported | — | listed in the parity table as deprecated in R |

The linear-referencing family (`get_hydro_location`, `rescale_measures`, `add_measures`, `get_partial_length`, `navigate_connected_paths`) is ported as a group, since each is measure arithmetic plus one primitive and they share the G-9 conventions. The aggregate-flowpath family added in 1.3.0 (`get_confluences`, `add_hydro_location_distance`, `index_hydro_location_distance`, `get_subpath`) is a second group, ported together after the first: the four share the axis built from `length_km`, which does not use `frommeas`/`tomeas`, and the G-9 signed-distance and confluence-placement rules. They depend on `add_toids` or the node pattern and on `sort_network` when `topo_sort` is absent.

## 3. Open items

| Item | Blocks | Owner |
| --- | --- | --- |
| ~~Scale fixture is HU04 0204, the Delaware (decided 2026-09-26; 3DHP has no coastal coverage). Open: which product supplies it.~~ Closed 2026-10-25: NHDPlus HR, distributed by HU4, the denser network, identifiers outside 32-bit range. Python identifiers are strings regardless (I-2); R carries these as `character`. | — | — |
| ~~PyPI name claim: `hydroloom` unclaimed on 2026-10-14 (not re-queried since). Account with 2FA; TestPyPI account; the 0.0.1 stub uploads on a user-scoped token, then a project-scoped token replaces it and the user-scoped one is revoked; `py-v*` as a protected tag pattern; the co-maintainer as Owner at the first real release~~ Deferred 2026-10-25: release setup is David's, done when the first release (M5, `py-v0.1.0`) is ready, outside the agent-based process. The steps stay in VERIFICATION §7 for that day. | M5 release | David |
| ~~Token variables and the throwaway runner job on code.usgs.gov: egress to `upload.pypi.org` and `test.pypi.org`, `condaforge/miniforge3` through the proxy, the DOI certificate as a file-type variable~~ Deferred 2026-10-25 with the row above: checked when the first release is prepared, not before M4. | M5 release | David |
| ~~Which mirror is authoritative for `main`~~ Closed 2026-10-14: development on GitHub, official builds on code.usgs.gov, by current practice and the README | — | — |
| ~~Co-maintainer from the gdptools side, named from the first commit~~ Deferred 2026-10-25: a person is identified but not formally involved. Exemplar and named-kernel items are flagged in the Log so the review can happen when they join; nothing blocks on it. | — | David |
| WBD dissolve benchmark, run before the dissolve implementation is chosen | M6 `dissolve_polygons` | M6 |
| ~~`FutureWarning` for one minor release (A-11) against a 0.x contract that allows breaking minors: before 1.0 the warning is a courtesy, not a promise; decide whether to say so in the README~~ Closed 2026-10-25: say so. The README's provisional disclaimer (DESIGN §9) states that a pre-1.0 minor release may change or remove an interface without a warning cycle; lands with the M5 README. | — | — |
| ~~Whether the aggregate-flowpath group (1.3.0, phase 3) is inside the 1.0 line; DESIGN §9 says parity through phase 2~~ Closed 2026-10-25: inside. 1.0 is parity with hydroloom 1.3.0, phases 1 to 3. | — | — |
| ~~`.gitignore` ignores `dev` and `.kiro` wholesale in the 1.3.0 checkout; `dev/plan/`, `dev/milestones/`, and `.kiro/` minus `workflow-output/` have to be un-ignored before anything the plan commits there exists~~ Closed 2026-10-25: negations in place, with one more for `.kiro/context/pr-body-template.md`, which the `*temp*` pattern otherwise swallows. | — | — |
| ~~The existing root `AGENTS.md` (R style, S3 hierarchy, decomposition concepts) merges with the port rules in `kiro-config.md` §2; the decomposition definitions describe the `decomposition` branch, not `main`~~ Closed 2026-10-25: port rules appended as the last section of `AGENTS.md`; the decomposition concept section removed, since that branch never merged. | — | — |

## 4. Intended changes to the R package

Each is a NEWS entry and new expectations, landed on `python-port` in the milestone shown (D-16); the next CRAN release collects whatever has landed, and none is scheduled against a milestone (1.3.0, CRAN the week of 2026-10-26, is the last release the plan assumes). None is captured first: each is either an accident no user could rely on or a named behavior change.

| Change | Rule | Milestone |
| --- | --- | --- |
| jsonlite to Imports; vocabulary read from `inst/extdata` at runtime | S-3 | M1 |
| Version floors on every Import; `dplyr (>= 1.1.0)`, `sf (>= 1.0-0)` | D-2 | M2 |
| Identifier ingest: whole-valued double or integer64 to `integer` when within 32-bit range, else `character` | I-2 | M3 |
| Sibling order in `sort_network` by identifier only | D-4 | M3 |
| `check_hy_graph`: SCC always; `loop_check` removed; `component` and `reciprocal` labels | D-8 | M3 |
| `hy_capabilities()` returns a frame (function, available, via) from a hand-written code table, checked by a testthat test against the manifest's `requires` | A-4 | M3 |
| One condition class, `hydroloom_representation_error` | A-10 | M3 |
| Divergence and level-path ties fall through to identifier byte order | D-9, D-11 | M6 |
| Missing `weight_attribute` raises naming the row | S-6 | M6 |
| Zero-length geometry yields measure 0 with a warning | S-6 | M6 |
| Projection-based point indexing in place of nearest vertex | G-6 | M6 |
| Geographic input to distance functions: warn one minor release, then raise; 0.01° default dropped | G-4 | M6 |
| `work_crs` default in `dissolve_polygons` deprecated, then required | G-5 | M6 |
| Error text from the per-function `remedies` table | A-10 | M6, per function |

The G-4 and G-5 rows count minor releases that may be far apart. Whether each keeps its two steps or collapses into one is decided at the release that carries them.
