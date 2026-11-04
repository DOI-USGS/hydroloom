# Kickoff: M5, run 4

Base `acd5488087d9d23e61c7d01d434b89a6b4034d79` on `python-port`. Effort high. Model claude-opus-5.5.

Written by the orchestrator from the `REVIEW_11032026-run3` dispositions (both filled and
transcribed to `dev/plan/RULES.md` in `acd5488`, P-3 and P-1) and the open BACKLOG M5 rows
(P-5). The run carries the rest of the M5 deliverable: (4) `sort_network` and
`add_topo_sort` with the SCC kernel, (5) `navigate_network_dfs`, then `capabilities`,
`is_dendritic`, hatch-vcs, and the permutation-invariance property. Effort is high because
the run carries the two kernels the plan reviews most closely (`_kernels/walk.py` and the
named many-start DFS, K-6); the run 3 sweep compared medium with high on one unit only.

Who does what. The orchestrator builds nothing (P-5). Units 1, 2 and 5 are plan, manifest,
R and roxygen work: `general-task-execution`, reviewed by `hl-style-reviewer` on the unit's
diff, one fix round; unit 5's composed docstring is read by `hl-modeler-reader`. Unit 9 is
packaging outside `src/hydroloom/` and `tests/python/`: `general-task-execution`, style
review. Units 3, 4, 6, 7, 8 and 10 are `port-function` runs: manifest marks and new cases by
`general-task-execution` first where the unit names them, then `hl-implementer`,
`hl-spec-reviewer` and `hl-style-reviewer` on the diff, one fix round. A parity
regeneration a Python unit needs runs in R and goes to `general-task-execution` inside the
unit. Every unit is one Log entry in `dev/milestones/M5.md`, written by `hl-aggregator` in
mode `record`.

## Units, in dependency order

1. Plan commit — SPEC S-4 gains the parity sentence item 1 adds to P-3: a function whose
   every case carries `r_only` reads `r, python` when its Python name is in
   `hydroloom.__all__`, and `r` otherwise. The BACKLOG M5 row for the Python-only raise
   cases is split: the `check_hy_graph` Python-only case moves to a new M6 row, since
   `check_hy_graph` is phase 2 and ROADMAP M5 "Do not touch" keeps phase-2 functions out;
   the `sort_network_dups.gpkg` and `id_missing` cases stay in the M5 row for unit 4 (plan;
   RULES P-2).
2. `add_toids` route and the parity generator (description, R change, cases and
   expectations) — item 2, P-1: `via: hy_topo: make_node_topology` is dropped from the
   `add_toids.md` front matter and from the R `hy_capabilities` row, the A-4 code table is
   corrected, and NEWS gets the disposition's line: `hy_capabilities()` no longer offers
   `make_node_topology` as the route to `add_toids()` for a table that has `toid`. The
   `hy_capabilities` expectation layers that change regenerate under that NEWS entry; R
   conformance stays green. The SP-02 Log cell (M5.md, run 3 unit 6 entry) is filled with
   `see REVIEW_11032026-run3 item 2`. Item 1, P-3: `hydroloom_names_unnamed_input` takes
   `r_only: hydroloom_names.md §Signature`; the parity generator
   (`tests/testthat/helper-conformance-parity.R`) reads an all-`r_only` function as
   `r, python` when its Python name is in the `__all__` list of
   `src/hydroloom/__init__.py`, and as `r` when the file is absent; the `parity.md` header
   sentence says so; `parity.md` regenerates (`hydroloom_names` still reads `r`). The
   front matter is not a binding section, so no reader round.
3. Naming and `add_toids` Python follow-up (Python) — `register_names` joins `__all__`
   (item 1, P-3); its cases keep `supported_by: [r]` with their `r_only` anchors. The unit's
   Log entry names, for each Postcondition in `hydroloom_names.md`, the pytest test that
   covers it; a Postcondition with no test gets one in this unit. The `add_toids`
   `@requires` registration drops the `hy_topo: make_node_topology` route (item 2).
   `parity.md` regenerates and `hydroloom_names` reads `r, python`.
4. `sort_network` and `add_topo_sort` (cases, then Python) — deliverable (4). First the
   manifest: each `sort_network` and `add_topo_sort` case goes `[r, python]` where its
   `prepare` runs in both harnesses (P-1), or takes `r_only` with its anchor (P-5); the
   BACKLOG M5 row's remaining cases are added as `supported_by: [python]`,
   `generator: [python]` raise cases: the repeated (`id`, `toid`) pair on
   `sort_network_dups.gpkg` (D3-217, B-5) and `id_missing` on a fixture with one and two
   missing `id` values, the two-value table carrying `toid` (D3-210, I-12). Then
   `hl-implementer`: the SCC cycle check in `_kernels/walk.py` (D-8, scipy
   `connected_components(connection="strong")`), `sort_network` with sibling order by
   identifier byte order (D-4), `add_topo_sort` replacing the input column (B-4); `__all__`
   and `supported_by: [r, python]` (A-15, A-18). The BACKLOG M5 row closes in this unit.
5. `navigate_network_dfs` roxygen (roxygen) — the BACKLOG M5 A-13 row: `@param x` gets text
   of its own, since `network` can be `IndexIds` and the shared `network` sentence does not
   hold; bare `x` in running text becomes the parameter's name; the "`main` element must be
   included" sentence names the `IndexIds` attribute in a language-neutral sentence and the
   R list element in an "In R," sentence (P-4). `devtools::document()`, then
   `docstrings.json` regenerated; R conformance stays green. `hl-modeler-reader` reads the
   composed docstring; findings route per P-4, and agent-drafted wording joins the Any time
   roxygen row. The BACKLOG M5 row closes in this unit.
6. `navigate_network_dfs` (cases, then Python) — deliverable (5), a named kernel (K-6):
   marks as in unit 4, then `hl-implementer`: the many-start DFS by the signature the
   description names, siblings in identifier byte order (D-5, D-6), the traversal `order`
   column (S-7); `__all__` and `supported_by: [r, python]`. Flagged in the Log as a
   named-kernel item for the second review.
7. `capabilities` (cases, then Python) — marks as in unit 4, then `hl-implementer`:
   `capabilities` from the `@requires` registry, answering rather than raising on a table
   with no `id` (B-1), with no `include_conversions` argument (B-5) and the `add_toids`
   route as unit 3 left it (P-1).
8. `is_dendritic` (cases, then Python) — marks as in unit 4, including the Python raise
   case D3-210 left pending, then `hl-implementer`; names aligned first (D3-29, D3-197).
   Detection on a node table with only a repeated `fromnode` stays in its BACKLOG M6 row.
9. hatch-vcs (packaging) — VERIFICATION 1.7: version from the `py-v*` tag with `tag_regex`
   and `git_describe_command --match "py-v*"` under `[tool.hatch.version.raw-options]`,
   `__version__` via `importlib.metadata`, the static `version = "0.0.0"` placeholder
   removed. `python -m build` and `pixi run test` pass with no `py-v*` tag in the
   repository (a `.devN` or fallback version).
10. Permutation invariance (Python tests) — D-7 and VERIFICATION §4: a hypothesis property
    over every manifest function with a Python implementation, shuffling input rows,
    sorting both outputs by `id`, asserting equal; for `sort_network` and
    `navigate_network_dfs` the un-sorted variant. A violation the property finds becomes a
    BACKLOG row with its shrunk graph, under the milestone that owns the function
    (VERIFICATION §4, bin 2).

## BACKLOG rows pulled in

- `supported_by: [python]` raise cases on `sort_network_dups.gpkg` and the
  `check_hy_graph` Python-only case, and an `id_missing` case (BACKLOG M5; §4 row not
  needed) — unit 1 moves the `check_hy_graph` half to M6; unit 4 adds the rest and closes
  the row.
- A-13 language-neutral pass on the composed `navigate_network_dfs` docstring (BACKLOG M5;
  §4 row not needed) — unit 5.

Not pulled in: the Linux `R CMD build .` check (no Linux environment on this machine; WSL
has no distribution installed), and the `py-v0.1.0` release setup (David). Both stay in
BACKLOG M5.

## Last logged read per description

| description | read SHA |
| --- | --- |
| add_toids.md | `8a369ab`; frozen (P-4); unit 2 front matter only, no reader round |
| hy_capabilities.md | `d1d6317`; frozen (P-4) |
| hydroloom_names.md | `d617614`; frozen (P-4) |
| sort_network.md | `8a369ab`; frozen (P-4) |
| add_topo_sort.md | `ef47f6f`; frozen (P-4) |
| navigate_network_dfs.md | `1807e81`; frozen (P-4) |
| is_dendritic.md | `9434139`; frozen (P-4) |

## Done-when items the script does not check

- Every phase-1 manifest case passes under `pixi run test` on win-64. Three platforms, both
  Python versions, and `py-min-versions` need a push; report as not met and name the
  workflow files (`.github/workflows/py-test.yaml`, `.github/workflows/py-min-versions.yaml`).
- The parity table: report which phase-1 functions read `r, python`, and which still carry
  `[r]` cases with no `r_only` and why.
- Permutation invariance holds on every fixture (unit 10): report the functions covered and
  any violation's BACKLOG row.
- `R CMD check --as-cran` on the tarball: report the result and which notes come from the
  environment.
- Not in this run, so report each as not met: the `hydroloom` name on PyPI and TestPyPI, the
  wheel installing clean in the release job, the Linux `R CMD build` check, and the release.
