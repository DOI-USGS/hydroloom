---
requires:
  pattern: [hy_node]
  columns: []
  via:
    hy_topo: make_node_topology
---

# add_toids

## Signature

R: `add_toids(x, return_dendritic = TRUE)`, with methods for `data.frame`, `hy`, `hy_topo`, `hy_flownetwork`, and `hy_node`.

| argument | type | default |
| --- | --- | --- |
| `x` | table (data.frame, tibble, or sf) | required |
| `return_dendritic` | flag: R `logical` length one, Python `bool` | `TRUE` / `True` |

Python (ROADMAP §2, SPEC A-16): `add_toids(network, *, return_dendritic=True)`, returning a DataFrame, or a GeoDataFrame when the input is one. `network` is R's `x` (A-9).

Return: the input table with one column added.

| column | type | when |
| --- | --- | --- |
| `toid` | identifier, the type of `id` (I-1, I-2) | always |
| every input column | its input name and type | always; `fromnode` keeps its input values |

R places `toid` second and, with `return_dendritic = TRUE`, moves `fromnode` to the last attribute position (before geometry). Column position is not contract; expected outputs compare by name (D-14). A `data.frame` input returns with its input column names (`toid` has no input name, so it is `toid`) and no hydroloom class. An `hy` input returns class `hy_topo` when `id` is unique, which `return_dendritic = TRUE` guarantees, and `hy_flownetwork` when it repeats; the `orig_names` and `dendritic` attributes are not carried. Python returns no class and no attribute; `representation(network)` (A-2) gives the same pattern name.

No named kernel (K-6); the work is a join on node identifiers and no CSR index is built.

## Preconditions

The input, after `hy()` (R) or `as_hy` (Python), must be the `hy_node` pattern: `id`, `fromnode`, `tonode`, unique `id`, no `toid`. With `return_dendritic = TRUE` (the default) a `divergence` column is also required; it is not in `requires.columns` because `return_dendritic = FALSE` runs without it. What is raised, in check order:

- `return_dendritic = FALSE`: a deprecation warning (`return_dendritic_deprecated`), from the `data.frame` method before any other check, so it precedes the errors below. R `warning()`; Python `FutureWarning` (A-11). The hy-class methods do not warn. The case `add_toids_return_dendritic_deprecated` asserts the warning; the `_not_dendritic` cases compare their output only.
- `toid` present with unique `id` (R `hy_topo` or `hy_leveled`): stop (`toid_already_present`).
- `toid` present with repeated `id` (R `hy_flownetwork`): stop (`toid_already_present`, D3-38); Python raises `RepresentationError`.
- no `toid` and not the `hy_node` pattern (node columns missing, or `id` repeats): stop with the representation guidance (`needs_node`).
- `return_dendritic = TRUE` without `divergence`: stop (`divergence_required`).
- `return_dendritic = TRUE` with `divergence` missing on one or more rows, at a split or not: stop naming the rows by `id` (`divergence_missing`, I-9, D3-56).
- `return_dendritic = TRUE` with a broken divergence coding at a split, a node that is the `tonode` of at least one flowline and the `fromnode` of two or more: every downstream flowline there has `divergence` 2, or two or more have a `divergence` other than 2. Stop naming the downstream flowlines at each such split by `id` (`divergence_coding_invalid`, D3-72). A node that is the `fromnode` of one flowline only is not a split, whatever that flowline's `divergence`. The check reads any value other than 2 as the main path; a main path coded 0 passes it with a warning (Edge cases, D3-174).

With `return_dendritic = FALSE`, `divergence` is not read, so none of the last three applies and neither tolerated-coding warning of Edge cases is raised.

## Definition

Roxygen: generates a toid attribute from node topology by joining tonode and fromnode attributes. `return_dendritic`: remove non dendritic paths if TRUE; requires a "divergence" flag where 1 is main and 2 is secondary.

As a property of the input: a row's `toid` is the `id` of a flowline whose `fromnode` equals the row's `tonode`. With `return_dendritic = TRUE`, a minor path at a split (a flowline coded `divergence` 2 that shares its `fromnode` with another flowline) is not a downstream target; a lone downstream flowline coded 2 is tolerated with a warning (Edge cases). A row with a missing `tonode`, or whose `tonode` matches no eligible `fromnode`, is a terminal and carries the reserved outlet value (I-5). A flowline with a missing `tonode` is still the downstream target of the flowlines that reach its `fromnode`; its upstream connection is decided at its `fromnode` (D3-56). This holds in both modes. With `return_dendritic = FALSE` a row with k eligible downstream flowlines appears k times, once per `toid`; with `return_dendritic = TRUE` every row appears once, since a split with no main path or more than one stops (Preconditions).

## Algorithm

1. `data.frame` method: warn if `return_dendritic = FALSE` (`return_dendritic_deprecated`); run `hy()`; dispatch on the class; restore input names on return (`hy_reverse`). Python: `_as_hy` then `_route` (A-3, A-17), then `_hy_reverse`.
2. Dispatch by representation (Preconditions). Only the `hy_node` pattern proceeds.
3. With `return_dendritic = TRUE`: stop if any `divergence` is missing (`divergence_missing`); stop if any split has a broken coding (`divergence_coding_invalid`, Preconditions). Only then come the two tolerated-coding warnings, so a split that stops never warns: warn naming by `id` each main path at a split coded 0 (`divergence_zero_main_path`, D3-174), and each lone downstream flowline coded 2, one whose `fromnode` is some flowline's `tonode` and the `fromnode` of no other flowline (`divergence_minor_only_path`, D3-158). Then mark each flowline whose `divergence` is 2 as ineligible to receive an upstream connection, except a lone downstream flowline, which stays eligible. The source `divergence` attribute decides; no main path is computed here (D-9: the source attribute wins).
4. Give each row whose `tonode` is missing `toid` equal to the outlet value. These rows remain eligible as downstream targets (D3-56).
5. Join the rows with a `tonode`: upstream row u matches downstream row d when `u.tonode == d.fromnode` and d is eligible. Each match is one output row (u's columns, `toid = d.id`). A row with no match gets the outlet value. Missing `fromnode` matches nothing.
6. Outlet value (I-5): `""` when `id` is `character` (always in Python); otherwise `0` in the type of `id`, which after `hy()` is `integer` (`0L`), so `toid` has the type of `id` (I-2, D3-36).
7. Row order, binding (D-14): output rows follow the input order of u, a row with a missing `tonode` keeping its input position (D3-56); rows sharing one u are contiguous, their `toid` in the input order of the matching d rows. This is the only tie rule: at a divergence the order of a flowline's downstream connections is the input row order of the downstream flowlines, not identifier order (D3-37). The function builds no adjacency index, so the adjacency order rule for `make_index_ids` does not apply; nothing here is sorted, so D-3 holds vacuously and D-4 does not apply.
8. R: `fromnode` values are restored from the input (the masking in step 3 is internal); re-classify (`classify_hy`): unique `id` gives `hy_topo`, repeated `id` gives `hy_flownetwork` (AGENTS.md conversion routes). With `return_dendritic = FALSE` on a network with a divergence, `id` repeats, so the result is the flownetwork pattern.

No sentinel: the join produces identifiers, not dense positions, and nothing crosses the K-1 boundary. Nothing depends on randomness or scheduling (D-15).

## Postconditions

- Every input column is present with its input values on every output row derived from that input row; `fromnode` values equal the input's.
- `toid` has no missing value and has the type of `id` (always `str` in Python).
- For every output row, `toid` is either an `id` in the output or the outlet value (`""` for character `id`, `0` otherwise).
- For every output row whose `toid` is an `id`, that flowline's `fromnode` equals the row's `tonode`.
- With `return_dendritic = TRUE`, no output row's `toid` is the `id` of a minor path at a split: a flowline whose `divergence` is 2 and whose `fromnode` is the `fromnode` of another flowline.
- A flowline with a missing `tonode` has the outlet value as `toid` and is the `toid` of every row whose `tonode` equals its `fromnode`, subject to the rule above.
- The set of `id` values in the output equals the set in the input; every input row appears at least once.
- With `return_dendritic = TRUE`, the row count equals the input's and `id` is unique; the result is the topo pattern.
- With `return_dendritic = FALSE`, a row appears once per eligible downstream flowline, or once with the outlet value when it has none.
- Output rows follow input row order, rows with a missing `tonode` included (step 7).
- D-7: for a row-permuted input, the multiset of (`id`, `toid`) pairs and every row's values are unchanged; only row order follows the permutation (step 7).

Conformance compares every column on the synthetic cases. On the real-network cases (`new_hope.gpkg`, `new_hope_character_id.gpkg`, `nhdplushr.gpkg`) it compares the identifier and `toid` only, as each case's `columns:` list gives, along with row count and row order; pass-through of the other columns is checked on the synthetic cases (D3-163, D3-185).

## Complexity

The join and the split check are O(n log n) time (O(n + m) with hashing) and O(n + m) space, for n rows and m output connections; m = n when no divergence is kept.

## Edge cases

- One flowline: returns one row with the outlet value as `toid`.
- No connections (every `tonode` unmatched, or every `tonode` missing): every row is a terminal; row order is input order.
- Zero rows: returns zero rows with `toid` typed as `id` (I-11).
- A divergence whose downstream flowlines all have `divergence` other than 2, with `return_dendritic = TRUE`: stops naming those flowlines by `id` (`divergence_coding_invalid`). In `add_toids_divergence_without_minor` (the worked example's first five rows with flowline 3 coded 1) the rows are flowlines 2 and 3 (D3-72).
- A divergence whose downstream flowlines all have `divergence` 2 (no main path), with `return_dendritic = TRUE`: stops naming those flowlines by `id` (`divergence_coding_invalid`). In `add_toids_divergence_without_main` (the worked example's first five rows with flowline 2 coded 2) the rows are flowlines 2 and 3 (D3-72).
- A main path coded 0 at a split that passes the coding check, with `return_dendritic = TRUE`: tolerated with a warning (`divergence_zero_main_path`) naming those flowlines by `id`. NHDPlus codes the main path 1 and uses 0 for a flowline not below a divergence; the check reads any value other than 2 as the main path, so the 0 has that one reading. The case `add_toids_divergence_zero_main` (split at node 20, flowline 2 coded 0 and flowline 3 coded 2) asserts the warning naming flowline 2 and compares no output (D3-174).
- A lone downstream flowline coded 2, one whose `fromnode` is reached by some flowline and leaves no other flowline, with `return_dendritic = TRUE`: not a split, so the coding check does not stop. Tolerated with a warning (`divergence_minor_only_path`) naming those flowlines by `id`; a clipped network leaves this shape where the main path of a divergence lies outside it. The case `add_toids_divergence_minor_only` (the worked example without flowline 2, so flowline 3 alone leaves node 20) asserts the warning naming flowline 3 and compares no output (D3-158). A flowline coded 2 whose `fromnode` no flowline reaches is a headwater, has no upstream connection to make, and raises no warning.
- Missing `divergence` on any flowline, with `return_dendritic = TRUE`: stops naming the rows by `id` (`divergence_missing`), at a split or not. In `add_toids_missing_divergence` the row is flowline 2, at a split; in `add_toids_missing_divergence_off_split` it is flowline 4, the only flowline leaving node 30 (D3-56).
- A flowline with a missing `tonode` and a valid `fromnode`: a terminal that still receives its upstream connections. For `add_toids_missing_tonode_target` the result is (1, 2), (2, outlet), (3, 2); in `add_toids_missing_tonode_first` the flowline with a missing `tonode` keeps its first row position (D3-56).
- Duplicated row (repeated `id`) without `toid`: stops (`needs_node`). With `toid`: stops (`toid_already_present`, Preconditions).
- Input already has `toid`: stops (`toid_already_present`); with `return_dendritic = FALSE` the deprecation warning comes first.
- Missing weight, zero-length line, empty name: not applicable; those columns pass through.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| `return_dendritic = FALSE` (warning) | keeping every downstream connection at a divergence by calling `add_toids()` with `return_dendritic` false is deprecated | all | `to_flownetwork()`, which keeps both the main and minor paths | `return_dendritic_deprecated` |
| `toid` already present | the network already gives each flowline its downstream flowline in `toid`; `add_toids` only builds `toid` from `fromnode`/`tonode` | all | none needed: use the network as it is, without `add_toids()` | `toid_already_present` |
| not the node pattern: no `fromnode`/`tonode` | the network has no `toid` and lacks `fromnode`/`tonode`, the identifiers of the points at each end of every flowline | all | add `fromnode` and `tonode` columns from the source data | `needs_node` |
| not the node pattern: `id` repeats | the network has no `toid` and gives one flowline more than one row | the rows sharing an `id` | remove the duplicated rows | `needs_node` |
| no `divergence` with `return_dendritic = TRUE` | keeping only the main path at a divergence needs the `divergence` column, which marks the main path 1 and minor paths 2; the network has none | all | `add_divergence()`, given the coastal and inland outlets, to compute `divergence` | `divergence_required` |
| `divergence` missing with `return_dendritic = TRUE` | `divergence` is missing on one or more flowlines, so the main path cannot be told from a minor one there | the rows where `divergence` is missing | `add_divergence()`, given the coastal and inland outlets, to compute `divergence` | `divergence_missing` |
| broken divergence coding with `return_dendritic = TRUE` | at one or more splits every downstream flowline is coded 2, leaving no main path, or two or more are not coded 2, leaving a flowline with more than one downstream flowline | the rows of the downstream flowlines at each split whose divergence coding is broken | `add_divergence()`, given the coastal and inland outlets, to recompute `divergence` | `divergence_coding_invalid` |
| main path coded 0 at a split with `return_dendritic = TRUE` (warning) | at one or more splits the one downstream flowline not coded 2 is coded 0, the code for a flowline not below a divergence, where NHDPlus codes the main path 1; it is read as the main path | the downstream flowlines coded 0 at a split, by `id` | `add_divergence()`, given the coastal and inland outlets, to recompute `divergence` | `divergence_zero_main_path` |
| lone downstream flowline coded 2 with `return_dendritic = TRUE` (warning) | one or more flowlines coded 2, a minor path, is the only flowline leaving the point where the flowline above it ends; it is connected to so the flowline above is not left a terminal | the lone downstream flowlines coded 2, by `id` | `add_divergence()`, given the coastal and inland outlets, to recompute `divergence` on the network as clipped | `divergence_minor_only_path` |

## References

- R source: `R/add_toids.R` (`add_toids` and methods, `try_add_toids`), `R/hy_classes.R` (`classify_hy`, `hy_classify_and_redispatch`, `hy_dispatch_error`, `hy_guidance_node`), `R/hy.R` (`hy`, `hy_reverse`), `R/utils.R` (`get_outlet_value`).
- Tests: `tests/testthat/test_add_toids.R`, `tests/testthat/test_hy_classes.R` ("producers stamp correct output class", "add_toids warns for return_dendritic = FALSE").
- Description: `algorithms/hy.md` step 8 (the `add_topo` path calls `add_toids(x, return_dendritic = <divergence present>)`).
- NHDPlus Version 2 User Guide (McKay and others, 2012): `Divergence` 0 none, 1 main path, 2 minor path; `FromNode`/`ToNode` node topology.
- SPEC I-1, I-2, I-5, I-9, I-11, D-3, D-4, D-7, D-9, D-14, A-3, A-9, A-10, A-11, A-16; AGENTS.md "Conversion routes".

## Worked example

Fixture `fixtures/add_toids_worked_example.gpkg`, aspatial, six rows, all columns INTEGER:

| id | fromnode | tonode | divergence |
| --- | --- | --- | --- |
| 1 | 10 | 20 | 0 |
| 2 | 20 | 30 | 1 |
| 3 | 20 | 40 | 2 |
| 4 | 30 | 50 | 0 |
| 5 | 40 | 50 | 0 |
| 6 | 50 | 60 | 0 |

Flowline 1 splits at node 20 into the main path 2 and the minor path 3, which rejoin through 4 and 5 at node 50 above the single outlet 6. `add_toids(x)` returns six rows: (`id`, `toid`) = (1, 2), (2, 4), (3, 5), (4, 6), (5, 6), (6, 0). Flowline 3 receives no upstream connection, so its upstream end becomes a headwater while its downstream end still feeds the main path through 5 and 6. `add_toids(x, return_dendritic = FALSE)` warns and returns seven rows: (1, 2), (1, 3), (2, 4), (3, 5), (4, 6), (5, 6), (6, 0); with rows 2 and 3 of the input swapped it returns (1, 3), (1, 2), (3, 5), (2, 4), (4, 6), (5, 6), (6, 0). `toid` is `integer` in R, the type of `id`; Python `str` with `""` for the outlet.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording R 1.3.0 behavior. Intended change I-2 (ROADMAP §4, M3): `toid` takes the type of `id` (pairing per D3-06) and the integer outlet value is `0L`; today `toid` is `double` for every non-character `id`. Captured, not changed (no §4 row): rows with a missing `tonode` move to the end (a D-14 departure); a missing `divergence` makes a flowline ineligible as a target; a flowline with a missing `tonode` is never a target; flownetwork input recurses in R, where Python raises `toid_already_present`. The A-10 condition-class change applies to the `needs_node` stop. A-11: Python's deprecation is a `FutureWarning`. No A-9 gap: A-16 matches R.
- 2026-10-26, suite 0.1.0, review fix round: a missing `divergence` treated as a minor path and a flowline with a missing `tonode` never a target are recorded as R 1.3.0 behavior, not Python contract; their cases are R-only (D3-56, narrowing D3-37). Postconditions state that a divergence coded other than 2 on two branches gives a repeated `id` under `return_dendritic = TRUE`. Added the edge case and case for a divergence with no main path. The worked example gains flowline 6 so the minor path rejoins above one outlet. Remedies cells reworded; `needs_node` split by cause.
- 2026-10-27, suite 0.1.0, M3 close: dispositions applied. D3-56 (ROADMAP §4 row for `add_toids` missing `divergence` and `tonode`, M3): with `return_dendritic = TRUE` a missing `divergence` stops (`divergence_missing`, I-9); a flowline with a missing `tonode` is a terminal under I-5 that keeps its input row position (D-14) and stays the target of its upstream flowlines. D3-72, in the same R change and NEWS entry: a split with no main path or more than one stops (`divergence_coding_invalid`), so `return_dendritic = TRUE` always returns a unique `id`. D3-38, landing with the D3-56 change: an `add_toids.hy_flownetwork` method stops with `toid_already_present`, in place of R 1.3.0's recursion. D3-37 narrowed: only the input-order rule among a flowline's repeated rows stands. The cases for missing `divergence`, missing `tonode`, and both broken codings become shared. Remedies aligned to `remedies.json` (D3-146).
- 2026-10-28, suite 0.1.0, M3 close-out run 2 (description sweep): the text states the landed behavior in present tense, and the R 1.3.0 comparisons move to the entries above (D3-159, D3-172, D3-36). I-2 has landed: `toid` takes the type of `id`, with outlet `0L` for an `integer` `id`. R change for D3-174 and D3-158 (ROADMAP §4 tolerated-codings row, NEWS): with `return_dendritic = TRUE`, after the coding check, a main path coded 0 at a split is read as the main path and warns `divergence_zero_main_path`; a lone downstream flowline coded 2 is connected to and warns `divergence_minor_only_path`, where R used to leave the flowline above it a terminal. Both warnings name rows by `id` and are recorded under Edge cases as tolerated; their cases assert the warning and store no layer (D3-182, D3-186). The Postcondition and Definition on minor paths now cover a minor path at a split only, since the lone flowline coded 2 is connected to. Remedies gains rows for both drafted keys (D3-180, D3-146). `add_toids_missing_divergence_off_split` added under Edge cases (D3-177). The D-5 citation is dropped, since the function builds no adjacency index (D3-177). Postconditions state what the real-network cases compare (`columns:`, D3-163, D3-185). The `hy_flownetwork` method is listed in Signature (D3-38).