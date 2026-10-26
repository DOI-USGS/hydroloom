---
requires:
  pattern: [hy_node]
  columns: []
  via:
    hy_topo: make_node_topology
---

# add_toids

## Signature

R: `add_toids(x, return_dendritic = TRUE)`, with methods for `data.frame`, `hy`, `hy_topo`, and `hy_node`.

| argument | type | default |
| --- | --- | --- |
| `x` | table (data.frame, tibble, or sf) | required |
| `return_dendritic` | flag: R `logical` length one, Python `bool` | `TRUE` / `True` |

Python (ROADMAP §2, SPEC A-16): `add_toids(network, *, return_dendritic=True)`, returning a DataFrame, or a GeoDataFrame when the input is one. `network` is R's `x` (A-9).

Return: the input table with one column added.

| column | type | when |
| --- | --- | --- |
| `toid` | identifier, the type of `id` (I-1, I-2; post-change, see Changes) | always |
| every input column | its input name and type | always; `fromnode` keeps its input values |

R places `toid` second and, with `return_dendritic = TRUE`, moves `fromnode` to the last attribute position (before geometry). Column position is not contract; expected outputs compare by name (D-14). A `data.frame` input returns with its input column names (`toid` has no input name, so it is `toid`) and no hydroloom class. An `hy` input returns class `hy_topo` when `id` is unique and `hy_flownetwork` when it repeats; the `orig_names` and `dendritic` attributes are not carried. Python returns no class and no attribute; `representation(network)` (A-2) gives the same pattern name.

No named kernel (K-6); the work is a join on node identifiers and no CSR index is built.

## Preconditions

The input, after `hy()` (R) or `as_hy` (Python), must be the `hy_node` pattern: `id`, `fromnode`, `tonode`, unique `id`, no `toid`. With `return_dendritic = TRUE` (the default) a `divergence` column is also required; it is not in `requires.columns` because `return_dendritic = FALSE` runs without it. What is raised, in check order:

- `return_dendritic = FALSE`: a deprecation warning (`return_dendritic_deprecated`), from the `data.frame` method before any other check, so it precedes the errors below. R `warning()`; Python `FutureWarning` (A-11). The hy-class methods do not warn.
- `toid` present with unique `id` (R `hy_topo` or `hy_leveled`): stop (`toid_already_present`).
- `toid` present with repeated `id` (R `hy_flownetwork`): R 1.3.0 recurses between the `data.frame` method and `hy()` until the evaluation depth limit; Python raises `RepresentationError` (`toid_already_present`). See Changes.
- no `toid` and not the `hy_node` pattern (node columns missing, or `id` repeats): stop with the representation guidance (`needs_node`).
- `return_dendritic = TRUE` without `divergence`: stop (`divergence_required`).

## Definition

Roxygen: generates a toid attribute from node topology by joining tonode and fromnode attributes. `return_dendritic`: remove non dendritic paths if TRUE; requires a "divergence" flag where 1 is main and 2 is secondary.

As a property of the input: a row's `toid` is the `id` of a flowline whose `fromnode` equals the row's `tonode`. A flowline can be a downstream target only if its own `tonode` is present and, with `return_dendritic = TRUE`, its `divergence` is neither 2 nor missing. A row with a missing `tonode`, or whose `tonode` matches no eligible `fromnode`, is an outlet and carries the reserved outlet value (I-5). With `return_dendritic = FALSE` a row with k eligible downstream flowlines appears k times, once per `toid`; with `return_dendritic = TRUE` it appears once whenever at most one of the flowlines leaving its `tonode` has a `divergence` other than 2.

## Algorithm

1. `data.frame` method: warn if `return_dendritic = FALSE` (`return_dendritic_deprecated`); run `hy()`; dispatch on the class; restore input names on return (`hy_reverse`). Python: `_as_hy` then `_route` (A-3, A-17), then `_hy_reverse`.
2. Dispatch by representation (Preconditions). Only the `hy_node` pattern proceeds.
3. With `return_dendritic = TRUE`, mark each flowline whose `divergence` is 2 or missing as ineligible to receive an upstream connection. The source `divergence` attribute decides; no main path is computed here (D-9: the source attribute wins). Missing counts as ineligible because R tests `divergence == 2` through `ifelse`, which yields missing; captured, not changed.
4. Set aside rows whose `tonode` is missing; each gets `toid` equal to the outlet value. These rows are also ineligible as downstream targets.
5. Join the remaining rows: upstream row u matches downstream row d when `u.tonode == d.fromnode` and d is eligible. Each match is one output row (u's columns, `toid = d.id`). A row with no match gets the outlet value. Missing `fromnode` matches nothing.
6. Outlet value (I-5): `""` when `id` is `character` (always in Python); otherwise `0`. Today R writes `0` as `double` and `toid` is `double` whatever the type of `id`; post-change (I-2) `toid` has the type of `id`, and the outlet value is `0L` for `integer`.
7. Row order, binding: the rows from step 5 in input order of u; rows sharing one u are contiguous, their `toid` in the input order of the matching d rows; then the rows from step 4, in input order. This is the only tie rule: at a divergence the order of a flowline's downstream connections is the input row order of the downstream flowlines, not identifier order (D-5 concerns `make_index_ids` adjacency and does not apply; nothing here is sorted, D-3 holds vacuously, D-4 does not apply).
8. R: `fromnode` values are restored from the input (the masking in step 3 is internal); re-classify (`classify_hy`): unique `id` gives `hy_topo`, repeated `id` gives `hy_flownetwork` (AGENTS.md conversion routes). With `return_dendritic = FALSE` on a network with a divergence, `id` repeats, so the result is the flownetwork pattern.

No sentinel: the join produces identifiers, not dense positions, and nothing crosses the K-1 boundary. Nothing depends on randomness or scheduling (D-15).

## Postconditions

- Every input column is present with its input values on every output row derived from that input row; `fromnode` values equal the input's.
- `toid` has no missing value and has the type of `id` (post-change in R; always `str` in Python).
- For every output row, `toid` is either an `id` in the output or the outlet value (`""` for character `id`, `0` otherwise).
- For every output row whose `toid` is an `id`, that flowline's `fromnode` equals the row's `tonode`.
- No output row's `toid` is the `id` of a flowline whose `tonode` is missing.
- With `return_dendritic = TRUE`, no output row's `toid` is the `id` of a flowline whose `divergence` is 2 or missing.
- The set of `id` values in the output equals the set in the input; every input row appears at least once.
- With `return_dendritic = TRUE`, the row count equals the input's when no `tonode` is shared by two eligible flowlines' `fromnode`; `id` is then unique.
- With `return_dendritic = FALSE`, a row appears once per eligible downstream flowline, or once with the outlet value when it has none.
- Rows with a missing `tonode` follow every other row, in input order; the other rows keep input order (step 7).
- D-7: for a row-permuted input, the multiset of (`id`, `toid`) pairs and every row's values are unchanged; only row order follows the permutation (step 7).

## Complexity

The join is O(n log n) time (O(n + m) with hashing) and O(n + m) space, for n rows and m output connections; m = n when no divergence is kept.

## Edge cases

- One flowline: returns one row with the outlet value as `toid`.
- No connections (every `tonode` unmatched, or every `tonode` missing): every row is an outlet; row order is input order with missing-`tonode` rows last.
- Zero rows: returns zero rows with `toid` typed (I-11); today `double` for an `integer` `id`, post-change `integer`.
- A divergence whose downstream flowlines all have `divergence` other than 2, with `return_dendritic = TRUE`: the upstream flowline appears once per downstream flowline and `id` repeats; no warning; R `hy` input returns `hy_flownetwork`.
- Missing `divergence` on a flowline below a divergence, with `return_dendritic = TRUE`: that flowline receives no upstream connection, so an upstream flowline whose other downstream has `divergence` 2 becomes an outlet.
- A flowline with a missing `tonode` and a valid `fromnode`: it is never a downstream target, so the flowline above it becomes an outlet.
- Duplicated row (repeated `id`) without `toid`: stops (`needs_node`). With `toid`: Preconditions.
- Input already has `toid`: stops (`toid_already_present`); with `return_dendritic = FALSE` the deprecation warning comes first.
- Missing weight, zero-length line, empty name: not applicable; those columns pass through.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| `return_dendritic = FALSE` (warning) | the non-dendritic edge list from `add_toids` is deprecated | all | `to_flownetwork()` for a non-dendritic edge list | `return_dendritic_deprecated` |
| `toid` already present | the network already has downstream identifiers; `add_toids` builds them from `fromnode`/`tonode` | all | use the network as it is, or drop `toid` first | `toid_already_present` |
| not the node pattern | no `fromnode`/`tonode` columns, or `id` repeats | all | supply `fromnode`/`tonode`, or `make_node_topology()` to build them | `needs_node` |
| no `divergence` with `return_dendritic = TRUE` | minor paths cannot be removed without a divergence flag | all | `add_divergence()` to compute one, or `to_flownetwork()` to keep every path | `divergence_required` |

## References

- R source: `R/add_toids.R` (`add_toids` and methods, `try_add_toids`), `R/hy_classes.R` (`classify_hy`, `hy_classify_and_redispatch`, `hy_dispatch_error`, `hy_guidance_node`), `R/hy.R` (`hy`, `hy_reverse`), `R/utils.R` (`get_outlet_value`).
- Tests: `tests/testthat/test_add_toids.R`, `tests/testthat/test_hy_classes.R` ("producers stamp correct output class", "add_toids warns for return_dendritic = FALSE").
- Description: `algorithms/hy.md` step 8 (the `add_topo` path calls `add_toids(x, return_dendritic = <divergence present>)`).
- NHDPlus Version 2 User Guide (McKay and others, 2012): `Divergence` 0 none, 1 main path, 2 minor path; `FromNode`/`ToNode` node topology.
- SPEC I-1, I-2, I-5, I-11, D-3, D-5, D-7, D-9, D-14, A-3, A-9, A-10, A-11, A-16; AGENTS.md "Conversion routes".

## Worked example

Fixture `fixtures/add_toids_worked_example.gpkg`, aspatial, five rows, all columns INTEGER:

| id | fromnode | tonode | divergence |
| --- | --- | --- | --- |
| 1 | 10 | 20 | 0 |
| 2 | 20 | 30 | 1 |
| 3 | 20 | 40 | 2 |
| 4 | 30 | 50 | 0 |
| 5 | 40 | 50 | 0 |

`add_toids(x)` returns five rows: (`id`, `toid`) = (1, 2), (2, 4), (3, 5), (4, 0), (5, 0); flowline 3 is the minor path and receives no upstream connection. `add_toids(x, return_dendritic = FALSE)` warns and returns six rows: (1, 2), (1, 3), (2, 4), (3, 5), (4, 0), (5, 0); with rows 2 and 3 of the input swapped it returns (1, 3), (1, 2), (3, 5), (2, 4), (4, 0), (5, 0). `toid` is `double` today, `integer` post-change; Python `str` with `""` for the outlets.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording R 1.3.0 behavior. Intended change I-2 (ROADMAP §4, M3): `toid` takes the type of `id` (pairing per D3-06) and the integer outlet value is `0L`; today `toid` is `double` for every non-character `id`. Captured, not changed (no §4 row): rows with a missing `tonode` move to the end (a D-14 departure); a missing `divergence` makes a flowline ineligible as a target; a flowline with a missing `tonode` is never a target; flownetwork input recurses in R, where Python raises `toid_already_present`. The A-10 condition-class change applies to the `needs_node` stop. A-11: Python's deprecation is a `FutureWarning`. No A-9 gap: A-16 matches R.
