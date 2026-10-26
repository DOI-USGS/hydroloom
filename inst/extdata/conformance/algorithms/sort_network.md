---
requires:
  pattern: [hy_topo, hy_flownetwork]
  columns: []
  via:
    hy_node: add_toids
---

# sort_network

## Signature

R: `sort_network(x, split = FALSE, outlets = NULL)`, with methods for `data.frame`, `hy`, `hy_node`, `hy_topo`, and `hy_flownetwork`.

| argument | type | default |
| --- | --- | --- |
| `x` | table (data.frame, tibble, or sf) | required |
| `split` | flag: R `logical` length one, Python `bool` | `FALSE` / `False` |
| `outlets` | identifiers, the type of `id`; any length | `NULL` / `None` |

Python (ROADMAP §2, SPEC A-16): `sort_network(network, *, split=False, outlets=None)`, returning a DataFrame, or a GeoDataFrame when the input is one (A-8). `network` is R's `x` (A-9). No A-9 gap: A-16 matches R.

Return: the input rows, reordered (rows not reached are dropped, Postconditions), with one column added when `split = TRUE`.

| column | type | when |
| --- | --- | --- |
| every input column | its input name and type | always |
| `toid` | identifier, the type of `id` | always; added by `add_toids` on the node route |
| `terminal_id` | identifier, the type of `id` (I-1; R converts with `as(<names>, class(id))`, so `double` today where `id` is `double`, `integer` after I-2) | `split = TRUE` |

Row order is the output (D-4, S-4 `order: exact`). R places `id`, `toid` first and `terminal_id` after the other attributes, geometry last; column position is not contract (D-14). A `data.frame` input returns with its input column names and no hydroloom class. An `hy` input returns `hy_topo` for `hy_topo` input and `hy_flownetwork` for flownetwork input. Python returns no class; `representation(network)` gives the pattern (A-2).

No named kernel (K-6, K-11). The walk is an unnamed, language-local kernel below the K-1 boundary.

## Preconditions

The input, after `hy()` (R) or `as_hy` (Python), must be one of:

- `hy_topo` (unique `id` with `toid`; `hy_leveled` satisfies it, A-3). Checks, in order: the `id`/`toid` type check (warning `id_toid_type_mismatch` when one is character and the other is not; R then fails inside the data.table join; after I-2 with D3-06 pairing the two always share a type and the warning cannot fire), then the cycle check (stop, `network_cycle`).
  - Today (hydroloom 1.3.0) the cycle check is the reciprocal-pair join in `check_hy_graph(x)`: it stops only when some row's `toid` names a flowline whose own `toid` is the row's `id`. That join also catches a self-loop (`toid == id`). A longer cycle passes the check and is dropped by the sort with the `sort_missed_features` warning.
  - Post-change (D-8): `check_hy_graph(x)` runs strongly connected components over the `id`/`toid` graph. Any component larger than one, or any self-loop, stops `sort_network` with `network_cycle` before any sorting; the problem rows carry `component`, and a size-two component also `reciprocal`, in `check_hy_graph`'s return. Every cycle the 1.3.0 join catches is still caught; a three-flowline cycle now stops where it used to warn.
- `hy_flownetwork` (repeated `id` with `toid`, I-6). Only the type check runs; no cycle check, today or post-change (D3 row proposed). A cycle drops its flowlines with the `sort_missed_features` warning.
- `hy_node` (no `toid`, `fromnode`/`tonode`): converted by `add_toids(x)` (default `return_dendritic = TRUE`, so `divergence` is then required, `divergence_required`), with an R `message()`; Python converts through `_route` silently (A-3, A-12). The result is `hy_topo` or, where `add_toids` keeps two downstream rows, `hy_flownetwork`.
- Anything else (no `toid` and no node columns): stop with the representation guidance (`needs_topo`).

`outlets` values that are not an `id` are ignored.

## Definition

Roxygen: given a network with an id and toid, returns a sorted and potentially split set of output. Sort is from top to bottom so traversing the response from top to bottom will go from upstream to downstream. Can also be used as a very fast implementation of upstream with tributaries navigation. The full network from each outlet is returned in sorted order. If a network includes diversions, all flowlines downstream of the diversion are visited prior to continuing upstream. `split`: if TRUE, the result will be split into independent networks identified by the id of their outlet; the outlet id of each independent network is added as a "terminal_id" attribute. `outlets`: if specified, only the network emanating from these outlets will be considered and returned; if outlets does not include all outlets from a given network containing diversions, a partial network may be returned.

As a property, post-change (D-4), for a network without repeated `id`: the output is the concatenation, over outlets in ascending identifier byte order, of each outlet's block, where the block of flowline f is the blocks of the flowlines whose `toid` is f, in ascending identifier byte order of those flowlines, followed by f. Every flowline therefore follows everything upstream of it, and the flowlines upstream of f together with f are contiguous. Identifier byte order is the order of the decimal strings (I-1, D-1): integer `id` 100 precedes 2. `sort_network` reads `id` and `toid` only. A flownetwork follows the procedure in Algorithm, which reduces to this property where no `id` repeats.

## Algorithm

1. Route (Preconditions): `data.frame` method runs `hy()`, dispatches, and restores input names (`hy_as_dataframe`); `hy` re-dispatches on the class; `hy_node` converts with `add_toids`. Python `_as_hy`, `_route`, `_hy_reverse` (A-3, A-17). Geometry is set aside and joined back by `id` after the sort (R `get_hyg`/`put_hyg`).
2. Checks (Preconditions): `check_hy_outlets(x, fix = FALSE)` on both patterns; the cycle check on `hy_topo` only.
3. Build the index in both directions over distinct (`id`, `toid`) pairs: `make_index_ids(mode = "both")`, a CSR pair in Python (K-2, K-3), R's padded `make_index_ids_impl` list. Dense position follows first appearance of each `id`; it is not contract. A downstream connection to a `toid` outside the network counts toward a flowline's downstream count.
4. Starts. With `outlets = NULL`: every `id` with at least one row whose `toid` is not an `id` (I-4; the value is not consulted, so `0`, `""`, `-1`, `NA`, or an unmatched identifier all mark an outlet). If there is none, warn `no_outlet` and continue with no starts. With `outlets`: the `id` values in `outlets`.
5. Walk, below the K-1 boundary on dense positions. Each flowline holds a countdown initialized to its number of distinct downstream connections. For each start in the start order, push it and walk depth-first with a LIFO stack: pop a flowline, give it the next visit number, then for each upstream flowline u, in the upstream order: if u's countdown is 1, push u; otherwise decrement it. A flowline is thus pushed only from the visit of its last downstream connection, so on a flownetwork every downstream path is visited before continuing upstream. A start is visited when its turn comes whether or not its countdown is spent; with `outlets` naming two flowlines of one network, the inner one is visited twice and keeps its later visit number. R stops with `sort_runaway` if the loop runs more than 2n steps (a guard).
6. Tie rules, binding. Post-change (D-4): starts are taken in descending identifier byte order and the upstream order is ascending identifier byte order, so the reversed visit order of step 7 puts outlets and siblings in ascending order (Definition). In Python the ascending upstream order is the D-5 adjacency order of the `from` index; in R the new code sorts with `dplyr::arrange` or `data.table::setorder` on the identifier as character (D-1). Today (1.3.0): starts are taken in order of first appearance of the `id` in the input, and the upstream order is input row order, so outlets appear in reverse input order and siblings in input order.
7. Output: keep rows whose `id` was visited, in descending visit number (the reverse of the visit order: upstream first). Rows sharing one `id` stay contiguous. Post-change they are ordered by `toid` in identifier byte order; today they are in input order. Rows equal in both `id` and `toid` keep input order in either case (D-3, stable sorts).
8. With `outlets = NULL`, if fewer `id` values were visited than exist, warn `sort_missed_features` (cycles, or flowlines whose countdown never reaches 1). No warning with `outlets`.
9. With `split = TRUE`: `terminal_id` is the start from whose walk the flowline was visited, joined by `id`. If any flowline was visited twice (step 5), stop with `split_outlets_overlap`.
10. R re-classifies the result (`classify_hy`) on the `hy_topo` route.

Sentinel (K-4): the stack and the dense upstream view use −1 in Python; R's existing body uses 0 in its stack and `NA` in the padded matrix, and new R code for D-4 uses 0. D rules relied on: D-1, D-3, D-4, D-5 (post-change upstream order), D-7, D-8 (post-change check), D-14. Nothing depends on randomness or scheduling (D-15).

## Postconditions

- Every output row is an input row with its values unchanged; `terminal_id` is the only added column.
- The set of output `id` values is the set reached from the starts; with `outlets = NULL` and no warning it is every input `id`, and the output row count equals the input row count.
- On a network without repeated `id` and without a warning, every flowline's row comes after the rows of every flowline upstream of it; on a flownetwork, every flowline comes after every flowline upstream of it except where a start is visited early (Edge cases).
- Rows sharing an `id` are contiguous.
- Post-change (D-4), no repeated `id`: for siblings a and b (same `toid`) with a before b in identifier byte order, every row of a's block precedes every row of b's block; outlet blocks appear in ascending identifier byte order of the outlets.
- Post-change (D-4): rows sharing an `id` are in identifier byte order of `toid`.
- Post-change (D-4, D-7): idempotent, `sort_network(sort_network(x))` equals `sort_network(x)`; permutation-invariant without re-sorting, a row-permuted input gives the same output in the same order, except that rows equal in `id` and `toid` but different elsewhere keep their relative input order.
- Post-change (D-8), `hy_topo` input: no output exists for a network with a cycle; the call stops.
- With `split = TRUE`, `terminal_id` is constant over each block and equals the `id` of a start; with `outlets = NULL` it is an outlet's `id`.
- `terminal_id` has the type of `id` and no missing value.

## Complexity

The walk is O(n + m) time and O(n + m) space for n distinct `id` values and m distinct (`id`, `toid`) pairs. The D-4 ordering adds O(m log m) for the adjacency sort; the final reorder is O(r log r) for r rows.

## Edge cases

- One flowline: returns its row; with `split = TRUE`, `terminal_id` is its `id`.
- No connections (every row an outlet): post-change the rows in ascending identifier byte order (S-6: `all_terminals` gives 100, 2, 30, 4, 7); today in reverse input order.
- A flowline with no connections inside a larger network: it is its own outlet block, placed by its identifier among the outlets.
- Duplicated row (repeated `id` and `toid`, a flownetwork): returned as given, both rows contiguous, no warning. S-6 lists "duplicated edge in a flownetwork: raises"; R does not, and no §4 row changes it, so this is captured (D3 row proposed).
- Zero rows: R 1.3.0 stops inside the index build ("subscript out of bounds", after a `max` warning); no `remedies.json` key, since the message is not hydroloom's. Python returns zero rows with every column typed (I-11). Captured R-only (D3 row proposed).
- No outlet (every flowline in a cycle): `hy_topo` today warns `no_outlet` and `sort_missed_features` and returns zero rows; post-change it stops with `network_cycle`. Flownetwork: the two warnings, zero rows, today and post-change.
- `outlets` naming no `id`: zero rows, no warning.
- A flowline with one downstream row leaving the network and another inside it: it is a start; visited as its own start, it can appear before its in-network downstream flowline.
- Missing weight, zero-length line, empty name: not applicable; those columns pass through.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| no `toid` and no node columns | the table has no downstream identifiers to sort by | all | `add_toids()` to build `toid` from `fromnode`/`tonode`, or `hy(x, add_topo = TRUE)` | `needs_topo` |
| cycle in the network | flowlines whose `toid` leads back to themselves (today only pairs that point at each other, and self-loops) | the cycle's rows, from `check_hy_graph()` | `check_hy_graph(x)` to list them, then correct their `toid` | `network_cycle` |
| `id` and `toid` of different types (warning) | one is character and the other is not, so outlet detection may be wrong | all | give `id` and `toid` one type | `id_toid_type_mismatch` |
| no outlet (warning) | no row's `toid` leaves the network | all | correct the `toid` of the outlet flowline | `no_outlet` |
| flowlines not reached (warning) | some flowlines were not reached from any outlet, usually a cycle | the flowlines missing from the result | `check_hy_graph(x)` to find the cycle | `sort_missed_features` |
| `split = TRUE` with nested outlets | two `outlets` lie in one network, so a flowline would belong to two | flowlines reached from both | drop the inner outlet, or use `split = FALSE` | `split_outlets_overlap` |
| walk did not end (guard) | the walk ran past twice the network size | all | `check_hy_graph(x)` | `sort_runaway` |

## References

- R source: `R/sort_network.R` (`sort_network` and methods, `sort_network_impl`), `R/check_hy_graph.R` (`check_hy_graph`, `check_hy_outlets`), `R/make_index_ids.R` (`make_index_ids_impl`, `make_to_dt`, `make_from_dt`), `R/hy_classes.R` (`hy_as_dataframe`, `hy_classify_and_redispatch`, `hy_node_to_topo`, `hy_dispatch_error`, `hy_guidance_topo`), `R/utils.R` (`is_outlet`, `get_hyg`, `put_hyg`).
- Tests: `tests/testthat/test_sort_network.R`, `tests/testthat/test_outlet_conventions.R`, `tests/testthat/test_hy_classes.R`.
- Descriptions: `algorithms/add_toids.md` (node route), `algorithms/hy_capabilities.md` (code table row).
- DESIGN "Sibling order in `sort_network` by identifier alone"; SPEC I-1, I-4, I-6, I-11, D-1, D-3, D-4, D-5, D-7, D-8, D-14, D-15, K-1 to K-4, S-6, A-3, A-9, A-10, A-12, A-16.

## Worked example

Fixture `fixtures/sort_siblings.gpkg`, aspatial, INTEGER columns:

| id | toid |
| --- | --- |
| 30 | 4 |
| 2 | 4 |
| 10 | 4 |
| 4 | 5 |
| 5 | 0 |

Three siblings (30, 2, 10) flow to 4, which flows to the outlet 5. Post-change (D-4) `sort_network(x)` returns `id` 10, 2, 30, 4, 5: siblings in identifier byte order ("10" < "2" < "30"), then 4, then 5. Any row permutation of the input returns the same order. Today R returns 30, 2, 10, 4, 5 (siblings in input order). With `split = TRUE` every `terminal_id` is 5.

A second check on `divergence_attribute.gpkg` (node route, outlets 12 and 13): post-change 4, 1, 2, 3, 5, 6, 7, 8, 10, 11, 9, 12, 13 ("11" precedes "9"); today 13, 9, 4, 1, 2, 3, 5, 6, 7, 8, 10, 11, 12.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording R 1.3.0 behavior, with three intended changes (ROADMAP §4, M3) stated above. D-4: outlets and siblings by identifier byte order, rows sharing an `id` by `toid`; today DFS-visit order from input row order (outlets reversed, siblings in input order). D-8: on `hy_topo` the cycle check is strongly connected components, so every cycle stops with `network_cycle`; today only reciprocal pairs and self-loops stop, and longer cycles warn and drop. I-2: `id`, `toid`, `terminal_id` typed per I-2, and the `id`/`toid` type mismatch cannot arise through `hy()`. The A-10 condition-class change applies to the `needs_topo` stop. Captured, not changed (no §4 row): the flownetwork method runs no cycle check; duplicated edges return without raising (S-6 says raises); zero rows stop in R (I-11 says zero rows; Python returns them). `requires.pattern` lists `hy_flownetwork`, which the 1.3.0 `hy_capabilities()` predicate omits.
