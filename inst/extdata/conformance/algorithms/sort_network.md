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
- `hy_flownetwork` (repeated `id` with `toid`, I-6). The type check, then, post-change (D-8, D3-41), the same cycle check as on `hy_topo`: any cycle stops `sort_network` with `network_cycle` before any sorting. Today (1.3.0) only the type check runs, and a cycle drops its flowlines with the `sort_missed_features` warning.
- `hy_node` (no `toid`, `fromnode`/`tonode`): converted by `add_toids(x)` (default `return_dendritic = TRUE`, so `divergence` is then required, `divergence_required`), with an R `message()`; Python converts through `_route` silently (A-3, A-12). Post-change (D3-56, D3-72) the conversion also stops where `divergence` is missing on some rows (`divergence_missing`) or a divergence has no main path or more than one (`divergence_coding_invalid`). The result is `hy_topo` or, where `add_toids` keeps two downstream rows, `hy_flownetwork`.
- Anything else (no `toid` and no node columns): stop with the representation guidance (`needs_topo`).

Post-change (D-4 row of ROADMAP §4, D3-51), an `outlets` value that is not an `id` stops with `outlet_not_in_network`. Today such values are ignored.

## Definition

Roxygen: given a network with an id and toid, returns a sorted and potentially split set of output. Sort is from top to bottom so traversing the response from top to bottom will go from upstream to downstream. Can also be used as a very fast implementation of upstream with tributaries navigation. The full network from each outlet is returned in sorted order. If a network includes diversions, all flowlines downstream of the diversion are visited prior to continuing upstream. `split`: if TRUE, the result will be split into independent networks identified by the id of their outlet; the outlet id of each independent network is added as a "terminal_id" attribute. `outlets`: if specified, only the network emanating from these outlets will be considered and returned; if outlets does not include all outlets from a given network containing diversions, a partial network may be returned.

As a property, post-change (D-4), for a network without repeated `id`: the output is the concatenation, over outlets in ascending identifier byte order, of each outlet's block, where the block of flowline f is the blocks of the flowlines whose `toid` is f, in ascending identifier byte order of those flowlines, followed by f. Every flowline therefore follows everything upstream of it, and the flowlines upstream of f together with f are contiguous. Here and throughout, "upstream of f" means reachable from f by following `toid` backward, not physically upstream: on the `hy_node` route `add_toids` keeps only the main path at each divergence, so a minor path begins as a headwater of its own run, and the flowline above the divergence is not guaranteed to precede it (the `divergence_attribute` order in Worked example shows this). Identifier byte order is the order of the decimal strings (I-1, D-1): integer `id` 100 precedes 2. `sort_network` reads `id` and `toid` only. A flownetwork follows the procedure in Algorithm, which reduces to this property where no `id` repeats.

The order is the one NHDPlus encodes in hydrologic sequence (`Hydroseq`, a number that decreases downstream; `hydroseq` is the `topo_sort` alias, D3-51): sorting a dendritic NHDPlus table by descending `Hydroseq` also puts every flowline after everything upstream of it. The two orders share that partial order and break ties differently, so they need not agree row for row. `terminal_id` plays the part of NHDPlus `TerminalPa`, which identifies each flowline's terminal flowline by that flowline's `Hydroseq`; here it is the terminal flowline's `id`.

## Algorithm

1. Route (Preconditions): `data.frame` method runs `hy()`, dispatches, and restores input names (`hy_as_dataframe`); `hy` re-dispatches on the class; `hy_node` converts with `add_toids`. Python `_as_hy`, `_route`, `_hy_reverse` (A-3, A-17). Geometry is set aside and joined back by `id` after the sort (R `get_hyg`/`put_hyg`).
2. Checks (Preconditions): `check_hy_outlets(x, fix = FALSE)` and, post-change (D-8, D3-41), the cycle check on both patterns; today the cycle check runs on `hy_topo` only.
3. Build the index in both directions over distinct (`id`, `toid`) pairs: `make_index_ids(mode = "both")`, a CSR pair in Python (K-2, K-3), R's padded `make_index_ids_impl` list. Dense position follows first appearance of each `id`; it is not contract. A downstream connection to a `toid` outside the network counts toward a flowline's downstream count; this holds after D3-76 leaves such connections out of the `make_index_ids` index, so the sort counts them itself.
4. Starts. With `outlets = NULL`: every `id` with at least one row whose `toid` is not an `id` (I-4; the value is not consulted, so `0`, `""`, `-1`, `NA`, or an unmatched identifier all mark an outlet). If there is none, today warn `no_outlet` and continue with no starts; post-change a network with no outlet holds a cycle, which step 2 stops first (D3-106). With `outlets`: the `id` values in `outlets`; post-change a value that is not an `id` stops with `outlet_not_in_network`.
5. Walk, below the K-1 boundary on dense positions. Each flowline holds a countdown initialized to its number of distinct downstream connections. For each start in the start order, push it and walk depth-first with a LIFO stack: pop a flowline, give it the next visit number, then for each upstream flowline u, in the upstream order: if u's countdown is 1, push u; otherwise decrement it. A flowline is thus pushed only from the visit of its last downstream connection, so on a flownetwork every downstream path is visited before continuing upstream. A start is visited when its turn comes whether or not its countdown is spent; with `outlets` naming two flowlines of one network, the inner one is visited twice and keeps its later visit number. R stops with `sort_runaway` if the loop runs more than 2n steps (a guard).
6. Tie rules, binding. Post-change (D-4): starts are taken in descending identifier byte order and the upstream order is ascending identifier byte order, so the reversed visit order of step 7 puts outlets and siblings in ascending order (Definition). The upstream order is the sort's own and equals the D-5 adjacency order of the `from` index, which D3-40 makes binding again for `make_index_ids` (and D-6 for `navigate_network_dfs`); in R `sort_froms_by_rank` orders the padded upstream matrix by the identifier as character (D-1). Today (1.3.0): starts are taken in order of first appearance of the `id` in the input, and the upstream order is input row order, so outlets appear in reverse input order and siblings in input order.
7. Output: keep rows whose `id` was visited, in descending visit number (the reverse of the visit order: upstream first). Rows sharing one `id` stay contiguous. Post-change they are ordered by `toid` in identifier byte order; today they are in input order. Rows equal in both `id` and `toid` keep input order in either case (D-3, stable sorts).
8. With `outlets = NULL`, if fewer `id` values were visited than exist, today warn `sort_missed_features` (cycles, or flowlines whose countdown never reaches 1). Post-change only a cycle leaves a flowline unvisited, and step 2 stops on it first, so the warning goes (D3-107). No warning with `outlets`.
9. With `split = TRUE`: `terminal_id` is the start from whose walk the flowline was visited, joined by `id`. If any flowline was visited twice (step 5), stop with `split_outlets_overlap`.
10. R re-classifies the result (`classify_hy`) on the `hy_topo` route.

Sentinel (K-4): the stack and the dense upstream view use −1 in Python; R's existing body uses 0 in its stack and `NA` in the padded matrix, and new R code for D-4 uses 0. D rules relied on: D-1, D-3, D-4, D-5 (D3-40), D-7, D-8 (post-change check, both patterns), D-14. Nothing depends on randomness or scheduling (D-15).

## Postconditions

- Every output row is an input row with its values unchanged; `terminal_id` is the only added column.
- The set of output `id` values is the set reached from the starts; with `outlets = NULL` and no warning it is every input `id`, and the output row count equals the input row count.
- On a network without repeated `id` and without a warning, every flowline's row comes after the rows of every flowline upstream of it; on a flownetwork, every flowline comes after every flowline upstream of it except where a start is visited early (Edge cases).
- Rows sharing an `id` are contiguous.
- Post-change (D-4), no repeated `id`: for siblings a and b (same `toid`) with a before b in identifier byte order, every row of a's block precedes every row of b's block; outlet blocks appear in ascending identifier byte order of the outlets.
- Post-change (D-4): rows sharing an `id` are in identifier byte order of `toid`.
- Post-change (D-4, D-7): idempotent, `sort_network(sort_network(x))` equals `sort_network(x)`; permutation-invariant without re-sorting, a row-permuted input gives the same output in the same order, except that rows equal in `id` and `toid` but different elsewhere keep their relative input order.
- Post-change (D-8, D3-41), `hy_topo` or `hy_flownetwork` input: no output exists for a network with a cycle; the call stops.
- Post-change (D-4 row, D3-51): no output exists when an `outlets` value is not an `id`; the call stops.
- Post-change (I-11, D3-46): zero-row input gives zero-row output with every input column and, with `split = TRUE`, `terminal_id`, each typed.
- With `split = TRUE`, `terminal_id` is constant over each block and equals the `id` of a start; with `outlets = NULL` it is an outlet's `id`.
- `terminal_id` has the type of `id` and no missing value.

## Complexity

The walk is O(n + m) time and O(n + m) space for n distinct `id` values and m distinct (`id`, `toid`) pairs. The D-4 ordering adds O(m log m) for the adjacency sort; the final reorder is O(r log r) for r rows.

## Edge cases

- One flowline: returns its row; with `split = TRUE`, `terminal_id` is its `id`.
- No connections (every row an outlet): post-change the rows in ascending identifier byte order (S-6: `all_terminals` gives 100, 2, 30, 4, 7); today in reverse input order.
- A flowline with no connections inside a larger network: it is its own outlet block, placed by its identifier among the outlets.
- Duplicated row (repeated `id` and `toid`, a flownetwork): returned as given, both rows contiguous, no warning. S-6 lists "duplicated edge in a flownetwork: raises"; the ROADMAP §4 row that makes it raise at ingest lands in M6 (D3-57), so in M3 this is captured (D3-46). Real tables carry such rows: in `sort_network_dups` one NextGen flowpath has several rows with the same `id` and `toid` and different `divide_id` or `poi_id`, and the Postconditions keep them all.
- Zero rows: post-change (I-11, D3-46) zero rows with every column typed, in both languages. R 1.3.0 stops inside the index build ("subscript out of bounds", after a `max` warning), a message that is not hydroloom's and has no `remedies.json` key.
- No outlet (every flowline in a cycle): today `hy_topo` warns `no_outlet` and `sort_missed_features` and returns zero rows, and a flownetwork does the same. Post-change both stop with `network_cycle` (D-8, D3-41).
- `outlets` naming no `id`: post-change stops with `outlet_not_in_network`; today zero rows, no warning.
- A divergence with one branch leaving the table and one inside it: a distributary that reaches the coast or crosses a processing-unit boundary while its other branch stays in the basin. The diverging flowline is a start (step 4), and the basin outlet's walk never pushes it (its countdown stops at 1). Two consequences: with `split = TRUE` the diverging flowline and the whole drainage above it get the diverging flowline's `id` as `terminal_id`, not the basin outlet's; and that block can land below its in-network downstream flowline, so a downstream accumulation in output order can reach that flowline before the drainage above is added. Fixture `sort_divergence_leaves.gpkg` (2 diverges to 3 inside and to 9 outside): today 3, 1, 4, 2, 2 with `terminal_id` 3 on id 3 and 2 elsewhere, so 3 precedes 2; post-change (D-4) 1, 4, 2, 2, 3, the same `terminal_id` values. This is captured behavior; whether to change it is the maintainer's call. `navigate_network_dfs` binds the same reading of the outside branch (D3-59).
- Missing weight, zero-length line, empty name: not applicable; those columns pass through.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| no `toid` and no node columns | the function needs each flowline's downstream flowline (`toid`), and the network has neither `toid` nor `fromnode`/`tonode`, the identifiers of the points at each end of every flowline that `add_toids()` builds `toid` from | all | Add a toid column giving each flowline's downstream flowline, then call again. | `needs_topo` |
| node table with no `divergence` column | keeping only the main path where a flowline splits into two or more downstream flowlines (a divergence) needs the `divergence` column, which marks the main path 1 and minor paths 2; the network has none | all | add_divergence(), given the coastal and inland outlets, to compute divergence. | `divergence_required` |
| node table with `divergence` missing on some rows (post-change, through `add_toids`) | `add_toids()` was asked to keep only the main path at each divergence, and `divergence` is missing on one or more flowlines, so the main path cannot be told from a minor one there | the rows where divergence is missing | add_divergence(), given the coastal and inland outlets, to compute divergence. | `divergence_missing` |
| node table with a divergence coded without one main path (post-change, through `add_toids`) | at one or more splits every downstream flowline is coded 2, leaving no main path, or two or more are not coded 2, leaving a flowline with more than one downstream flowline | the rows of the downstream flowlines at each split whose divergence coding is broken | add_divergence(), given the coastal and inland outlets, to recompute divergence. | `divergence_coding_invalid` |
| cycle in the network | some flowlines drain back into themselves: following `toid` downstream from one of them returns to it, so the network has no upstream-to-downstream order | the flowlines in each cycle, as check_hy_graph() returns them | check_hy_graph() to list them, then correct their toid. | `network_cycle` |
| `id` and `toid` of different types (warning) | one of `id` and `toid` is text and the other is not, so a `toid` can fail to match the `id` it points to and a flowline can be taken for an outlet, the flowline where water leaves the network | all | Convert id and toid to one type, for example text on both. | `id_toid_type_mismatch` |
| no outlet (warning; 1.3.0 only, dropped with D3-106) | no flowline's `toid` leaves the network, so no flowline is an outlet and the sort has nowhere to start; such a network contains a cycle, and the flowlines in it and those draining into it are left out of the sort | all | check_hy_graph() to list the flowlines in the cycle; then give the outlet flowline a toid that is not an id. | `no_outlet` |
| flowlines not reached (warning; 1.3.0 only, dropped with D3-107) | some flowlines were not reached from any outlet, usually because they drain into a cycle, a run of flowlines whose `toid` leads back to themselves; they and the cycle are left out of the result | the flowlines missing from the result | check_hy_graph() to find the cycle. | `sort_missed_features` |
| `split = TRUE` with nested outlets | with `split` true, each of the flowlines given in `outlets` gets its own group of upstream flowlines; one of the outlets passed is upstream of another, so the flowlines above the upstream one would belong to both groups | the flowlines reached from both outlets | Remove the upstream one of the two from outlets, then call again. | `split_outlets_overlap` |
| an `outlets` value that is not an `id` (post-change) | an outlet passed to the sort is not the `id` of any flowline in the network, so the sort has no flowline to start from for it | the outlets not found among id | Pass values of id as outlets. | `outlet_not_in_network` |
| walk did not end (guard) | the sort took more than twice as many steps as there are flowlines, which a cycle causes | all | check_hy_graph() to find the cycle. | `sort_runaway` |

## References

- R source: `R/sort_network.R` (`sort_network` and methods, `sort_network_impl`), `R/check_hy_graph.R` (`check_hy_graph`, `check_hy_outlets`), `R/make_index_ids.R` (`make_index_ids_impl`, `make_to_dt`, `make_from_dt`), `R/hy_classes.R` (`hy_as_dataframe`, `hy_classify_and_redispatch`, `hy_node_to_topo`, `hy_dispatch_error`, `hy_guidance_topo`), `R/utils.R` (`is_outlet`, `get_hyg`, `put_hyg`).
- Tests: `tests/testthat/test_sort_network.R`, `tests/testthat/test_outlet_conventions.R`, `tests/testthat/test_hy_classes.R`.
- Descriptions: `algorithms/add_toids.md` (node route), `algorithms/hy_capabilities.md` (code table row).
- NHDPlus hydrologic sequence (`Hydroseq`): McKay, L., and others, 2012, NHDPlus Version 2: User Guide, U.S. Environmental Protection Agency (Value Added Attributes).
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
- 2026-10-26, suite 0.1.0: review round (hydrology reader). Edge case for a divergence with one branch leaving the table, with fixture `sort_divergence_leaves.gpkg`; "upstream" defined as reachable through `toid`; relation to NHDPlus `Hydroseq`; Remedies fix texts for `needs_topo`, `sort_missed_features`, `sort_runaway`, `split_outlets_overlap`, and a `divergence_required` row; decision ids D3-40, D3-41, D3-46, D3-55, D3-57, D3-59 cited in place of proposed rows.
- 2026-10-26, suite 0.1.0: remedies pass. The `network_cycle` Remedies row no longer says only pairs and self-loops stop; since D-8 every cycle does (D3-88). No behavior change.
- 2026-10-27, suite 0.1.0: M3 close, intended changes from the dispositions, each landing in R in this run. D3-41 (ROADMAP §4 D-8 row): the cycle check runs on the flownetwork route too, so a cycle there stops with `network_cycle` where 1.3.0 warns `sort_missed_features` and drops rows; with it the `no_outlet` and `sort_missed_features` warnings become unreachable and go (D3-106, D3-107). D3-51 (§4 D-4 row): an `outlets` value that is not an `id` stops with `outlet_not_in_network`; 1.3.0 ignores it. D3-46 (§4 I-11 row): zero-row input returns typed zero-row output in R as in Python. D3-40 (§4 D-5/D-6 row): D-5 binds the adjacency order the sort walks; D3-76 leaves outside connections out of the `make_index_ids` index, and the sort keeps counting them (step 3). D3-56, D3-72 (§4 `add_toids` row): the node route stops on `divergence_missing` and `divergence_coding_invalid`. D3-71: `TerminalPa` sentence in Definition. D3-146: Remedies texts from `remedies.json`.
