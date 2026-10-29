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

Return: the input rows, reordered, with one column added when `split = TRUE`. With `outlets = NULL` every row is returned; with `outlets`, only the rows of the networks above those outlets (Postconditions).

| column | type | when |
| --- | --- | --- |
| every input column | its input name and type | always |
| `toid` | identifier, the type of `id` | always; added by `add_toids` on the node route |
| `terminal_id` | identifier, the type of `id` (I-1, I-2; R converts with `as(<names>, class(id))`, so `integer` or `character` as `id` is) | `split = TRUE` |

Row order is the output (D-4, S-4 `order: exact`). R places `id`, `toid` first and `terminal_id` after the other attributes, geometry last; column position is not contract (D-14). A `data.frame` input returns with its input column names and no hydroloom class. An `hy` input returns `hy_topo` for `hy_topo` input and `hy_flownetwork` for flownetwork input. Python returns no class; `representation(network)` gives the pattern (A-2).

Conformance compares every returned column on every case except those that list `columns:` in the manifest (the `new_hope*.gpkg`, `nhdplushr.gpkg`, and `network.gpkg` cases, D3-185, D3-189), which compare the identifier, `toid`, and with `split = TRUE` `terminal_id`; row order and row count are compared on every case.

No named kernel (K-6, K-11). The walk is an unnamed, language-local kernel below the K-1 boundary.

## Preconditions

The input, after `hy()` (R) or `as_hy` (Python), must be one of:

- `hy_topo` (unique `id` with `toid`; `hy_leveled` satisfies it, A-3). Checks, in order: the `id`/`toid` type check (warning `id_toid_type_mismatch` when one is character and the other is not; `hy()` gives `id` and `toid` one type, I-2 with D3-06, so the warning cannot fire on input that came through `hy()`), then the cycle check (D-8): `check_hy_graph(x)` runs strongly connected components over the `id`/`toid` graph, and any component larger than one, or any self-loop, stops `sort_network` with `network_cycle` before any sorting. The problem rows carry `component`, and a size-two component also `reciprocal`, in `check_hy_graph`'s return.
- `hy_flownetwork` (repeated `id` with `toid`, I-6). The same type check and the same cycle check (D-8, D3-41): any cycle stops `sort_network` with `network_cycle` before any sorting.
- `hy_node` (no `toid`, `fromnode`/`tonode`): converted by `add_toids(x)` (default `return_dendritic = TRUE`, so `divergence` is then required, `divergence_required`), with an R `message()`; Python converts through `_route` silently (A-3, A-12). The conversion also stops where `divergence` is missing on some rows (`divergence_missing`) or a divergence has no main path or more than one (`divergence_coding_invalid`) (D3-56, D3-72). Two codings it tolerates with a warning are under Edge cases. The result is `hy_topo` or, where `add_toids` keeps two downstream rows, `hy_flownetwork`.
- Anything else (no `toid` and no node columns): stop with the representation guidance (`needs_topo`).

An `outlets` value that is not an `id` stops with `outlet_not_in_network`, one absent value among present ones included (D-4 row of ROADMAP §4, D3-51, D3-154).

## Definition

Roxygen: given a network with an id and toid, returns a sorted and potentially split set of output. Sort is from top to bottom so traversing the response from top to bottom will go from upstream to downstream. Can also be used as a very fast implementation of upstream with tributaries navigation. The full network from each outlet is returned in sorted order. If a network includes diversions, all flowlines downstream of the diversion are visited prior to continuing upstream. `split`: if TRUE, the result will be split into independent networks identified by the id of their outlet; the outlet id of each independent network is added as a "terminal_id" attribute. `outlets`: if specified, only the network emanating from these outlets will be considered and returned; if outlets does not include all outlets from a given network containing diversions, a partial network may be returned.

As a property (D-4), for a network without repeated `id`: the output is the concatenation, over outlets in ascending identifier byte order, of each outlet's block, where the block of flowline f is the blocks of the flowlines whose `toid` is f, in ascending identifier byte order of those flowlines, followed by f. Every flowline therefore follows everything upstream of it, and the flowlines upstream of f together with f are contiguous. Here and throughout, "upstream of f" means reachable from f by following `toid` backward, not physically upstream: on the `hy_node` route `add_toids` keeps only the main path at each divergence, so a minor path begins as a headwater of its own run, and the flowline above the divergence is not guaranteed to precede it (the `divergence_attribute` order in Worked example shows this). Identifier byte order is the order of the decimal strings (I-1, D-1): integer `id` 100 precedes 2. `sort_network` reads `id` and `toid` only. A flownetwork follows the procedure in Algorithm, which reduces to this property where no `id` repeats.

The order is the one NHDPlus encodes in hydrologic sequence (`Hydroseq`, a number that decreases downstream; NHDPlus `hydroseq` and 3DHP `hydrosequence` are `topo_sort` aliases, D3-51, add_topo_sort H-04): sorting a dendritic NHDPlus table by descending `Hydroseq` also puts every flowline after everything upstream of it. The two orders share that partial order and break ties differently, so they need not agree row for row. With `outlets = NULL` on a network without repeated `id`, `terminal_id` plays the part of NHDPlus `TerminalPa`, which identifies each flowline's terminal flowline by the terminal flowline's `Hydroseq`; here it is the terminal flowline's `id`.

## Algorithm

1. Route (Preconditions): `data.frame` method runs `hy()`, dispatches, and restores input names (`hy_as_dataframe`); `hy` re-dispatches on the class; `hy_node` converts with `add_toids`. Python `_as_hy`, `_route`, `_hy_reverse` (A-3, A-17). Geometry is set aside and joined back by `id` after the sort (R `get_hyg`/`put_hyg`).
2. Checks (Preconditions): `check_hy_outlets(x, fix = FALSE)`, then the cycle check `check_hy_graph(x)` on both patterns (D-8, D3-41).
3. Outlets check: with `outlets`, stop with `outlet_not_in_network` if any value is not an `id`, before the index is built (D3-154). Build the index in both directions over distinct (`id`, `toid`) pairs: `make_index_ids(mode = "both")`, a CSR pair in Python (K-2, K-3), R's padded `make_index_ids_impl` list. Dense position follows first appearance of each `id`; it is not contract. A downstream connection to a `toid` outside the network counts toward a flowline's downstream count. `make_index_ids` leaves such connections out of its index (D3-76), so the sort counts them itself (D3-155); R's index keeps the outside slot, and its count includes it.
4. Starts. With `outlets = NULL`: every `id` with at least one row whose `toid` is not an `id` (I-4; the value is not consulted, so `0`, `""`, `-1`, `NA`, or an unmatched identifier all mark an outlet). A network with no outlet holds a cycle, which step 2 stops first (D3-106). With `outlets`: the `id` values in `outlets`.
5. Walk, below the K-1 boundary on dense positions. Each flowline holds a countdown initialized to its number of distinct downstream connections. For each start in the start order, push it and walk depth-first with a LIFO stack: pop a flowline, give it the next visit number, then for each upstream flowline u, in the upstream order: if u's countdown is 1, push u; otherwise decrement it. A flowline is thus pushed only from the visit of its last downstream connection, so on a flownetwork every downstream path is visited before continuing upstream. A start is visited when its turn comes whether or not its countdown is spent; with `outlets` naming two flowlines of one network, the inner one is visited twice and keeps its later visit number. R stops with `sort_runaway` if the loop runs more than 2n steps (a guard).
6. Tie rules, binding (D-4): starts are taken in descending identifier byte order and the upstream order is ascending identifier byte order, so the reversed visit order of step 7 puts outlets and siblings in ascending order (Definition). The order of the `outlets` argument is not read (D3-50). The upstream order is the sort's own and equals the D-5 adjacency order of the `from` index (D3-40); in R `sort_froms_by_rank` orders the padded upstream matrix by the identifier as character (D-1), ranked by `id_byte_rank` in C locale.
7. Output: keep rows whose `id` was visited, in descending visit number (the reverse of the visit order: upstream first). Rows sharing one `id` stay contiguous and are ordered by `toid` in identifier byte order. Rows equal in both `id` and `toid` keep input order (D-3, stable sorts).
8. With `outlets = NULL` every `id` is visited: only a cycle leaves a flowline unvisited, and step 2 stops on it first, so no warning is raised (D3-107). With `outlets`, flowlines outside the named outlets' networks are left out, with no warning.
9. With `split = TRUE`: `terminal_id` is the start from whose walk the flowline was visited, joined by `id`. If any flowline was visited twice (step 5), stop with `split_outlets_overlap`.
10. R re-classifies the result (`classify_hy`) on the `hy_topo` route.

Sentinel (K-4): the stack and the dense upstream view use −1 in Python; R's body uses 0 in its stack and `NA` in the padded matrix, and the D-4 ranking adds no sentinel. D rules relied on: D-1, D-3, D-4, D-5 (D3-40), D-7, D-8 (both patterns), D-14. Nothing depends on randomness or scheduling (D-15).

## Postconditions

- Every output row is an input row with its values unchanged; `terminal_id` is the only added column.
- The set of output `id` values is the set reached from the starts; with `outlets = NULL` it is every input `id`, and the output row count equals the input row count.
- On a network without repeated `id`, every flowline's row comes after the rows of every flowline upstream of it; on a flownetwork, every flowline comes after every flowline upstream of it, except that the block of a flowline with a downstream connection outside the table can land below its in-network downstream flowline (Edge cases), and with `outlets` so can the block of a named flowline with a downstream connection the walks do not reach.
- Rows sharing an `id` are contiguous.
- No repeated `id` (D-4): for siblings a and b (same `toid`) with a before b in identifier byte order, every row of a's block precedes every row of b's block; outlet blocks appear in ascending identifier byte order of the outlets.
- Rows sharing an `id` are in identifier byte order of `toid` (D-4).
- Idempotent, `sort_network(sort_network(x))` equals `sort_network(x)`; permutation-invariant without re-sorting, a row-permuted input gives the same output in the same order, except that rows equal in `id` and `toid` but different elsewhere keep their relative input order (D-4, D-7).
- `hy_topo` or `hy_flownetwork` input: no output exists for a network with a cycle; the call stops (D-8, D3-41).
- No output exists when an `outlets` value is not an `id`; the call stops (D3-51, D3-154).
- Zero-row input gives zero-row output with every input column and, with `split = TRUE`, `terminal_id`, each typed (I-11, D3-46).
- With `split = TRUE`, `terminal_id` is constant over each block and equals the `id` of a start; with `outlets = NULL` it is an outlet's `id`.
- `terminal_id` has the type of `id` and no missing value.

## Complexity

The walk is O(n + m) time and O(n + m) space for n distinct `id` values and m distinct (`id`, `toid`) pairs. The D-4 ordering adds O(m log m) for the adjacency sort; the final reorder is O(r log r) for r rows.

## Edge cases

- One flowline: returns its row; with `split = TRUE`, `terminal_id` is its `id`.
- No connections (every row an outlet): the rows in ascending identifier byte order (S-6: `all_terminals` gives 100, 2, 30, 4, 7).
- A flowline with no connections inside a larger network: it is its own outlet block, placed by its identifier among the outlets.
- Duplicated row (repeated `id` and `toid`, a flownetwork): returned as given, both rows contiguous, no warning. S-6 lists "duplicated edge in a flownetwork: raises"; the ROADMAP §4 row that makes it raise at ingest lands in M6 (D3-57), so in M3 this is captured (D3-46) and the rows are returned as given. `sort_network_dups` carries one NextGen flowpath on several rows with the same `id` and `toid` and different `divide_id` or `poi_id`; a flowpath has one catchment, so D3-57 treats those rows as a defect in the fixture once the ingest raise lands in M6.
- Zero rows: zero rows with every column typed, in both languages (I-11, D3-46).
- No outlet (every flowline in a cycle): stops with `network_cycle` on both patterns (D-8, D3-41).
- `outlets` naming a value that is not an `id`: stops with `outlet_not_in_network`, also when other values are `id`s (D3-154).
- A divergence with one branch leaving the table and one inside it: a distributary that reaches the coast or crosses a processing-unit boundary while its other branch stays in the basin. The diverging flowline is a start (step 4), and the basin outlet's walk never pushes it: the outside connection counts toward its countdown and is never visited, so that walk leaves the countdown at 1 (step 3, D3-155). Two consequences: with `split = TRUE` the diverging flowline and the whole drainage above it get the diverging flowline's `id` as `terminal_id`, not the basin outlet's; and that block can land below its in-network downstream flowline, so a downstream accumulation in output order can reach that flowline before the drainage above is added. That happens where the diverging flowline's `id` sorts after, in identifier byte order, the `id` of the outlet whose walk reaches its in-network downstream flowline: starts are walked in descending byte order (step 6) and the output reverses the walk, so the later-sorting start's block comes last. Fixture `sort_divergence_leaves.gpkg` (2 diverges to 3 inside and to 9 outside) returns 1, 4, 2, 2, 3, with `terminal_id` 3 on id 3 and 2 elsewhere: "2" sorts before "3", so the block of 2 lands above 3 (cases `sort_network_divergence_leaves`, `_split`). Fixture `sort_divergence_leaves_relabelled.gpkg` (the same network with 2 relabelled 5) returns 3, 1, 4, 5, 5, with `terminal_id` 3 on id 3 and 5 elsewhere: "5" sorts after "3", so the block of 5 lands below 3 (cases `sort_network_divergence_leaves_relabelled`, `_relabelled_split`). This is captured behavior (D3-71, D3-155). `navigate_network_dfs` binds the same reading of the outside branch (D3-59).
- A coastal terminal beside an inland one below one divergence: each is an outlet (I-4) and its own `terminal_id` (case `sort_network_coastal_inland_split`: `terminal_id` 5, 6, and 8 for a separate network ending inland). Drainage above the divergence takes the `terminal_id` of the terminal whose `id` sorts first in identifier byte order, whichever branch is the main path: 1 and 2 take 5.
- Tolerated with a warning, on the node route: a clipped network where the main path of a divergence lies outside the table and only the minor path (coded 2) is inside. `add_toids` connects the flowline above to the lone minor path and warns `divergence_minor_only_path`, naming those flowlines by `id` (D3-158; `algorithms/add_toids.md` Edge cases); the flowline above is then not an outlet, and the sort places it above the minor path as for any other flowline. The conformance case `add_toids_divergence_minor_only` asserts the warning only.
- Tolerated with a warning, on the node route: at a split that passes the coding check, a main path coded 0 is read as the main path, and `add_toids` warns `divergence_zero_main_path`, naming it by `id` (D3-174). The conformance case `add_toids_divergence_zero_main` asserts the warning only.
- Missing weight, zero-length line, empty name: not applicable; those columns pass through.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| no `toid` and no node columns | the function needs each flowline's downstream flowline (`toid`), and the network has neither `toid` nor `fromnode`/`tonode`, the identifiers of the points at each end of every flowline that `add_toids()` builds `toid` from | all | Add a toid column giving each flowline's downstream flowline, then call again. | `needs_topo` |
| node table with no `divergence` column | keeping only the main path where a flowline splits into two or more downstream flowlines (a divergence) needs the `divergence` column, which marks the main path 1 and minor paths 2; the network has none | all | add_divergence(), given the coastal and inland outlets, to compute divergence. | `divergence_required` |
| node table with `divergence` missing on some rows (through `add_toids`) | `add_toids()` was asked to keep only the main path at each divergence (`return_dendritic`), and `divergence` is missing on one or more flowlines, so the main path cannot be told from a minor one there | the rows where divergence is missing | add_divergence(), given the coastal and inland outlets, to compute divergence. | `divergence_missing` |
| node table with a divergence coded without one main path (through `add_toids`) | at one or more splits every downstream flowline is coded 2, leaving no main path, or two or more are not coded 2, leaving a flowline with more than one downstream flowline | the rows of the downstream flowlines at each split whose divergence coding is broken | add_divergence(), given the coastal and inland outlets, to recompute divergence. | `divergence_coding_invalid` |
| node table with a split whose main path is coded 0 (warning, through `add_toids`; key drafted, D3-180) | at one or more splits the one downstream flowline not coded 2 is coded 0, the code for a flowline not below a divergence, where NHDPlus codes the main path 1; `add_toids()` reads it as the main path and warns | the downstream flowlines coded 0 at a split, by id | add_divergence(), given the coastal and inland outlets, to recompute divergence. | `divergence_zero_main_path` |
| node table with a lone downstream flowline coded 2 (warning, through `add_toids`; key drafted, D3-180) | one or more flowlines coded 2 (a minor path) is the only flowline leaving the point where the flowline above it ends, as a clipped network leaves where the main path lies outside it; `add_toids()` connects to the lone flowline and warns | the lone downstream flowlines coded 2, by id | add_divergence(), given the coastal and inland outlets, to recompute divergence on the network as clipped. | `divergence_minor_only_path` |
| cycle in the network | some flowlines drain back into themselves: following `toid` downstream from one of them returns to it, so the network has no upstream-to-downstream order | the flowlines in each cycle, as check_hy_graph() returns them | check_hy_graph() to list them, then correct their toid. | `network_cycle` |
| `id` and `toid` of different types (warning) | one of `id` and `toid` is text and the other is not, so a `toid` can fail to match the `id` it points to and a flowline can be taken for an outlet, the flowline where water leaves the network | all | Convert id and toid to one type, for example text on both. | `id_toid_type_mismatch` |
| `split = TRUE` with nested outlets | with `split` true, each of the flowlines given in `outlets`, where each sorted group ends, gets its own group of upstream flowlines; one of the outlets passed is upstream of another, so the flowlines above the upstream one would belong to both groups | the flowlines reached from both outlets | Remove the upstream one of the two from outlets, then call again. | `split_outlets_overlap` |
| an `outlets` value that is not an `id` | an outlet passed to the sort is not the `id` of any flowline in the network, so the sort has no flowline to start from for it | the outlets not found among id | Pass values of id as outlets. | `outlet_not_in_network` |
| walk did not end (guard) | the sort took more than twice as many steps as there are flowlines, which a cycle causes | all | check_hy_graph() to find the cycle. | `sort_runaway` |

`remedies.json` still lists `sort_network` in `raised_by` for `no_outlet` and `sort_missed_features`; the function no longer raises either warning (D3-106, D3-107), and the keys are kept until the M6 `remedies.json` read (D8-3, D3-153).

## References

- R source: `R/sort_network.R` (`sort_network` and methods, `sort_network_impl`, `id_byte_rank`, `sort_froms_by_rank`), `R/check_hy_graph.R` (`check_hy_graph`, `check_hy_outlets`), `R/make_index_ids.R` (`make_index_ids_impl`, `make_to_dt`, `make_from_dt`), `R/hy_classes.R` (`hy_as_dataframe`, `hy_classify_and_redispatch`, `hy_node_to_topo`, `hy_dispatch_error`, `hy_guidance_topo`), `R/utils.R` (`is_outlet`, `get_hyg`, `put_hyg`).
- Tests: `tests/testthat/test_sort_network.R`, `tests/testthat/test_outlet_conventions.R`, `tests/testthat/test_hy_classes.R`.
- Descriptions: `algorithms/add_toids.md` (node route), `algorithms/hy_capabilities.md` (code table row).
- NHDPlus hydrologic sequence (`Hydroseq`): McKay, L., and others, 2012, NHDPlus Version 2: User Guide, U.S. Environmental Protection Agency (Value Added Attributes).
- DESIGN "Sibling order in `sort_network` by identifier alone"; SPEC I-1, I-2, I-4, I-6, I-11, D-1, D-3, D-4, D-5, D-7, D-8, D-14, D-15, K-1 to K-4, S-6, A-3, A-9, A-10, A-12, A-16.

## Worked example

Fixture `fixtures/sort_siblings.gpkg`, aspatial, INTEGER columns:

| id | toid |
| --- | --- |
| 30 | 4 |
| 2 | 4 |
| 10 | 4 |
| 4 | 5 |
| 5 | 0 |

Three siblings (30, 2, 10) flow to 4, which flows to the outlet 5. `sort_network(x)` returns `id` 10, 2, 30, 4, 5: siblings in identifier byte order ("10" < "2" < "30"), then 4, then 5 (D-4). Any row permutation of the input returns the same order (`sort_siblings_permuted.gpkg`). With `split = TRUE` every `terminal_id` is 5.

A second check on `divergence_attribute.gpkg` (node route, outlets 12 and 13): 4, 1, 2, 3, 5, 6, 7, 8, 10, 11, 9, 12, 13 ("11" precedes "9").

## Changes

- 2026-10-25, suite 0.1.0: first description, recording R 1.3.0 behavior, with three intended changes (ROADMAP §4, M3) stated above. D-4: outlets and siblings by identifier byte order, rows sharing an `id` by `toid`; 1.3.0 used DFS-visit order from input row order (outlets reversed, siblings in input order; `sort_siblings` gave 30, 2, 10, 4, 5). D-8: on `hy_topo` the cycle check is strongly connected components, so every cycle stops with `network_cycle`; 1.3.0 stopped only on reciprocal pairs and self-loops, and longer cycles warned and dropped. I-2: `id`, `toid`, `terminal_id` typed per I-2, and the `id`/`toid` type mismatch cannot arise through `hy()`. The A-10 condition-class change applies to the `needs_topo` stop. Captured, not changed (no §4 row): the flownetwork method runs no cycle check; duplicated edges return without raising (S-6 says raises); zero rows stop in R (I-11 says zero rows; Python returns them). `requires.pattern` lists `hy_flownetwork`, which the 1.3.0 `hy_capabilities()` predicate omits.
- 2026-10-26, suite 0.1.0: review round (hydrology reader). Edge case for a divergence with one branch leaving the table, with fixture `sort_divergence_leaves.gpkg`; "upstream" defined as reachable through `toid`; relation to NHDPlus `Hydroseq`; Remedies fix texts for `needs_topo`, `sort_missed_features`, `sort_runaway`, `split_outlets_overlap`, and a `divergence_required` row; decision ids D3-40, D3-41, D3-46, D3-55, D3-57, D3-59 cited in place of proposed rows.
- 2026-10-26, suite 0.1.0: remedies pass. The `network_cycle` Remedies row no longer says only pairs and self-loops stop; since D-8 every cycle does (D3-88). No behavior change.
- 2026-10-27, suite 0.1.0: M3 close, intended changes from the dispositions, each landing in R in this run. D3-41 (ROADMAP §4 D-8 row): the cycle check runs on the flownetwork route too, so a cycle there stops with `network_cycle` where 1.3.0 warns `sort_missed_features` and drops rows; with it the `no_outlet` and `sort_missed_features` warnings become unreachable and go (D3-106, D3-107). D3-51 (§4 D-4 row): an `outlets` value that is not an `id` stops with `outlet_not_in_network`; 1.3.0 ignores it. D3-46 (§4 I-11 row): zero-row input returns typed zero-row output in R as in Python. D3-40 (§4 D-5/D-6 row): D-5 binds the adjacency order the sort walks; D3-76 leaves outside connections out of the `make_index_ids` index, and the sort keeps counting them (step 3). D3-56, D3-72 (§4 `add_toids` row): the node route stops on `divergence_missing` and `divergence_coding_invalid`. D3-71: `TerminalPa` sentence in Definition. D3-146: Remedies texts from `remedies.json`.
- 2026-10-28, suite 0.1.0: M3 close unit 2 (hydrology reader H-01 to H-04). H-01: `TerminalPa` sentence bound to `outlets = NULL`. H-02: duplicated-row Edge case reads `sort_network_dups` as the fixture defect D3-57 describes. H-03: the condition under which the outside-branch block lands below its downstream flowline. H-04: Edge case for the node-route form (D3-158). No behavior change.
- 2026-10-28, suite 0.1.0: M3 close run 2, description sweep (rule 2). The changes of the 2026-10-27 entry have landed in R (D-4 in `sort_network_impl` through `id_byte_rank` and `sort_froms_by_rank`; D-8 on both routes, D3-41; the `no_outlet` and `sort_missed_features` warnings removed, D3-106, D3-107, D8-3; typed zero rows, D3-46; `outlet_not_in_network` for any absent value, D3-154; I-2 ingest), so the text states them in present tense (D3-159, D3-172). Node route: `add_toids` now connects a lone downstream flowline coded 2 with warning `divergence_minor_only_path` (D3-158) and warns `divergence_zero_main_path` on a main path coded 0 (D3-174); the H-04 Edge case is restated as tolerated with a warning (standing rule 5), and both drafted keys (D3-180) get Remedies rows. Remedies rows for `no_outlet` and `sort_missed_features` dropped; `split_outlets_overlap` and `divergence_missing` texts aligned to `remedies.json` (D3-146). Edge cases name `sort_divergence_leaves_relabelled.gpkg` and its cases (sort_network H-03) and `sort_network_coastal_inland_split` (H-06). Step 6 cites D-5 (now tagged on `sort_siblings*`) and drops the D-6 parenthetical (D3-177). Signature states what conformance compares on real-network cases (D3-185, D3-189, standing rule 6). `hydrosequence` named as a `topo_sort` alias (add_topo_sort H-04). No behavior change beyond the `add_toids` warnings.
- 2026-10-29 (M3 close run 3, reader round D3-201): H-01 (flownetwork order exception names the outside-connection block and, with `outlets`, a named flowline whose other downstream connection the walks do not reach), H-02 (`terminal_id` above a divergence by identifier byte order; `TerminalPa` parallel limited to networks without repeated `id`), H-03 (conformance paragraph keyed to `columns:` in the manifest). No behavior change.
