---
requires:
  pattern: [hy_topo, hy_flownetwork]
  columns: []
  via:
    hy_node: add_toids
---

# make_index_ids

## Signature

R: `make_index_ids(x, mode = "to", long_form = NULL)`, with methods for `data.frame`, `hy`, `hy_topo`, and `hy_flownetwork`. Python (ROADMAP §2, SPEC A-6, A-16): `make_index_ids(network, *, mode="to")`. `network` is R's `x` (A-9). Python has no `long_form`; see Changes.

| argument | type | default |
| --- | --- | --- |
| `x` / `network` | table (data.frame, tibble, or sf; Python DataFrame or GeoDataFrame) | required |
| `mode` | one of `"to"`, `"from"`, `"both"`: R `character` length one, Python `str` | `"to"` |
| `long_form` | R only, deprecated: `NULL` or flag | `NULL` |

Roxygen gives the argument and return prose: "makes index ids for the provided hy object. These can be used for graph traversal algorithms such that the row number and id are equal"; `mode` "determines the direction of the graph. If "to", the graph will be directed from the `id` to the `toid`. If "from", the graph will be directed from the `toid` to the `id`. If "both", the list will contain both a "from" and a "to" element containing each version of the graph."

Return, R: a list. Mode `"to"`: `to` (integer matrix, one column per unique `id`, one row per neighbor slot, padded with `NA`; an outlet's column holds the R sentinel `0`, K-4), `lengths` (double, neighbors per node; see Edge cases for the outlet value), `main` (logical matrix aligned with `to`, only when the input has `downmain`), and `to_list` (data.frame: `id` in the identifier type, `indid` integer 1..n, `toindid` list column, `main` list column when present). Mode `"from"`: the same with names `froms`, `lengths`, `main` (only when the input has `upmain`), and `froms_list` (`id`, `indid`, `fromindid`, `main`); a headwater's column holds `NA` and its length is 0. Mode `"both"`: `list(to = <to list>, from = <from list>)`. With `long_form = TRUE`, the deprecated long table `id`, `indid`, `toindid`.

Return, Python: an `IndexIds` (K-3): `indptr` int64 length n + 1, `indices` int64 length m (dense positions), `uniques` (pandas Index of `str` identifiers mapping dense position to `id`), `mode` (`"to"` or `"from"`), `main` (bool array length m aligned with `indices`, or `None`), `edge_id` (`None`; see Changes). Mode `"both"` returns the pair `(to, from)`.

Expectation (S-7 graph layer): one row per edge, columns `id` and `neighbor` (identifier, stored as text), `position` (0-based rank within the node's neighbor list, in identifier byte order as Algorithm step 7 states, D-5), and `main` (flag) when the input carries `downmain` (`"to"`) or `upmain` (`"from"`). No `edge_id` column: R never returns one from `make_index_ids`. Terminals have no rows; the dense position (`indid`, `uniques` order) is not contract. Mode `"both"` is one layer with a `mode` column (`"to"`, `"from"`).

Kernel. None is named (D3-44). The CSR build is the K-2 construction, which DESIGN §4 does not list as a kernel candidate; it lies below the boundary in `hydroloom/_kernels/csr.py` (K-1, K-9) as plain numpy shaped by the author, and R keeps its list form (K-3). `IndexIds.transpose()` builds the other direction the same way. No sentinel appears in CSR (K-4); `ell()` pads with −1.

## Preconditions

The input, after `hy()` (R) or `as_hy` (Python), must be the `hy_topo` pattern (unique `id` with `toid`; `hy_leveled` satisfies it, A-3) or the `hy_flownetwork` pattern (repeated `id` with `toid`, I-6). `downmain` and `upmain` are read when present and are not required. What is raised, in check order:

- not `hy_topo`, `hy_leveled`, or `hy_flownetwork` (no `toid`; the `hy_node` pattern included): stop with the representation guidance (`needs_topo`). R does not convert a node table; Python registers no `convert` for `make_index_ids` and raises `RepresentationError` the same way. `requires.via` gives `add_toids` as the call to make first.
- `mode` not one of `"to"`, `"from"`, `"both"`: stop (`index_mode_invalid`). Python raises `ValueError`.
- `hy_topo` only: `id` and `toid` of different types, one `character`, warns (`id_toid_type_mismatch`) and the join then fails inside data.table. After the I-2 change, with `id` and `toid` typed as a pair (D3-06), `hy()` cannot produce this input and the warning is unreachable through the public route.
- the network contains a cycle (`network_cycle`), on both routes (D-8, D3-41). `check_hy_graph(x)` computes strongly connected components, and any component larger than one, or any self-loop (`toid == id`), stops `make_index_ids`; a three-flowline cycle stops as a reciprocal pair does. On a flownetwork the check reads every row, so a cycle through a minor path at a divergence stops too. Python raises `HydroloomError` (`network_cycle`).
- R only, `long_form` not `NULL`: a deprecation warning (`long_form_deprecated`), after the checks above.

The `hy_flownetwork` route runs the cycle check but not the outlet check (D3-41).

## Definition

A node is one flowline (one unique `id`); in a catchment network, one catchment. In mode `"to"`, the neighbors of a flowline are the distinct `toid` values on its rows that are also an `id` in the table (I-4); a `toid` that is no `id` is an outlet and contributes no neighbor. In mode `"from"`, the neighbors of a flowline are the flowlines whose `toid` it is, so the `"from"` graph is the `"to"` graph with every edge reversed. Each neighbor list is in byte order of the neighbors' identifiers (D-5, D-1); two edges to one neighbor, which only rows differing in `downmain` or `upmain` give, keep input row order between them. When the input carries `downmain`, each `"to"` edge carries that row's `downmain` as `main`; when it carries `upmain`, each `"from"` edge carries the `upmain` of the row that defines the edge.

## Algorithm

1. Route (A-3). `data.frame`: `hy()`, then dispatch; the `hy` method reclassifies and re-dispatches or raises `needs_topo`. Python: `_as_hy`, then `_route` with native bodies for `hy_topo` and `hy_flownetwork`. Nothing is restored on return; the output is not a table.
2. Check `mode` (`index_mode_invalid`).
3. Checks before the body. `hy_topo` only: run the outlet check (`check_hy_outlets(x, fix = FALSE)`, the type warning in Preconditions). Both routes: run `check_hy_graph(x)` and stop with `network_cycle` unless it returns `TRUE` (D-8, D3-41). Drop geometry. `check_hy_graph` builds its graph through its own index, never through the exported `make_index_ids()`, which would call `check_hy_graph()` again.
4. Shared body (`make_index_ids_impl()` in R, used by both methods; the `hy_flownetwork` method skips the outlet check in step 3). Keep the columns `id`, `toid`, and `downmain`/`upmain` when present, and drop repeated rows over those columns, keeping the first. A duplicated edge therefore collapses to one; two rows for one edge that differ in `downmain` or `upmain` both stay.
5. Dense positions: unique `id` in first-appearance order, 1..n in R (`indid`), 0..n−1 in Python (`factorize`). Map each `toid` to a position; a `toid` with no `id` is an outlet: R records `toindid = 0` (the K-4 sentinel in 1-based code), Python drops the edge.
6. `"to"`: edges are (position of `id`, position of `toid`), `main` from `downmain`. `"from"`: edges reversed, `main` from the `upmain` of the row that defines the edge, joined per edge and not by `id` (D3-43). `"both"`: build `"to"` and transpose it.
7. Order each neighbor list by the byte order of the neighbors' identifiers (D-5, D-1), R integer identifiers compared as their decimal strings, so COMID 100 precedes 30 precedes 4. Two edges to the same neighbor keep the order of their rows after step 4. Python gets this order from one stable argsort on (owning position, neighbor byte rank) after the bincount (K-2, D-3); the rank is computed above the boundary, so nothing below it sees an identifier (K-5). R orders each list by the same rank.
8. Python builds the CSR index below the boundary (K-1, K-2); the identifier-to-position map, the byte rank, and `main` alignment stay above it. R formats the per-node lists into the padded matrix (ELLPACK view) with `NA` padding.

Tie rule, binding: neighbor identifier byte order (D-5), then input row order between two edges to one neighbor; dense position order is free. Sentinel: none in CSR; −1 in the Python ELL view, `0` (outlet, `"to"`) and `NA` (headwater, `"from"`) in R's matrices. Nothing depends on randomness or scheduling (D-15).

## Postconditions

- Every `id` and every `neighbor` in the graph layer is an `id` of the input.
- The `"to"` edge set equals the set of distinct (`id`, `toid`) pairs whose `toid` is an `id` of the input (I-4), each pair once unless its rows differ in `downmain` or `upmain` (Edge cases).
- The `"from"` layer is the `"to"` layer with `id` and `neighbor` exchanged: the same edge multiset and the same row count (D3-43).
- For each (`mode`, `id`), `position` takes the values 0..k−1 once each, and the neighbors in position order are in identifier byte order (D-5), with input row order only between two edges to one neighbor (Algorithm step 7).
- A node with no neighbor in a mode has no rows in that mode; for `hy_topo` input every node has at most one `"to"` neighbor.
- `main` is present in `"to"` exactly when `downmain` is an input column and in `"from"` exactly when `upmain` is, and equals that column on the row defining the edge; it is never missing.
- Input that returns holds no cycle, on either route: no strongly connected component larger than one and no self-loop (D-8, D3-41).
- `IndexIds`: `len(indptr) == len(uniques) + 1`, `indptr[0] == 0`, `indptr` non-decreasing, `indptr[-1] == len(indices)`, every index in `[0, n)`, `uniques` unique and equal as a set to the input `id`; `main` is `None` or has length m; `edge_id` is `None`.
- `make_index_ids(network, mode="both")` equals `(make_index_ids(network, mode="to"), make_index_ids(network, mode="from"))`, and the second equals the first's `transpose()` (K-3).
- A permutation of the input rows leaves the graph layer, `position` included, unchanged once sorted by `mode`, `id`, `position` (D-7); the one exception is two edges to one neighbor, whose relative position follows row order.
- Zero-row input returns, in both languages, an index with no nodes and no edges and a graph layer with no rows (I-11).

## Complexity

O(n + m log m) time for the factorize, bincount, and the one argsort, O(n + m) space, for n unique `id` and m edges. R's padded matrix takes O(n · k) space for the largest neighbor count k.

## Edge cases

- One flowline: no edges. R returns a 1 × 1 matrix (`0` in `"to"`, `NA` in `"from"`); Python `indptr = [0, 0]`, empty `indices`; the layer has no rows.
- No connections (all terminals): no edges, no layer rows; every `toid` is an outlet whatever its value (I-4).
- A flowline with no edges inside a larger network: it is a node with an empty neighbor list in both modes.
- A minor path that rejoins the main path (`return_divergence.gpkg` after `add_toids(return_dendritic = FALSE)`): flowline 6 leaves flowline 1 at a divergence and splits again into 7 and 8; 7 rejoins the main path at 4 and 8, through 9, at 5. Flowline 1 lists 2 and 6 downstream, 6 lists 7 and 8; flowline 4 lists 3 and 7 upstream, 5 lists 4 and 9. Nothing is special-cased.
- Duplicated row (the same `id`, `toid` twice in a flownetwork): collapsed to one edge, no warning. S-6 lists a duplicated edge as raising; the raise lands in ingest with D3-57 in M6, so the case stays R-only here (D3-46, D3-57).
- One connection on two rows that differ in `downmain` (rows (1, 2, TRUE), (1, 2, FALSE), (2, 0, TRUE)): both rows survive step 4, so flowline 1 lists flowline 2 twice in `"to"`, at position 0 with `main` TRUE and at position 1 with `main` FALSE, and flowline 2 lists flowline 1 twice in `"from"`, without `main` since the input has no `upmain`. One connection cannot be both the main path and a minor one; this is a coding error in the junction table that the `hy_flownetwork` invariants do not catch (one TRUE row per `id` still holds). R returns it without a warning; captured, R-only. Under D3-57 a repeated pair differing in `downmain` or `upmain` raises, Python first and R with the M6 change (D3-74); that raise is not applied here.
- Zero rows: both languages return typed empty output (I-11, D3-46). R returns the list for the mode with a zero-column matrix, a zero-length `lengths`, and a zero-row `to_list` or `froms_list` whose columns keep their types; Python returns an `IndexIds` with `indptr = [0]` and no edges; the graph layer has no rows. R 1.3.0 stopped inside the matrix formatting ("subscript out of bounds", after a `max()` warning).
- R `lengths`: in `"to"` an outlet counts 1 (its sentinel slot), not 0; in `"from"` a headwater counts 0. The graph layer and Python do not carry this.
- Missing weight, zero-length line, empty name: not applicable; only `id`, `toid`, `downmain`, `upmain` are read.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| not the topo or flownetwork pattern | The function needs each flowline's downstream flowline (toid), and the network has neither toid nor fromnode/tonode, the identifiers of the points at each end of every flowline that add_toids() builds toid from; the representation is then hy. Where the network has fromnode/tonode, the functions convert it themselves, except check_hy_graph() in R, which stops here with representation hy_node; add_toids() fixes that case. Where the downstream column has a source name the vocabulary lacks, registering it as toid (hydroloom_names() in R, register_names() in Python) is the alternative. | all | Add a toid column giving each flowline's downstream flowline, then call again. | `needs_topo` |
| `mode` not `"to"`, `"from"`, `"both"` | mode is not recognized: "to" lists each flowline's downstream flowlines, "from" its upstream flowlines, "both" gives both. | none | Pass one of the three values. | `index_mode_invalid` |
| cycle, either route | Some flowlines drain back into themselves: following toid downstream from one of them returns to it, so the network has no upstream-to-downstream order. | the flowlines in each cycle, as check_hy_graph() returns them | check_hy_graph() to list them, then correct their toid. | `network_cycle` |
| `id` and `toid` of different types (warning, R only) | One of id and toid is text and the other is not, so a toid can fail to match the id it points to and a flowline can be taken for an outlet, the flowline where water leaves the network. | all | Convert id and toid to one type, for example text on both. | `id_toid_type_mismatch` |
| `long_form` given (warning, R only) | The long table output of make_index_ids is deprecated. | all | Drop long_form. The default output's to_list table holds the same id, indid, and toindid, with one row per flowline and toindid a list of the downstream positions in place of one row per connection. | `long_form_deprecated` |

This description owns the `network_cycle` text (D3-60), which `sort_network` also raises from its own stop.

## References

- R source: `R/make_index_ids.R` (`make_index_ids` and methods, `make_index_ids_impl`, `make_to_dt`, `make_from`, `make_from_dt`, `call_format_index_ids`, `format_index_ids_internal`), `R/check_hy_graph.R` (`check_hy_graph`, `check_hy_outlets`), `R/hy_classes.R` (`hy_classify_and_redispatch`, `hy_dispatch_error`, `hy_guidance_topo`).
- Tests: `tests/testthat/test_make_index_ids.R`; `tests/testthat/test_check_hy_graph.R` (callers of `mode = "both"`).
- Descriptions: `algorithms/add_toids.md` (the conversion from `hy_node`), `algorithms/hy.md` (I-2 pairing, D3-06), `algorithms/hy_capabilities.md` (the code-table row).
- USGS, USGS hydrofabric data theme, Hydrofabric components (networks), https://water.usgs.gov/themes/hydrofabric/: the network as upstream and downstream connections between flowlines, which this index stores in another form.
- NHDPlus Version 2 User Guide (McKay and others, 2012), PlusFlow table: one row per connection from an upstream flowline to a downstream one, the edge list this index is built from.
- Saad, Y., 2003, Iterative Methods for Sparse Linear Systems, 2nd ed., SIAM, §3.4 (compressed sparse row and ELLPACK storage).
- SPEC K-1 to K-6, K-9, K-10, S-6, S-7, I-1, I-2, I-4, I-6, I-11, D-1, D-3, D-5, D-7, D-8, D-15, A-3, A-6, A-9, A-10, A-16.

## Worked example

Fixture `fixtures/make_index_ids_worked_example.gpkg`, aspatial, five rows, INTEGER columns: (`id`, `toid`) = (30, 10), (4, 10), (100, 10), (10, 7), (7, 0). Three headwaters join at flowline 10, which drains to the outlet 7.

`make_index_ids(x, mode = "both")`, graph layer (binding, identifier byte order, D-5):

| mode | id | position | neighbor |
| --- | --- | --- | --- |
| to | 30 | 0 | 10 |
| to | 4 | 0 | 10 |
| to | 100 | 0 | 10 |
| to | 10 | 0 | 7 |
| from | 10 | 0 | 100 |
| from | 10 | 1 | 30 |
| from | 10 | 2 | 4 |
| from | 7 | 0 | 10 |

Byte order compares the decimal strings of R integer identifiers as it does Python `str` identifiers, so COMID 100 precedes 30 precedes 4, where numeric order would give 4, 30, 100.

Fixture `make_index_ids_worked_example_permuted.gpkg` holds the same rows as (10, 7), (100, 10), (7, 0), (30, 10), (4, 10) and gives the same layer (D-7). R 1.3.0 listed flowline 10's upstream flowlines in input row order instead: 30, 4, 100 for the first fixture and 100, 30, 4 for the permuted one.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording R 1.3.0 behavior. Intended changes applied in the binding sections: I-2 (ROADMAP §4, M3), whole `double` identifiers become `integer` and out-of-range ones `character` in `to_list$id`, with `id` and `toid` typed as a pair (D3-06), which makes the type-mismatch input work; the graph layer, compared as text, is unchanged otherwise. D-8 (ROADMAP §4, M3), the cycle stop covers every strongly connected component larger than one and self-loops, on the `hy_topo` route only (D3-41). Captured, not changed: duplicated edges collapse (S-6 says raise); zero-row input stops in R (I-11 says return) (D3-46). A-9 gap: Python omits the deprecated `long_form`; `edge_id` is always `None` because R never returns one. The A-10 condition-class change applies to the `needs_topo` stop.
- Pending, not applied, no ROADMAP §4 row (D3-40, D3-43). D-5: neighbor lists in identifier byte order, R integer identifiers ranked by their decimal strings, replacing input row order in Definition, Algorithm step 7, the tie rule, the position postcondition, and the first worked-example table; the D-7 postcondition then holds for `position` too, and the caller of `csr_from_edges` passes the byte-order rank. With it, the R defect in `"from"` mode on a flownetwork with `upmain`: `upmain` is joined by `id`, which duplicates every upstream edge of a flowline whose rows carry different `upmain` values (167 extra edges on `new_hope`) and mislabels `main`; the fix joins `upmain` per edge. Cases that change carry `changes_with: d5` or `from_upmain`.
- 2026-10-26: review fixes. The binding order is today's input row order (D3-40), with D-5 moved here as pending; the conflicting-`downmain` edge case and its case added; remedies keys renamed to `network_cycle` and `id_toid_type_mismatch` (D3-60); remedy and definition wording revised; hydrofabric reference added.
- 2026-10-26, suite 0.1.0: remedies pass. The `network_cycle` Remedies row no longer says only reciprocal pairs stop; since D-8 every cycle does (D3-88). No behavior change.
- 2026-10-27, suite 0.1.0: M3 dispositions. D3-40 (ROADMAP §4 "Neighbor order in `make_index_ids` and sibling order in `navigate_network_dfs` by identifier, not input row order; a `toid` outside the network is a terminal", D-5, M3): D-5 is binding again in Definition, Algorithm step 7, the tie rule, the position and D-7 postconditions, and the worked example, replacing input row order; a `toid` outside the network was already a terminal here. D3-43, inside the same row: `"from"` mode joins `upmain` per edge, not by `id`, so the 167 extra edges on `new_hope` and the mislabelled `main` go. D3-41 (ROADMAP §4 D-8 row, "`sort_network` and `make_index_ids` call it on the flownetwork route too"): the cycle check runs on both routes; the outlet check stays on `hy_topo`. D3-46 (ROADMAP §4 "Zero-row input returns typed zero-row output", I-11): R returns typed empty output in place of its internal stop. D3-44 with D3-74: the `csr_from_edges` block and its `rank` argument are dropped; the build is K-2's. D3-146: Remedies aligned with `remedies.json`. M3 H-08: a `return_divergence` case and edge case; H-09: the PlusFlow table cited.
- 2026-10-28, suite 0.1.0: M3 close unit 2, H-02. The `return_divergence` edge case gives the second divergence on flowline 6 and both rejoins, at 4 and at 5. No behavior change.
