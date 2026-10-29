---
requires:
  pattern: [hy_topo, hy_flownetwork]
  columns: []
  via:
    hy_node: add_toids
---

# add_topo_sort

## Signature

R: `add_topo_sort(x, outlets = NULL)`, with methods for `data.frame`, `hy`, `hy_topo`, `hy_node`, and `hy_flownetwork`.

| argument | type | default |
| --- | --- | --- |
| `x` | table (data.frame, tibble, or sf) | required |
| `outlets` | identifier vector, compared to `id` by value; R `NULL` or a vector, Python `None` or a sequence | `NULL` / `None` |

Python (ROADMAP §2): `add_topo_sort(network, *, outlets=None)`, returning a DataFrame, or a GeoDataFrame when the input is one. `network` is R's `x` (A-9). Neither language has a `split` argument; SPEC A-16 agrees (D3-47).

Return: the rows of the sorted network (`sort_network(x, outlets = outlets)`), in its row order, with one column added.

| column | type | when |
| --- | --- | --- |
| `topo_sort` | count: R `integer`, Python `int32` (I-9, vocabulary type `count`) | always |
| every input column except an input `topo_sort` | its input name and type | always |

When the input already has a `topo_sort` column (NHDPlus `Hydroseq` and 3DHP `hydrosequence` are its aliases, add_topo_sort H-04), both languages replace it with the computed one, under the input's name on the `data.frame` route (D3-51, D3-73). The NHDPlus `DnHydroseq`, `UpHydroseq`, and `DnMinorHyd` columns and the 3DHP `dnhydrosequence` and `uphydrosequence` columns pass through and no longer point at values in the table; `DnHydroseq` and `dnhydrosequence` are `dn_topo_sort` aliases and `UpHydroseq` and `uphydrosequence` are `up_topo_sort` aliases, so on the `hy` route they pass through under those names, and neither is recomputed (D3-184). See Changes.

Column position is not contract; expected outputs compare by name (D-14). R places `id` and `toid` first and `topo_sort` last (after the geometry column on sf input). A `data.frame` input returns with its input column names and no hydroloom class. An `hy` input returns class `hy_topo` from the `hy_topo` route and no hydroloom class from the `hy_flownetwork` route. Python returns no class and no attribute.

Conformance compares every returned column on the synthetic cases. On the real-network cases (`new_hope.gpkg`, `new_hope_3dhp.gpkg`, `nhdplushr.gpkg`) it compares the identifier, `toid`, and the `topo_sort` column under its returned name (`Hydroseq`, `hydrosequence`), the columns each case lists under `columns:` in the manifest (D3-185); row order and row count are compared on every case.

No named kernel (K-6). The sort is a walk below the kernel boundary (K-1, K-5) over a CSR index built in both directions (K-2, K-3; `make_index_ids(mode = "both")`).

## Preconditions

The input, after `hy()` (R) or `as_hy` (Python), must be the `hy_topo` pattern (`id`, `toid`, unique `id`; `hy_leveled` satisfies it, A-3) or the `hy_flownetwork` pattern (`id`, `toid`, repeated `id`). What happens otherwise, in check order:

- the `hy_node` pattern (`fromnode`/`tonode`, no `toid`): converted with `add_toids()` (default `return_dendritic = TRUE`), then sorted as `hy_topo`. R prints a message first (no key; A-12: Python prints nothing). The conversion raises what `add_toids` raises: without a `divergence` column that is `divergence_required`; with `divergence` missing on some rows, `divergence_missing` (D3-56); with a divergence that has no main path or more than one, `divergence_coding_invalid` (D3-72). A flowline with a missing `tonode` becomes a terminal and stays the downstream flowline of those above it (D3-56). The conversion keeps main paths only, so a minor path below a divergence is sorted as a headwater (see Edge cases).
- none of the three patterns (no `toid` and no node columns): stop with the representation guidance (`needs_topo`).
- `id` and `toid` of different types, one `character`: R warns (`id_toid_type_mismatch`) and continues. `hy()` gives `id` and `toid` one type (I-2 with D3-06), so the warning cannot fire on input that came through `hy()`. Python identifiers are always `str`.
- a cycle in the `id`/`toid` graph, on either route (the node route included, after conversion): stop (`network_cycle`). `check_hy_graph` finds every cycle through strongly connected components (D-8), and the `hy_flownetwork` route runs it as the `hy_topo` route does (D3-41). See Algorithm step 2.
- an `outlets` value that is not an `id`: stop (`outlet_not_in_network`, D3-51).

## Definition

Roxygen: calls `sort_network` without support for splitting the network and adds a `nrow:1` `topo_sort` attribute. `outlets`: same as id in x. if specified, only the network emanating from these outlets will be considered and returned.

Vocabulary `topo_sort`: similar to hydrosequence in NHDPlus. Large `topo_sort` values are upstream of small `topo_sort` values. Note that there are many valid topological sort orders of a directed graph.

As a property of the input: the output holds the rows `sort_network` returns, upstream before downstream. Each distinct `id` gets one `topo_sort` value; with k distinct identifiers the values are k down to 1 in output row order, so the first flowline has k and the last outlet has 1. A flowline's value is larger than the value of every flowline in the output its `toid` names. The values number this table only: they are not NHDPlus `Hydroseq` or 3DHP `hydrosequence` values and cannot be compared with them, or with the `DnHydroseq`, `UpHydroseq`, `DnMinorHyd`, `dnhydrosequence`, and `uphydrosequence` columns of the same table. Which of the many valid orders is returned is fixed by the sibling rule (D-4, Algorithm step 5).

## Algorithm

1. `data.frame` method: run `hy()`; dispatch on the class; restore input names on return. Python: `_as_hy` then `_route` (A-3), then `_hy_reverse`.
2. Dispatch by representation (Preconditions). The `hy_node` route applies `add_toids()` and continues as `hy_topo`. Both routes call `check_hy_outlets(fix = FALSE)` (the type warning) and then `check_hy_graph(x)`, whose one check is strongly connected components (D-8); the sort stops with `network_cycle` when any component is larger than one or any row has `toid == id`. The `hy_flownetwork` route runs the check as the `hy_topo` route does (D3-41).
3. Starts (I-4): with `outlets` missing, every distinct `id` whose `toid` is not an `id` in the table (`is_outlet`); the value of `toid` is not consulted. With `outlets` given, every distinct `id` equal to one of `outlets`; repeats and the order of `outlets` do not matter, and a start upstream of another start adds nothing. An `outlets` value that is not an `id` stops the sort with `outlet_not_in_network` before the walk (D3-51).
4. Build the CSR index in both directions (`make_index_ids_impl(mode = "both")`): each distinct `id` once; upstream neighbors of a flowline are the rows whose `toid` is that `id`.
5. Walk, the tie rule that binds. Set `ready` for each flowline to its number of distinct downstream flowlines. Take the starts one at a time; from each, depth-first with a stack: record the flowline as visited, then for each upstream neighbor decrement its `ready`, pushing it onto the stack when the decrement leaves it at zero (all its downstream flowlines are visited), and continue from the last pushed flowline. A flowline with two downstream flowlines (a divergence in the flownetwork pattern) is visited only after both. The output order is the reverse of the visit order.
   - Siblings by identifier byte order and nothing else (D-4, D3-50). Upstream neighbors are pushed in ascending byte order and starts are taken in descending byte order, so in the output sibling subtrees appear in ascending byte order of the sibling's `id` and outlet networks in ascending byte order of the outlet's `id`. Byte order compares the identifier as its decimal string (I-1, D-1): `"10"` precedes `"9"`. Rows sharing an `id` (flownetwork pattern) are contiguous and ordered by `toid` byte order; rows equal in `id` and `toid` keep input order (D-3).
   - On a dendritic network the order is a post-order walk from each outlet, children in ascending byte order.
6. Rewrite the table in output order: keep rows whose `id` was visited; repeated-`id` rows travel together.
7. `topo_sort`: number the distinct `id` values of the output k down to 1 in output order, as integers; join back by `id`, so repeated-`id` rows share a value. An input `topo_sort` column is dropped before the join, so the computed column replaces it (D3-51).
8. R: `sort_network` drops geometry for the walk and joins it back by `id` (`get_hyg`, `put_hyg`); `classify_hy` re-stamps the `hy_topo` route.
9. Zero rows: with `outlets` missing, the result is zero rows with every input column and `topo_sort` typed (I-11, D3-46); no walk is run.

Sentinel (K-4): the dense per-node arrays (stack, visit order) use 0 for "none" in R's 1-based code and −1 in a Python kernel; CSR has none. Nothing depends on randomness or scheduling (D-15). D-7 holds in its un-sorted form: output order is contract.

## Postconditions

- Every output row is an input row with its input values; `topo_sort` is the only added column, and it replaces an input `topo_sort` (D3-51).
- `topo_sort` has no missing value and the vocabulary type `count`.
- With k distinct `id` values in the output, the `topo_sort` values are exactly the integers 1 to k, non-increasing in output row order, one value per `id`.
- For every output row whose `toid` is an `id` in the output, the row's `topo_sort` is greater than that flowline's.
- Rows sharing an `id` are contiguous.
- With `outlets` missing, the output holds every input row; a network with a cycle raises `network_cycle` on either route instead of returning part of the rows (D-8, D3-41).
- With `outlets` given, every value of `outlets` is an `id` (else `outlet_not_in_network`), every output `id` drains to one of `outlets` along `toid`, and a flowline is present only when every one of its downstream flowlines is present (the partial network the roxygen note describes).
- The last output row of a non-empty result is an outlet or one of `outlets` and has `topo_sort` 1.
- Zero-row input with `outlets` missing returns zero rows with `topo_sort` typed (I-11).
- D-4, D-7 un-sorted variant: for a row-permuted input the output is identical, row order included.
- D-4: sorting the output again returns the same row order and `topo_sort` values (idempotence).

## Complexity

The walk is O(n + m) time and space for n rows and m connections; building the index and rewriting the table in output order are O(n log n) with sorting (O(n + m) with hashing).

## Edge cases

- One flowline: returns the row with `topo_sort` 1.
- No connections (every row an outlet): returns every row with `topo_sort` k to 1, in ascending identifier byte order (S-6 all terminals).
- Zero rows: returns zero rows with every input column and `topo_sort` typed, R `integer` and Python `int32` (I-11, D3-46). R 1.3.0 stopped inside `sort_network` with an internal error. See Changes.
- `outlets` with a value that is no `id`: stops with `outlet_not_in_network`, in both languages and on zero-row input too (D3-51). R 1.3.0 stopped with an internal error when no value was an `id` and ignored the absent values otherwise.
- No outlet at all (every `toid` names an `id`): such a network has a cycle, so the cycle check stops first (`network_cycle`) on either route (D3-41). R 1.3.0 warned `no_outlet`; that warning is removed (D3-106).
- A cycle with an outlet beside it: stops (`network_cycle`) on either route (D-8, D3-41); case `add_topo_sort_loop2`. R 1.3.0 warned (`sort_missed_features`) and returned only the rows that drain to an outlet; on `loop2` as a flownetwork that was 50 of 839 rows. That warning is removed (D3-107).
- A divergence whose branches rejoin downstream (a braided reach, a loop in the channel pattern) is not a cycle: following `toid` downstream from any flowline never returns to it, so the check passes and the sort returns every row (D3-150). The diverging flowline sorts above both branches and the rejoining flowline below both. Case `add_topo_sort_loop` (`loop.gpkg` as a flownetwork, 97 of 98 divergences rejoining) returns 1425 rows.
- Divergence on node input: the conversion keeps main paths only, so a minor path below a divergence starts as a headwater and its `topo_sort` is not bound to be smaller than the value of the flowline above the divergence. Converting with `to_flownetwork()` first gives an order that holds across both branches of every divergence, as NHDPlus `Hydroseq` does.
- Tolerated with a warning, on the node route: a clipped network where the main path of a divergence lies outside the table and only the minor path (coded 2) is inside. `add_toids` connects the flowline above to the lone minor path and warns `divergence_minor_only_path`, naming those flowlines by `id` (D3-158; `algorithms/add_toids.md` Edge cases); the flowline above is then not an outlet, and the sort places it above the minor path. The conformance case `add_toids_divergence_minor_only` asserts the warning only.
- Tolerated with a warning, on the node route: at a split that passes the coding check, a main path coded 0 is read as the main path, and `add_toids` warns `divergence_zero_main_path`, naming it by `id` (D3-174). The conformance case `add_toids_divergence_zero_main` asserts the warning only.
- `outlets` with a divergence in a flownetwork: a flowline above a divergence is left out when one of its branches leaves the requested basin, even when its main path drains to the outlet (the roxygen note). In `return_divergence` as a flownetwork with `outlets = 3`, flowline 1's main path 1 → 2 → 3 reaches outlet 3, but its minor path 6 does not; ids 2 and 3 return and 1 is left out.
- Duplicated row (one `id` with the same `toid` twice, flownetwork pattern): both rows return, contiguous, sharing one `topo_sort` (case `add_topo_sort_duplicated_edge`). S-6 lists this fixture as raising; R returns it. Captured as R behavior, not Python contract; the raise at ingest is planned for M6 (D3-46, D3-57). See Changes.
- Input already carrying `topo_sort` (NHDPlus `Hydroseq`, 3DHP `hydrosequence`): both languages replace the column with the computed one under the input's name (D3-51, D3-73, add_topo_sort H-04); cases `add_topo_sort_new_hope`, `add_topo_sort_nhdplushr`, `add_topo_sort_new_hope_3dhp`, `add_topo_sort_coastal`. The 3DHP `dnhydrosequence` column, a `dn_topo_sort` alias, passes through unchanged and is not recomputed (D3-184).
- Missing weight, zero-length line, empty name: not applicable; those columns pass through.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| no `toid` and no `fromnode`/`tonode` | The function needs each flowline's downstream flowline (toid), and the network has neither toid nor fromnode/tonode, the identifiers of the points at each end of every flowline that add_toids() builds toid from; the representation is then hy. | all | Add a toid column giving each flowline's downstream flowline, then call again. | `needs_topo` |
| node conversion without `divergence` | Keeping only the main path where a flowline splits into two or more downstream flowlines (a divergence) needs the divergence column, which marks the main path 1 and minor paths 2; the network has none. | all | add_divergence(), given the coastal and inland outlets, to compute divergence. | `divergence_required` |
| node conversion with `divergence` missing on some rows (through `add_toids`) | add_toids() was asked to keep only the main path at each divergence (return_dendritic), and divergence is missing on one or more flowlines, so the main path cannot be told from a minor one there. | the rows where divergence is missing | add_divergence(), given the coastal and inland outlets, to compute divergence. | `divergence_missing` |
| node conversion with a divergence coded without one main path (through `add_toids`) | At one or more splits every downstream flowline is coded 2, leaving no main path, or two or more are not coded 2, leaving a flowline with more than one downstream flowline. | the rows of the downstream flowlines at each split whose divergence coding is broken | add_divergence(), given the coastal and inland outlets, to recompute divergence. | `divergence_coding_invalid` |
| node conversion with a split whose main path is coded 0 (warning, through `add_toids`; key drafted, D3-180) | At one or more splits the one downstream flowline not coded 2 is coded 0, the code for a flowline not below a divergence, where NHDPlus codes the main path 1; add_toids() reads it as the main path and warns. | the downstream flowlines coded 0 at a split, by id | add_divergence(), given the coastal and inland outlets, to recompute divergence. | `divergence_zero_main_path` |
| node conversion with a lone downstream flowline coded 2 (warning, through `add_toids`; key drafted, D3-180) | One or more flowlines coded 2 (a minor path) is the only flowline leaving the point where the flowline above it ends, as a clipped network leaves where the main path lies outside it; add_toids() connects to the lone flowline and warns. | the lone downstream flowlines coded 2, by id | add_divergence(), given the coastal and inland outlets, to recompute divergence on the network as clipped. | `divergence_minor_only_path` |
| cycle, either route | Some flowlines drain back into themselves: following toid downstream from one of them returns to it, so the network has no upstream-to-downstream order. | the flowlines in each cycle, as check_hy_graph() returns them | check_hy_graph() to list them, then correct their toid. | `network_cycle` |
| outlet not an `id` | An outlet passed to the sort is not the id of any flowline in the network, so the sort has no flowline to start from for it. | the outlets not found among id | Pass values of id as outlets. | `outlet_not_in_network` |
| `id` and `toid` types differ (warning, R only) | One of id and toid is text and the other is not, so a toid can fail to match the id it points to and a flowline can be taken for an outlet, the flowline where water leaves the network. | all | Convert id and toid to one type, for example text on both. | `id_toid_type_mismatch` |
| sort runaway (guard; no probe reaches it) | The sort took more than twice as many steps as there are flowlines, which a cycle causes. | all | check_hy_graph() to find the cycle. | `sort_runaway` |

`remedies.json` lists `divergence_missing`, `divergence_coding_invalid`, `divergence_zero_main_path`, and `divergence_minor_only_path` for `add_toids` only, though the node conversion reaches them here. It still lists `add_topo_sort` in `raised_by` for `no_outlet` and `sort_missed_features`; the sort no longer raises either warning (D3-106, D3-107), and the keys are kept until the M6 `remedies.json` read (D8-3, D3-153).

## References

- R source: `R/sort_network.R` (`add_topo_sort` and methods, `sort_network` methods, `sort_network_impl`), `R/make_index_ids.R` (`make_index_ids_impl`), `R/check_hy_graph.R` (`check_hy_graph`, `check_hy_outlets`), `R/hy_classes.R` (`hy_as_dataframe`, `hy_classify_and_redispatch`, `hy_node_to_topo`, `hy_dispatch_error`, `hy_guidance_topo`), `R/utils.R` (`is_outlet`, `get_hyg`, `put_hyg`).
- Tests: `tests/testthat/test_sort_network.R` ("get_sorted", "add_topo_sort deals with diversions"), `tests/testthat/test_hy_classes.R`.
- Descriptions: `algorithms/add_toids.md` (the node conversion), `algorithms/hy_capabilities.md` (the code-table row).
- Vocabulary: `inst/extdata/hy_vocab.json`, `topo_sort` aliases `hydroseq` (NHDPlus) and `hydrosequence` (3DHP); `dn_topo_sort` aliases `dnhydroseq` and `dnhydrosequence`; `up_topo_sort` aliases `uphydroseq` and `uphydrosequence`.
- NHDPlus Version 2 User Guide (McKay and others, 2012): `Hydroseq`, the hydrologic sequence number, smaller downstream.
- Kahn, A.B., 1962, Topological sorting of large networks: Communications of the ACM, v. 5, no. 11, p. 558–562 (the `ready` count is the in-degree rule).
- SPEC I-1, I-2, I-4, I-5, I-9, I-11, D-1, D-3, D-4, D-7, D-8, D-14, D-15, K-1 to K-5, A-3, A-9, A-10, A-12, A-16, S-6, S-7.

## Worked example

Fixture `fixtures/add_topo_sort_worked_example.gpkg`, aspatial, six rows, `id` and `toid` INTEGER:

| id | toid |
| --- | --- |
| 10 | 2 |
| 9 | 2 |
| 2 | 3 |
| 3 | 0 |
| 5 | 3 |
| 7 | 0 |

Outlets 3 and 7; 10 and 9 flow to 2; 2 and 5 flow to 3. `add_topo_sort(x)` returns (`id`, `topo_sort`) = (10, 6), (9, 5), (2, 4), (5, 3), (3, 2), (7, 1) for this input and for any permutation of its rows (D-4): `"10"` precedes `"9"`, `"2"` precedes `"5"`, and outlet `"3"` precedes `"7"`. R 1.3.0 returned (7, 6), (10, 5), (9, 4), (2, 3), (5, 2), (3, 1): outlet 7, last in input, came first, and siblings kept input order. `add_topo_sort(x, outlets = 3)` returns (10, 5), (9, 4), (2, 3), (5, 2), (3, 1), as R 1.3.0 did; `add_topo_sort(x, outlets = 99)` stops with `outlet_not_in_network`.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording R 1.3.0 behavior. Intended changes (ROADMAP §4, M3): D-4, sibling and outlet order by identifier byte order, which changes row order and `topo_sort` values on any network with a confluence or more than one outlet; D-8, every cycle stops through strongly connected components on the `hy_topo` route (today only reciprocal pairs and self-loops stop there); the `hy_flownetwork` route keeps no cycle check (D3-41, D3-55); I-2, identifier types on ingest, which reaches `toid` on the node route; A-10, the `needs_topo` stop takes the condition class. A-9 gap: A-16 lists `split=False`, which R does not have; Python follows R. Captured, not changed (no §4 row): `topo_sort` is `double`; an input `topo_sort` column yields `topo_sort.x`/`topo_sort.y`; zero rows and `outlets` naming no `id` stop with an internal error where Python returns zero rows (I-11); the S-6 duplicated edge returns rather than raises; `hy_capabilities()` reports the function unavailable on `hy_flownetwork` although it runs there.
- 2026-10-26, suite 0.1.0: remedies pass. The `network_cycle` Remedies row no longer says only a pair or a self-loop stops; since D-8 every cycle does (D3-88). No behavior change.
- 2026-10-27, suite 0.1.0: dispositions from the M3 review. D-4 (D3-50) and D-8 landed in R on 2026-10-26 and are stated as behavior, not as pending. D3-41 (ROADMAP §4 D-8 row): the cycle check runs on the `hy_flownetwork` route too, so a cycle stops there with `network_cycle` and `no_outlet` and `sort_missed_features` are no longer reached (D3-106, D3-107); R change in unit 3. D3-51 (ROADMAP §4 D-4 row): `topo_sort` is written as `integer` and replaces an input `topo_sort` column in both languages (D3-73), and an `outlets` value that is not an `id` raises `outlet_not_in_network`; R change in unit 3. D3-46 (ROADMAP §4 I-11 row): zero-row input returns zero rows with `topo_sort` typed; R change in unit 3. D3-56, D3-72: the node conversion raises `divergence_missing` and `divergence_coding_invalid` and treats a missing `tonode` as a terminal. D3-47: SPEC A-16 now lists no `split`. D3-146: Remedies aligned with `remedies.json`. D3-42: `hy_capabilities()` reports the function available on `hy_flownetwork`.
- 2026-10-28, suite 0.1.0: M3 close, unit 2: H-01, an Edge cases entry for the braided reach, which is not a cycle (D3-150). No behavior change.
- 2026-10-28, suite 0.1.0: M3 close run 2, description sweep (rule 2). The changes of the 2026-10-27 entry have landed in R (D-4; D-8 on both routes, D3-41; the `no_outlet` and `sort_missed_features` warnings removed, D3-106, D3-107, D8-3; integer `topo_sort` replacing an input one and `outlet_not_in_network`, D3-51, D3-154; typed zero rows, D3-46; I-2 ingest), so the text states them in present tense (D3-159, D3-172). H-04: 3DHP `hydrosequence` is a `topo_sort` alias in the vocabulary, so on `new_hope_3dhp.gpkg` the computed values replace `hydrosequence` under that name and no separate `topo_sort` column is returned; `dnhydrosequence` is not aliased and passes through (D3-184). Node route: `add_toids` connects a lone downstream flowline coded 2 with warning `divergence_minor_only_path` (D3-158) and warns `divergence_zero_main_path` on a main path coded 0 (D3-174); both are Edge cases tolerated with a warning (standing rule 5) with Remedies rows for the drafted keys (D3-180). Remedies rows for `no_outlet` and `sort_missed_features` dropped, and rows for `divergence_missing` and `divergence_coding_invalid` added, aligned to `remedies.json` (D3-146). Signature states what conformance compares on real-network cases (D3-185, standing rule 6). No behavior change in `add_topo_sort` beyond the vocabulary alias and the `add_toids` warnings.
- 2026-10-29, suite 0.1.0: M3 close run 3 (D3-184). 3DHP `dnhydrosequence` is a `dn_topo_sort` alias and `uphydrosequence` an `up_topo_sort` alias in the vocabulary, after the NHDPlus aliases. `add_topo_sort` does not recompute either column; they pass through, under the hydroloom names on the `hy` route and the input names on the `data.frame` route. No expected layer of this function moves.
