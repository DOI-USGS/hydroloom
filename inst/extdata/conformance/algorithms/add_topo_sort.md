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

Python (ROADMAP §2): `add_topo_sort(network, *, outlets=None)`, returning a DataFrame, or a GeoDataFrame when the input is one. `network` is R's `x` (A-9). SPEC A-16 lists `split=False`; R has no `split` argument, so the R signature wins (A-9) and Python has none. See Changes.

Return: the rows of the sorted network (`sort_network(x, outlets = outlets)`), in its row order, with one column added.

| column | type | when |
| --- | --- | --- |
| `topo_sort` | count: R `integer`, Python `int32` (I-9, vocabulary type `count`) | always |
| every input column | its input name and type | always |

Today R writes `topo_sort` as `double`, except `integer` when one distinct `id` is sorted; the harness compares after coercion to the vocabulary type (S-7), so no case sees the difference. When the input already has a `topo_sort` column (NHDPlus `Hydroseq` is its `nhdplus` alias), R 1.3.0 returns the input column as `topo_sort.x` and the computed one as `topo_sort.y`, and the `data.frame` route does not restore the input's name for either; Python replaces the input column with the computed one. See Changes.

Column position is not contract; expected outputs compare by name (D-14). R places `id` and `toid` first and `topo_sort` last (after the geometry column on sf input). A `data.frame` input returns with its input column names and no hydroloom class. An `hy` input returns class `hy_topo` from the `hy_topo` route and no hydroloom class from the `hy_flownetwork` route. Python returns no class and no attribute.

No named kernel (K-6). The sort is a walk below the kernel boundary (K-1, K-5) over a CSR index built in both directions (K-2, K-3; `make_index_ids(mode = "both")`).

## Preconditions

The input, after `hy()` (R) or `as_hy` (Python), must be the `hy_topo` pattern (`id`, `toid`, unique `id`; `hy_leveled` satisfies it, A-3) or the `hy_flownetwork` pattern (`id`, `toid`, repeated `id`). What happens otherwise, in check order:

- the `hy_node` pattern (`fromnode`/`tonode`, no `toid`): converted with `add_toids()` (default `return_dendritic = TRUE`), then sorted as `hy_topo`. R prints a message first (no key; A-12: Python prints nothing). The conversion raises what `add_toids` raises; without a `divergence` column that is `divergence_required`.
- none of the three patterns (no `toid` and no node columns): stop with the representation guidance (`needs_topo`).
- `id` and `toid` of different types, one `character`: R warns (`id_toid_type_mismatch`) and continues; post-change (I-2 with D3-06 pairing) both columns are `character` and the warning is not reached. Python identifiers are always `str`.
- a cycle in the `id`/`toid` graph: stop (`network_cycle`). Today only the `hy_topo` route checks, and only for a pair of flowlines that point at each other, a self-loop included; post-change (D-8) every cycle, through strongly connected components, on both routes. See Algorithm step 2.

## Definition

Roxygen: calls `sort_network` without support for splitting the network and adds a `nrow:1` `topo_sort` attribute. `outlets`: same as id in x. if specified, only the network emanating from these outlets will be considered and returned.

Vocabulary `topo_sort`: similar to hydrosequence in NHDPlus. Large `topo_sort` values are upstream of small `topo_sort` values. Note that there are many valid topological sort orders of a directed graph.

As a property of the input: the output holds the rows `sort_network` returns, upstream before downstream. Each distinct `id` gets one `topo_sort` value; with k distinct identifiers the values are k down to 1 in output row order, so the first flowline has k and the last outlet has 1. A flowline's value is larger than the value of every flowline in the output its `toid` names. Which of the many valid orders is returned is fixed by the sibling rule (D-4, Algorithm step 5).

## Algorithm

1. `data.frame` method: run `hy()`; dispatch on the class; restore input names on return. Python: `_as_hy` then `_route` (A-3), then `_hy_reverse`.
2. Dispatch by representation (Preconditions). The `hy_node` route applies `add_toids()` and continues as `hy_topo`. Both routes call `check_hy_outlets(fix = FALSE)` (the type warning). Today the `hy_topo` route then calls `check_hy_graph(x)`, which joins `toid` to `id` and fails when a flowline's downstream flowline points back at it; the `hy_flownetwork` route does no graph check. Post-change (D-8): both routes call `check_hy_graph`, whose one check is strongly connected components, and stop with `network_cycle` when any component is larger than one or any row has `toid == id`.
3. Starts (I-4): with `outlets` missing, every distinct `id` whose `toid` is not an `id` in the table (`is_outlet`); the value of `toid` is not consulted. With `outlets` given, every distinct `id` equal to one of `outlets`; repeats and the order of `outlets` do not matter, an `outlets` value that is not an `id` is ignored, and a start upstream of another start adds nothing.
4. Build the CSR index in both directions (`make_index_ids_impl(mode = "both")`): each distinct `id` once; upstream neighbors of a flowline are the rows whose `toid` is that `id`.
5. Walk, the tie rule that binds. Set `ready` for each flowline to its number of distinct downstream flowlines. Take the starts one at a time; from each, depth-first with a stack: record the flowline as visited, then for each upstream neighbor decrement its `ready`, pushing it onto the stack when the decrement leaves it at zero (all its downstream flowlines are visited), and continue from the last pushed flowline. A flowline with two downstream flowlines (a divergence in the flownetwork pattern) is visited only after both. The output order is the reverse of the visit order.
   - Today the upstream neighbors are taken in input row order and the starts in the order the index holds them, which is input row order of first appearance; so sibling subtrees appear in output in input row order and outlet networks in reverse input row order. The order depends on input row order.
   - Post-change (D-4): siblings by identifier byte order and nothing else. Upstream neighbors are pushed in ascending byte order and starts are taken in descending byte order, so in the output sibling subtrees appear in ascending byte order of the sibling's `id` and outlet networks in ascending byte order of the outlet's `id`. Byte order compares the identifier as its decimal string (I-1, D-1): `"10"` precedes `"9"`. Rows sharing an `id` (flownetwork pattern) are contiguous and ordered by `toid` byte order; today they keep input row order (D-3 stable arrange).
   - On a dendritic network the post-change order is a post-order walk from each outlet, children in ascending byte order.
6. Rewrite the table in output order: keep rows whose `id` was visited; repeated-`id` rows travel together.
7. `topo_sort`: number the distinct `id` values of the output k down to 1 in output order; join back by `id`, so repeated-`id` rows share a value.
8. R: `sort_network` drops geometry for the walk and joins it back by `id` (`get_hyg`, `put_hyg`); `classify_hy` re-stamps the `hy_topo` route.

Sentinel (K-4): the dense per-node arrays (stack, visit order) use 0 for "none" in R's 1-based code and −1 in a Python kernel; CSR has none. Nothing depends on randomness or scheduling (D-15). D-7 holds post-change in its un-sorted form: output order is contract.

## Postconditions

- Every output row is an input row with its input values; `topo_sort` is the only added column (Python; R adds `topo_sort.y` and renames an existing `topo_sort` to `topo_sort.x`, see Signature).
- `topo_sort` has no missing value and the vocabulary type `count`.
- With k distinct `id` values in the output, the `topo_sort` values are exactly the integers 1 to k, non-increasing in output row order, one value per `id`.
- For every output row whose `toid` is an `id` in the output, the row's `topo_sort` is greater than that flowline's.
- Rows sharing an `id` are contiguous.
- With `outlets` missing and no cycle, the output holds every input row; post-change (D-8) a network with a cycle raises instead of returning part of the rows.
- With `outlets` given, every output `id` drains to one of `outlets` along `toid`, and a flowline is present only when every one of its downstream flowlines is present (the partial network the roxygen note describes).
- The last output row is an outlet or one of `outlets` and has `topo_sort` 1.
- Post-change (D-4, D-7 un-sorted variant): for a row-permuted input the output is identical, row order included.
- Post-change (D-4): sorting the output again returns the same row order and `topo_sort` values (idempotence; in R the second call also carries the `.x`/`.y` columns).

## Complexity

The walk is O(n + m) time and space for n rows and m connections; building the index and rewriting the table in output order are O(n log n) with sorting (O(n + m) with hashing).

## Edge cases

- One flowline: returns the row with `topo_sort` 1.
- No connections (every row an outlet): returns every row with `topo_sort` k to 1; today in reverse input order, post-change in ascending identifier byte order (S-6 all terminals).
- Zero rows: R stops inside `sort_network` with an internal error (no key); Python returns zero rows with `topo_sort` typed `int32` (I-11). See Changes.
- `outlets` naming no `id`: R stops with the same internal error; Python returns zero rows with `topo_sort` typed.
- No outlet at all (every `toid` names an `id`): today R warns twice (`no_outlet`, `sort_incomplete`) and then stops with the internal error; post-change the cycle check stops first (`network_cycle`), since such a network has a cycle.
- A cycle with an outlet beside it: today the `hy_topo` route warns (`sort_incomplete`) and returns only the rows that drain to an outlet, unless the cycle is a pair or a self-loop, which stops; the flownetwork route warns and returns part of the rows. Post-change both stop (`network_cycle`).
- Duplicated row (one `id` with the same `toid` twice, flownetwork pattern): both rows return, contiguous, sharing one `topo_sort`. S-6 lists this fixture as raising; R does not. See Changes.
- Input already carrying `topo_sort`: R returns `topo_sort.x` and `topo_sort.y`; Python replaces the column.
- Missing weight, zero-length line, empty name: not applicable; those columns pass through.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| not an edge-list or node pattern | no `toid`, and no `fromnode`/`tonode` to build it from | all | `add_toids()` to build `toid` from `fromnode`/`tonode`, or `hy(x, add_topo = TRUE)` | `needs_topo` |
| node conversion without `divergence` | `add_toids()` cannot remove minor paths without a divergence flag | all | `add_divergence()`, or `to_flownetwork()` to keep every path | `divergence_required` |
| cycle | flowlines whose downstream path returns to themselves; today only a pair or a self-loop | the rows in the cycle | `check_hy_graph()` to list them, then correct their `toid` | `network_cycle` |
| `id` and `toid` types differ (warning) | outlet detection compares across types | all | give `id` and `toid` one type | `id_toid_type_mismatch` |
| no outlet (warning) | no row's `toid` leaves the table | all | add or mark an outlet; post-change `network_cycle` is raised first | `no_outlet` |
| rows not reached (warning) | some rows do not drain to an outlet, usually through a cycle | the rows left out | `check_hy_graph()`; post-change not reached without `outlets` | `sort_incomplete` |
| runaway walk | the walk took more than twice as many steps as there are flowlines | all | `check_hy_graph()`; no probe reaches it | `sort_runaway` |

## References

- R source: `R/sort_network.R` (`add_topo_sort` and methods, `sort_network` methods, `sort_network_impl`), `R/make_index_ids.R` (`make_index_ids_impl`), `R/check_hy_graph.R` (`check_hy_graph`, `check_hy_outlets`), `R/hy_classes.R` (`hy_as_dataframe`, `hy_classify_and_redispatch`, `hy_node_to_topo`, `hy_dispatch_error`, `hy_guidance_topo`), `R/utils.R` (`is_outlet`, `get_hyg`, `put_hyg`).
- Tests: `tests/testthat/test_sort_network.R` ("get_sorted", "add_topo_sort deals with diversions"), `tests/testthat/test_hy_classes.R`.
- Descriptions: `algorithms/add_toids.md` (the node conversion), `algorithms/hy_capabilities.md` (the code-table row).
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

Outlets 3 and 7; 10 and 9 flow to 2; 2 and 5 flow to 3. Today `add_topo_sort(x)` returns (`id`, `topo_sort`) = (7, 6), (10, 5), (9, 4), (2, 3), (5, 2), (3, 1): outlet 7, last in input, comes first, and siblings keep input order. Post-change (D-4) it returns (10, 6), (9, 5), (2, 4), (5, 3), (3, 2), (7, 1) for this input and for any permutation of its rows: `"10"` precedes `"9"`, `"2"` precedes `"5"`, and outlet `"3"` precedes `"7"`. `add_topo_sort(x, outlets = 3)` returns (10, 5), (9, 4), (2, 3), (5, 2), (3, 1) before and after the change.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording R 1.3.0 behavior. Intended changes (ROADMAP §4, M3): D-4, sibling and outlet order by identifier byte order, which changes row order and `topo_sort` values on any network with a confluence or more than one outlet; D-8, every cycle stops through strongly connected components on both routes (direction per the proposed decision; today only reciprocal pairs and self-loops stop, on the `hy_topo` route); I-2, identifier types on ingest, which reaches `toid` on the node route; A-10, the `needs_topo` stop takes the condition class. A-9 gap: A-16 lists `split=False`, which R does not have; Python follows R. Captured, not changed (no §4 row): `topo_sort` is `double`; an input `topo_sort` column yields `topo_sort.x`/`topo_sort.y`; zero rows and `outlets` naming no `id` stop with an internal error where Python returns zero rows (I-11); the S-6 duplicated edge returns rather than raises; `hy_capabilities()` reports the function unavailable on `hy_flownetwork` although it runs there.
