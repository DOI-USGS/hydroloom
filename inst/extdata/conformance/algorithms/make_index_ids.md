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

Expectation (S-7 graph layer): one row per edge, columns `id` and `neighbor` (identifier, stored as text), `position` (0-based rank within the node's neighbor list, D-5 order), and `main` (flag) when the input carries `downmain` (`"to"`) or `upmain` (`"from"`). No `edge_id` column: R never returns one from `make_index_ids`. Terminals have no rows; the dense position (`indid`, `uniques` order) is not contract. Mode `"both"` is one layer with a `mode` column (`"to"`, `"from"`).

Kernel. The CSR build is the kernel this function names (K-2, K-3). It lives in `hydroloom/_kernels/csr.py`, which K-9 places below the boundary as language-local, so the signature below binds the Python package; R keeps its list form (K-3, K-10) and is not extracted.

```text
csr_from_edges(src, dst, n, rank) -> (indptr, indices, order)
  src     int64[m]   dense position of the node that owns the edge in this mode
                     ("to": the upstream flowline; "from": the downstream flowline)
  dst     int64[m]   dense position of the neighbor
  n       int        number of nodes (unique id)
  rank    int64[n]   rank of each dense position in identifier byte order (D-1, D-5),
                     computed above the boundary; the kernel never sees an identifier (K-5)
  indptr  int64[n+1] indptr[0] = 0, non-decreasing, indptr[n] = m
  indices int64[m]   neighbors of node i are indices[indptr[i]:indptr[i+1]], ordered by rank
  order   int64[m]   permutation of the input edges into CSR order; the caller aligns
                     per-edge arrays with it (main_csr = main[order])
```

`IndexIds.transpose()` calls the same kernel with `src` and `dst` exchanged. No sentinel appears in CSR (K-4); `ell()` pads with −1.

## Preconditions

The input, after `hy()` (R) or `as_hy` (Python), must be the `hy_topo` pattern (unique `id` with `toid`; `hy_leveled` satisfies it, A-3) or the `hy_flownetwork` pattern (repeated `id` with `toid`, I-6). `downmain` and `upmain` are read when present and are not required. What is raised, in check order:

- not `hy_topo`, `hy_leveled`, or `hy_flownetwork` (no `toid`; the `hy_node` pattern included): stop with the representation guidance (`needs_topo`). R does not convert a node table; Python registers no `convert` for `make_index_ids` and raises `RepresentationError` the same way. `requires.via` gives `add_toids` as the call to make first.
- `mode` not one of `"to"`, `"from"`, `"both"`: stop (`index_mode_invalid`). Python raises `ValueError`.
- `hy_topo` only: `id` and `toid` of different types, one `character`, warns (`identifier_type_mismatch`) and the join then fails inside data.table. After the I-2 change, with `id` and `toid` typed as a pair (D3-06), `hy()` cannot produce this input and the warning is unreachable through the public route.
- `hy_topo` only: the network contains a cycle (`graph_cycle`). Today R runs the reciprocal-pair join in `check_hy_graph(x)` and stops on any row whose downstream flowline drains back to it; a self-loop (`toid == id`) is caught by the same join. After the D-8 change `check_hy_graph` always computes strongly connected components, so any component larger than one, and any self-loop, stops `make_index_ids`; a three-flowline cycle, which passes today, then stops. Python raises `HydroloomError` (`graph_cycle`).
- R only, `long_form` not `NULL`: a deprecation warning (`long_form_deprecated`), after the checks above.

The `hy_flownetwork` route runs neither the outlet check nor the cycle check; a cycle in a flownetwork passes.

## Definition

A node is a unique `id`. In mode `"to"`, the neighbors of a flowline are the distinct `toid` values on its rows that are also an `id` in the table (I-4); a `toid` that is no `id` is an outlet and contributes no neighbor. In mode `"from"`, the neighbors of a flowline are the flowlines whose `toid` it is, so the `"from"` graph is the `"to"` graph with every edge reversed. Each neighbor list is in identifier byte order (D-5). When the input carries `downmain`, each `"to"` edge carries that row's `downmain` as `main`; when it carries `upmain`, each `"from"` edge carries the `upmain` of the row that defines the edge.

## Algorithm

1. Route (A-3). `data.frame`: `hy()`, then dispatch; the `hy` method reclassifies and re-dispatches or raises `needs_topo`. Python: `_as_hy`, then `_route` with native bodies for `hy_topo` and `hy_flownetwork`. Nothing is restored on return; the output is not a table.
2. Check `mode` (`index_mode_invalid`).
3. `hy_topo` only: run the outlet check (`check_hy_outlets(x, fix = FALSE)`, the type warning in Preconditions), then `check_hy_graph(x)`; stop with `graph_cycle` unless it returns `TRUE`. Drop geometry. After D-8, `check_hy_graph` must build its graph through `make_index_ids_impl()` or its own index, never through the exported `make_index_ids()`, which would call `check_hy_graph()` again.
4. Shared body (`make_index_ids_impl()` in R, used by both methods; the `hy_flownetwork` method skips step 3). Keep the columns `id`, `toid`, and `downmain`/`upmain` when present, and drop repeated rows over those columns. A duplicated edge therefore collapses to one.
5. Dense positions: unique `id` in first-appearance order, 1..n in R (`indid`), 0..n−1 in Python (`factorize`). Map each `toid` to a position; a `toid` with no `id` is an outlet: R records `toindid = 0` (the K-4 sentinel in 1-based code), Python drops the edge.
6. `"to"`: edges are (position of `id`, position of `toid`), `main` from `downmain`. `"from"`: edges reversed, `main` from the edge row's `upmain`. `"both"`: build `"to"` and transpose it.
7. Order each neighbor list by identifier byte order of the neighbor (D-5, D-1): one stable argsort on (owning position, neighbor rank) after the bincount (K-2, D-3). For an R `integer` identifier the rank is that of its decimal string, so 100 precedes 30 precedes 4, as it does for the Python `str` identifiers. Today R keeps the order the data.table merges produce, which is input row order; see Changes.
8. Python builds the CSR index below the boundary with `csr_from_edges` (K-1, K-2); the identifier-to-position map, `rank`, and `main` alignment stay above it. R formats the per-node lists into the padded matrix (ELLPACK view) with `NA` padding.

Tie rule, binding: D-5 above, nothing else; dense position order is free. Sentinel: none in CSR; −1 in the Python ELL view, `0` (outlet, `"to"`) and `NA` (headwater, `"from"`) in R's matrices. Nothing depends on randomness or scheduling (D-15).

## Postconditions

- Every `id` and every `neighbor` in the graph layer is an `id` of the input.
- The `"to"` edge set equals the set of distinct (`id`, `toid`) pairs whose `toid` is an `id` of the input (I-4), each pair once unless its rows differ in `downmain` or `upmain`.
- The `"from"` layer is the `"to"` layer with `id` and `neighbor` exchanged: the same edge multiset and the same row count (Python, and R after the fix in Changes).
- For each (`mode`, `id`), `position` takes the values 0..k−1 once each, and the neighbors in position order are non-decreasing in identifier byte order (D-5).
- A node with no neighbor in a mode has no rows in that mode; for `hy_topo` input every node has at most one `"to"` neighbor.
- `main` is present in `"to"` exactly when `downmain` is an input column and in `"from"` exactly when `upmain` is, and equals that column on the row defining the edge; it is never missing.
- `hy_topo` input that returns holds no cycle: no reciprocal pair and no self-loop today; no strongly connected component larger than one after D-8.
- `IndexIds`: `len(indptr) == len(uniques) + 1`, `indptr[0] == 0`, `indptr` non-decreasing, `indptr[-1] == len(indices)`, every index in `[0, n)`, `uniques` unique and equal as a set to the input `id`; `main` is `None` or has length m; `edge_id` is `None`.
- `make_index_ids(network, mode="both")` equals `(make_index_ids(network, mode="to"), make_index_ids(network, mode="from"))`, and the second equals the first's `transpose()` (K-3).
- D-7: the graph layer, sorted by `mode`, `id`, `position`, is unchanged by a permutation of the input rows (after the D-5 change in R).

## Complexity

O(n + m log m) time for the factorize, bincount, and the one argsort, O(n + m) space, for n unique `id` and m edges. R's padded matrix takes O(n · k) space for the largest neighbor count k.

## Edge cases

- One flowline: no edges. R returns a 1 × 1 matrix (`0` in `"to"`, `NA` in `"from"`); Python `indptr = [0, 0]`, empty `indices`; the layer has no rows.
- No connections (all terminals): no edges, no layer rows; every `toid` is an outlet whatever its value (I-4).
- A flowline with no edges inside a larger network: it is a node with an empty neighbor list in both modes.
- Duplicated row (the same `id`, `toid` twice in a flownetwork): collapsed to one edge, no warning. S-6 lists a duplicated edge as raising; that is not a ROADMAP §4 change, so it is captured, not applied.
- Zero rows: R stops inside the matrix formatting ("subscript out of bounds", after a `max()` warning); Python returns an `IndexIds` with `indptr = [0]` and no edges (I-11). The R behavior is captured and has no remedies key.
- R `lengths`: in `"to"` an outlet counts 1 (its sentinel slot), not 0; in `"from"` a headwater counts 0. The graph layer and Python do not carry this.
- Missing weight, zero-length line, empty name: not applicable; only `id`, `toid`, `downmain`, `upmain` are read.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| not the topo or flownetwork pattern | the network has no `toid` (downstream identifier) | all | `add_toids()` to build `toid` from `fromnode`/`tonode`, or `hy(x, add_topo = TRUE)` | `needs_topo` |
| `mode` not `"to"`, `"from"`, `"both"` | the graph direction is not recognized | none | pass one of the three values | `index_mode_invalid` |
| cycle in a topo network | flowlines drain back into themselves (today: pairs that reference each other) | the flowlines in the cycle, as `check_hy_graph()` returns them | `check_hy_graph()` to list them, then correct their `toid` | `graph_cycle` |
| `id` and `toid` of different types (warning, R today) | outlet detection compares an integer column to a character one | all | convert both to one type before `hy()` | `identifier_type_mismatch` |
| `long_form` given (warning, R only) | the long table output is deprecated | all | drop `long_form` | `long_form_deprecated` |

## References

- R source: `R/make_index_ids.R` (`make_index_ids` and methods, `make_index_ids_impl`, `make_to_dt`, `make_from`, `make_from_dt`, `call_format_index_ids`, `format_index_ids_internal`), `R/check_hy_graph.R` (`check_hy_graph`, `check_hy_outlets`), `R/hy_classes.R` (`hy_classify_and_redispatch`, `hy_dispatch_error`, `hy_guidance_topo`).
- Tests: `tests/testthat/test_make_index_ids.R`; `tests/testthat/test_check_hy_graph.R` (callers of `mode = "both"`).
- Descriptions: `algorithms/add_toids.md` (the conversion from `hy_node`), `algorithms/hy.md` (I-2 pairing, D3-06), `algorithms/hy_capabilities.md` (the code-table row).
- Saad, Y., 2003, Iterative Methods for Sparse Linear Systems, 2nd ed., SIAM, §3.4 (compressed sparse row and ELLPACK storage).
- SPEC K-1 to K-6, K-9, K-10, S-6, S-7, I-1, I-2, I-4, I-6, I-11, D-1, D-3, D-5, D-7, D-8, D-15, A-3, A-6, A-9, A-10, A-16.

## Worked example

Fixture `fixtures/make_index_ids_worked_example.gpkg`, aspatial, five rows, INTEGER columns: (`id`, `toid`) = (30, 10), (4, 10), (100, 10), (10, 7), (7, 0). Three headwaters join at flowline 10, which drains to the outlet 7.

`make_index_ids(x, mode = "both")`, graph layer after D-5:

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

Today R lists the upstream neighbors of 10 in input order, 30, 4, 100; with the rows permuted to (10, 7), (100, 10), (7, 0), (30, 10), (4, 10) it lists 100, 30, 4. Byte order gives 100, 30, 4 for both, where numeric order would give 4, 30, 100.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording R 1.3.0 behavior. Intended changes applied in the binding sections: I-2 (ROADMAP §4, M3), whole `double` identifiers become `integer` and out-of-range ones `character` in `to_list$id`, with `id` and `toid` typed as a pair (D3-06), which makes the type-mismatch input work; the graph layer, compared as text, is unchanged otherwise. D-8 (ROADMAP §4, M3), the cycle stop covers every strongly connected component larger than one and self-loops. Proposed, not in ROADMAP §4 (decision rows): D-5 neighbor order by identifier byte order, where R 1.3.0 keeps input row order although ROADMAP §2 says "no intended change"; `"from"` mode on a flownetwork with `upmain` joins `upmain` by `id`, which duplicates every upstream edge of a flowline whose rows carry different `upmain` values (167 extra edges on `new_hope`) and mislabels `main`, recorded and to be fixed with D-5. Captured, not changed: duplicated edges collapse (S-6 says raise); zero-row input stops in R (I-11 says return). A-9 gap: Python omits the deprecated `long_form`; `edge_id` is always `None` because R never returns one. The A-10 condition-class change applies to the `needs_topo` stop.
