---
requires:
  pattern: [hy_topo, hy_flownetwork]
  columns: []
  via:
    hy_node: add_toids
---

# navigate_network_dfs

## Signature

R: `navigate_network_dfs(x, starts, direction = "down", reset = FALSE)`; one function, no S3 methods.

| argument | type | default |
| --- | --- | --- |
| `x` | table (data.frame, tibble, or sf), or the list `make_index_ids()` returns (`mode = "to"` for downstream, `"from"` for upstream) | required |
| `starts` | identifier vector, values of `id` | required |
| `direction` | string: `"down"`, `"up"`, `"downmain"`, `"upmain"` | `"down"` |
| `reset` | flag: R `logical` length one, Python `bool` | `FALSE` / `False` |

Python (ROADMAP §2, SPEC A-16, A-17): `navigate_network_dfs(network, starts, *, direction="down", reset=False)`. `network` is R's `x` (A-9) and is a frame or an `IndexIds` (K-3); an `IndexIds` is taken by an `isinstance` branch before any name alignment. `direction` takes the four R values (A-9: the R signature wins over A-16's `"up"`/`"down"`; Changes).

Return: a list with one element per unique start, in the order of first appearance in `starts`. Each element is a list of paths; each path holds identifiers in visit order. R paths are atomic vectors in the type of `id` (I-1) and carry names from a counter that runs across starts; the names are not contract. Python paths are pandas `Index` objects of `str` (I-1, I-2). An element with nothing left to visit is an empty list. Expectations use the S-7 traversal layer:

| column | type |
| --- | --- |
| `start` | identifier, the start of the element |
| `path` | integer, 0-based index of the path within its element |
| `order` | integer, 0-based visit position across the element's paths |
| `id` | identifier |

Named kernel (K-6, the many-start DFS of DESIGN §4). Python `_kernels/walk.py` and R `R/kernel-walk.R`, same names and order:

`dfs_paths(indptr, indices, starts, main, reset, outlet_break) -> (visit, path, start_ptr)`

- `indptr` int64, length n + 1, and `indices` int64, length m: the CSR graph in the walk direction (K-2), `mode = "to"` for `down`/`downmain`, `"from"` for `up`/`upmain`; neighbor lists in the order `make_index_ids` gives (D-5).
- `starts` int64: dense positions (0 to n − 1), unique, first-appearance order.
- `main` bool array of length m aligned with `indices`, or `None` (R `NULL`): when given, only edges with `main` true are followed.
- `reset` bool.
- `outlet_break` bool: an empty neighbor range counts as one break (true for `down`, false otherwise).
- `visit` int64: dense positions in visit order, all starts concatenated; `path` int64 aligned with `visit`, the 0-based path within its start; `start_ptr` int64, length k + 1, offsets into `visit` per start.

R passes `double` and the R kernel casts; R positions are 1-based with sentinel 0 (K-4). Today's R body (`navigate_network_dfs_internal`) does the same walk on the padded matrix and is not yet in this shape (Changes).

## Preconditions

A table input goes through `hy(x, clean = TRUE)` (R) or `as_hy` (Python) and must then be the `hy_topo` or `hy_flownetwork` pattern: `id` and `toid`, `id` unique or repeated. A `hy_node` table (`fromnode`/`tonode`, no `toid`) is converted first, silently: `add_toids(return_dendritic = FALSE)` for `down`/`up`, `to_flownetwork()` for `downmain`/`upmain` (R `try_add_toids`). The main directions also need `upmain` (for `upmain`) or `downmain` (for `downmain`) columns after that step, as `to_flownetwork()` writes them. A list or `IndexIds` input must match the direction: a `"to"` graph for `down`/`downmain`, a `"from"` graph for `up`/`upmain`, and it must carry `main` for a main direction. Every start must be an `id`. What is raised, in check order:

- list input in the wrong direction: stop (`index_ids_direction_mismatch`).
- list input without `main` for a main direction: stop (`index_ids_missing_main`).
- input neither a data.frame nor a `make_index_ids()` list: stop (`network_not_frame_or_index_ids`).
- table not convertible to `hy_topo`/`hy_flownetwork`: stop from `make_index_ids()` with the representation guidance (`needs_topo`).
- `hy_topo` table with a cycle: stop from `make_index_ids()` (`network_cycle`). Today only a flowline pair that reference each other or a self-loop stops; after D-8 every cycle does (Changes). `hy_flownetwork` input is not checked and its cycles are walked.
- main direction with no main-path columns or element after conversion: stop (`main_path_required`). A `hy_node` table lacking `divergence` or `levelpath` first warns from `to_flownetwork()` (`node_to_flownetwork_no_main`), then stops here.
- a start not among `id`, or missing: stop (`start_not_in_network`).

R matches `direction` by substring: a value containing `"up"` walks upstream, one containing `"main"` follows main connections, and any other value walks downstream without error. Python raises `ValueError` (`direction_invalid`) for a value outside the four.

## Definition

Roxygen: given a starting node, return all reachable paths. Once visited, a node is marked as visited and will not take part in a future path. `reset`: if TRUE, reset graph for each start such that later paths will have overlapping results.

As a property of the input: element k holds every flowline reachable from start k by following connections in the walk direction (downstream along `toid`, or upstream against it), only main connections for `downmain`/`upmain`, each once, in depth-first visit order. With `reset = FALSE` a flowline already returned for an earlier start is left out, and the walk does not pass through it. The element is split into paths where the walk backtracks after reaching an outlet (downstream only) or after arriving again at a flowline it has already expanded; a later path begins at the flowline the walk turns to next.

## Algorithm

1. Input route. A `make_index_ids()` list (R: names `to`, `lengths`, `to_list`, or `froms`, `lengths`, `froms_list`; Python: `IndexIds`, by `mode`) is checked against `direction` and used as is. Otherwise: `hy(x, clean = TRUE)`; `try_add_toids()` (Preconditions); `make_index_ids(mode = "from")` when `direction` contains `"up"`, else `mode = "to"`. Through a `hy_topo` table, `make_index_ids` runs `check_hy_outlets` and `check_hy_graph` first.
2. A main direction needs the `main` element (R) or `main` array (Python); `make_index_ids` writes it from `downmain` in `"to"` mode and from `upmain`, per upstream flowline, in `"from"` mode.
3. Starts: every start must match an `id` (R `%in%`, which compares a numeric start with a character `id` as text). Map them to dense positions and keep the unique ones in first-appearance order (R `unique(match(starts, id))`). An empty `starts` returns an empty list.
4. Walk, below the kernel boundary (K-1, K-5), per start s in order. Binding rules:
   - Edge state is per neighbor slot. A slot is unfollowed until its source flowline is expanded; expanding a flowline marks all its slots followed. A flowline is visited the first time it is popped. With `reset = TRUE`, both states are cleared before each start; with `reset = FALSE` they carry over.
   - The stack starts as [s]. Pop an entry. A break entry sets the break flag. A flowline entry: if the break flag is set, close the current path when it is non-empty and clear the flag; if the flowline is unvisited, mark it and append it to the current path; then expand it, whether or not it was just visited.
   - Expanding flowline v pushes, for each slot of v in neighbor-list order: the neighbor if the slot is unfollowed, else a break; then marks every slot followed. With `outlet_break`, a flowline with no slots pushes one break. With `main`, only unfollowed slots whose `main` is true are pushed, and nothing else.
   - Stop when the stack is empty; close the current path when non-empty. An element with no visited flowline is empty.
5. Sibling tie rule, binding. The stack is last in, first out, so at a flowline with several neighbors the walk goes first to the last neighbor in its list and backtracks through the rest in reverse. Today `make_index_ids` lists neighbors in input row order: in `"to"` mode the order of the (`id`, `toid`) rows of the flowline, in `"from"` mode the order of the upstream flowlines' rows. The visit order and the path split therefore depend on input row order; D-5 and D-6 are not met, and D-7 holds only for each element's set of identifiers. No ROADMAP §4 row changes this in M3. If the proposed §4 row (Changes) lands, neighbors are listed in identifier byte order (D-1, D-5) and the walk visits the smallest identifier first (D-6), so the kernel pushes the list in reverse.
6. Sentinel (K-4): R's padded matrix uses 0 both for an outlet slot and for a followed slot, which is why both push a break. The Python kernel uses CSR with no sentinel, a boolean array of length m for followed slots, and −1 on the stack for a break.
7. Output: identifiers from dense positions (`uniques`), split by path. R names paths with a counter that runs across starts and counts empty paths; the names are not contract.

D-15 holds: nothing depends on randomness or scheduling. D-3 and D-14 do not apply: nothing is sorted and no frame is returned.

## Postconditions

- The result has one element per unique start, in first-appearance order.
- No path is empty; an element is empty exactly when, with `reset = FALSE`, its start was returned for an earlier start.
- When an element is non-empty, its first identifier is its start.
- Within one element no identifier appears twice; with `reset = FALSE` no identifier appears twice in the whole result.
- With `reset = TRUE`, element k's identifier set equals the set reachable from start k in the walk direction (main connections only for a main direction), start included.
- With `reset = FALSE`, element k's identifier set equals the set reachable from start k without passing through an identifier in an earlier element.
- Every identifier in an element other than its first is a neighbor, in the walk direction, of an identifier earlier in the same element.
- For `down`, `downmain`, and `upmain`, each identifier in a path after the first is a neighbor, in the walk direction, of the identifier before it.
- For `downmain` and `upmain`, every non-empty element is one path.
- For `down`, the last identifier of every path is an outlet (I-4) or has a downstream neighbor returned before it.
- `order` runs 0 to (element size − 1) without gaps; `path` starts at 0 and steps by 0 or 1 along `order`.
- D-7, today: for a row-permuted input, each element's set of identifiers is unchanged, and for a main direction the whole result is unchanged. After the proposed D-6 change: the whole result is unchanged for every direction.

## Complexity

O(n + m) time and space per start with `reset = TRUE`, O(n + m) over all starts with `reset = FALSE`, plus building the index (O(n + m), see `make_index_ids`). R also copies the padded matrix once for `reset = TRUE`, O(n × the largest neighbor count).

## Edge cases

- One flowline: one element holding one path with that identifier, in either direction.
- No connections (all terminals): each start returns a one-identifier path; every start is its own element.
- Empty `starts`: returns an empty list.
- Zero-row network: R stops inside `make_index_ids` with "subscript out of bounds" after a warning from `max()`; I-11 is not met. Captured, not changed (no §4 row).
- Duplicated row (the same `id`/`toid` pair twice in a flownetwork): `make_index_ids` collapses it with `distinct()`; the walk proceeds and nothing is raised, so the S-6 "raises" behavior is not met. Captured.
- Repeated start: kept once.
- A start already returned for an earlier start, with `reset = FALSE`: its element is an empty list.
- A cycle in a `hy_flownetwork`: each flowline is visited once and the walk ends.
- A flowline with both a `toid` in the table and a `toid` outside it: R pushes a break for the outside slot among the real ones, so the split depends on slot order; CSR drops the outside connection. No case covers this, and Python is not required to match it.
- `direction = "up"` at a headwater pushes nothing, so the walk does not split there; downstream splits at every outlet.
- Missing weight, zero-length line, empty name: not applicable.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| list input in the wrong direction | a downstream graph cannot be walked upstream, nor the reverse | all | `make_index_ids(x, mode = "from")` for `up`/`upmain`, `mode = "to"` for `down`/`downmain` | `index_ids_direction_mismatch` |
| list input without `main` for a main direction | main connections are not in the graph | all | `make_index_ids()` on `to_flownetwork()` output | `index_ids_missing_main` |
| input neither a table nor a `make_index_ids()` list | nothing to navigate | all | pass the network table or `make_index_ids()` output | `network_not_frame_or_index_ids` |
| not convertible to `id`/`toid` | no downstream identifiers and no node columns | all | `add_toids()`, or `hy(x, add_topo = TRUE)` | `needs_topo` |
| cycle in a `hy_topo` table | flowlines refer back to one another | the rows `check_hy_graph()` returns | `check_hy_graph()` to find them, then correct `toid` | `network_cycle` |
| main direction without main-path columns | no `upmain`/`downmain` to follow | all | `to_flownetwork()` | `main_path_required` |
| `hy_node` table without `divergence`/`levelpath`, main direction (warning, then the row above) | node topology alone cannot give main connections | all | `add_divergence()`, `add_levelpaths()` | `node_to_flownetwork_no_main` |
| start not in the network, or missing | a start is not an `id` | the starts | pass values of `id` | `start_not_in_network` |
| unknown `direction` (Python only) | not one of the four directions | none | `"down"`, `"up"`, `"downmain"`, `"upmain"` | `direction_invalid` |

`needs_topo` and `network_cycle` are raised by `make_index_ids` and shared with its description, whose keys govern if they differ. `node_to_flownetwork_no_main` belongs to `to_flownetwork`.

## References

- R source: `R/navigate_network_dfs.R` (`navigate_network_dfs`, `navigate_network_dfs_internal`), `R/add_toids.R` (`try_add_toids`), `R/make_index_ids.R` (`make_index_ids`, `make_to_dt`, `make_from`, `make_from_dt`, `format_index_ids_internal`), `R/to_flownetwork.R`, `R/hy.R`.
- Tests: `tests/testthat/test_navigate_network_dfs.R`; `tests/testthat/test-conformance-converters.R` (traversal layer).
- Callers that rely on the split and on `reset = FALSE`: `R/add_divergence.R`.
- Descriptions: `algorithms/make_index_ids.md`, `algorithms/add_toids.md`, `algorithms/check_hy_graph.md`.
- SPEC I-1, I-2, I-4, I-11, K-1 to K-6, D-1, D-5, D-6, D-7, D-8, D-15, S-6, S-7, A-9, A-10, A-16, A-17.

## Worked example

Fixture `fixtures/dfs_tie.gpkg`, aspatial, INTEGER columns, rows in this order:

| id | toid |
| --- | --- |
| 1 | 10 |
| 1 | 9 |
| 10 | 4 |
| 9 | 4 |
| 4 | 0 |

Flowline 1 divides into 10 and 9, which join at 4, the outlet. `navigate_network_dfs(x, 1)` returns `list(list(c(1, 9, 4), 10))`: 1 pushes 10 then 9; 9 is popped first, then 4, whose outlet slot pushes a break; 10 begins the second path, and 4, already expanded, ends it. Traversal layer: (start, path, order, id) = (1, 0, 0, 1), (1, 0, 1, 9), (1, 0, 2, 4), (1, 1, 3, 10). `navigate_network_dfs(x, 4, direction = "up")` returns one path, `c(4, 9, 1, 10)`: 1 is a headwater, so the walk backtracks to 10 without a break. With the first two rows swapped (`fixtures/dfs_tie_permuted.gpkg`) the results are `list(list(c(1, 10, 4), 9))` and `c(4, 10, 1, 9)`, which is also what the proposed D-6 rule gives for either row order, since `"10"` precedes `"9"` in byte order.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording R 1.3.0 behavior. A-9 gaps against A-16: `direction` also takes `"upmain"` and `"downmain"`; the result is per unique start and split into paths, not one flat vector aligned with `starts`; plan edit proposed. Intended changes that apply (ROADMAP §4, M3): D-8, after which `make_index_ids` stops on every cycle in a `hy_topo` table, so `three_cycle` raises `network_cycle` where it now returns; I-2, after which identifiers keep the ingest type (the `nhdplushr` identifiers become `character`; values compared as text are unchanged); A-10, which classes the `needs_topo` stop. D-6 is not a §4 change: today's sibling order (step 5) is captured, a §4 row for D-5/D-6 is proposed, and the R walk would be rewritten as the `dfs_paths` kernel with it. Captured, not changed: zero-row input stops (I-11), a duplicated edge is collapsed rather than raised (S-6), `direction` is matched by substring.
