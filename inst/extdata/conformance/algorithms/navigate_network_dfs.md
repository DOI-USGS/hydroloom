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

Python (ROADMAP §2, SPEC A-16, A-17): `navigate_network_dfs(network, starts, *, direction="down", reset=False)`. `network` is R's `x` (A-9) and is a frame or an `IndexIds` (K-3); an `IndexIds` is taken by an `isinstance` branch before any name alignment. `direction` takes the four R values, as A-16 lists them (D3-47).

Return: a list with one element per unique start, in the order of first appearance in `starts`. Each element is a list of paths; each path holds identifiers in visit order. R paths are atomic vectors in the type of `id` (I-1) and carry names from a counter that runs across starts; the names are not contract. Python paths are pandas `Index` objects of `str` (I-1, I-2). An element with nothing left to visit is an empty list. Expectations use the S-7 traversal layer:

| column | type |
| --- | --- |
| `start` | identifier, the start of the element |
| `path` | integer, 0-based index of the path within its element |
| `order` | integer, 0-based visit position across the element's paths |
| `id` | identifier |

Named kernel (K-6, the many-start DFS of DESIGN §4). Its signature binds both languages, R at extraction (K-10, D3-44). The kernel will live in Python `_kernels/sequential.py` and R `R/kernel-sequential.R`, where the K-1 module table and K-10 place the many-start DFS (D3-157), with the same names and order in both:

`dfs_paths(indptr, indices, starts, main, reset) -> (visit, path, start_ptr)`

- `indptr` int64, length n + 1, and `indices` int64, length m: the CSR graph `make_index_ids` builds (K-2, K-3), `mode = "to"` for `down`/`downmain`, `"from"` for `up`/`upmain`, neighbor lists in identifier byte order (D-5). A `toid` outside the network contributes no edge, so a terminal has an empty neighbor range (I-4, K-4); frame input and `IndexIds` input give the same graph.
- `starts` int64: dense positions (0 to n − 1), unique, first-appearance order.
- `main` bool array of length m aligned with `indices`, or `None` (R `NULL`): when given, only edges with `main` true are followed.
- `reset` bool.
- `visit` int64: dense positions in visit order, all starts concatenated; `path` int64 aligned with `visit`, the 0-based path within its start; `start_ptr` int64, length k + 1, offsets into `visit` per start.

R passes `double` and the R kernel casts; positions are 0-based at the kernel boundary and 1-based inside the R kernel, with sentinel 0 there (K-4, K-10). The R body (`navigate_network_dfs_internal`) does the same walk on the padded matrix; it takes this shape when the kernel is extracted (K-10).

## Preconditions

A table input goes through `hy(x, clean = TRUE)` (R) or `as_hy` (Python) and must then be the `hy_topo` or `hy_flownetwork` pattern: `id` and `toid`, `id` unique or repeated. A `hy_node` table (`fromnode`/`tonode`, no `toid`) is converted first, with no message of its own: `add_toids(return_dendritic = FALSE)` for `down`/`up`, `to_flownetwork()` for `downmain`/`upmain` (R `try_add_toids`), which warns as the last bullet below says. The main directions also need `upmain` (for `upmain`) or `downmain` (for `downmain`) columns after that step, as `to_flownetwork()` writes them. A list or `IndexIds` input must match the direction: a `"to"` graph for `down`/`downmain`, a `"from"` graph for `up`/`upmain`, and it must carry `main` for a main direction. Every start must be an `id`. What is raised, in check order:

- list input in the wrong direction: stop (`index_ids_direction_mismatch`).
- list input without `main` for a main direction: stop (`index_ids_missing_main`).
- input neither a data.frame nor a `make_index_ids()` list: stop (`network_not_frame_or_index_ids`).
- table not convertible to `hy_topo`/`hy_flownetwork`: stop from `make_index_ids()` with the representation guidance (`needs_topo`).
- table with a cycle, `hy_topo` or `hy_flownetwork`: stop from `make_index_ids()` (`network_cycle`), which runs `check_hy_graph()` on both routes (D-8, D3-41). A list or `IndexIds` input is not checked again.
- main direction with no main-path columns or element after conversion: stop (`main_path_required`). A `hy_node` table lacking `divergence` or `levelpath` first warns from `to_flownetwork()` (`node_to_flownetwork_no_main`), then stops here.
- a start not among `id`, or missing: stop (`start_not_in_network`).

R matches `direction` by substring: a value containing `"up"` walks upstream, one containing `"main"` follows main connections, and any other value walks downstream without error. Python raises `ValueError` (`direction_invalid`) for a value outside the four.

## Definition

Roxygen: given a starting node, return all reachable paths. Once visited, a node is marked as visited and will not take part in a future path. `reset`: if TRUE, reset graph for each start such that later paths will have overlapping results.

As a property of the input: element k holds every flowline reachable from start k by following connections in the walk direction (downstream along `toid`, or upstream against it), only main connections for `downmain`/`upmain`, each once, in depth-first visit order; where the walk can go on to more than one flowline (a divergence downstream, a confluence upstream), it takes the one with the smallest identifier in byte order first (D-6). With `reset = FALSE` a flowline already returned for an earlier start is left out, and the walk does not pass through it. The element is split into paths, each a run of flowlines in which every flowline drains to the next (downstream) or receives the next (upstream). A path ends at a terminal, a flowline with no flowline below it (an outlet, downstream) or above it (a headwater, upstream), or at a flowline the walk leaves for one it has already visited; the next path begins at the flowline the walk turns to next. A `toid` outside the network connects to nothing: a flowline whose only `toid` is outside is an outlet, and an outside `toid` beside one in the network neither ends nor splits a path (D3-76, D3-156). The four directions walk the connections in the table; they are not the UT, UM, DM, and DD modes of `navigate_hydro_network`.

## Algorithm

1. Input route. A `make_index_ids()` list (R: names `to`, `lengths`, `to_list`, or `froms`, `lengths`, `froms_list`; Python: `IndexIds`, by `mode`) is checked against `direction` and used as is. Otherwise: `hy(x, clean = TRUE)`; `try_add_toids()` (Preconditions); `make_index_ids(mode = "from")` when `direction` contains `"up"`, else `mode = "to"`. On both routes `make_index_ids` runs `check_hy_graph` first (D-8, D3-41), and on a `hy_topo` table `check_hy_outlets` as well.
2. A main direction needs the `main` element (R) or `main` array (Python); `make_index_ids` writes it from `downmain` in `"to"` mode and, in `"from"` mode, from the `upmain` of the row that defines each edge (D3-43).
3. Starts: every start must match an `id` (R `%in%`, which compares a numeric start with a character `id` as text). Map them to dense positions and keep the unique ones in first-appearance order (R `unique(match(starts, id))`). An empty `starts` returns an empty list, on a zero-row network as on any other (I-11, D3-46).
4. Walk, below the kernel boundary (K-1, K-5), per start s in order. Binding rules:
   - Edge state is per neighbor slot. A slot is unfollowed until its source flowline is expanded; expanding a flowline marks all its slots followed. A flowline is visited the first time it is popped. With `reset = TRUE`, both states are cleared before each start; with `reset = FALSE` they carry over.
   - The stack starts as [s], with an empty current path and the break flag clear. Pop an entry. A break entry sets the break flag. A flowline entry: if the break flag is set, close the current path when it is non-empty and clear the flag; if the flowline is unvisited, mark it and append it to the current path; then expand it, whether or not it was just visited.
   - Expanding flowline v with no slot, a terminal (an outlet downstream, a headwater upstream), pushes one break, on every arrival at v, the first included. Expanding any other v pushes, for each slot of v in reverse neighbor-list order, a break if the slot is followed, else the neighbor; then marks every slot followed. A `toid` outside the network has no slot (Signature) and adds no break of its own: a flowline whose only `toid` is outside is a terminal, and one with a `toid` in the network as well expands as if it had that one alone (D3-76, D3-156). With `main`, only unfollowed slots whose `main` is true are pushed, and no break, at a terminal or elsewhere.
   - Stop when the stack is empty; close the current path when non-empty. An element with no visited flowline is empty.
5. Sibling tie rule, binding. Neighbor lists are in identifier byte order as `make_index_ids` gives them (D-1, D-5), integer identifiers compared as their decimal strings, so `"10"` precedes `"9"`. The stack is last in, first out and the kernel pushes each list in reverse, so at a flowline with several neighbors the walk goes first to the smallest identifier and backtracks through the rest in byte order (D-6). Visit order and path split therefore depend on the network and not on input row order (D-7).
6. Sentinel (K-4). The Python kernel uses CSR with no sentinel: followed slots are a boolean array of length m, and −1 marks a break on the stack. R's padded matrix uses 0 for a followed slot, which pushes a break, and `make_to_dt` also records 0 (`toindid = 0`) for a `toid` outside the network. The R walk treats a flowline with no slot other than `toindid = 0` as a terminal and pushes one break on every arrival there; at any other flowline it skips a `toindid = 0` slot on first expansion and pushes nothing for it. On a later arrival every slot reads as followed and pushes a break, and an extra break from an outside slot changes nothing, since the break flag is already set. R's list input and Python's `IndexIds` then walk the same graph (Changes).
7. Output: identifiers from dense positions (`uniques`), split by path. R names paths with a counter that runs across starts and counts empty paths; the names are not contract.

D-15 holds: nothing depends on randomness or scheduling. The function sorts nothing and returns no frame.

## Postconditions

- The result has one element per unique start, in first-appearance order.
- No path is empty; an element is empty exactly when, with `reset = FALSE`, its start was returned for an earlier start.
- When an element is non-empty, its first identifier is its start.
- Within one element no identifier appears twice; with `reset = FALSE` no identifier appears twice in the whole result.
- With `reset = TRUE`, element k's identifier set equals the set reachable from start k in the walk direction (main connections only for a main direction), start included.
- With `reset = FALSE`, element k's identifier set equals the set reachable from start k without passing through an identifier in an earlier element.
- Every identifier in an element other than its first is a neighbor, in the walk direction, of an identifier earlier in the same element.
- Within a path, each identifier after the first is a neighbor, in the walk direction, of the identifier before it. A path ends at a terminal or at a flowline the walk leaves for one already visited.
- For `downmain` and `upmain`, every non-empty element is one path.
- For `down` and `up`, with `reset = TRUE` or for the first start, if every reachable flowline has at most one neighbor in the walk direction, the element is one path; an outlet (`down`) or a headwater (`up`) ends a path, and a later branch begins a new one.
- At a flowline with several unvisited neighbors, the neighbor with the smallest identifier in byte order is visited first (D-6).
- `order` runs 0 to (element size − 1) without gaps; `path` starts at 0 and steps by 0 or 1 along `order`.
- D-7: for a row-permuted input the whole result is unchanged, for every direction.

## Complexity

O(n + m) time and space per start with `reset = TRUE`, O(n + m) over all starts with `reset = FALSE`, plus building the index (O(n + m), see `make_index_ids`). R also copies the padded matrix once for `reset = TRUE`, O(n × the largest neighbor count).

## Edge cases

- One flowline: one element holding one path with that identifier, in either direction.
- No connections (all terminals): each start returns a one-identifier path; every start is its own element.
- Empty `starts`: returns an empty list.
- Zero-row network: with empty `starts`, returns an empty list, a zero-row traversal layer (I-11, D3-46); any start raises `start_not_in_network`.
- Duplicated row (the same `id`/`toid` pair twice in a flownetwork): R collapses it with `distinct()` in `make_index_ids` and returns `[[1, 2]]` without raising; case `navigate_network_dfs_duplicated_edge` captures this and is R-only. S-6 makes a repeated (`id`, `toid`) pair an error raised at ingest (D3-57): Python `as_hy` raises `id_toid_pair_repeated` from its first release, and R follows under the M6 §4 row.
- Repeated start: kept once.
- A start already returned for an earlier start, with `reset = FALSE`: its element is an empty list.
- A cycle in a `hy_flownetwork`: raises `network_cycle` from `make_index_ids`, as in a `hy_topo` (D3-41). `loop2.gpkg`, which holds two three-flowline cycles, raises.
- A flowline with both a `toid` in the table and a `toid` outside it, common in a clipped extract where one branch of a divergence leaves the subset: the outside connection is not a terminal and does not break the path (D3-76, D3-156). On `sort_divergence_leaves.gpkg`, where 2 divides into 3 and into 9 outside the table, the walk from 1 returns `[[1, 2, 3]]` for either order of the two rows of 2 (`sort_divergence_leaves_permuted.gpkg`). `loop.gpkg` has one such flowline.
- A walk reaching an outlet (`down`) or a headwater (`up`) pushes a break there on every arrival, so the path ends and the next branch begins a new one (D3-156).
- Missing weight, zero-length line, empty name: not applicable.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| list input in the wrong direction | An index built for downstream navigation (mode "to") cannot be walked upstream, nor one built for upstream navigation (mode "from") downstream. | all | make_index_ids() with mode "from" for up or upmain, "to" for down or downmain. | `index_ids_direction_mismatch` |
| list input without `main` for a main direction | The index carries no main-path flags (main), which upmain and downmain navigation follow at each divergence and confluence. make_index_ids() carries them only from the upmain/downmain columns, which to_flownetwork() sets from levelpath. | all | add_levelpaths(), then to_flownetwork(), then make_index_ids() on the result. | `index_ids_missing_main` |
| input neither a table nor a `make_index_ids()` list | The input is neither a network table nor an index from make_index_ids(), so there is nothing to navigate. | all | Pass the network table or make_index_ids() output. | `network_not_frame_or_index_ids` |
| not convertible to `id`/`toid` | The function needs each flowline's downstream flowline (toid), and the network has neither toid nor fromnode/tonode, the identifiers of the points at each end of every flowline that add_toids() builds toid from; the representation is then hy. Where the network has fromnode/tonode, the functions convert it themselves, except check_hy_graph() in R, which stops here with representation hy_node; add_toids() fixes that case. Where the downstream column has a source name the vocabulary lacks, registering it as toid (hydroloom_names() in R, register_names() in Python) is the alternative. | all | Add a toid column giving each flowline's downstream flowline, then call again. | `needs_topo` |
| cycle in the network, `hy_topo` or `hy_flownetwork` | Some flowlines drain back into themselves: following toid downstream from one of them returns to it, so the network has no upstream-to-downstream order. | the flowlines in each cycle, as check_hy_graph() returns them | check_hy_graph() to list them, then correct their toid. | `network_cycle` |
| main direction without main-path columns | upmain and downmain navigation follow the main path at each divergence and confluence; the network has no upmain/downmain columns to say which flowline that is. | all | add_levelpaths(), then to_flownetwork(), which sets upmain and downmain from levelpath. | `main_path_required` |
| `hy_node` table without `divergence`/`levelpath`, main direction (warning, then the row above) | fromnode/tonode alone do not say which flowline is the main path at a divergence or confluence, so the converted network carries no upmain/downmain. | all | add_divergence(), given the coastal and inland outlets, then add_levelpaths(), before the conversion. | `node_to_flownetwork_no_main` |
| start not in the network, or missing | A start is missing or is not an id in the network. | the starts not found | Pass values of id as starts. | `start_not_in_network` |
| `hy_node` table, main direction (Python only, until `to_flownetwork` is ported) | A main direction follows upmain/downmain, which only to_flownetwork() builds from a node table, and to_flownetwork() is not yet available in Python. | all | Build the flownetwork in R with to_flownetwork(), or supply toid with downmain or upmain columns. | `node_main_needs_flownetwork` |
| unknown `direction` (Python only) | direction is not one of down, up, downmain, upmain. | none | Pass 'down', 'up', 'downmain', or 'upmain'. | `direction_invalid` |

`needs_topo` and `network_cycle` are raised by `make_index_ids` and shared with its description, whose keys govern if they differ. `node_to_flownetwork_no_main` belongs to `to_flownetwork`.

## References

- R source: `R/navigate_network_dfs.R` (`navigate_network_dfs`, `navigate_network_dfs_internal`), `R/add_toids.R` (`try_add_toids`), `R/make_index_ids.R` (`make_index_ids`, `make_to_dt`, `make_from`, `make_from_dt`, `format_index_ids_internal`), `R/to_flownetwork.R`, `R/hy.R`.
- Tests: `tests/testthat/test_navigate_network_dfs.R`; `tests/testthat/test-conformance-converters.R` (traversal layer).
- Callers that rely on `reset = FALSE`: `R/add_divergence.R`. `add_divergence` flattens each outlet's upstream element to a set; `add_return_divergence` reads the last flowline of each path and so relies on the split.
- Descriptions: `algorithms/make_index_ids.md`, `algorithms/add_toids.md`, `algorithms/check_hy_graph.md`.
- SPEC I-1, I-2, I-4, I-11, K-1 to K-6, K-10, D-1, D-5, D-6, D-7, D-8, D-15, S-6, S-7, A-9, A-10, A-16, A-17.

## Worked example

Fixture `fixtures/dfs_tie.gpkg`, aspatial, INTEGER columns, rows in this order:

| id | toid |
| --- | --- |
| 1 | 10 |
| 1 | 9 |
| 10 | 4 |
| 9 | 4 |
| 4 | 0 |

Flowline 1 divides into 10 and 9, which join at 4, the outlet. `navigate_network_dfs(x, 1)` returns `list(list(c(1, 10, 4), 9))`: 1 lists 10 before 9 in byte order and pushes them in reverse; 10 is popped first, then 4, an outlet, which pushes a break; the break ends the path, so 9 begins the second; 4, popped again from 9, is already visited and pushes a break again, which ends the walk. Traversal layer: (start, path, order, id) = (1, 0, 0, 1), (1, 0, 1, 10), (1, 0, 2, 4), (1, 1, 3, 9). `navigate_network_dfs(x, 4, direction = "up")` returns `list(list(c(4, 10, 1), 9))`: 1 is a headwater and pushes a break, so 9 begins a second path. With the rows of 1 swapped and the rows of 10 and 9 swapped (`fixtures/dfs_tie_permuted.gpkg`), both results are the same (D-7). R 1.3.0 gives `list(list(c(1, 9, 4), 10))` and `c(4, 9, 1, 10)` on the first row order and `list(list(c(1, 10, 4), 9))` and `c(4, 10, 1, 9)` on the second, from input-order siblings, a break at the outlet, and none at the headwater; the cases carry the R change (Changes).

## Changes

- 2026-10-25, suite 0.1.0: first description, recording R 1.3.0 behavior. A-9 gaps against A-16: `direction` also takes `"upmain"` and `"downmain"`; the result is per unique start and split into paths, not one flat vector aligned with `starts`; plan edit proposed. Intended changes that apply (ROADMAP §4, M3): D-8, after which `make_index_ids` stops on every cycle in a `hy_topo` table, so `three_cycle` raises `network_cycle` where it now returns; I-2, after which identifiers keep the ingest type (the `nhdplushr` identifiers become `character`; values compared as text are unchanged); A-10, which classes the `needs_topo` stop. D-6 is not a §4 change: today's sibling order (step 5) is captured, a §4 row for D-5/D-6 is proposed, and the R walk would be rewritten as the `dfs_paths` kernel with it. Captured, not changed: zero-row input stops (I-11), a duplicated edge is collapsed rather than raised (S-6), `direction` is matched by substring.
- 2026-10-25, suite 0.1.0, after the hydrology review: a connection leaving the table breaks the path in its slot position, as R does, and binds Python (D3-59); `dfs_paths` takes `outside` in place of `outlet_break`. Definition states that an upstream path split carries no meaning. Remedy wording reworded in hydrographic terms.
- 2026-10-27, suite 0.1.0: M3 dispositions. D3-40 with D3-76 (ROADMAP §4 "Neighbor order in `make_index_ids` and sibling order in `navigate_network_dfs` by identifier, not input row order; a `toid` outside the network is a terminal", D-5, D-6, M3): D-5 and D-6 bind again; siblings are walked smallest identifier first; a `toid` outside the network is a terminal with no slot, so neither an outlet nor a divergence branch leaving the table breaks a path, frame and index input walk the same graph, and `dfs_paths` loses `outside` (D3-59 superseded). R change: `navigate_network_dfs_internal` pushes neighbors in reverse byte order and skips `toindid = 0` slots on first expansion; the `d6` cases are regenerated. D3-44: `dfs_paths` binds both languages, R at extraction (K-10). D3-43 (same §4 row): `"from"` mode takes `main` per edge. D3-41 (ROADMAP §4 D-8 row): `make_index_ids` checks cycles on the flownetwork route, so a cycle in a `hy_flownetwork` raises `network_cycle` (`loop2_down`). D3-46 (ROADMAP §4 "Zero-row input returns typed zero-row output", I-11, M3): a zero-row network with empty `starts` returns an empty list in both languages (`empty_network`, shared). D3-47: A-16 lists the four directions. D3-146: Remedies texts aligned to `remedies.json`. H-07: Definition says the directions are not `navigate_hydro_network` modes.
- 2026-10-28, suite 0.1.0: H-01, H-02, H-03, H-04 from the unit-2 hydrology read, with D3-156 as revised: every terminal (an outlet downstream, a headwater upstream) pushes a break on every arrival, and an outside `toid` adds no slot and no break, so each path is a run of flowlines draining one to the next; step 4, step 6, Definition, Postconditions, Edge cases, and the worked example restated (`dfs_tie` down `[[1,10,4],[9]]`, up `[[4,10,1],[9]]`). The outlet split downstream is kept from R 1.3.0; the headwater split upstream is new (D-5/D-6 §4 row). `add_divergence` output does not move: it walks upstream from each outlet and keeps only the set of flowlines per outlet, which D3-156 does not change. `add_return_divergence` reads the last flowline of each downstream path of each minor branch; its network comes from `add_toids` on the full node table, so its only outside `toid` values are outlets, where R 1.3.0 already breaks, and the new split matches R there; only the D-6 sibling order can move it, at a flowline in a minor branch with one neighbor already visited and one not. Neither function has conformance cases yet. Definition names divergences and confluences in place of "neighbor".
- 2026-10-28, suite 0.1.0: description sweep before M3 closes. D3-157: the Signature places `dfs_paths` in `_kernels/sequential.py` and `R/kernel-sequential.R`, as the K-1 module table and K-10 do; neither file exists yet. D3-159, D3-172: the Signature states what the R body does without dating it. Unit-4 fixture pass: the D-3 and D-14 anchors leave Algorithm, since the function sorts nothing and returns no frame. D3-165: case `navigate_network_dfs_two_outlets_character_start` is dropped as the same call as `navigate_network_dfs_two_outlets_down`. The Remedies table matches `remedies.json` (D3-146). No R change in this run touches the function.
- 2026-10-29 (M3 close run 3, reader round D3-201): H-01, H-02, H-03, H-04, H-05.
- 2026-11-04 (M5 run 4 unit 6 SP-01, an implementer finding on a frozen description, P-4): the Postcondition on one-path elements states the "if" direction only. The "only if" direction fails where a flowline divides and its branches rejoin: on 1 → {2, 3}, 2 → 3, walked down, R returns `list(list(c(1, 2, 3)))`, one path, though 1 has two neighbors. SP-02: Remedies gains the Python-only `node_main_needs_flownetwork`, an agent-drafted key for a `hy_node` table in a main direction, which Python cannot convert until `to_flownetwork` is ported (BACKLOG M6).
