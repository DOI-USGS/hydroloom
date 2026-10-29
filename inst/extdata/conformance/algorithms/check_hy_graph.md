---
requires:
  pattern: [hy_topo, hy_flownetwork]
  columns: []
  via:
    hy_node: add_toids
---

# check_hy_graph

## Signature

R: `check_hy_graph(x, divergence_check = FALSE, fraction_tol = 1e-6)`. The D-8 change
(ROADMAP §4, M3) removed `loop_check`, so a call that passes it stops with R's "unused
argument" error. Python (ROADMAP §2, A-9): `check_hy_graph(network, *,
divergence_check=False, fraction_tol=1e-6)`, `network` in R's place of `x`. A-16 gives
the same signature (D3-47).

| argument | type | default |
| --- | --- | --- |
| `x` / `network` | table (data.frame, tibble, or sf; DataFrame or GeoDataFrame) | required |
| `divergence_check` | `flag`: R `logical` length one, Python `bool` | `FALSE` / `False` |
| `fraction_tol` | `float64` scalar, non-negative | `1e-6` |

Roxygen gives the argument prose: `x` "data.frame network compatible with
hydroloom_names"; `divergence_check` "if TRUE, `divergence` and, where present,
`divergence_fraction` are checked for internal consistency as described in details";
`fraction_tol` "numeric tolerance used when checking that `divergence_fraction` sums to
1 across a `fromnode` group".

Returns one of four values, listed in check order. The function never raises on a
problem it finds; it returns the problem rows (A-10).

1. No problem found: `TRUE` (R `logical` length one; Python `True`). The expectation is
   the S-7 scalar layer, one row, `value = TRUE`.
2. A divergence coding problem (only with `divergence_check`): the problem rows of the
   network in hydroloom names, in input row order, with one column appended (D-14). The
   divergence result carries no `row` column: `id` identifies the flowline (I-12,
   D3-160).

   | column | type | meaning |
   | --- | --- | --- |
   | `divergence_issue` | text | the failed checks, joined by `"; "` in check order |

3. A repeated (`id`, `toid`) pair (always checked, D3-166, D3-209): a table with the
   columns `id` and `toid` only, one row per distinct pair held by more than one input
   row, in order of each pair's first row in the input. It carries no geometry. Python
   returns it from its first release and R in M6 (D3-166).

4. A cycle (always checked, D-8): the rows on a cycle, in input row order, with two
   columns appended (D-14). The cycle result carries no `row` column either: `id`
   identifies the flowline (I-12, D3-54).

   | column | type | meaning |
   | --- | --- | --- |
   | `component` | count, R `integer`, Python `int32` | which cycle the row belongs to, numbered from 1 |
   | `reciprocal` | `flag` | `TRUE` when the cycle has exactly two flowlines |

For the divergence and cycle tables: geometry is kept when the input has it (A-8), and
columns are in hydroloom names, not the input's source names. Both tables are the R
class `hy()` gives the subset: `hy_topo` or `hy_flownetwork`, or `hy_node` for a
divergence result on a node-topology table (D3-213).

No named kernel is involved (K-6). The strongly connected components walk lives in
`_kernels/walk.py` (SPEC module table) and in R in the internal base R Tarjan function
`scc_labels()` (`R/check_hy_graph.R`); its shape is the author's.

## Preconditions

The network needs `id` and `toid` in hydroloom names. R calls `hy()` on input that does
not inherit `hy` or `hy_flownetwork`, so source names resolve through the vocabulary
aliases. Repeated `id` is accepted: `hy()` routes it to `hy_flownetwork` (I-6) and every
row is an edge of the graph. Missing `toid` becomes the reserved outlet value on ingest
(I-5); a terminal is any row whose `toid` is not an `id` (I-4).

A node-topology table (`fromnode`/`tonode`, no `toid`, A-2 `hy_node`) is not converted
in either language (D3-45, superseding D3-58). The function stops with `needs_topo`
(in R, the second of the key's `r_text` entries), raised as
`hydroloom_representation_error` in R and `RepresentationError` in Python (D3-87,
D3-90). The `via` entry above is guidance, not a conversion the function applies. The
one call that gives the function a `toid` is `add_toids(return_dendritic = FALSE)`,
which keeps every connection at a divergence (and warns that the argument is
deprecated), so a cycle through a minor path is found on its output and a table without
`divergence` does not stop. `add_toids()` with its
default keeps only main paths, so such a cycle is not found, and it stops with
`divergence_required` on a table without `divergence`. A table with
neither `toid` nor `fromnode`/`tonode` stops with `needs_topo` in both languages (D3-60).
Raising on unusable input is read as consistent with A-10, which covers the graph
problems the check finds (D3-61).

`divergence_check = TRUE` reads `fromnode` and `divergence`, and `divergence_fraction`
when that column is present. A missing `fromnode` or `divergence` column stops with
`divergence_check_missing_columns`, naming the absent columns. The divergence check runs
before the `toid` requirement is tested and before cycle detection, in both languages, so
its stop comes first whatever the graph holds. On a node-topology table with
`divergence_check = TRUE`, a missing `divergence` stops with
`divergence_check_missing_columns`, a coding problem returns the divergence result, and
only a table that passes the divergence check stops with `needs_topo`.

## Definition

A cycle is a set of flowlines in which water leaving any one of them can, by following
`toid`, return to it. In graph terms these are the strongly connected components of the
`id` to `toid` graph that hold more than one flowline, plus any flowline whose `toid` is
its own `id` (a self-loop). A dendritic or divergent river network has none: flow never
returns upstream. `reciprocal` marks a cycle a user repairs at one flowline: two
flowlines each giving the other as `toid` means one of the two is coded against the
flow, and the label does not say which (D3-75).

The result is `TRUE` exactly when the network has no cycle (and, with
`divergence_check`, no divergence coding problem; a repeated (`id`, `toid`) pair also
keeps it from `TRUE`, Postconditions). The cycle result lists every row whose
`id` and `toid` lie in the same cycle. For a table with unique `id` every flowline in a
cycle contributes its one row. For a flownetwork, a flowline in a cycle may also have a
row leading out of the cycle (to a divergence's other path); that row is not returned,
since cutting it would not break the cycle.

`component` groups rows by cycle. The numbering is fixed by the identifiers alone:
cycles are numbered from 1 in byte order (D-1) of each cycle's least identifier, every
identifier compared as its decimal string (S-7). `reciprocal` is `TRUE` for a cycle of
exactly two distinct flowlines and `FALSE` otherwise; a self-loop is a cycle of one, so
`FALSE`.

The divergence check enforces the attribute contract `accumulate_downstream()` relies on
(roxygen details, reused): 0 marks a feature not downstream of a divergence, 1 the
primary path downstream of a divergence, 2 a diverted path (NHDPlus coding). Features are
grouped by shared `fromnode`, and each `id` counts once in its group, so on a flownetwork
the rows a flowline has for its several downstream connections are one outgoing feature
(D3-213); a feature with a missing `fromnode` belongs to no group.
The checks, each a label in `divergence_issue`, in this order:

1. `divergence` is populated and in `{0, 1, 2}`: `"divergence not in {0, 1, 2}"`.
2. Every node with more than one outgoing feature has exactly one `divergence == 1`; all
   members of a failing group are reported: `"node without exactly one divergence ==
   1"`.
3. No `divergence == 0` on a node with more than one outgoing feature: `"divergence == 0
   at a divergence"`.
4. No `divergence` of 1 or 2 on a node with a single outgoing feature: `"divergence of 1
   or 2 where flow does not split"`.

With a `divergence_fraction` column, three more:

5. `divergence_fraction` is populated and in `[0, 1]`, compared exactly, without
   `fraction_tol`: `"divergence_fraction not in [0, 1]"`.
6. `divergence_fraction` sums to 1 within `fraction_tol` across each `fromnode` group,
   each `id` counted once: `"divergence_fraction does not sum to 1 at a node"`.
7. No `divergence == 2` feature takes the entire flow (`divergence_fraction >= 1`,
   compared exactly, without `fraction_tol`): `"divergence == 2 takes all flow"`.

Any number of `divergence == 2` features at one node is valid.

## Algorithm

1. R: if the input does not inherit `hy` or `hy_flownetwork`, call `hy()`. Python:
   `as_hy` without the repeated-pair raise (`id_toid_pair_repeated`); `check_hy_graph`
   lists those pairs in step 3 (D3-166). The `toid` requirement (`needs_topo`) is tested
   after step 2, as Preconditions state.
2. If `divergence_check`, stop with `divergence_check_missing_columns` when `fromnode` or
   `divergence` is absent; otherwise evaluate checks 1 to 7 of Definition per row, joining
   labels with `"; "` in check order. If any row has a label, return those rows with
   `divergence_issue`, in input order. The repeated-pair and cycle checks do not run.
3. Python (R in M6, D3-166): find the (`id`, `toid`) pairs held by more than one row. If
   any, return them as a table of `id` and `toid`, one row per distinct pair, in order of
   each pair's first row in the input (D3-209). The cycle check does not run.
4. Map identifiers to dense positions; keep the edges whose `toid` is an `id` (I-4).
   Build a CSR index in the `to` direction (K-2); this is below the kernel boundary
   (K-1, K-5): positions in, a component label per position out.
5. Compute strongly connected components (R: Tarjan in base R, iterative,
   `scc_labels()`; Python: `scipy.sparse.csgraph.connected_components(connection="strong")`,
   D-8).
6. Mark a row when its edge is a self-loop (`toid == id`), or when `id` and `toid` share a
   component of more than one position.
7. If no row is marked, return `TRUE`.
8. Number the components that hold a marked row from 1 by byte order (D-1) of each
   component's least identifier as decimal string (S-7). Set `reciprocal` to whether the
   component holds exactly two distinct identifiers. Return the marked rows, in input
   order, with `component` and `reciprocal` appended.

Tie rules: the component numbering in step 8 is the only order the function chooses, and
it depends on identifiers only, so the cycle result is unchanged by row order except for
the order of its rows, which follows the input (D-7). The repeated-pair table follows
each pair's first row in the input. There is no sentinel: a terminal has an empty
neighbor range (K-4). Row order of the result is input order (D-14).

`sort_network()` and `make_index_ids()` call `check_hy_graph(x)` with the defaults on
both routes, `hy_topo` and `hy_flownetwork`, and stop with `network_cycle` unless it
returns `TRUE` (D-8, D3-41); their descriptions carry that stop. A cycle in a
flownetwork, as in `loop2` and `flownetwork_cycle`, stops there. The `loop` fixture
holds braided reaches that rejoin downstream and no cycle (D3-150), so
`check_hy_graph_loop` returns `TRUE` and the `loop` cases of those functions return.

## Postconditions

For every input that does not stop:

- The result is `TRUE` or a table; it is `TRUE` exactly when no row is on a cycle,
  no (`id`, `toid`) pair is repeated, and, with `divergence_check`, no row has a
  divergence issue. The repeated-pair clause holds in Python from its first release and
  in R from M6 (D3-57, D3-166); until M6, R returns `TRUE` on a repeated pair on no
  cycle.
- A repeated (`id`, `toid`) pair does not raise here, unlike every other function,
  which raises at ingest (S-6, D3-57): Python returns the repeated-pair table, one row
  of `id` and `toid` per distinct repeated pair (A-10, D3-209); R does so in M6
  (D3-166).
- With `divergence_check`, a network with a divergence problem returns the divergence
  result only; its repeated pairs and cycles are not listed (D3-75).
- A network with a repeated pair returns the repeated-pair table only; its cycles are
  not listed (D3-209).
- Every returned row (cycle result) has `toid` equal to some `id`, and following `toid`
  from that `toid` returns to the row's `id`.
- In the cycle result, every row whose `id` and `toid` lie on a common cycle is
  returned; no other row is.
- Returned rows of the divergence and cycle results keep input order, and each equals
  an input row in its hydroloom columns; no input row is returned twice.
- In the cycle result, `component` takes every value in `1..k` for some `k >= 1`; two
  rows share a `component` exactly when their `id` values lie on a common cycle.
- Ordering cycles by their least identifier as decimal string, in byte order, gives
  `component` 1, 2, ..., k.
- `reciprocal` is `TRUE` exactly when the rows of that `component` hold two distinct
  `id` values; it is constant within a `component`.
- A self-loop row is returned with `reciprocal = FALSE`, in a `component` of its own
  unless another row of the same `id` lies on a longer cycle.
- Permuting input rows permutes the returned rows accordingly and leaves the set of
  (`id`, `toid`, `component`, `reciprocal`) unchanged (D-7).
- Adding rows that lie on no cycle does not change `component` or `reciprocal` of the
  rows returned.
- No result table carries `row` (D3-54, D3-160). The divergence result never carries
  `component` or `reciprocal`; the cycle result never carries `divergence_issue`; the
  repeated-pair table carries `id` and `toid` only.
- With `divergence_check`, raising `fraction_tol` never adds a row with the label
  `"divergence_fraction does not sum to 1 at a node"`.
- The input is not modified (A-8).

## Complexity

O(n + m) time and space: one pass to map identifiers, one CSR build (a sort on n rows,
O(n log n) in practice), one Tarjan pass. The divergence check is one grouped pass,
O(n).

## Edge cases

- One flowline: `TRUE`, unless its `toid` is its own `id`, which returns the row with
  `component = 1`, `reciprocal = FALSE`.
- No connections (all terminals): `TRUE`.
- Duplicated row (the same `id`/`toid` twice, a flownetwork): SPEC S-6 makes a repeated
  pair an error, raised at ingest by every other function (D3-57). `check_hy_graph` is
  the one function that does not raise on it: Python returns the pair as one row of the
  repeated-pair table from its first release, and R does so in M6 (D3-166, D3-209). Until M6, R returns `TRUE` when
  the edge is on no cycle and both copies, as cycle rows, when it is; the case
  `check_hy_graph_duplicated_edge` holds R's `TRUE` until the Python-only case is added
  when Python generates (M5).
- Node-topology input: stops with `needs_topo` in both languages (D3-45), cases
  `check_hy_graph_new_hope_node` and `check_hy_graph_network_node`; with
  `divergence_check`, only after the divergence check passes.
- A divergence whose minor branch is absent (an extract that keeps one branch, or a
  diverted path leaving the table): with `divergence_check`, the remaining branch's
  `divergence` of 1 or 2 is labeled `"divergence of 1 or 2 where flow does not split"`.
  The coding is correct in the full fabric and flagged in the extract; fixture
  `divergence_minor_absent`.
- Zero-row network: `TRUE` (I-11).
- Missing `toid`: filled on ingest (I-5), a terminal.
- Missing `divergence` with `divergence_check`: the row is labeled `"divergence not in
  {0, 1, 2}"`. Missing `fromnode`: the row is in no node group, and only checks 1, 5, and
  7 apply to it.
- A divergence problem beside a cycle, with `divergence_check`: the divergence rows alone
  are returned (D3-75); fixture `check_divergence_and_cycle`, cases
  `check_hy_graph_divergence_and_cycle_divergence_check` and, without the check,
  `check_hy_graph_divergence_and_cycle`.
- A flowline whose rows at its node disagree in `divergence` or `divergence_fraction`
  (a flownetwork coded inconsistently): not detected, and the result on such input is
  not contract (D3-213). A candidate check for later.
- Missing weight, zero-length line, empty name: not read.
- Identifiers that are character (`loop2`, braced GUIDs) or outside 32 bits: compared as
  text; numbering by byte order of the strings.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| `divergence_check = TRUE` without `divergence` | the divergence check reads which path is main (`divergence`), and the column is absent | all; the absent columns are listed | `add_divergence()`, given the coastal and inland outlets, for `divergence` | `divergence_check_missing_columns` |
| `divergence_check = TRUE` without `fromnode` | the divergence check groups flowlines by the point where they begin (`fromnode`), which the flowlines leaving a divergence share, and the column is absent | all; the absent columns are listed | `make_node_topology()` with the divergences given (`add_div`), which builds `fromnode` from `toid` so the flowlines leaving a divergence share it | `divergence_check_missing_columns` |
| a node-topology table (`fromnode`/`tonode`, no `toid`), both languages (D3-45) | the network has no `toid`, each flowline's downstream flowline | all | `add_toids(return_dendritic = FALSE)` | `needs_topo` |
| no recognized `toid` and no `fromnode`/`tonode` (source columns not in the vocabulary, as `ID`/`toNexID` in `sort_test`) | the network has neither `toid` nor the `fromnode`/`tonode` identifiers to build it from | all | add a `toid` column giving each flowline's downstream flowline; where the column exists under a source name, register it as `toid` (`hydroloom_names()` in R, `register_names()` in Python) | `needs_topo` |

Both keys are errors (`kind: error`): `needs_topo` raises `RepresentationError` in Python
and `hydroloom_representation_error` in R; `divergence_check_missing_columns` raises
`HydroloomError` in Python and a plain R error. The `divergence_check_missing_columns`
entry covers both of its rows above in one `condition`, and its `fix` gives
`add_divergence()`. Three `remedies.json` fields still read otherwise than this
description and wait for the M6 review (check_hy_graph H-03, D3-153): the `needs_topo`
`condition` says the functions convert a table with `fromnode`/`tonode` and that
`add_toids()` fixes the node-table case, its `message_python` says the network lacks
`fromnode`/`tonode` too, and `divergence_required.raised_by` lists `check_hy_graph`,
which `check_hy_graph` does not raise itself (it comes from the `add_toids()` call that
gives the table its `toid`).

A cycle or a divergence issue is a returned value, never a condition (A-10). The
`divergence_issue` labels are data values compared exactly, not `remedies.json` text.
The `loop_check` warnings and message of 1.3.0 went with the argument and have no key.

## References

- `R/check_hy_graph.R`: `check_hy_graph()`, `check_divergence_internal()`,
  `add_divergence_issue()`, `check_cycles_internal()`, `scc_labels()`.
- `R/sort_network.R` (`sort_network.hy_topo`, `sort_network.hy_flownetwork`) and
  `R/make_index_ids.R` (`make_index_ids.hy_topo`, `make_index_ids.hy_flownetwork`): the
  callers that stop unless the result is `TRUE`.
- `tests/testthat/test_check_hy_graph.R`.
- Tarjan, R. (1972), Depth-first search and linear graph algorithms, SIAM Journal on
  Computing 1(2), 146-160.
- NHDPlus Version 2 User Guide (McKay and others, 2012), `Divergence` attribute coding.
- USGS hydrofabric theme, Network section, and SPEC S-2: `divergence_fraction`, the
  fraction of flow to each branch, summing to one over `fromnode` (checks 5 to 7).
- SPEC D-1, D-7, D-8, D-14, I-4, I-5, I-6, I-11, I-12, A-2, A-3, A-8, A-9, A-10, K-1,
  K-2, K-4, K-5, K-6, S-7; DESIGN "SCC always in `check_hy_graph`".

## Worked example

Fixture `fixtures/three_cycle.gpkg`, aspatial, six rows:

| id | toid |
| --- | --- |
| 1 | 2 |
| 2 | 3 |
| 3 | 4 |
| 4 | 2 |
| 5 | 6 |
| 6 | 0 |

Flowlines 2, 3, and 4 form a cycle of three; 1 drains into it, and 5 to 6 is a separate
tree. It returns three rows:

| id | toid | component | reciprocal |
| --- | --- | --- | --- |
| 2 | 3 | 1 | FALSE |
| 3 | 4 | 1 | FALSE |
| 4 | 2 | 1 | FALSE |

On `fixtures/reciprocal_pair.gpkg` (5 to 6 and 6 to 5 beside a tree of 1 to 4 and 7 to
5) the result is rows 5 and 6, `component = 1`, `reciprocal = TRUE`, and flowline 7,
which drains into the pair, is not returned.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording hydroloom 1.3.0 behavior and the
  D-8 change (ROADMAP §4, M3): strongly connected components always run, `loop_check` is
  removed, and cycle rows carry `row`, `component`, and `reciprocal` in input order in
  place of today's `toid_check` join ordered by `toid`. Cases capturing today's cycle
  result and every `loop_check` case carry `changes_with: D-8`; the `loop_check` cases
  are dropped, not regenerated. A-16 has no row for this function; the signature follows
  R (A-9). A-10's "returns a table" is read as "returns problem rows, never raises on
  them"; `TRUE` on a clean network is kept in both languages. R does not convert a
  node-topology input as Python does (A-3).
- 2026-10-26, suite 0.1.0: review fixes. Duplicated edge stays `TRUE`, S-6 not applied
  (D3-57); node-input limits stated (D3-58); flownetwork `sort_network` route named as
  not calling the check (D3-55); Remedies rows split one per fix; a fixture for a
  divergence with its minor branch absent; `divergence_fraction` reference.
- 2026-10-26, suite 0.1.0: remedies pass. Preconditions give R's stop on a table with no
  `toid` as the D-8 text ("check_hy_graph requires toid"), raised as
  `hydroloom_representation_error`, in place of the dplyr error (D3-88). No behavior
  change.
- 2026-10-28, suite 0.1.0: M3 close. D3-54 (ROADMAP §4, D-8 row): the cycle result
  drops `row`; Signature, Algorithm step 7, Postconditions, and the worked example
  restated. D3-41 (ROADMAP §4, D-8 row): `sort_network()` and `make_index_ids()` call the
  check on the flownetwork route too, so `loop2` and `flownetwork_cycle` stop with
  `network_cycle` there. D3-45 supersedes D3-58: no conversion of a node table in
  either language, `needs_topo` in both, and the divergence check runs before the `toid`
  requirement so node-table cases share. D3-75: the "almost always" sentence replaced by
  what `reciprocal` marks. D3-47: A-16 now carries this signature. D3-146: Remedies
  conditions and fixes aligned with `remedies.json`.
- 2026-10-28, suite 0.1.0: unit-2 fixes H-01, H-02, H-05. H-01 (D3-57, D3-166): a
  repeated (`id`, `toid`) pair is listed in the result, Python from its first release
  and R in M6; Postconditions and Edge cases restated, the `duplicated_edge` case keeps
  R's `TRUE`. H-02: the node-table fix is `add_toids(return_dendritic = FALSE)` in
  Preconditions and Remedies. H-05: the `reciprocal` sentence in Definition.
- 2026-10-28, suite 0.1.0: M3 close-out run 2. D3-160 (ROADMAP §4, D-8 row; R change
  in this run): the divergence result drops `row` as the cycle result did (D3-54), so
  Signature item 2, Algorithm step 2, and Postconditions say neither result carries it.
  D3-159, D3-172: text dating behavior ("today", "after D-8") restated in present tense;
  the flownetwork call of D3-41 is stated as landed. The 1.3.0 behavior the earlier text
  carried is recorded here only: the cycle test was a reciprocal-pair join returning
  `toid_check` ordered by `toid`, which found two-flowline cycles and self-loops and
  missed longer cycles (`three_cycle` returned `TRUE`), and `loop_check = TRUE` ran a
  headwater walk that warned and, on `loop2`, reported three rows on no cycle. D3-146:
  Remedies states the key kinds and classes and lists the `remedies.json` fields left
  for M6 (H-03, D3-153). The quoted `needs_topo` R text in Preconditions is replaced by
  the key (A-10). References give `check_cycles_internal()`, `scc_labels()`, and the
  flownetwork callers.
- 2026-10-29 (M3 close run 3, reader round D3-201): H-01 in part (Algorithm step 1 for
  Python reads `as_hy` without the repeated-pair raise; Signature says the shape of
  repeated-pair rows is open, D3-209; the `component` Postcondition is qualified to rows
  on a cycle), H-02, H-03 (description part), H-04 (rows counted, from
  `check_divergence_internal()`), H-05, H-06 (exact comparison, from
  `check_divergence_internal()`), H-07, H-08 (the divergence table keeps the input's
  class, as R does).
- 2026-10-29 (M3 close run 4): D3-213 (R change in this run): the divergence check counts
  each `id` once per `fromnode`, in the counts of checks 2 to 4 and the sum of check 6, so
  a flownetwork with correct coding passes; case
  `check_hy_graph_divergence_attribute_flownetwork_divergence_check`. Both result tables
  are the class `hy()` gives the subset. Definition and Signature restated.
- 2026-10-30 (M3 close run 4): D3-209: a repeated (`id`, `toid`) pair returns its own
  table, `id` and `toid`, one row per distinct pair, checked after the divergence check
  and before the cycle check; Python from its first release, R in M6 (D3-166). Signature
  gives four return values in check order, Algorithm gains step 3, and Postconditions
  and Edge cases restated; the `component` Postcondition is stated for the cycle result.
