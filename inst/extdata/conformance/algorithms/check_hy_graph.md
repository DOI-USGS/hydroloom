---
requires:
  pattern: [hy_topo, hy_flownetwork]
  columns: []
  via:
    hy_node: add_toids
---

# check_hy_graph

## Signature

R today: `check_hy_graph(x, loop_check = FALSE, divergence_check = FALSE, fraction_tol =
1e-6)`. After the D-8 change (ROADMAP §4, M3): `check_hy_graph(x, divergence_check =
FALSE, fraction_tol = 1e-6)`; `loop_check` is removed, so a call that passes it stops
with R's "unused argument" error. Python (ROADMAP §2, A-9): `check_hy_graph(network, *,
divergence_check=False, fraction_tol=1e-6)`, `network` in R's place of `x`. A-16 lists
no row for this function; the signature follows R.

| argument | type | default |
| --- | --- | --- |
| `x` / `network` | table (data.frame, tibble, or sf; DataFrame or GeoDataFrame) | required |
| `divergence_check` | `flag`: R `logical` length one, Python `bool` | `FALSE` / `False` |
| `fraction_tol` | `float64` scalar, non-negative | `1e-6` |
| `loop_check` (removed by D-8) | `flag` | `FALSE` |

Roxygen gives the argument prose: `x` "data.frame network compatible with
hydroloom_names"; `divergence_check` "if TRUE, `divergence` and, where present,
`divergence_fraction` are checked for internal consistency as described in details";
`fraction_tol` "numeric tolerance used when checking that `divergence_fraction` sums to
1 across a `fromnode` group". The `loop_check` text ("the entire network is walked from
top to bottom searching for loops") is deleted with the argument.

Returns one of three values. The function never raises on a problem it finds; it
returns the problem rows (A-10).

1. No problem found: `TRUE` (R `logical` length one; Python `True`). The expectation is
   the S-7 scalar layer, one row, `value = TRUE`.
2. A divergence coding problem (only with `divergence_check`): the problem rows of the
   network in hydroloom names, in input row order, with two columns appended (D-14):

   | column | type | meaning |
   | --- | --- | --- |
   | `row` | count, R `integer`, Python `int32` | 1-based position of the row in the input |
   | `divergence_issue` | text | the failed checks, joined by `"; "` in check order |

3. A cycle (always checked after D-8): the rows on a cycle, in input row order, with
   three columns appended (D-14):

   | column | type | meaning |
   | --- | --- | --- |
   | `row` | count, R `integer`, Python `int32` | 1-based position of the row in the input |
   | `component` | count, R `integer`, Python `int32` | which cycle the row belongs to, numbered from 1 |
   | `reciprocal` | `flag` | `TRUE` when the cycle has exactly two flowlines |

   Geometry is kept when the input has it (A-8). The returned table is the R class `hy()`
   gives the subset (`hy_topo` or `hy_flownetwork`), not the input's source names.

Today's cycle result (the reciprocal-pair join) differs: a tibble with `toid` first,
then `id` and the other hydroloom columns, `row`, and `toid_check` (the downstream
flowline's `toid`), ordered by `toid`; any column other than `id` and `toid` appears
twice with `.x` and `.y` suffixes. `loop_check = TRUE` today returns the rows of the
`hy` object whose `id` the walk reported, without `row`.

No named kernel is involved (K-6). The strongly connected components walk lives in
`_kernels/walk.py` (SPEC module table) and in an R base Tarjan kernel; its shape is the
author's.

## Preconditions

The network needs `id` and `toid` in hydroloom names. R calls `hy()` on input that does
not inherit `hy` or `hy_flownetwork`, so source names resolve through the vocabulary
aliases. Repeated `id` is accepted: `hy()` routes it to `hy_flownetwork` (I-6) and every
row is an edge of the graph. Missing `toid` becomes the reserved outlet value on ingest
(I-5); a terminal is any row whose `toid` is not an `id` (I-4).

A node-topology table (`fromnode`/`tonode`, no `toid`, A-2 `hy_node`): Python converts it
with `add_toids()` before the check (A-3 `convert`, the `via` entry above), which keeps
only main paths at divergences. R does not convert; today it stops inside dplyr ("Can't
rename columns that don't exist. Column `toid` doesn't exist."). A table with neither
`toid` nor `fromnode`/`tonode` stops in both languages; the hydroloom condition, once the
A-10 change lands, is `needs_topo`.

`divergence_check = TRUE` reads `fromnode` and `divergence`, and `divergence_fraction`
when that column is present. A missing `fromnode` or `divergence` column stops with
`divergence_check_missing_columns`, naming the absent columns. The check runs before
cycle detection, so the stop comes first whatever the graph holds.

## Definition

A cycle is a set of flowlines in which water leaving any one of them can, by following
`toid`, return to it. In graph terms these are the strongly connected components of the
`id` to `toid` graph that hold more than one flowline, plus any flowline whose `toid` is
its own `id` (a self-loop). A dendritic or divergent river network has none: flow never
returns upstream. A cycle almost always means a `toid` that points the wrong way, and a
cycle of two flowlines (`reciprocal`) is the common case, one flowline digitized against
the flow direction.

After D-8 the result is `TRUE` exactly when the network has no cycle (and, with
`divergence_check`, no divergence coding problem). Otherwise it lists every row whose
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
grouped by shared `fromnode`; a feature with a missing `fromnode` belongs to no group.
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

5. `divergence_fraction` is populated and in `[0, 1]`: `"divergence_fraction not in [0,
   1]"`.
6. `divergence_fraction` sums to 1 within `fraction_tol` across each `fromnode` group:
   `"divergence_fraction does not sum to 1 at a node"`.
7. No `divergence == 2` feature takes the entire flow: `"divergence == 2 takes all
   flow"`.

Any number of `divergence == 2` features at one node is valid.

Today, without `loop_check`, the cycle test is the reciprocal-pair join: a row is
returned when the `toid` of its downstream flowline equals its own `id`. That finds every
two-flowline cycle and every self-loop, and misses longer cycles (the `three_cycle`
fixture returns `TRUE`). `loop_check = TRUE` runs a walk from the headwaters that warns
"stuck in a loop at" and "found a loop at" with an identifier and returns the rows of
the reported identifiers; it builds its index with `make_index_ids()`, which itself runs
the pairwise test and stops on a reciprocal pair or a self-loop before the walk starts,
and on the `loop2` fixture it reports nine rows, three of them on no cycle.

## Algorithm

1. R: if the input does not inherit `hy` or `hy_flownetwork`, call `hy()`. Python:
   `as_hy`, then the representation route (A-3).
2. If `divergence_check`, stop with `divergence_check_missing_columns` when `fromnode` or
   `divergence` is absent; otherwise evaluate checks 1 to 7 of Definition per row, joining
   labels with `"; "` in check order. If any row has a label, return those rows with
   `row` and `divergence_issue`, in input order. The cycle check does not run.
3. Map identifiers to dense positions; keep the edges whose `toid` is an `id` (I-4).
   Build a CSR index in the `to` direction (K-2); this is below the kernel boundary
   (K-1, K-5): positions in, a component label per position out.
4. Compute strongly connected components (R: Tarjan in base R, iterative; Python:
   `scipy.sparse.csgraph.connected_components(connection="strong")`, D-8).
5. Mark a row when its edge is a self-loop (`toid == id`), or when `id` and `toid` share a
   component of more than one position.
6. If no row is marked, return `TRUE`.
7. Number the components that hold a marked row from 1 by byte order (D-1) of each
   component's least identifier as decimal string (S-7). Set `reciprocal` to whether the
   component holds exactly two distinct identifiers. Return the marked rows, in input
   order, with `row`, `component`, `reciprocal` appended.

Tie rules: the component numbering in step 7 is the only order the function chooses, and
it depends on identifiers only, so the result is unchanged by row order except for `row`
(D-7). There is no sentinel: a terminal has an empty neighbor range (K-4). Row order of
the result is input order (D-14).

Today (until D-8) steps 3 to 7 are replaced by: a left join of each row's `toid` onto
`id`, carrying the downstream row's `toid` as `toid_check`, keeping rows with `id ==
toid_check`, ordered by `toid` (data.table `merge`). `loop_check` adds the walk before
the join.

`sort_network()` and `make_index_ids()` on `hy_topo` call `check_hy_graph(x)` with the
defaults and stop unless it returns `TRUE`. After D-8 that stop covers every cycle; today
a longer cycle passes and `sort_network()` drops its rows with the warning "some
features missed in sort". Their descriptions carry that stop.

## Postconditions

After D-8, for every input that does not stop:

- The result is `TRUE` or a table; it is `TRUE` exactly when no row is on a cycle and,
  with `divergence_check`, no row has a divergence issue.
- Every returned row (cycle result) has `toid` equal to some `id`, and following `toid`
  from that `toid` returns to the row's `id`.
- Every row whose `id` and `toid` lie on a common cycle is returned; no other row is.
- `row` values are distinct, ascending, in `1..n`, and the returned rows equal the input
  rows at those positions in their hydroloom columns.
- `component` takes every value in `1..k` for some `k >= 1`; two rows share a
  `component` exactly when their `id` values lie on a common cycle.
- Ordering cycles by their least identifier as decimal string, in byte order, gives
  `component` 1, 2, ..., k.
- `reciprocal` is `TRUE` exactly when the rows of that `component` hold two distinct
  `id` values; it is constant within a `component`.
- A self-loop row is returned with `reciprocal = FALSE`, in a `component` of its own
  unless another row of the same `id` lies on a longer cycle.
- Permuting input rows permutes `row` accordingly and leaves the set of (`id`, `toid`,
  `component`, `reciprocal`) unchanged (D-7).
- Adding rows that lie on no cycle does not change `component` or `reciprocal` of the
  rows returned.
- The divergence result never carries `component` or `reciprocal`; the cycle result
  never carries `divergence_issue`.
- With `divergence_check`, raising `fraction_tol` never adds a row with the label
  `"divergence_fraction does not sum to 1 at a node"`.
- The input is not modified (A-8).

## Complexity

O(n + m) time and space: one pass to map identifiers, one CSR build (a sort on n rows,
O(n log n) in practice), one Tarjan pass. The divergence check is one grouped pass,
O(n). Today's pairwise join is O(n log n); the `loop_check` walk it replaces is
O(n · m) in the worst case, from a depth-first search at each node.

## Edge cases

- One flowline: `TRUE`, unless its `toid` is its own `id`, which returns the row with
  `component = 1`, `reciprocal = FALSE`.
- No connections (all terminals): `TRUE`.
- Duplicated row (the same `id`/`toid` twice, a flownetwork): `TRUE` when the edge is on
  no cycle; both copies are returned when it is.
- Zero-row network: `TRUE` (I-11).
- Missing `toid`: filled on ingest (I-5), a terminal.
- Missing `divergence` with `divergence_check`: the row is labeled `"divergence not in
  {0, 1, 2}"`. Missing `fromnode`: the row is in no node group, and only checks 1, 5, and
  7 apply to it.
- Missing weight, zero-length line, empty name: not read.
- Identifiers that are character (`loop2`, braced GUIDs) or outside 32 bits: compared as
  text; numbering by byte order of the strings.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| `divergence_check = TRUE` without `fromnode` or `divergence` | the divergence check cannot group features or read their coding | all; the absent columns are named | `add_divergence()` on a node-topology network, or call without `divergence_check` | `divergence_check_missing_columns` |
| no `toid` and no `fromnode`/`tonode` (R: no `toid`) | the network has no downstream connections to walk | all | `add_toids()` on a network with `fromnode`/`tonode` | `needs_topo` |

A cycle or a divergence issue is a returned value, never a condition (A-10). The
`divergence_issue` labels are data values compared exactly, not `remedies.json` text.
Today's `loop_check` warnings ("stuck in a loop at", "found a loop at") and the message
"found loop at" are removed with the argument and get no key.

## References

- `R/check_hy_graph.R`: `check_hy_graph()`, `check_divergence_internal()`,
  `add_divergence_issue()`, `check_hy_graph_internal()`, `loop_search_dfs()`.
- `R/sort_network.R` (`sort_network.hy_topo`) and `R/make_index_ids.R`
  (`make_index_ids.hy_topo`): the callers that stop unless the result is `TRUE`.
- `tests/testthat/test_check_hy_graph.R`.
- Tarjan, R. (1972), Depth-first search and linear graph algorithms, SIAM Journal on
  Computing 1(2), 146-160.
- NHDPlus Version 2 User Guide (McKay and others, 2012), `Divergence` attribute coding.
- SPEC D-1, D-7, D-8, D-14, I-4, I-5, I-6, I-11, A-2, A-3, A-8, A-9, A-10, K-1, K-2,
  K-4, K-5, K-6, S-7; DESIGN "SCC always in `check_hy_graph`".

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
tree. Today `check_hy_graph(x)` returns `TRUE`: no flowline's downstream neighbor points
straight back. After D-8 it returns three rows:

| id | toid | row | component | reciprocal |
| --- | --- | --- | --- | --- |
| 2 | 3 | 2 | 1 | FALSE |
| 3 | 4 | 3 | 1 | FALSE |
| 4 | 2 | 4 | 1 | FALSE |

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
