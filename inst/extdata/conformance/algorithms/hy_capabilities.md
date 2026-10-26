---
requires:
  pattern: [any]
  columns: []
  via: {}
---

# hy_capabilities

## Signature

R: `hy_capabilities(x, include_conversions = FALSE)`. Python:
`capabilities(network, *, include_conversions=False)` (ROADMAP §2; A-9 renames the first
argument and keeps R's second, see Changes).

| argument | type | default |
| --- | --- | --- |
| `x` / `network` | any object; a table in practice | none |
| `include_conversions` | logical / `bool` | `FALSE` / `False` |

Returns, after the A-4 change, a frame with one row per function in the code table, in
code-table order:

| column | type | meaning |
| --- | --- | --- |
| `function` | text (R `character`, Python `str`) | the function's shared name |
| `available` | flag (R `logical`, Python `bool`), never missing | callable on the input |
| `via` | text, never missing; `""` when empty | the one conversion call that makes the function callable |

Before the change (hydroloom 1.3.0) R returns a named logical vector with the same names
in the same order; its expectation layer is `name`, `value` (D3-13).

Roxygen gives the argument and return prose: "What operations are available for this
network?"; "Returns a named logical vector indicating which hydroloom functions can
operate on the input object given its current class and columns."; `x` "object to
query."; `include_conversions` "If TRUE, also marks functions reachable via a single
representation conversion." The `@returns` line changes with A-4.

No kernel is involved (K-1, K-11).

## Preconditions

None. The function accepts any object and never raises; `requires` is `[any]`. No
representation check and no conversion is applied to the input.

In R the representation is read from the class `hy()` stamped (`hy_topo`, `hy_leveled`,
`hy_node`, `hy_flownetwork`); a table that has not been through `hy()` has none, whatever
its column names. In Python the representation is `representation(network)` computed
from the columns (A-2), since no class rides on a frame (I-14).

## Definition

Each row reports whether one hydroloom function can run on the input as it stands, and
if not, which single representation conversion would let it run. The answer comes from
a code table, one entry per function, holding that function's `requires` (pattern,
columns, via; S-8). Neither package reads the manifest at runtime; a testthat test (R)
and `test_api.py` (Python) assert that each code-table entry whose function is listed in
the manifest equals the `requires` front matter of that function's description (A-4,
D3-33). Functions in the manifest with no code-table entry (`hy`, `hy_reverse`, and the
other name and class helpers) are not checked.

`available` tests the representation and the fixed columns in `columns`. It does not
test attributes a function reads through an argument (the name and weight of
`add_levelpaths`, the variable of `accumulate_downstream`), and it does not test the
data: a network with repeated edges or missing values is reported by its pattern alone.
A `TRUE` row says the function accepts the input's shape, not that the call succeeds.

The code table after the A-4 change, in row order. Each phase-1 entry is its
description's `requires` (D3-42); the other entries are hydroloom 1.3.0's predicates
until their descriptions land. In 1.3.0, `sort_network`, `add_topo_sort`, and
`check_hy_graph` are `hy_topo` only, so a flownetwork reports them unavailable there:

| function | pattern | columns | via |
| --- | --- | --- | --- |
| `add_toids` | hy_node | | hy_topo: `make_node_topology` |
| `make_node_topology` | hy_topo | | hy_node: `add_toids` |
| `to_flownetwork` | hy_leveled | `divergence` | |
| `sort_network` | hy_topo, hy_flownetwork | | hy_node: `add_toids` |
| `add_topo_sort` | hy_topo, hy_flownetwork | | hy_node: `add_toids` |
| `add_levelpaths` | hy_topo | | hy_node: `add_toids` |
| `add_pathlength` | hy_topo | | hy_node: `add_toids` |
| `add_streamorder` | hy_topo | | hy_node: `add_toids` |
| `accumulate_downstream` | hy_topo | | hy_node: `add_toids` |
| `check_hy_graph` | hy_topo, hy_flownetwork | | hy_node: `add_toids` |
| `navigate_hydro_network` | hy_topo | | hy_node: `add_toids` |
| `add_pfafstetter` | hy_leveled | | |
| `add_streamlevel` | hy_leveled | | |
| `add_divergence` | hy_node | | hy_topo: `make_node_topology` |
| `add_return_divergence` | hy_node | `divergence` | hy_topo: `make_node_topology` |
| `subset_network` | hy_node | | hy_topo: `make_node_topology` |
| `navigate_network_dfs` | hy_topo, hy_flownetwork | | hy_node: `add_toids` |
| `make_index_ids` | hy_topo, hy_flownetwork | | hy_node: `add_toids` |
| `make_attribute_topology` | any | `geom` | |
| `index_points_to_lines` | any | `geom` | |
| `get_confluences` | hy_topo, hy_node | | |
| `add_hydro_location_distance` | hy_topo, hy_node | `aggregate_id`, `length_km` | |
| `index_hydro_location_distance` | hy_topo, hy_node | `aggregate_id`, `length_km` | |
| `get_subpath` | hy_topo, hy_node | `aggregate_id`, `length_km` | |

The last three rows are the aggregate-flowpath group, gated in 1.3.0 by the
`has_aggregate_flowpath` predicate. `geom` stands for a geometry column: R tests that
the input is an `sf` object; Python tests for a geometry column (G-14).

For a function `f` and input with representation `r`:

- `f` is directly callable when `r` satisfies `f`'s pattern and every column in `f`'s
  `columns` is present. `hy_leveled` satisfies `hy_topo` (A-3). A pattern of `any` is
  satisfied by every input, including one with no representation.
- `via` is the entry of `f`'s via map for `r` (with the same subsumption) when `f` is not
  directly callable, and `""` otherwise or when the map has no entry for `r`. The via
  entry is reported without testing `f`'s columns.
- `available` is "directly callable" when `include_conversions` is `FALSE`, and
  "directly callable or `via` is not `""`" when it is `TRUE`.

## Algorithm

R, 1.3.0:

1. Read class flags `is_topo`, `is_leveled`, `is_node`, `is_fn` by `inherits()`, and
   column flags for `divergence`, the `sf` class, and `aggregate_id` with `length_km`.
2. Build the direct vector, one predicate per row of the code table.
3. If `include_conversions` is `FALSE`, return it.
4. If `is_node`, set the eleven rows whose via map has a `hy_node` entry to `TRUE`; if
   `is_topo` and not `is_node`, set the four rows whose via map has a `hy_topo` entry to
   `TRUE`. Return the vector.

After the A-4 change, R and Python: compute the representation once; for each code-table
entry in order, evaluate the three rules of Definition; return the frame. The R code
table is hand-written in `R/hy_classes.R`; the Python one is the `@requires` registry
the decorator fills (A-3).

Row order is the code-table order, never the input's row order, and the output does not
depend on input rows beyond the `id` uniqueness test inside `representation` (D-7,
D-14). No tie rule or sentinel applies. No CSR index is built.

## Postconditions

- The output has one row per code-table entry; `function` is unique and in code-table
  order (D-14).
- `available` and `via` are never missing.
- With `include_conversions = FALSE`, `via != ""` implies `available` is `FALSE`.
- With `include_conversions = TRUE`, `available` is the `FALSE`-call `available` or
  `via != ""`, row by row; `via` is the same for both calls.
- Every non-empty `via` is `add_toids` or `make_node_topology`.
- On an input with no representation, only rows whose pattern is `any` can be `TRUE`, and
  every `via` is `""`.
- On a `hy_flownetwork` input, `available` is `TRUE` for `sort_network`,
  `add_topo_sort`, `check_hy_graph`, `navigate_network_dfs`, and `make_index_ids`,
  `FALSE` for every other row not of pattern `any`, and every `via` is `""`. In 1.3.0
  only `navigate_network_dfs` and `make_index_ids` are `TRUE` (D3-42).
- For each code-table entry whose function is listed in the manifest, the entry equals
  that function's description `requires` (A-4, D3-33; the testthat test and
  `test_api.py`).
- The output is unchanged by any permutation of the input rows (D-7).
- The input is not modified (A-8).

## Complexity

O(f) time and space in the number of code-table entries, plus the cost of reading the
representation: O(1) in R (class test), O(n) in Python (`id` uniqueness).

## Edge cases

- One flowline: after `hy()` on `single_flowline.gpkg` (which has `toid` and node
  columns) the input is `hy_topo`, so the hy_topo rows are available and the node rows
  are not, though the table carries `fromnode`/`tonode`. When a table has both `toid`
  and node columns, `classify_hy` stamps `hy_topo`.
- No connections (all terminals): the representation alone decides; `toid` values are
  not read.
- Zero rows: after `hy()`, `hy_topo`; the same output as any `hy_topo` without node
  columns. The output's row count is the code table's, not the input's.
- Duplicated row (the 1 → 2 row of `duplicated_edge.gpkg` repeated): R classes the
  table `hy_flownetwork`; the call returns with the five flownetwork rows available (two
  in 1.3.0) and every `via` `""`. It does not raise. `available` reflects the
  flownetwork pattern only; the repeated row is a data error that S-6 makes the
  consuming functions raise on, which R does not yet do (D3-46, D3-57).
- Divergence (repeated `id` with distinct `toid`, `sort_network_dups.gpkg`,
  `loop2.gpkg`): `hy_flownetwork`, the same output as the duplicated row.
- Node table without a `divergence` column (`divergence_no_attribute.gpkg`):
  `add_divergence` is available and `add_return_divergence` is not, by its `columns`
  gate; with `include_conversions = TRUE` it stays unavailable, since its via map has no
  `hy_node` entry.
- Not passed through `hy()` (R): no representation; only `make_attribute_topology` and
  `index_points_to_lines` can be `TRUE`, and only on an `sf` input.
- Missing weight, zero-length line, empty name: not read.

## Remedies

none

## References

- `R/hy_classes.R`: `hy_capabilities()`, `classify_hy()`, `as_hy_node()`, and the
  `hy_topo`, `hy_leveled`, `hy_node`, `hy_flownetwork` documentation pages.
- `tests/testthat/test_hy_classes.R`: "hy_capabilities returns correct logical vector"
  and "hy_capabilities reports aggregate flowpath functions on node input".
- SPEC A-2, A-3, A-4, A-8, A-9, A-16, D-7, D-14, G-14, I-14, S-4, S-7, S-8; ROADMAP §2
  and §4; milestone decisions D3-01 to D3-03, D3-13, D3-33, D3-42, D3-46, and D3-57.

## Worked example

Fixture `fixtures/capabilities_leveled.gpkg`, aspatial, run after `hy()` (case
`prepare`), which stamps `hy_leveled`:

| id | toid | topo_sort | levelpath | levelpath_outlet_id | divergence |
| --- | --- | --- | --- | --- | --- |
| 1 | 2 | 3 | 1 | 3 | 0 |
| 2 | 3 | 2 | 1 | 3 | 0 |
| 3 | 0 | 1 | 1 | 3 | 0 |

With `include_conversions = FALSE`, fifteen rows are available: `make_node_topology`,
`to_flownetwork`, `sort_network`, `add_topo_sort`, `add_levelpaths`, `add_pathlength`,
`add_streamorder`, `accumulate_downstream`, `check_hy_graph`, `navigate_hydro_network`,
`add_pfafstetter`, `add_streamlevel`, `navigate_network_dfs`, `make_index_ids`,
`get_confluences`. The other nine are not. Of those nine, `add_toids`, `add_divergence`,
`add_return_divergence`, and `subset_network` have `via = "make_node_topology"`; the
geometry and aggregate-flowpath rows have `via = ""`. With `include_conversions = TRUE`
those four become available too, nineteen in all.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording hydroloom 1.3.0 behavior (a
  named logical vector) and stating the ROADMAP §4 change A-4: `hy_capabilities()`
  returns the (`function`, `available`, `via`) frame from a hand-written code table,
  with a testthat test asserting each entry for a manifest function equals that
  function's `requires`. Cases capturing the vector carry `changes_with: A-4`; the A-4
  step regenerates their layers in the frame shape. Gap with SPEC A-4 and A-16, which
  give `hy_capabilities(x)` and `capabilities(network)`: R has `include_conversions =
  FALSE`, which A-9 keeps on both sides.
- 2026-10-26, suite 0.1.0: review round. The code table takes the phase-1 entries from
  their descriptions, so `sort_network`, `add_topo_sort`, and `check_hy_graph` accept
  `hy_flownetwork` after A-4 (D3-42); Definition states that `available` tests shape,
  not data or argument-named attributes; edge cases for a divergence and for a node
  table without `divergence`.
