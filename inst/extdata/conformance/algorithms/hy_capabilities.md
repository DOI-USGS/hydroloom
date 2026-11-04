---
requires:
  pattern: [any]
  columns: []
  via: {}
---

# hy_capabilities

## Signature

R: `hy_capabilities(x, include_conversions = FALSE)`. Python: `capabilities(network)`
(ROADMAP §2; A-4 and A-16). Python takes no second argument, since `via` carries what
`include_conversions` reports: `available` is directly callable and `via` gives the one
conversion (D3-31, see Changes).

| argument | type | default |
| --- | --- | --- |
| `x` / `network` | any object; a table in practice | none |
| `include_conversions` | logical; R only | `FALSE` |

Returns a frame with one row per function in the code table, in code-table order (A-4):

| column | type | meaning |
| --- | --- | --- |
| `function` | text (R `character`, Python `str`) | the function's shared name |
| `available` | flag (R `logical`, Python `bool`), never missing | callable on the input |
| `via` | text, never missing; `""` when empty | the one conversion call that makes the function callable |

R returns a `data.frame`; its expectation layer has the three columns above.

Roxygen gives the argument and return prose: "What operations are available for this
network?"; "Returns a data.frame indicating which hydroloom functions can operate on the
input object given its current class and columns."; `x` "object to query.";
`include_conversions` "If TRUE, also marks functions reachable via a single
representation conversion." The `@returns` text gives the three columns.

No kernel is involved (K-1, K-11).

## Preconditions

None. The function accepts any object and never raises; `requires` is `[any]`. No
representation check and no conversion is applied to the input.

In R the representation is read from the class `hy()` stamped (`hy_topo`, `hy_leveled`,
`hy_node`, `hy_flownetwork`); a table that has not been through `hy()` has none, whatever
its column names. In Python the representation is `representation(network)` computed
from the columns (A-2), since no class rides on a frame (I-14). `representation` raises
`no_id_column` on a table with no `id` column (D3-162); `capabilities` does not raise
there but treats the table as having no representation, so only rows whose pattern is
`any` can be available and every `via` is `""` (D3-167). On a raw table holding a
repeated (`id`, `toid`) pair, which `as_hy` raises on (`id_toid_pair_repeated`, D3-57),
`capabilities` does not raise either: the repeated `id` makes the table a flownetwork,
and it returns the `hy_flownetwork` answer (D3-196).

## Definition

Each row reports whether one hydroloom function can run on the input as it stands, and
if not, which single representation conversion would let it run. The answer comes from
a code table, one entry per function, holding that function's `requires` (pattern,
columns, via; S-8). Neither package reads the manifest at runtime; a testthat test (R,
`test-conformance-capabilities.R`) and `test_api.py` (Python) assert that each code-table entry whose function is listed in
the manifest equals the `requires` front matter of that function's description (A-4,
D3-33). Functions in the manifest with no code-table entry (`hy`, `hy_reverse`, and the
other name and class helpers) are not checked.

`available` tests the representation and the fixed columns in `columns`. It does not
test attributes a function reads through an argument (the name and weight of
`add_levelpaths`, the variable of `accumulate_downstream`), and it does not test the
data: a network with repeated edges or missing values is reported by its pattern alone.
A `TRUE` row says the function accepts the input's shape, not that the call succeeds.
A `FALSE` row does not say the call fails: a `hy_topo` that keeps `fromnode`/`tonode`
reports `add_divergence`, `add_return_divergence`, and `subset_network` unavailable
though they run through the node downcast (D3-32), and the 1.3.0 entries for
`add_levelpaths`, `add_streamorder`, and `accumulate_downstream` report a flownetwork
unavailable though R runs them; those entries are corrected as their descriptions land
in M6 (D3-70).

The code table, in row order; in R it is `hy_requires` in `R/hy_classes.R`. Each
phase-1 entry is its description's `requires` (D3-42); the other entries keep hydroloom
1.3.0's predicates until their descriptions land in M6 (D3-70):

| function | pattern | columns | via |
| --- | --- | --- | --- |
| `add_toids` | hy_node | | |
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

The last three rows are the aggregate-flowpath group, which reads `aggregate_id` and
`length_km`. `geom` stands for a geometry column: R tests that
the input is an `sf` object; Python tests for a geometry column (G-14).

For a function `f` and input with representation `r`:

- `f` is directly callable when `r` satisfies `f`'s pattern and every column in `f`'s
  `columns` is present. `hy_leveled` satisfies `hy_topo` (A-3). A pattern of `any` is
  satisfied by every input, including one with no representation.
- `via` is the entry of `f`'s via map for `r` (with the same subsumption) when `f` is not
  directly callable, and `""` otherwise or when the map has no entry for `r`. The via
  entry is reported without testing `f`'s columns.
- `available` is "directly callable" when `include_conversions` is `FALSE`, and
  "directly callable or `via` is not `""`" when it is `TRUE`. Python's `available` is
  always "directly callable", the R `FALSE` call.

## Algorithm

R and Python, for each code-table entry in order:

1. Test the pattern against the input's representation. R reads it by `inherits()` on
   the class `hy()` stamped (D3-32); Python computes `representation(network)` once, and
   a table with no `id` column has none (D3-167).
2. Test each column in `columns`; `geom` is the `sf` class in R and a geometry column in
   Python. The entry is directly callable when both tests pass.
3. Take `via` from the via map key the representation satisfies, `""` when no key does,
   and set it to `""` where the entry is directly callable. Each via map has at most one
   key.
4. In R, `available` is the direct result, or the direct result or `via != ""` when
   `include_conversions` is `TRUE`. In Python it is the direct result.

R returns `data.frame(function, available, via)`. The R code table is hand-written in
`R/hy_classes.R`; the Python one is the `@requires` registry the decorator fills (A-3).

Row order is the code-table order of A-4, never the input's row order, and the output
does not depend on input rows beyond the `id` uniqueness test inside `representation`
(D-7). Case `hy_capabilities_new_hope_node_shuffled` holds the D-7 pair of
`hy_capabilities_new_hope_node` and stores the same frame. No tie rule or sentinel
applies. No CSR index is built.

## Postconditions

- The output has one row per code-table entry; `function` is unique and in code-table
  order (A-4).
- `available` and `via` are never missing.
- With `include_conversions = FALSE` in R, and always in Python, `via != ""` implies
  `available` is `FALSE`.
- With `include_conversions = TRUE` (R only), `available` is the `FALSE`-call
  `available` or `via != ""`, row by row; `via` is the same for both calls.
- Every non-empty `via` is `add_toids` or `make_node_topology`.
- On an input with no representation, only rows whose pattern is `any` can be `TRUE`, and
  every `via` is `""`.
- On a `hy_flownetwork` input, `available` is `TRUE` for `sort_network`,
  `add_topo_sort`, `check_hy_graph`, `navigate_network_dfs`, and `make_index_ids`,
  `FALSE` for every other row not of pattern `any`, and every `via` is `""` (D3-42).
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
  columns. The output's row count is the code table's, not the input's. Case
  `hy_capabilities_empty_network_no_conversions` is the call Python can run;
  `hy_capabilities_empty_network` passes `include_conversions = TRUE` and is R only.
- Duplicated row (the 1 → 2 row of `duplicated_edge.gpkg` repeated): R classes the
  table `hy_flownetwork`; the call returns with the five flownetwork rows available and
  every `via` `""`. It does not raise. `available` reflects the flownetwork pattern only.
  The repeated (`id`, `toid`) pair is a data error that raises at ingest in `hy()` and
  `as_hy` under D3-57, in Python from its first release and in R from M6 (D3-46);
  Python `capabilities` returns the same flownetwork answer and does not raise (D3-196).
  Case `hy_capabilities_duplicated_edge` captures R's return.
- Divergence (repeated `id` with distinct `toid`, `loop2.gpkg`): `hy_flownetwork`, the
  same output as the duplicated row.
- Real NextGen table (`sort_network_dups_distinct.gpkg`, which is
  `sort_network_dups.gpkg` with its three repeated (`id`, `toid`) pairs removed): every
  `id` is unique, so `hy()` stamps `hy_topo`. Twelve rows are available, and
  `add_divergence`, `add_return_divergence`, and `subset_network` have
  `via = "make_node_topology"`; with `include_conversions = TRUE` (R only) fifteen are
  available. `add_toids` has `via = ""`, since the table has `toid`. Cases
  `hy_capabilities_sort_network_dups` and
  `hy_capabilities_sort_network_dups_conversions` (D3-57, D3-210).
- Node table without a `divergence` column (`divergence_no_attribute.gpkg`):
  `add_divergence` is available and `add_return_divergence` is not, by its `columns`
  gate; with `include_conversions = TRUE` (R only) it stays unavailable, since its via
  map has no `hy_node` entry.
- Not passed through `hy()` (R): no representation; only `make_attribute_topology` and
  `index_points_to_lines` can be `TRUE`, and only on an `sf` input.
- Missing weight, zero-length line, empty name: not read.

## Remedies

none. Python `capabilities` does not raise `no_id_column` on a table with no `id`
column (D3-167), nor `id_toid_pair_repeated` on a repeated (`id`, `toid`) pair (D3-196).

## References

- `R/hy_classes.R`: `hy_capabilities()`, `classify_hy()`, `as_hy_node()`, and the
  `hy_topo`, `hy_leveled`, `hy_node`, `hy_flownetwork` documentation pages.
- `tests/testthat/test_hy_classes.R`: "hy_capabilities reports aggregate flowpath
  functions on node input"; `tests/testthat/test-conformance-capabilities.R`:
  "hy_capabilities code table equals the manifest requires".
- SPEC A-2, A-3, A-4, A-8, A-9, A-16, D-7, G-14, I-14, K-1, K-11, S-4, S-7, S-8;
  ROADMAP §2 and §4; milestone decisions D3-01 to D3-03, D3-13, D3-14, D3-31, D3-32,
  D3-33, D3-42, D3-46, D3-57, D3-70, D3-89, D3-162, D3-167, D3-177, D3-196, and D3-210.

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
`get_confluences`. The other nine are not. Of those nine, `add_divergence`,
`add_return_divergence`, and `subset_network` have `via = "make_node_topology"`; `add_toids`
and the geometry and aggregate-flowpath rows have `via = ""`. Python reports the same
fifteen, with `via = "make_node_topology"` on the three node rows. With
`include_conversions = TRUE` (R only) those three become available too, eighteen in all.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording hydroloom 1.3.0 behavior (a
  named logical vector) and stating the ROADMAP §4 change A-4: `hy_capabilities()`
  returns the (`function`, `available`, `via`) frame from a hand-written code table,
  with a testthat test asserting each entry for a manifest function equals that
  function's `requires`. Each case's manifest comment names `changes_with: A-4`; the
  annotation itself is stripped under D3-14, and the A-4 step regenerates the layers in
  the frame shape. Gap with SPEC A-4 and A-16, which
  give `hy_capabilities(x)` and `capabilities(network)`: R has `include_conversions =
  FALSE`, which A-9 keeps on both sides.
- 2026-10-26, suite 0.1.0: review round. The code table takes the phase-1 entries from
  their descriptions, so `sort_network`, `add_topo_sort`, and `check_hy_graph` accept
  `hy_flownetwork` after A-4 (D3-42); Definition states that `available` tests shape,
  not data or argument-named attributes; edge cases for a divergence and for a node
  table without `divergence`.
- 2026-10-26, suite 0.1.0: A-4 landed (`c215e3a`). Hydroloom 1.3.0 returned a named
  logical vector with the same names in the same order (expectation layer `name`,
  `value`, D3-13), built from class and column flags, and with `include_conversions =
  TRUE` set the rows with a `hy_node` via entry on a node input and those with a
  `hy_topo` via entry on a topo input. It reported `sort_network`, `add_topo_sort`, and
  `check_hy_graph` unavailable on a flownetwork. R now walks the `hy_requires` code
  table and returns the frame; all layers were regenerated in the frame shape.
- 2026-10-28, suite 0.1.0: per D3-31 and D3-89, Python `capabilities(network)` takes no
  `include_conversions` argument, as SPEC A-4 and A-16 give it; R keeps the argument.
  The cases that pass `include_conversions = TRUE` stay R only. This closes the A-4 and
  A-16 gap the first entry recorded; the argument now differs between the languages,
  which A-9 does not exempt (D3-31).
- 2026-10-28, suite 0.1.0: M3 close unit 2, H2-01 to H2-03. Python `capabilities` never
  raises; a table with no `id` column has no representation (D3-167). The A-9 gap is
  named in the entry above; the `include_conversions = TRUE` statements in the worked
  example and edge cases are marked R only.
- 2026-10-28, suite 0.1.0: M3 close-out sweep (D3-159, D3-172). Signature, Definition,
  Algorithm, and Postconditions state the A-4 frame in present tense; the 1.3.0 vector
  and steps move to the A-4 entry above. Roxygen quotations follow the current
  `@description`. D-14 is no longer cited, since row order is the code table's (A-4),
  not the input's (decision 7a, D3-177). Algorithm names the D-7 case
  `hy_capabilities_new_hope_node_shuffled`; Edge cases name the zero-row cases
  (H2-04) and the D3-57 ingest raise on a repeated pair. References drop the removed
  testthat name and add `test-conformance-capabilities.R`. No R change in this run
  touches the function.
- 2026-10-29, suite 0.1.0, M3 close run 3, D3-196: on a raw table with a repeated
  (`id`, `toid`) pair, Python `capabilities` returns the `hy_flownetwork` answer and does
  not raise (Preconditions, Edge cases, Remedies).
- 2026-10-29 (M3 close run 3, reader round D3-201): H-02, H-05, H-06, H-07.
- 2026-10-30, suite 0.1.0, M3 close run 4, D3-215: the two `sort_network_dups` cases
  read `sort_network_dups_distinct.gpkg`, which has unique `id` and is `hy_topo`; the
  Divergence edge case keeps `loop2.gpkg` only, and a new edge case states the
  `hy_topo` answer on the distinct NextGen table.
- 2026-11-03, suite 0.1.0, M5 run 4 unit 2 (REVIEW_11032026-run3 item 2, RULES P-1): the
  `add_toids` code-table entry drops its `hy_topo: make_node_topology` via, since
  `add_toids` stops a table that has `toid` (`toid_already_present`). Definition, the
  NextGen edge case and the worked example now give three node rows with
  `via = "make_node_topology"` on a topo input; the regenerated layers are named in the
  M5 Log entry for the unit. No binding section changes.
