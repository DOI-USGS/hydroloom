---
requires:
  pattern: [any]
  columns: []
  via: {}
---

# is_dendritic

## Signature

R: `is_dendritic(x, validate_data = TRUE)`. Python: `is_dendritic(network, *,
validate_data=True)` (SPEC A-16; A-9 keeps the R argument).

| argument | type | default |
| --- | --- | --- |
| `x` / `network` | a table; any representation or none | none |
| `validate_data` | `flag`: R `logical` length one, Python `bool` | `TRUE` / `True` |

Returns one value of vocabulary type `flag`: R `logical` of length one, never `NA`;
Python `bool`. The expectation is the S-7 scalar layer: one row, one column named
`value`, compared `exact`. In R no case raises or warns, so every case stores a layer.

Roxygen gives the argument and return prose: "Checks whether the network contains any
divergences"; `x` "data.frame or hy object to test"; `validate_data` "If `FALSE`, only
checks the `divergence` column (fast). If `TRUE` (default), also checks for duplicated
ids"; returns "logical. TRUE if the network is dendritic."

No kernel is involved (K-1, K-11).

## Preconditions

None. The function accepts any table, including a zero-row table and one in source
names; `requires` is `[any]`. R raises nothing. Python aligns names and types `id` as
`as_hy` does and raises what `as_hy` raises (D3-29), including the S-6 repeated (`id`,
`toid`) pair (D3-57). No representation check and no
conversion is applied. It reads three columns when present, by their hydroloom names:
`divergence`, `toid`, and `id`. R does no alias lookup, so in R a table in source names
(`Divergence`, `tocomid`) is read as if those columns were absent; call `hy()` first.
Python aligns names first, as `representation` does (D3-29, D3-34), so a table in source
names is read under the hydroloom names `as_hy` would give its columns (D3-197).

`validate_data` must be a single `TRUE` or `FALSE`. R stops with the base error
"missing value where TRUE/FALSE needed" on `NA`, and only when the divergence test has
not already returned; Python requires `bool`. Neither is a hydroloom condition.

## Definition

The result is `FALSE` when either holds, and `TRUE` otherwise:

1. a `divergence` column is present and holds at least one value equal to 2 (a minor
   path leaving a divergence, NHDPlus coding: 0 none, 1 main, 2 minor); missing values
   are ignored;
2. `validate_data` is true, a `toid` column is present, and `id` repeats (the
   flownetwork pattern, I-6, where a flowline with two downstream connections has two
   rows).

The answer is about what the table records, not about the river. A node-topology table
(`fromnode`/`tonode`, unique `id`, A-2 `hy_node`) whose divergences are encoded only by a
shared `fromnode` and that has no `divergence` column is `TRUE`. The converse also
holds: an NHDPlusV2 table after `hy()` is `hy_topo`, with unique `id` and a `toid` graph
that is a tree, and it is `FALSE` because its minor-path flowlines keep `divergence` 2
(case `is_dendritic_new_hope`). In R a table in source names is `TRUE` whatever it holds, because none of the
three names is present; Python aligns the names first and gives the answer for the
aligned table (D3-197). The `dendritic` attribute that R's `hy()` sets is not read.

## Algorithm

1. If `divergence` is a column and any non-missing value equals 2, return `FALSE`.
2. If `validate_data`, `toid` is a column, and any `id` value is a repeat of an earlier
   one, return `FALSE`. When `id` is absent the test finds no repeat. How missing `id`
   values compare here, and that step 1 ignores a missing `divergence`, are captured
   behavior stated in Edge cases, not tie rules (reader round H-05).
3. Return `TRUE`.

No tie rule or sentinel applies. The result does not depend on row order (D-7); case
`is_dendritic_new_hope_shuffled` holds the same value as `is_dendritic_new_hope` on the
permuted rows of `new_hope_shuffled.gpkg`. No CSR index is built.

## Postconditions

- The result is a single non-missing logical (R) or `bool` (Python).
- The result is unchanged by any permutation of the input rows (D-7).
- `is_dendritic(x, validate_data = TRUE)` implies `is_dendritic(x, validate_data =
  FALSE)`: setting `validate_data` false can only turn `FALSE` into `TRUE`.
- If `divergence` is present and contains 2, the result is `FALSE` for both values of
  `validate_data`.
- If `divergence` is absent or holds no 2, the result with `validate_data = FALSE` is
  `TRUE`.
- With `validate_data` true, a table with `toid` is `FALSE` whenever its representation
  is `hy_flownetwork` (A-2).
- The result is unchanged by adding, removing, or changing any column other than
  `divergence`, `toid`, and `id`; in Python, any column other than those that name
  alignment maps to them.
- The input is not modified (A-8).

## Complexity

O(n) time: one pass over `divergence` and one hash pass over `id`. O(n) space for the
repeat test, O(1) otherwise.

## Edge cases

- One flowline: `TRUE`, unless its `divergence` is 2.
- No connections (all terminals): `TRUE`; `toid` values are not read.
- Divergence in flownetwork form (a repeated `id` with distinct `toid` values, I-6): `FALSE`
  with `validate_data` true; `TRUE` with it false when there is no `divergence` 2.
- Duplicated edge (the same `id`/`toid` pair listed twice, S-6 `duplicated_edge.gpkg`): a
  data error, not a divergence, but the repeat test cannot tell the two apart, so the
  result is the same as for a divergence: `FALSE` with `validate_data` true
  (`is_dendritic_duplicated_edge`), `TRUE` with it false
  (`is_dendritic_duplicated_edge_no_validate`). These values are R's. S-6 places the raise
  on a repeated (`id`, `toid`) pair in the ingest step, `hy()` (Python `as_hy`). Python
  raises at alignment from its first release (D3-57); R returns the values above until
  the M6 row lands. The R raise, with `is_dendritic` no longer reading such a repeat as a
  divergence, is the ROADMAP §4 row for M6 (D3-57). The NextGen table
  `sort_network_dups.gpkg` repeats three (`id`, `toid`) pairs and no other `id`, so under
  D3-57 the fixture is the defect: the shared cases `is_dendritic_sort_network_dups` and
  `is_dendritic_sort_network_dups_no_validate` read its distinct rows,
  `sort_network_dups_distinct.gpkg`, whose `id` is unique, and both are `TRUE` (D3-210).
  The Python raise case on `sort_network_dups.gpkg` is `hy_sort_network_dups_pair_repeated`
  (M5 run 4 unit 4).
- Divergence shown only by a shared `fromnode` in a node table with no `divergence`
  column (`divergence_no_attribute.gpkg`, case `is_dendritic_divergence_no_attribute`):
  `TRUE`, as the Definition says. Detecting it from the repeated `fromnode` lands with
  `add_divergence` in M6 (D3-34).
- 3DHP `divergence` (`new_hope_3dhp.gpkg`, case `is_dendritic_new_hope_3dhp`): read as
  NHDPlus 0/1/2 coding without `hy()`. That coding is to be confirmed before M5; if it
  differs, the 3DHP alias carries its own values map (S-2) and `hy()` converts on ingest
  (D3-69).
- Minor path that leaves a divergence and returns to the main stem downstream
  (`return_divergence.gpkg`): `FALSE` for both values of `validate_data`, by the
  `divergence` 2 test.
- Repeated `id` without `toid` (node topology): `TRUE`; the repeat test needs `toid`.
- Missing `divergence` value: ignored. Missing `id` values: two or more count as a
  repeat (R `duplicated()`; pandas `duplicated()` matches). Both are captured, not
  contract, with no case (REVIEW_10282026 rule 4 as amended: tolerated, not advertised).
- `divergence` stored as text, or coded outside 0, 1, 2: R compares with `==` against 2,
  so text `"2"` reads as minor and 3 is ignored. This is captured R behavior, not
  contract; no case.
- Zero-row table: `TRUE`.
- Missing weight, zero-length line, empty name: not read; `TRUE` on the S-6 fixtures.

## Remedies

none

The function raises and warns on nothing of its own. In Python the keys `as_hy` raises
at alignment are inherited from `as_hy`. The base R error on `validate_data = NA` is
outside A-10.

## References

- `R/hy_classes.R`: `is_dendritic()`; `classify_hy()` for the repeated-`id` rule; the
  `print` method that calls `is_dendritic(x, validate_data = FALSE)`.
- `R/hy.R`: `hy()`, which sets the `dendritic` attribute by the same divergence test.
- `tests/testthat/test_hy_classes.R`: "is_dendritic works correctly", "is_dendritic
  detects duplicated id with toid".
- NHDPlus Version 2 User Guide (McKay and others, 2012), `Divergence` attribute coding.
- SPEC A-2, A-8, A-9, A-16, D-7, I-6, S-6, S-7.

## Worked example

A four-flowline table in flownetwork form, where flowline 1 divides and both branches
reach flowline 4:

| id | toid |
| --- | --- |
| 1 | 2 |
| 1 | 3 |
| 2 | 4 |
| 3 | 4 |
| 4 | 0 |

`is_dendritic(x)` is `FALSE`: `toid` is present and `id` 1 repeats, so flowline 1 has two
downstream connections. `is_dendritic(x, validate_data = FALSE)` is `TRUE`: the repeat
test is skipped and there is no `divergence` column to say which branch is minor.

With a `divergence` column the second form also finds it. Fixture
`fixtures/divergence_attribute.gpkg` codes the minor path leaving its divergence as 2, so
`is_dendritic(x)` and `is_dendritic(x, validate_data = FALSE)` are both `FALSE`;
expectation layer `is_dendritic_divergence_attribute` holds one row, `value = FALSE`.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording hydroloom 1.3.0 behavior. SPEC
  A-16 does not list `is_dendritic`; the signature follows R (A-9) with `network` first.
  No ROADMAP §4 change alters the result. The I-2 identifier ingest change alters the
  type of `id` after `hy()` but not whether it repeats, so cases that run `hy()` first
  keep their values; `is_dendritic_nhdplushr` pins this on identifiers outside 32-bit
  range.
- 2026-10-26, suite 0.1.0: review round. Worked example moved from the duplicated edge
  to a divergence; divergence and duplicated edge separated in Edge cases, with the S-6
  departure stated (D3-57); return-divergence and NHDPlusHR cases added.
- 2026-10-27, suite 0.1.0: M3 close. SPEC A-16 now lists `is_dendritic` with the
  signature above (D3-35); Signature cites it. No binding change.
- 2026-10-28, suite 0.1.0: M3 close-out sweep. No R change in this run touches
  `is_dendritic`. D-7 case `is_dendritic_new_hope_shuffled` added (D3-175, D3-177).
  Edge cases give the ROADMAP §4 M6 row for the duplicated edge (D3-57), the deferred
  node-table detection (D3-34), and the unconfirmed 3DHP coding (D3-69). No binding
  change.
- 2026-10-29, suite 0.1.0, M3 close run 3, D3-197: Python `is_dendritic` aligns names
  first (D3-34, D3-29); R does no alias lookup (Preconditions, Definition,
  Postconditions). Case
  `is_dendritic_new_hope_source_names`, which passes source names, is R-only. Open for
  M6 with D3-34: whether R `is_dendritic` aligns names too; no R change now.
- 2026-10-29 (M3 close run 3, reader round D3-201): H-01, H-03, H-04.
- 2026-10-30, suite 0.1.0, M3 close run 4, D3-210: Python `is_dendritic` raises what
  `as_hy` raises, as before. Cases `is_dendritic_sort_network_dups` and
  `is_dendritic_sort_network_dups_no_validate` read `sort_network_dups_distinct.gpkg` and
  are shared; the first moves from `FALSE` to `TRUE`. The Python raise case on
  `sort_network_dups.gpkg` lands at M5 (duplicated-edge Edge case). No binding change.
- 2026-11-03, suite 0.1.0, M5 run 4 unit 4 (RULES P-2): the duplicated-edge Edge case
  names the Python raise case on `sort_network_dups.gpkg`,
  `hy_sort_network_dups_pair_repeated`, which has landed under `hy`. No binding change.
