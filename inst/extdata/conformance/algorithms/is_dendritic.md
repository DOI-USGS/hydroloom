---
requires:
  pattern: [any]
  columns: []
  via: {}
---

# is_dendritic

## Signature

R: `is_dendritic(x, validate_data = TRUE)`. Python: `is_dendritic(network, *,
validate_data=True)` (ROADMAP §2; A-16 puts `network` first, A-9 keeps the R argument).

| argument | type | default |
| --- | --- | --- |
| `x` / `network` | a table; any representation or none | none |
| `validate_data` | `flag`: R `logical` length one, Python `bool` | `TRUE` / `True` |

Returns one value of vocabulary type `flag`: R `logical` of length one, never `NA`;
Python `bool`. The expectation is the S-7 scalar layer: one row, one column named
`value`.

Roxygen gives the argument and return prose: "Checks whether the network contains any
divergences"; `x` "data.frame or hy object to test"; `validate_data` "If `FALSE`, only
checks the `divergence` column (fast). If `TRUE` (default), also checks for duplicated
ids"; returns "logical. TRUE if the network is dendritic."

No kernel is involved (K-1, K-11).

## Preconditions

None. The function accepts any table, including a zero-row table and one in source
names, and raises nothing; `requires` is `[any]`. No representation check and no
conversion is applied. It reads three columns when present, by their hydroloom names
only: `divergence`, `toid`, and `id`. No alias lookup is done, so a table in source names
(`Divergence`, `tocomid`) is read as if those columns were absent; call `hy()` (Python
`as_hy`) first.

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
shared `fromnode` and that has no `divergence` column is `TRUE`. A table in source names
is `TRUE` whatever it holds, because none of the three names is present. The `dendritic`
attribute that R's `hy()` sets is not read.

## Algorithm

1. If `divergence` is a column and any non-missing value equals 2, return `FALSE`.
2. If `validate_data`, `toid` is a column, and any `id` value is a repeat of an earlier
   one, return `FALSE`. Missing `id` values count as equal to each other (R
   `duplicated()`, pandas `duplicated()`). When `id` is absent the test finds no repeat.
3. Return `TRUE`.

The divergence test is numeric equality with 2: R compares with `==`, so a character
`"2"` also matches. Values other than 0, 1, 2 (such as 3) do not make the result `FALSE`.

No tie rule or sentinel applies. The result does not depend on row order (D-7). No CSR
index is built.

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
  `divergence`, `toid`, and `id`.
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
  result is the same as for a divergence. `is_dendritic` reads no connections and does not
  raise; S-6 "raises" is not applied (D3-57).
- Minor path that leaves a divergence and returns to the main stem downstream
  (`return_divergence.gpkg`): `FALSE` for both values of `validate_data`, by the
  `divergence` 2 test.
- Repeated `id` without `toid` (node topology): `TRUE`; the repeat test needs `toid`.
- Missing `divergence` value: ignored. Missing `id` values: two or more count as a
  repeat.
- Zero-row table: `TRUE`.
- Missing weight, zero-length line, empty name: not read; `TRUE` on the S-6 fixtures.

## Remedies

none

The function raises and warns on nothing of its own. The base R error on
`validate_data = NA` is outside A-10.

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
