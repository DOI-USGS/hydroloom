---
requires:
  pattern: [any]
  columns: []
  via: {}
---

# hy_network_type

## Signature

R: `hy_network_type(x)`. Python: `representation(network)` (ROADMAP §2, SPEC A-2, A-16;
A-9 renames the first argument).

| argument | type | default |
| --- | --- | --- |
| `x` / `network` | any object; a table in practice | none |

Returns one value of vocabulary type `text`: R `character` of length one, never `NA`;
Python `str`. The expectation is the S-7 scalar layer: one row, one column named `value`.

The value is one of `"hy_leveled"`, `"hy_topo"`, `"hy_node"`, `"hy_flownetwork"`, or
`"hy"` in both languages. R has a sixth value, `"not a hydroloom object"`, for an object
that carries none of the five classes; Python never returns it (see Definition and
Changes).

Roxygen gives the argument and return prose: "What representation pattern does this
network use?"; `x` "object to query."; returns "character. Most specific hydroloom class
name, or `"not a hydroloom object"` if x has no hydroloom class."

No kernel is involved (K-1, K-11).

## Preconditions

R: none. The function accepts any object, including `NULL`, a list, and a zero-row
table, reads only `class(x)`, and never raises.

Python: none on representation; `requires` is `[any]`. `representation` aligns the
column names and types `id` as `as_hy` does (Algorithm step 1), and it checks only what
those two steps read. Name alignment raises on an alias beside its hy name
(`name_alias_conflict`), on three or more aliases of one hy name
(`name_alias_ambiguous`), on two column names equal ignoring case
(`column_name_duplicate`), and on an empty column name (`column_name_empty`), and warns
on two aliases of one hy name (`name_alias_duplicate`). Typing `id` raises when `id`
holds both text and numbers (`identifier_mixed_type`, I-3), a value with a fractional
part (`identifier_not_whole`), an infinite value (`identifier_not_finite`), or a missing
value (`id_missing`, B-1). A table with no `id` column raises `RepresentationError`
(`no_id_column`, D3-130), where R's `hy()` warns and the R value is `"hy"`.
`representation` does not validate edges. A repeated (`id`, `toid`) pair, a `toid`,
`fromnode`, or `tonode` value with a fractional part, an infinite value, or mixed types,
broken `upmain`/`downmain` invariants, and a flag column not coded 0/1 neither raise nor
warn here, though `as_hy` raises or warns on each (`hy.md` Preconditions).

## Definition

Roxygen: returns the most specific hydroloom class. The class names map to database /
graph representation patterns: `"hy_leveled"`, enriched edge list (+ topo_sort,
levelpath); `"hy_topo"`, self-referencing edge list (id/toid, dendritic); `"hy_node"`,
fromnode/tonode graph (id/fromnode/tonode); `"hy_flownetwork"`, junction table
(id/toid/upmain/downmain); `"hy"`, name-aligned base object.

Two words in that list read narrower than the rule. "Dendritic" for `"hy_topo"` means each
flowline has at most one downstream flowline, one row per `id`; a table built with
`add_topo` from a source divergence attribute keeps its minor-path flowlines as rows whose
upstream connection was dropped, so it is `"hy_topo"` while `is_dendritic()` returns
`FALSE` (case `hy_network_type_new_hope_add_topo`). For `"hy_flownetwork"`, `upmain` and
`downmain` are optional: repeated `id` with `toid` is the whole pattern, and cases `loop`,
`duplicated_edge`, and `divergence_add_topo` carry neither column.

R: the value is the first of the five class names that appears in `class(x)`, in
class-vector order, or `"not a hydroloom object"` when none appears. Every hydroloom
constructor prepends its class, so the first one found is the most specific. The value
reports the class the object carries, which `hy()` set by the classification rule of
`hy.md` Algorithm step 9; the columns are not read again. A table read from a file
carries no hydroloom class, whatever its column names, so its value is
`"not a hydroloom object"`.

Python keeps no class on a frame (I-14), so `representation(network)` applies the same
classification rule to the frame on demand (A-2): the value equals R's
`hy_network_type(hy(x))` on the same table. With the columns named and the identifiers
typed as `as_hy` would leave them, first match wins:

1. `id` and `toid` present and `id` unique: `"hy_leveled"` when `topo_sort`,
   `levelpath`, and `levelpath_outlet_id` are all present, else `"hy_topo"`;
2. `id` and `toid` present and `id` repeated: `"hy_flownetwork"`. A repeated `id` with
   `toid` is a flowline listed once per downstream flowline, as at a divergence;
3. `id`, `fromnode`, and `tonode` present and `id` unique: `"hy_node"`;
4. otherwise: `"hy"`.

`toid` outranks the node columns: a table with `id`, `toid`, `fromnode`, and `tonode`
is `"hy_topo"`. Values other than `id` are not read; a missing `toid` does not change
the pattern.

The pattern does not say whether the rows are flowlines or catchments. A catchment table,
with flowpaths 1:1 with catchments, and a flowline table with the same columns get the same
value; the caller knows which graph it holds.

## Algorithm

R:

1. Take `found`, the elements of `class(x)` that are among `hy_leveled`, `hy_topo`,
   `hy_node`, `hy_flownetwork`, `hy`, in `class(x)` order.
2. If `found` is empty, return `"not a hydroloom object"`.
3. Return `found[1]`.

Python:

1. Align the column names as `as_hy` does (`align_names`, `hy.md` Algorithm step 2,
   with its tie rules) and type `id` as `as_hy` does (I-2).
2. Apply the four-branch rule of Definition, first match wins. `id` uniqueness is
   tested on the typed values; a zero-row table has unique `id`.

The tie rules that bind are the order of the four branches (`toid` before the node
columns; leveled only within unique-`id` topo) and, in R, the first-in-class-vector rule.
No sentinel (K-4) is read. The output is one value that depends only on the column set
and whether `id` repeats, not on row order (D-7; case `hy_network_type_new_hope_shuffled`
holds `new_hope` with its rows permuted and gives `"hy_node"`, as
`hy_network_type_new_hope` does), and on nothing random (D-15). No CSR
index is built.

## Postconditions

- The result is a single non-missing string, one of `"hy_leveled"`, `"hy_topo"`,
  `"hy_node"`, `"hy_flownetwork"`, `"hy"`, or, in R only, `"not a hydroloom object"`.
- The result is unchanged by any permutation of the input rows (D-7).
- R: `hy_network_type(hy(x))` is never `"not a hydroloom object"` for a table `x`.
- R: `hy_network_type(hy(x))` equals Python `representation(x)` for every table `x` that
  `hy()` accepts without the `no_id_column` warning.
- `"hy_leveled"` only if `id`, `toid`, `topo_sort`, `levelpath`, and
  `levelpath_outlet_id` are present and `id` is unique (R: after `hy()`).
- `"hy_flownetwork"` if and only if `id` and `toid` are present and `id` repeats (R:
  after `hy()`).
- `"hy_node"` only if `toid` is absent and `id`, `fromnode`, `tonode` are present with
  unique `id` (R: after `hy()`).
- Python: `representation(as_hy(network)) == representation(network)` for every
  `network` that `as_hy` accepts.
- Python: `representation(network)` does not depend on `toid`, `fromnode`, or `tonode`
  values, missing values included.
- The input is not modified (A-8).

## Complexity

R: O(1) time and space; only the class vector is read. Python: O(c·a) for name
alignment over c columns and a aliases, plus O(n) time and space for the `id`
uniqueness test.

## Edge cases

- One flowline: with `id`, `toid`, `fromnode`, `tonode` (fixture `single_flowline`),
  `"hy_topo"` after `hy()` and in Python; R `"not a hydroloom object"` on the table as
  read.
- No connections (all terminals): `"hy_topo"`; every `toid` is an outlet, which the rule
  does not read.
- Duplicated row (repeated `id` with `toid`, S-6 duplicated edge): `"hy_flownetwork"`;
  the function does not raise on it in R or Python, though Python's `as_hy` raises on the
  same table (`id_toid_pair_repeated`). The value does not tell a divergence from an
  exactly repeated row; both are a repeated `id` with `toid`.
- A `fromnode`, `tonode`, or `toid` value with a fractional part: not read, so Python
  returns the pattern; R's `hy()` in `prepare` stops (`identifier_not_whole`).
- A missing `id` value: Python raises (`id_missing`), giving the row positions (I-12).
  R's `hy()` keeps the missing value, and two of them count as a repeated `id`, so a
  table with `toid` is `"hy_flownetwork"`.
- Divergence with and without a `divergence` attribute, through `add_topo` (R only):
  `divergence_attribute` gives `"hy_topo"`, since the attribute drops the minor paths;
  `divergence_no_attribute` gives `"hy_flownetwork"`, since both downstream rows stay.
- Repeated `id` with `fromnode` and `tonode` and no `toid`: `"hy"`, since neither the
  flownetwork nor the node branch applies.
- No `id`: R `"hy"` after `hy()`, which warns (`no_id_column`); Python raises
  `RepresentationError` (D3-130). Case `hy_network_type_no_id` is R only: it applies
  `hy()` with the warning suppressed in `prepare` and stores `"hy"`; the Python raise has
  no case yet.
- `id` with neither `toid` nor both node columns: `"hy"`.
- Zero rows with `id` and `toid`: `"hy_topo"`.
- Two of the three level-path columns: `"hy_topo"`.
- Not a table (`NULL`, a list): R `"not a hydroloom object"`.
- Missing weight, zero-length line, empty name: not read; the value follows the column
  rule alone.
- R, an object whose class was changed after `hy()`: the value follows the class. An
  internal downcast to node topology (`as_hy_node()`) on a table holding both `toid` and
  the node columns reports `"hy_node"`; Python reports `"hy_topo"` for the same columns.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| alias beside its hy name (Python) | an input column renames to a hy name that another column already has, so two columns would carry one name | none; the columns are named in the message | drop or rename one of the two columns | `name_alias_conflict` |
| two aliases for one hy name (Python, warning) | two input columns are aliases of one hy name; the one later in the alias list took the name and the other kept its input name | none; the columns are named in the message | drop the column that should not carry the hy name | `name_alias_duplicate` |
| three or more aliases for one hy name (Python) | three or more input columns are aliases of one hy name, and none can be chosen to carry it | none; the columns are named in the message | drop all but one of the aliasing columns | `name_alias_ambiguous` |
| two column names equal ignoring case (Python) | names are compared lowercased, so two names equal ignoring case collide | none; the columns are named in the message | rename one column so the lowercased names differ | `column_name_duplicate` |
| empty column name (Python) | a column has no name | none; the column position is in the message | give the column a name | `column_name_empty` |
| no `id` column (Python error, D3-130) | no input column is `id` or one of its aliases, so the catchments or flowlines have no identifier | all | rename the identifier column to `id` | `no_id_column` |
| `id` column of mixed types (Python, I-3) | the `id` column holds both strings and numbers | the rows whose type differs from the first value's | cast the column to one type before the call | `identifier_mixed_type` |
| non-whole floating-point `id` (R `hy()` and Python, D3-07) | the `id` column arrives as floating-point numbers and a value has a fractional part, so it cannot be read as a whole-number identifier without changing the value | the rows whose value has a fractional part | correct the values, or cast the column to text, before the call | `identifier_not_whole` |
| infinite `id` (R `hy()` and Python, I2-3; key drafted, D3-180) | the `id` column arrives as floating-point numbers and a value is `Inf` or `-Inf`, which is neither a whole number nor text that names the same identifier | the rows whose value is infinite | correct the values before the call | `identifier_not_finite` |
| missing `id` value (Python, B-1; key drafted) | a row has no `id`, so the flowline or catchment on it cannot be identified | the row positions whose `id` is missing | give those rows an `id`, or drop them, before the call | `id_missing` |

R's `hy_network_type` raises and warns on nothing; in R the `no_id_column` warning and the
`identifier_not_whole` and `identifier_not_finite` raises come from `hy()` in `prepare`. Python's rows are the conditions `as_hy` raises or warns on in name
alignment and in typing `id` (`hy.md` Remedies); the keys are `hy`'s. The
conditions `as_hy` checks beyond those (`id_toid_pair_repeated`,
`flownetwork_multiple_downmain`, `flownetwork_multiple_upmain`, `flag_value_not_binary`,
and the identifier conditions on columns other than `id`) are not checked here.

## References

- `R/hy_classes.R`: `hy_network_type()`; `classify_hy()`, `new_hy_topo()`,
  `new_hy_leveled()`, `new_hy_node()`, `new_hy_flownetwork()` for the class each
  pattern carries; `as_hy_node()` for the downcast.
- `R/hy.R`: `hy()`, which stamps the class.
- `inst/extdata/conformance/algorithms/hy.md`, Algorithm step 9 (the classification
  rule).
- `tests/testthat/test_hy_classes.R`, "hy_network_type returns correct type".
- SPEC A-2, A-8, A-9, A-10, A-16, I-2, I-3, I-6, I-14, S-7, D-7, D-15; AGENTS.md "S3
  Class Hierarchy".
- USGS hydrofabric theme, Network: the primary and secondary upstream/downstream
  connections the four patterns encode.
- McKay and others (2012), NHDPlus Version 2: User Guide: the PlusFlow table (one row
  per connection, as in `"hy_flownetwork"`) and the FromNode/ToNode attributes (as in
  `"hy_node"`).

## Worked example

Fixture `fixtures/single_flowline.gpkg`, one flowline in EPSG:5070:

| id | toid | fromnode | tonode | name | divergence | length_km | da_sqkm | total_da_sqkm |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | 0 | 1 | 2 | A | 0 | 1 | 1 | 1 |

`hy_network_type(hy(x))` is `"hy_topo"`: `id` and `toid` are present and `id` is
unique, and `toid` outranks `fromnode`/`tonode`. `representation(network)` is
`"hy_topo"`. Expectation layer `hy_network_type_single_flowline` holds one row,
`value = "hy_topo"`. On the table as read, without `hy()`, R gives
`"not a hydroloom object"`.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording hydroloom 1.3.0 behavior. Gap
  with SPEC A-16 and A-2, which give "one of the four pattern names": R also returns
  `"hy"` and `"not a hydroloom object"`; A-9 keeps R's values. Python returns `"hy"` for
  a frame that matches no pattern and never `"not a hydroloom object"`, since it
  computes the pattern from columns after the `as_hy` alignment rather than reading a
  class. No ROADMAP §4 change alters the value: I-2 changes identifier types, not the
  column set or `id` repetition, and the A-10 condition class applies to the
  representation errors whose text quotes this value, not to this function.
- 2026-10-26, suite 0.1.0: hydrology review round. Definition says what "dendritic"
  and the optional `upmain`/`downmain` mean for the value, glosses a repeated `id` as a
  divergence, and states that the value does not tell flowlines from catchments; the
  divergence attribute pair and the 3DHP and NHDPlusHR cases are added; the
  `name_alias_ambiguous` row is added (D3-60). Behavior is unchanged.
- 2026-10-27, suite 0.1.0, M3 close: D3-28's plan edit gives `"hy"` in SPEC A-2 and the
  A-16 table, which closes the gap the 2026-10-25 line records for that value; the sixth
  R value stays R-only. D3-07 (ROADMAP §4 identifier-ingest row, I-2): a non-whole
  floating-point identifier raises `identifier_not_whole` in `as_hy`, and so in
  `representation` (Remedies). D3-130: Python raises `RepresentationError` on a table with
  no `id` column, where R's `hy()` warns and gives `"hy"` (Preconditions, Postconditions,
  Edge cases, Remedies). D3-146: Remedies texts aligned with `remedies.json`. H-08: the
  NHDPlusV2 PlusFlow table is cited.
- 2026-10-28, suite 0.1.0, M3 close unit 2: H-01, H-02 applied. Remedies says the R
  warning and raise come from `hy()` in `prepare`, not from `hy_network_type`, and the
  `identifier_not_whole` row covers R `hy()` too; the `identifier_mixed_type` fix reads
  "before the call". Behavior is unchanged.
- 2026-10-28, suite 0.1.0, M3 close run 2 sweep: the I-2 clause of the ROADMAP §4
  identifier-ingest row has landed in R `hy()`: an infinite identifier raises
  `identifier_not_finite` (I2-3, key drafted under D3-180), in `prepare` in R and in
  `representation` in Python (Remedies row added); `hy_network_type` itself is unchanged.
  Cases `hy_network_type_new_hope_shuffled` (D-7) and `hy_network_type_no_id` (H-03) are
  named where the description states their rule. Remedies drops its dated wording
  (D3-159, D3-172). H-05: under D3-147, R's `no_id_column` becomes an error in M6, with
  the ROADMAP §4 row "Error text from the per-function `remedies` table" (A-10); R's
  `hy()` then raises on a table with no `id`, as Python does, case
  `hy_network_type_no_id` becomes an error case, and the Postconditions exclusion of
  tables on which `hy()` warns `no_id_column` falls away.
- 2026-11-03, suite 0.1.0, M5 run 2 unit 2, REVIEW_11022026 item 3 (SP-03, SP-04; P-1):
  Preconditions are the section corrected. Python's `representation` aligns names and
  types `id`, as Algorithm step 1 reads, and validates no edge: a repeated (`id`, `toid`)
  pair, a non-whole or mixed-type `toid`, `fromnode`, or `tonode`, the
  `upmain`/`downmain` invariants, and a flag not coded 0/1 do not raise or warn here.
  Remedies lists the name-alignment rows `column_name_duplicate` and `column_name_empty`,
  narrows the identifier rows to `id`, and states which `as_hy` conditions are not
  checked. Postconditions qualify `representation(as_hy(network))` by what `as_hy`
  accepts; Edge cases add the non-whole node value. REVIEW_11022026 item 4 (SP-07,
  SP-S3; B-1): a missing `id` value raises `id_missing` (Preconditions, Edge cases,
  Remedies; key agent-drafted for the M6 read).
