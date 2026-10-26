---
requires:
  pattern: [any]
  columns: []
  via: {}
---

# hy

## Signature

R: `hy(x, clean = FALSE, add_topo = FALSE)`.

| argument | type | default |
| --- | --- | --- |
| `x` | table (data.frame, tibble, or sf) | required |
| `clean` | logical | `FALSE` |
| `add_topo` | logical | `FALSE` |

Python (ROADMAP §2, SPEC A-5, A-16): `as_hy(network, *, id_col=None, toid_col=None)`, returning a DataFrame, or a GeoDataFrame when the input is one. `network` is R's `x` (A-9). `clean` and `add_topo` are deliberate omissions (A-5). `id_col` and `toid_col` are Python-only keyword arguments, appended (A-9): each gives the input column to take as `id` or `toid`, ahead of the alias lookup; `None` leaves the lookup to decide. The private pair `_as_hy(network) -> (frame, names)` and `_hy_reverse(out, names)` (A-17) carry the rename map as a local value, never as frame state (I-14).

Return: the input table with these columns, in input order unless stated:

| column | type | when |
| --- | --- | --- |
| every input column whose lowercased name is a vocabulary term or alias | the hy name; the value's vocabulary type, identifiers per I-2 | always |
| every other input column | its input name and type, unchanged | unless `clean = TRUE`, which drops it |
| geometry | sf geometry, last; R names it `geom`, Python keeps the input name (G-14) | sf or GeoDataFrame input, unless `clean = TRUE` (R drops it) |
| `toid` | identifier, same type as `id` (I-2) | added second by `add_topo = TRUE` when the input has `fromnode` and `tonode` but no `toid` |

R also returns the class and attributes: class `hy` with one subclass from the classification rule in Algorithm step 9, or `hy_flownetwork` in place of `hy`; attribute `orig_names` (named character vector, input name to hy name); attribute `dendritic` (logical). Python returns no class and no attribute; `representation(network)` (A-2) computes the same pattern name on demand.

Identifier columns are the vocabulary terms of type `id`: `id`, `toid`, `fromnode`, `tonode`, `wbid`, `terminal_id`, `levelpath`, `up_levelpath`, `dn_levelpath`, `levelpath_outlet_id`, `vector_proc_unit`, `raster_proc_unit`, `aggregate_id`, `point_id`, `hydro_location_id`. After this function, each is R `integer` or `character` and Python `str` (I-1, I-2; post-change, see Changes).

No kernel; nothing here runs below the K-1 boundary.

## Preconditions

None on representation: `requires.pattern` is `any`. The function runs on any table, including one with no `id` column, and is the step every other function's representation check starts from. It raises when two input columns would take one hy name and one of them already is that name (`name_alias_conflict`), when three or more columns alias one hy name after the `permanent_identifier` rule (`name_alias_ambiguous`), when two column names are equal ignoring case (`column_name_duplicate`) or a column name is empty (`column_name_empty`), all from `align_names` (`align_names.md`), and when an input with `upmain` and `downmain` breaks the flownetwork main-path invariants (`flownetwork_multiple_downmain`, `flownetwork_multiple_upmain`). Python raises `TypeError` for an identifier column holding mixed types (I-3, `identifier_mixed_type`).

## Definition

Roxygen: converts a compatible dataset into a fabric s3 class. Automatically detects the network representation and classifies the result as the most specific hydroloom subclass (e.g. `hy_topo`, `hy_node`). With `add_topo = TRUE`, also auto-builds topology (e.g. toid from fromnode/tonode).

As a property of the input: every column that the vocabulary (or a name registered with `hydroloom_names()`) recognizes carries its hy name, every identifier column carries one of the two identifier types decided from its values, every missing `toid` carries the reserved outlet value for that type, and the row set and values are the input's. The representation the output belongs to is a function of which of `id`, `toid`, `fromnode`, `tonode`, `topo_sort`, `levelpath`, `levelpath_outlet_id` are present and whether `id` repeats.

## Algorithm

1. If the input is sf, set the geometry aside and work on the attribute table.
2. Align names (`align_names`; `align_names.md` Algorithm steps 2 to 7 hold here unchanged). Lowercase every column name. Look each up in the alias map: the vocabulary's `aliases` in vocabulary term order and, within a term, alias order, then names registered with `hydroloom_names()` in registration order. Tie rules, binding:
   - If an alias column and a column already holding its hy name are both present (`comid` beside `id`), stop (`name_alias_conflict`).
   - If any hy name has two or more candidate columns and `permanent_identifier` is a candidate, `permanent_identifier` drops out and keeps its input name, with a message in R. This holds whether or not `id` is the hy name with several candidates: `permanent_identifier` beside `tocomid` and `to_permanent_identifier` keeps its name rather than becoming `id`.
   - If a hy name still has exactly two candidates, the one later in alias-map order takes it, with a warning (`name_alias_duplicate`); the other keeps its input name. Column order does not enter. For `id` the map order is `id3dhp`, `comid`, `nhdplusid`, `featureid`, `permanent_identifier`, `from_permanent_identifier`, `reconciled_id`, `aggregated_id`, so `featureid` wins over `nhdplusid`.
   - If a hy name still has three or more candidates, stop (`name_alias_ambiguous`); R warns once per extra candidate first. The one exception is the same alias registered twice: both entries rename one column, so R warns and renames.
   - A column whose lowercased name is neither a hy name nor an alias gets its input name back, case preserved. A column whose lowercased name is a hy name keeps the lowercase form (`ID` becomes `id`).
   - On sf input the geometry is already set aside (step 1), so the two sf stops `align_names.md` records (`sf_geometry_name_case`, `sf_mixed_case_names`) do not occur.
3. If `clean = TRUE`, keep only columns that now carry a hy name, in their order; geometry is not reattached. Otherwise reattach the geometry as the last column (R names it `geom`; Python keeps the active geometry column's name, G-14).
4. Python only: reset the row index (I-12).
5. Identifier ingest (I-2, post-change). For each identifier column: a `character` column is unchanged; an `integer`, `integer64`, or whole-valued `double` column becomes `integer` when every non-missing value lies in −2147483647 to 2147483647, else the exact decimal string of each value (no exponent, no trailing `.0`); a missing value stays missing in the new type. `id` and `toid` are typed together, as are `fromnode` and `tonode`: if either column of a pair becomes or arrives as `character`, both become `character`. A `double` column holding a non-whole value, and a column of any other type (factor, logical), is left as it arrives. Python: every identifier column becomes its exact decimal string, whatever arrives; a column mixing strings and numbers raises (I-3). Current R (pre-change) skips this step: identifiers keep their arriving type.
6. If `toid` is present, replace each missing value with the reserved outlet value (I-5): `""` when `id` is `character`, else `0` (post-change `0L`, so `toid` stays `integer`). Python: always `""`, and the column returns in a non-nullable string dtype.
7. R: convert to a tibble (row names dropped, I-12), set `orig_names`, add class `hy`, and set `dendritic` to `FALSE` when a `divergence` column holds any value 2, else `TRUE`.
8. R, `add_topo = TRUE` only: if `fromnode` and `tonode` are present, `toid` is absent, and `id` is unique, build `toid` with `add_toids(x, return_dendritic = <divergence present>)`. The result has `toid` second; rows with a missing `tonode` move to the end, other rows keep input order; with `divergence` present, `fromnode` moves to the last attribute position and no flowline points to a minor path (divergence 2), so `id` stays unique; without it, a flowline above a divergence appears once per downstream flowline. `orig_names` gets no entry for `toid`. Post-change, step 5 applies to the new `toid`. Then, if `toid` is present and `id` repeats, warn (`nonunique_id_with_toid`).
9. Classify (`classify_hy`), first match wins, binding: `id` and `toid` with unique `id` gives `hy_topo`, promoted to `hy_leveled` when `topo_sort`, `levelpath`, and `levelpath_outlet_id` are all present; `id` and `toid` with repeated `id` gives `hy_flownetwork`, which replaces class `hy` and checks the `upmain`/`downmain` invariants when both columns are present; `id`, `fromnode`, and `tonode` with unique `id` gives `hy_node`; anything else stays plain `hy`. `toid` outranks the node columns: a table with both is `hy_topo`.

No sort is performed (D-3 holds vacuously). Without `add_topo`, output row order is input row order (D-14). Nothing depends on randomness or scheduling (D-15). Every value in a row depends only on that row, except the `add_topo` `toid` join and the step-5 type decision, which depend on the row set but not its order (D-7).

## Postconditions

- Row count equals input row count, except `add_topo = TRUE` on node input without `divergence`, where it is at least the input count.
- Without `add_topo`, the output row order is the input row order (D-14).
- Every output column name is either a vocabulary hy name or an input column name that is not a hy name when lowercased; an input column whose lowercased name is an alias keeps its input name only when step 2 gave the hy name to another column (the losing alias of a two-way tie, or `permanent_identifier` under its rule).
- No two output columns share a name.
- `toid`, when present, has no missing value.
- Every `toid` filled in step 6 is `""` when `id` is `character` (always in Python) and `0` otherwise in R.
- Post-change (I-2): every identifier column that arrived as `integer`, `integer64`, whole-valued `double`, or `character` is `integer` or `character` in R and `str` in Python; `id` and `toid` share one type; `fromnode` and `tonode` share one type.
- Post-change (I-2): an identifier column is R `integer` only if every non-missing value lies within ±2147483647.
- Post-change (I-2): the decimal string of every identifier value equals the decimal string of the arriving value.
- Values of every non-identifier column, and of `toid` on non-missing rows, equal the input values.
- With `clean = TRUE`, every column carries a hy name and the output has no geometry.
- With `clean = FALSE` and sf input, the output is sf, the geometry is the last column, and geometries equal the input geometries row for row.
- R: `representation` of the output is `hy_topo` or `hy_leveled` when `id` and `toid` are present and `id` is unique; `hy_flownetwork` when `id` repeats; `hy_node` when `toid` is absent and `id`, `fromnode`, `tonode` are present with unique `id`.
- R: attribute `dendritic` is `FALSE` when the output has a `divergence` column holding a value 2 (a minor path) and `TRUE` otherwise. It describes that column, not the topology returned: a repeated `id` without `divergence` is stamped `TRUE`, and an `add_topo = TRUE` result whose `toid` keeps main paths only is stamped `FALSE` when `divergence` holds a 2 (Edge cases).
- R: `hy_reverse(hy(x))` equals `x` for a tibble or sf tibble `x` whose identifier columns are already `integer` or `character` and whose `toid` has no missing value.
- Running the function twice gives the same columns and values as running it once.

## Complexity

Name alignment is O(c·a) for c columns and a aliases; identifier typing and the `toid` fill are O(n) per identifier column; space O(n·c) for the copied table. `add_topo` adds the node join, O(n log n) time and O(n + m) space for m connections.

## Edge cases

- One flowline (`single_flowline.gpkg`): returns it unchanged apart from names and types; `toid` filled if missing; R class `hy_topo` when `id` and `toid` are present.
- No connections, all terminals (`all_terminals.gpkg`): every `toid` is an outlet; missing values filled per step 6; R class `hy_topo`.
- Zero rows (`empty_network.gpkg`): returns zero rows with every column typed (I-11); R class `hy_topo`; identifier columns `integer` post-change when they arrive as integer or double.
- Duplicated row, the same `id` and `toid` twice (`duplicated_edge.gpkg`): R returns both rows as `hy_flownetwork` and does not raise. S-6 says a duplicated edge in a flownetwork raises; that rule is not applied in M3 (D3-46, D3-57), so the case is R-only.
- Duplicated `id` with `toid`: not an error (I-6); R returns `hy_flownetwork`, without class `hy`, so `is.hy()` on the result is `FALSE`; with `add_topo = TRUE` R also warns.
- `dendritic` describes the `divergence` column only (Postconditions). Captured as current behavior: `hy_duplicated_id` (flowline 1 flows to both 2 and 3, no `divergence`) is stamped `TRUE`; `hy_new_hope_add_topo` (main-path `toid`, `divergence` holding 2) is stamped `FALSE`.
- A `toid` naming a flowline not in the table (a subset cut above its true outlet, or a flow out of a processing unit): left as is; the row is a terminal by I-4.
- No `id` column: R returns plain `hy`; tibble emits an "Unknown or uninitialised column" warning (`no_id_column`).
- `id` `integer` and `toid` `character` with a missing `toid`: current R stops in the fill (`outlet_fill_type_mismatch`); post-change both become `character` and the fill is `""`.
- Factor `id` with a missing `toid`: R stops in the fill (`outlet_fill_type_mismatch`), before and after the change.
- Non-whole `double` identifier: left as `double` in R; Python gives its decimal string.
- Whole-valued `double` above 2^53: the value is already rounded on read; its decimal string is that of the stored double.
- Input already of class `hy`: names already aligned; `orig_names` is rebuilt from the current names, so the original names of the first call are lost.
- Missing weight (`missing_weight.gpkg`), zero-length line (`zero_length.gpkg`), empty name (`empty_name.gpkg`): `hy` reads no weight, geometry, or name, so each passes through; the missing `da_sqkm` stays missing, the zero-length geometry is returned as read (R names its column `geom`), and the empty name stays `""`. `empty_name.gpkg` has no `toid`, so R returns plain `hy`.
- Real networks: `coastal.gpkg` (NHDPlusV2, 32 coastal terminals with missing `tocomid`, filled with `0` by step 6); `nhdplushr.gpkg` (`NHDPlusID` beyond 32 bits becomes `id`, `Permanent_Identifier` drops out under the step-2 rule and keeps its name, R class `hy_node`; post-change `id` is `character`); `new_hope_3dhp.gpkg` (`id3dhp` becomes `id`, `character` identifiers, outlet `toid` already `""`).

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| alias beside its hy name | two columns would carry one hy name | none; columns named in the message | rename or drop one of the two columns before `hy()` | `name_alias_conflict` |
| two aliases for one hy name (warning) | the alias later in map order was used; the other keeps its input name | none; columns named in the message | drop the unwanted column, or register the intended mapping with `hydroloom_names()` | `name_alias_duplicate` |
| three or more aliases for one hy name | no single column can be chosen | none; columns named in the message | drop all but one of the aliasing columns before `hy()` | `name_alias_ambiguous` |
| two column names equal ignoring case | names are compared lowercased, so the two collide | none; columns named in the message | rename one column so the lowercased names differ | `column_name_duplicate` |
| empty column name | a column has no name | none; column position in the message | give the column a name | `column_name_empty` |
| repeated `id` after `add_topo` (warning) | divergences gave a flowline more than one row, so the table is a flownetwork, not `hy_topo` | rows with repeated `id` | `to_flownetwork()` to keep each divergence as its own row, or `make_node_topology()` to describe connections by the points where flowlines meet (nodes) | `nonunique_id_with_toid` |
| more than one `downmain` per `id` | a flowline has more than one main downstream path | rows with `downmain` TRUE and repeated `id` | correct `downmain` so each `id` has one main downstream connection | `flownetwork_multiple_downmain` |
| more than one `upmain` per `toid` | a flowline receives more than one main upstream path | rows with `upmain` TRUE and repeated `toid` | correct `upmain` so each `toid` has one main upstream connection | `flownetwork_multiple_upmain` |
| `id` and `toid` of incompatible types (from vctrs today) | the outlet fill cannot write the reserved value into `toid` | rows with missing `toid` | convert `id` and `toid` to one type, `as.character()` or `as.integer()` | `outlet_fill_type_mismatch` |
| no `id` column (warning, from tibble today) | no column was recognized as the flowline identifier | all | register the identifier column with `hydroloom_names()` or rename it to `id` | `no_id_column` |
| identifier column of mixed types (Python, I-3) | one column holds both strings and numbers | rows whose type differs from the first value's | cast the column to one type before `as_hy()` | `identifier_mixed_type` |

## References

- R source: `R/hy.R` (`hy`), `R/align_names.R` (`align_names`, `hydroloom_names`), `R/hy_classes.R` (`classify_hy`, `new_hy_topo`, `new_hy_leveled`, `new_hy_node`, `new_hy_flownetwork`), `R/add_toids.R` (`add_toids.hy_node`), `R/utils.R` (`get_outlet_value`, `is_outlet`), `R/vocab.R` (`vocab_alias_map`, `vocab_good_names`).
- Tests: `tests/testthat/test_hydroloom.R`, `tests/testthat/test_hy_classes.R`, `tests/testthat/test_outlet_conventions.R`.
- Vocabulary: `inst/extdata/hy_vocab.json` (terms, `type`, `aliases`; each alias's `source` field records the data model it comes from: `nhdplus` for NHDPlusV2 `comid`/`tocomid`, `nhdplushr` for `nhdplusid`, `3dhp` for `id3dhp`).
- Source data models: McKay and others (2012), NHDPlus Version 2: User Guide; Moore and others (2019), User's Guide for the National Hydrography Dataset Plus (NHDPlus) High Resolution, USGS Open-File Report 2019-1096.
- Descriptions: `align_names.md` (step 2 in full).
- SPEC I-1 to I-6, I-11, I-12, I-14, A-1, A-2, A-5, A-9, A-16, A-17, G-14; AGENTS.md "S3 Class Hierarchy".

## Worked example

Fixture `fixtures/hy_worked_example.gpkg`, aspatial, three rows, `COMID` and `ToCOMID` stored as REAL:

| COMID | ToCOMID | Notes |
| --- | --- | --- |
| 101 | 102 | a |
| 102 | 103 | b |
| 103 | NULL | c |

`hy(x)` returns, post-change:

| id | toid | Notes |
| --- | --- | --- |
| 101 | 102 | a |
| 102 | 103 | b |
| 103 | 0 | c |

`id` and `toid` are `integer` (R) and `str` (Python, with `""` in place of `0` on the last row); `Notes` keeps its name; R class `hy_topo`, `orig_names` `c(COMID = "id", ToCOMID = "toid", Notes = "Notes")`, `dendritic` `TRUE`. Before the change, R returns `id` and `toid` as `double` with the same values.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording R 1.3.0 behavior. Intended change I-2 (ROADMAP §4, M3) is stated as post-change behavior in Algorithm step 5, step 6 (`0L`), Postconditions, and Edge cases; the pairing of `id` with `toid` and `fromnode` with `tonode` in one type is this description's reading of I-2. A-9 gap: A-16 lists `as_hy(network, *, id_col=None, toid_col=None)`; the R signature `hy(x, clean = FALSE, add_topo = FALSE)` is recorded, `clean` and `add_topo` are A-5 omissions, and `id_col`/`toid_col` are Python-only. Language difference, not a change: R renames the geometry column to `geom` (`hy_reverse` restores it); Python keeps its name (G-14). The A-10 condition-class change does not apply: `hy` raises no representation error.
- 2026-10-26, suite 0.1.0, review fix round: step 2 tie rules brought into line with `align_names.md` as probed (D3-24): `permanent_identifier` drops out when any hy name has several candidates, and three or more candidates stop (`name_alias_ambiguous`, D3-60). The outlet-fill error key is `outlet_fill_type_mismatch` (D3-60). The `dendritic` attribute and the S-6 pass-through and real-network cases are stated; the duplicated-row case is captured as R-only (D3-46, D3-57).
