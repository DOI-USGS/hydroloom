---
requires:
  pattern: [any]
  columns: []
  via: {}
---

# align_names

## Signature

R: `align_names(x)`.

| argument | type | default |
| --- | --- | --- |
| `x` | table (data.frame, tibble, or sf) | required |

Python (ROADMAP §2, SPEC A-5, A-16): `align_names(network)`, returning a DataFrame, or a GeoDataFrame when the input is one (A-8). `network` is R's `x` (A-9). No Python-only argument.

Return: the input table with every column in its input position and every value unchanged. Only column names differ:

| column | name | type |
| --- | --- | --- |
| a column whose lowercased name is an alias in the alias map and which wins any tie (Algorithm) | the alias's hydroloom name | unchanged |
| a column whose lowercased name is a hydroloom name | that name, lowercase (`ID` becomes `id`) | unchanged |
| every other column, the geometry column included | its input name, case preserved | unchanged |

R returns the input class (data.frame, tibble, or sf) and adds no class or attribute; it is not `hy()`, so no identifier typing (I-2) and no `toid` fill (I-5) happen here. Python keeps no state on the frame (I-14). Argument and return prose come from roxygen (A-13). No kernel; nothing here runs below the K-1 boundary.

## Preconditions

None on representation: `requires.pattern` is `any`, and no column is required. The function stops when an alias column sits beside a column already holding its hydroloom name (`name_alias_conflict`), when three or more columns alias one hydroloom name after the `permanent_identifier` rule (`name_alias_ambiguous`), when two input column names are equal ignoring case (`column_name_duplicate`), and when a column name is empty (`column_name_empty`). It warns when two columns alias one hydroloom name (`name_alias_duplicate`). R only, as of hydroloom 1.3.0 with sf 1.1.1: on sf input, R also stops inside sf when the geometry column name has an uppercase letter (`sf_geometry_name_case`), and when any column is renamed while some attribute names are all lowercase and others are not (`sf_mixed_case_names`); the `new_hope.gpkg` fixture, with lowercase `mainstemid` beside `COMID`, is such an input. Python raises in neither sf case.

## Definition

Roxygen: this function aligns the attribute names in x with those used in hydroloom. See `hydroloom_names` for how to add more attribute name mappings if the attributes in your data are not supported. See `hydroloom_name_definitions` for definitions of the names used in hydroloom. Returns data.frame renamed to match hydroloom as possible.

As a property of the input: a column carries a hydroloom name when its lowercased name is that name, or when its lowercased name is an alias of that name in the alias map and no rule in Algorithm gives the name to another column. Every other column keeps its input name. Rows, values, types, and column order are the input's. The output depends on the set of column names and the alias map, not on row order or column order.

## Algorithm

The alias map is the registry `hydroloom_names()` returns: one (alias, hydroloom name) pair per vocabulary alias in `hy_vocab.json` term order and, within a term, alias order, followed by pairs registered with `hydroloom_names()` in registration order. The vocabulary includes the identity pair `toid` → `toid`. Python's map is the same list, vocabulary first, then `register_names()` entries (A-5).

1. Record the input names. Lowercase every column name; in R this includes the geometry column.
2. Candidates: the map pairs whose alias is a lowercased column name, in map order. A pair registered twice is two candidates.
3. Conflict, binding: if a candidate's alias differs from its hydroloom name and that hydroloom name is already a lowercased column name, stop (`name_alias_conflict`). `comid` beside `id` stops; `COMID` beside `ID` stops; `tocomid` beside `toid` stops; `toid` alone does not, its pair being the identity.
4. `permanent_identifier` rule, binding: if any hydroloom name has two or more candidates and `permanent_identifier` is a candidate, `permanent_identifier` drops out of the candidates and its column keeps its input name. This holds whether or not `id` is the name with several candidates: `permanent_identifier` beside `tocomid` and `to_permanent_identifier` keeps its name rather than becoming `id`. R emits a message; Python prints nothing (A-12).
5. Duplicate rule, binding: for each hydroloom name that still has several candidates, warn (`name_alias_duplicate`) and drop the candidate earliest in map order. With exactly two candidates, the one later in map order takes the hydroloom name and the other column keeps its input name: `featureid` wins over `nhdplusid`, `totda` over `totdasqkm`, `mainstemid` over `reachcode`, a registered alias over a vocabulary alias. Column order does not enter. With three or more, two or more candidates remain and the function stops (`name_alias_ambiguous`); R warns once per extra candidate before stopping. The same alias registered twice is the one exception: both entries rename one column, so the function warns and renames.
6. Rename each remaining candidate's column to its hydroloom name, in place.
7. Every column whose name is now not a vocabulary hydroloom name gets its input name back, case preserved. A column renamed through a registered pair whose value is not a hydroloom name therefore keeps its input name.

Steps 1 and 7 together give the lowercase rule: a column whose lowercased name is a hydroloom name keeps the lowercase form. No sort is performed (D-3 holds vacuously). Row order and column order are the input's (D-14). No value depends on row order (D-7); the tie rules read map order, never column order. Nothing depends on randomness, hashing, or scheduling (D-15). No sentinel and no CSR index (K-2, K-4).

## Postconditions

- Row count and column count equal the input's.
- Column i of the output holds the values of column i of the input, row for row, with the same type.
- Every output column name either is a vocabulary hydroloom name or equals the input name at that position.
- An output name that differs from the input name at that position is a vocabulary hydroloom name, and the lowercased input name is either that name or an alias of it in the alias map.
- No two output columns share a name.
- No input column whose lowercased name is a vocabulary hydroloom name changes except to lowercase.
- A permutation of the input rows permutes the output rows the same way and leaves the names unchanged (D-7).
- A permutation of the input columns permutes the output columns the same way and leaves each column's output name unchanged.
- Running the function on its own output returns that output unchanged.
- The input is not modified (A-8); the output has the input's class, and on GeoDataFrame or sf input the same active geometry column and geometries.

## Complexity

Time O(c + a) for c columns and a alias-map pairs with hashed lookup (R's `%in%` hashes), independent of n rows. Space O(c); R renames without copying column data, and Python's copy of the frame is O(n·c).

## Edge cases

- One flowline, no connections, a duplicated row, a missing weight, a zero-length line: not applicable; rows and values pass through untouched, so `single_flowline.gpkg` and `all_terminals.gpkg` return as read.
- Zero rows: returns zero rows with the aligned names and the input column types (I-11).
- No column matches the map: returns the input unchanged.
- Input already in hydroloom names: returned unchanged (idempotent).
- A missing `toid`: stays missing; filling it is `hy()`'s work (I-5).
- Two column names equal ignoring case (`ID` and `id`): stops (`column_name_duplicate`). A GeoPackage cannot hold such a pair, so no manifest case exercises it.
- An empty column name: stops (`column_name_empty`); not expressible in a GeoPackage either.
- An uppercase alias registered with `hydroloom_names()` never matches, since names are lowercased before lookup.
- sf input with an uppercase geometry column name, or with mixed-case attribute names and a rename: R stops inside sf (Preconditions); Python returns the aligned GeoDataFrame with the geometry column name unchanged (G-14). `hy()` sets the geometry aside first and does not hit either stop.
- Two conflicts at once (`comid` beside `id` and `tocomid` beside `toid`): R stops with the second input name printed as `NA`; the condition is `name_alias_conflict` either way.
- `permanent_identifier` beside `to_permanent_identifier` and nothing else: the two become `id` and `toid` with no message and no warning, since no hydroloom name has two candidates.
- `permanent_identifier` beside `tocomid` and `to_permanent_identifier`: step 4 drops `permanent_identifier` because `toid` has two candidates, and step 5 gives `toid` to `to_permanent_identifier`. The result has a `toid` column pointing to permanent identifiers and no `id` column; `permanent_identifier` keeps its name. This follows from step 4 applying when any hydroloom name has several candidates (D3-24).

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| alias beside its hydroloom name | two columns would carry one hydroloom name | none; columns named in the message | `dplyr::select(x, -<alias column>)` | `name_alias_conflict` |
| two aliases for one hydroloom name (warning) | the column named after "using" took the hydroloom name; the other aliasing column kept its input name | none; columns named in the message | `dplyr::select(x, -<unwanted column>)` | `name_alias_duplicate` |
| three or more aliases for one hydroloom name | no single column can be chosen | none; columns named in the message | `dplyr::select(x, -c(<all but one aliasing column>))` | `name_alias_ambiguous` |
| two column names equal ignoring case | names are compared lowercased, so the two collide | none; columns named in the message | `names(x)[<position>] <- "<new name>"` | `column_name_duplicate` |
| empty column name | a column has no name | none; column position in the message | `names(x)[<position>] <- "<new name>"` | `column_name_empty` |
| uppercase geometry column name on sf input (R only) | sf cannot find the geometry column after lowercasing | none | `hy(x)` | `sf_geometry_name_case` |
| mixed-case attribute names on sf input (R only) | sf cannot match its attribute-geometry table after the rename | none | `hy(x)` | `sf_mixed_case_names` |

Python raises `KeyError` for the first five stops (A-10, a vocabulary problem) and warns with the package warning class for `name_alias_duplicate` (A-11).

## References

- R source: `R/align_names.R` (`align_names`, `hydroloom_names`), `R/vocab.R` (`vocab_alias_map`, `vocab_good_names`), `R/zzz.R` (`.onLoad` seeds the registry), `R/hy.R` (calls `align_names` after setting geometry aside).
- Tests: `tests/testthat/test_align_names.R`.
- Vocabulary: `inst/extdata/hy_vocab.json` (`aliases` of each term, and each alias's `source` tag, `nhdplus`, `nhdplushr`, or `3dhp`, which gives the data model the alias comes from).
- NHDPlus Version 2 User Guide (McKay and others, 2012): the NHDPlusV2 attribute names (`COMID`, `ToCOMID`, `Hydroseq`, `TotDASqKM`, and others) behind the `nhdplus` aliases.
- USGS, USGS hydrofabric data theme, section "Hydrofabric datasets", which describes NHDPlusV2, NHDPlusHR, and 3DHP, the datasets behind the `nhdplus`, `nhdplushr`, and `3dhp` aliases: https://water.usgs.gov/themes/hydrofabric/.
- Descriptions: `hy.md` (Algorithm step 2), `hydroloom_names.md` (the registry).
- SPEC A-5, A-8, A-9, A-10, A-11, A-12, A-16, D-7, D-14, D-15, G-14, I-5, I-14.

## Worked example

Fixture `fixtures/hy_worked_example.gpkg` (shared with `hy`), aspatial, three rows, `COMID` and `ToCOMID` stored as REAL:

| COMID | ToCOMID | Notes |
| --- | --- | --- |
| 101 | 102 | a |
| 102 | 103 | b |
| 103 | NULL | c |

`align_names(x)` returns:

| id | toid | Notes |
| --- | --- | --- |
| 101 | 102 | a |
| 102 | 103 | b |
| 103 | NA | c |

`COMID` and `ToCOMID` lowercase to the aliases `comid` and `tocomid`; `Notes` lowercases to `notes`, not a hydroloom name, and gets its input name back. Values and types are unchanged: `id` and `toid` stay `double` in R, and the missing `toid` stays missing, where `hy()` would fill it.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording R 1.3.0 behavior. No ROADMAP §4 change applies: I-2 and I-5 are `hy()`'s, and `align_names` raises no representation error, so the A-10 condition class does not reach it. Step 4 and the three-candidate stop in step 5 are stated more narrowly in `hy.md` step 2, which says `permanent_identifier` drops only from a set of candidates it belongs to and that the latest of several candidates always wins; this description records R. The two sf stops are R accidents captured as current behavior, not fixed here.
- 2026-10-26, suite 0.1.0: hydrology review fix round. Edge cases add the `permanent_identifier`/`to_permanent_identifier` pair and the no-`id` outcome of the mixed table; References add the NHDPlusV2 user guide, the hydrofabric data theme, and the vocabulary `source` tags; the Remedies fix column gives one call per row. No behavior change.
