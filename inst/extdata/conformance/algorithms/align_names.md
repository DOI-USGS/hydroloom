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

None on representation: `requires.pattern` is `any`, and no column is required. The function stops when an alias column sits beside a column already holding its hydroloom name after the `permanent_identifier` rule (`name_alias_conflict`), when three or more columns alias one hydroloom name after the `permanent_identifier` rule (`name_alias_ambiguous`), when two input column names are equal ignoring case (`column_name_duplicate`), and when a column name is empty (`column_name_empty`). It warns when two columns alias one hydroloom name (`name_alias_duplicate`). R only, as of hydroloom 1.3.0 with sf 1.1.1: on sf input, R also stops inside sf when the geometry column name has an uppercase letter (`sf_geometry_name_case`), and when any column is renamed while some attribute names are all lowercase and others are not (`sf_mixed_case_names`); the `new_hope.gpkg` fixture, with lowercase `mainstemid` beside `COMID`, is such an input. Python raises in neither sf case.

## Definition

Roxygen: this function aligns the attribute names in x with those used in hydroloom. See `hydroloom_names` for how to add more attribute name mappings if the attributes in your data are not supported. See `hydroloom_name_definitions` for definitions of the names used in hydroloom. Returns data.frame renamed to match hydroloom as possible.

As a property of the input: a column carries a hydroloom name when its lowercased name is that name, or when its lowercased name is an alias of that name in the alias map and no rule in Algorithm gives the name to another column. Every other column keeps its input name. Rows, values, types, and column order are the input's. The output depends on the set of column names and the alias map, not on row order or column order.

## Algorithm

The alias map is the registry `hydroloom_names()` returns: one (alias, hydroloom name) pair per vocabulary alias in `hy_vocab.json` term order and, within a term, alias order, followed by pairs registered with `hydroloom_names()` in registration order, each alias lowercased on registration (D3-65). `hydroloom_names(clear = TRUE)` drops the registered pairs and keeps the vocabulary pairs, the registry as seeded at load (hydroloom_names H-03, D3-20). The vocabulary includes the identity pair `toid` → `toid`. Python's map is the same list, vocabulary first, then `register_names()` entries with each alias lowercased (A-5).

1. Record the input names. Lowercase every column name; in R this includes the geometry column.
2. Candidates: the map pairs whose alias is a lowercased column name, in map order. A pair registered twice is two candidates. Registered aliases, binding: where one alias has pairs to two or more different hydroloom names, only the latest of those pairs in map order is a candidate. At load no alias repeats, so this happens only when a registered pair gives an alias a second name, and a registered alias then resolves to its registered mapping, the vocabulary alias of another term included (B-4, REVIEW_11032026 item 4): after registering `comid` as `toid`, a `COMID` column is a candidate for `toid` only. Such an alias is never itself a hydroloom name other than its value, since `hydroloom_names()` and `register_names()` refuse one (`register_alias_is_name`, B-1), so a column already carrying a hydroloom name keeps it.
3. `permanent_identifier` rule, binding: if any hydroloom name has two or more candidates and `permanent_identifier` is a candidate, `permanent_identifier` drops out of the candidates and its column keeps its input name. For this count, a column whose lowercased name is the hydroloom name itself is one of that name's candidates, counted once (through the identity pair where the map has one), so `permanent_identifier` beside `id` drops out. This holds whether or not `id` is the name with several candidates: `permanent_identifier` beside `tocomid` and `to_permanent_identifier` keeps its name rather than becoming `id`. R emits a message; Python prints nothing (A-12). The rule runs before the conflict check (B-1).
4. Conflict, binding: if a candidate remaining after step 3 has an alias that differs from its hydroloom name and that hydroloom name is already a lowercased column name, stop (`name_alias_conflict`). `comid` beside `id` stops; `COMID` beside `ID` stops; `tocomid` beside `toid` stops; `toid` alone does not, its pair being the identity. A second pass over the output of a two-way tie stops here: the losing alias kept its input name beside the name it lost (step 5), as on the same input from the user.
5. Duplicate rule, binding: for each hydroloom name that still has several candidates, warn (`name_alias_duplicate`) and drop the candidate earliest in map order. With exactly two candidates, the one later in map order takes the hydroloom name and the other column keeps its input name: `featureid` wins over `nhdplusid`, `totda` over `totdasqkm`, `mainstemid` over `reachcode`, a registered alias over a vocabulary alias. Column order does not enter. With three or more, two or more candidates remain and the function stops (`name_alias_ambiguous`); R warns once per extra candidate before stopping. The same alias registered twice is the one exception: both entries rename one column, so the function warns and renames. In conformance, `align_names_duplicate_alias_warns` (and `hy_duplicate_alias_tie` under `hy`) asserts the warning and stores no layer; the plain cases `align_names_duplicate_alias`, `_duplicate_alias_swapped`, `_totda`, `_new_hope_totda`, and `_permanent_identifier_elsewhere` compare which column takes the name (D3-182).
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
- Running the function on its own output returns that output unchanged, except after a two-way tie: the losing alias then sits beside the name it lost, and the second pass stops (`name_alias_conflict`) as it would on the same input from the user (B-1).
- The input is not modified (A-8); the output has the input's class, and on GeoDataFrame or sf input the same active geometry column and geometries.

## Complexity

Time O(c + a) for c columns and a alias-map pairs with hashed lookup (R's `%in%` hashes), independent of n rows. Space O(c); R renames without copying column data, and Python's copy of the frame is O(n·c).

## Edge cases

- One flowline, no connections, a duplicated row, a missing weight, a zero-length line: not applicable; rows and values pass through untouched, so `single_flowline.gpkg` and `all_terminals.gpkg` return as read.
- Zero rows: returns zero rows with the aligned names and the input column types (I-11).
- No column matches the map: returns the input unchanged.
- Input already in hydroloom names: returned unchanged (idempotent), except for the losing alias of a two-way tie, which stops a second pass (Postconditions).
- A missing `toid`: stays missing; filling it is `hy()`'s work (I-5).
- Two column names equal ignoring case (`ID` and `id`): stops (`column_name_duplicate`). A GeoPackage cannot hold such a pair, so no manifest case exercises it.
- An empty column name: stops (`column_name_empty`); not expressible in a GeoPackage either.
- An alias registered with `hydroloom_names()` in uppercase or mixed case matches the column whose lowercased name equals it, since registration lowercases the alias and lookup lowercases the column names (D3-65): `hydroloom_names(c(MyID = "id"))` renames a column `MyID` or `myid` to `id`.
- sf input with an uppercase geometry column name, or with mixed-case attribute names and a rename: R stops inside sf (Preconditions); Python returns the aligned GeoDataFrame with the geometry column name unchanged (G-14). `hy()` sets the geometry aside first and does not hit either stop; `hy(x)` is the documented route for sf input, and `align_names` on sf is fixed in M6 only if a ported function needs it (D3-25).
- Two conflicts at once (`comid` beside `id` and `tocomid` beside `toid`): R stops with the second input name printed as `NA`; the condition is `name_alias_conflict` either way.
- `permanent_identifier` beside `to_permanent_identifier` and nothing else: the two become `id` and `toid` with no message and no warning, since no hydroloom name has two candidates.
- `permanent_identifier` beside `id`, as on a second pass over `align_names_nhdplushr` output: step 3 counts the `id` column as a candidate for `id`, drops `permanent_identifier`, and the table returns unchanged. R stops here with `name_alias_conflict`, running the conflict check first, until it moves to the step order above in M5 run 3 unit 3 (B-3).
- `permanent_identifier` beside `tocomid` and `to_permanent_identifier`: step 3 drops `permanent_identifier` because `toid` has two candidates, and step 5 gives `toid` to `to_permanent_identifier`. The result has a `toid` column pointing to permanent identifiers and no `id` column; `permanent_identifier` keeps its name. This follows from step 3 applying when any hydroloom name has several candidates (D3-24).
- Real networks: `nhdplushr.gpkg` (`NHDPlusID` becomes `id`; `Permanent_Identifier` keeps its name under step 3), `new_hope_3dhp.gpkg` (`id3dhp` becomes `id`, `toid` stays `toid`, and 3DHP `hydrosequence` becomes `topo_sort` and `dnhydrosequence` becomes `dn_topo_sort`), and `align_names_new_hope_totda.gpkg` (New Hope in source names with `TotDA` added: `totda` takes `total_da_sqkm` and `TotDASqKM` keeps its name). Their layers store only the columns their manifest `columns:` lists give: `id` for `align_names_nhdplushr`, `id` and `toid` for `align_names_new_hope_3dhp`, and `id`, `total_da_sqkm`, `TotDASqKM` for `align_names_new_hope_totda` (D3-163). Renames of the other columns are compared on the synthetic cases, among them `align_names_network` and `align_names_network_shuffled`, which keep every column.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| alias beside its hydroloom name | An input column renames to a hydroloom name that another column already has, so two columns would carry one name. | none; the columns are named in the message | Drop or rename one of the two columns. | `name_alias_conflict` |
| two aliases for one hydroloom name (warning) | Two input columns are aliases of one hydroloom name. The one later in the alias list (a registered alias comes after the vocabulary's) took the name, and the message gives it after "using"; the other column kept its input name. Registering the intended mapping (hydroloom_names() in R, register_names() in Python) is the alternative to dropping a column. | none; the columns are named in the message | Drop the column that should not carry the hydroloom name. | `name_alias_duplicate` |
| three or more aliases for one hydroloom name | Three or more input columns are aliases of one hydroloom name. When aliases compete, the one earliest in the alias list is set aside: with two, the other takes the name (name_alias_duplicate); with three or more, two or more remain and none can be chosen to carry it. | none; the columns are named in the message | Drop all but one of the aliasing columns. | `name_alias_ambiguous` |
| two column names equal ignoring case | Column names are compared lowercased, so two names equal ignoring case collide. | none; the columns are named in the message | Rename one column so the lowercased names differ. | `column_name_duplicate` |
| empty column name | A column has no name. | none; the column position is in the message | Give the column a name. | `column_name_empty` |
| uppercase geometry column name on sf input (R only) | On an sf table, sf cannot find the geometry column after its name is lowercased. | none | `hy(x)` | `sf_geometry_name_case` |
| mixed-case attribute names on sf input (R only) | On an sf table with mixed-case attribute names, sf loses track of which attribute columns go with the geometry after the rename. | none | `hy(x)` | `sf_mixed_case_names` |

Python raises `KeyError` for the four stops `name_alias_conflict`, `name_alias_ambiguous`, `column_name_duplicate`, and `column_name_empty` (A-10, a vocabulary problem; D3-26) and warns with `HydroloomWarning` for `name_alias_duplicate` (A-11). The two sf keys carry no Python class.

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
- 2026-10-28, suite 0.1.0: M3 close. `hydroloom_names()` lowercases an alias on registration (D3-65), so an uppercase registered alias matches; the alias map in Algorithm and the Edge case state this, and Python's `register_names()` does the same. It lands in R with a NEWS line. The Remedies table takes its condition, rows, and fix texts from `remedies.json` (D3-146), and the Python class sentence lists the four `KeyError` stops (D3-26).
- 2026-10-28, suite 0.1.0: M3 close description sweep. The ROADMAP §4 S-2 rows carry D3-65 and the `clear = TRUE` reset. `hydroloom_names(clear = TRUE)` keeps the vocabulary pairs of the alias map and drops the registered ones (hydroloom_names H-03, D3-20); the alias map paragraph states it. The vocabulary makes 3DHP `hydrosequence` an alias of `topo_sort` (add_topo_sort H-04), so `align_names_new_hope_3dhp` renames it; Edge cases state it. `hy.md` step 2 matches steps 2 to 7 here (D3-24). Step 5 gives the warning-only case and the plain tie cases (D3-182); Edge cases give the `columns:` lists of the real-network cases (D3-163) and the sf route (D3-25). The `name_alias_duplicate` row of Remedies takes the registration sentence of its `remedies.json` condition (D3-146). No change to Signature, Preconditions, Postconditions, or a tie rule.
- 2026-10-29, suite 0.1.0: M3 close run 3. D3-184: 3DHP `dnhydrosequence` and `uphydrosequence` are `dn_topo_sort` and `up_topo_sort` aliases, after the NHDPlus ones; the `new_hope_3dhp.gpkg` bullet names the `dnhydrosequence` rename. The `align_names_new_hope_3dhp` layer stores `id` and `toid` only and does not move.
- 2026-11-03, suite 0.1.0, M5 run 3 unit 2, REVIEW_11032026 item 1: steps 3 and 4 trade places, so the `permanent_identifier` rule runs before the conflict check and counts a column already holding the name among its candidates; the order follows RULES B-1, and R, which runs the conflict check first, moves to it in M5 run 3 unit 3 (B-3). The idempotence Postcondition carries the two-way-tie exception: the losing alias sits beside the name it lost and a second pass raises `name_alias_conflict` (B-1). Preconditions, Edge cases (a `permanent_identifier`-beside-`id` bullet; step numbers), and the idempotence bullet follow. REVIEW_11032026 item 4 (B-4, REVIEW_11032026 item 4): step 2 resolves a registered alias that is the vocabulary alias of another term to the registered mapping.
- 2026-11-03, suite 0.1.0, M5 run 3 unit 2 fix round, hydro H-01 (RULES B-1): step 2 states that a registered alias is never a hydroloom name other than its value, since registration refuses one (`register_alias_is_name`, `hydroloom_names.md` step 3), so the Postcondition that a column already holding a hydroloom name changes only to lowercase stays true.
