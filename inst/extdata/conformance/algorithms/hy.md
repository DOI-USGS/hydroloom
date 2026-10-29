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

Identifier columns are the vocabulary terms of type `id`: `id`, `toid`, `fromnode`, `tonode`, `wbid`, `terminal_id`, `levelpath`, `up_levelpath`, `dn_levelpath`, `levelpath_outlet_id`, `vector_proc_unit`, `raster_proc_unit`, `aggregate_id`, `point_id`, `hydro_location_id`. After this function, each is Python `str`; in R each that arrives as `integer`, `integer64`, `double`, or `character` is `integer` or `character`, and a factor or logical column is left as it arrives, except under the step-5 pair rule (I-1, I-2, step 5).

Flag columns are the vocabulary terms of type `flag`: `terminal_flag`, `start_flag`, `upmain`, `downmain`. After this function, one that arrives as numbers coded 1/0 (R `integer` or `double`, not `integer64`) is R `logical` and Python boolean (`bool`, or nullable `boolean` when a value is missing) (S-2, M-18). A numeric flag column holding any other value is tolerated with a warning (Edge cases).

No kernel; nothing here runs below the K-1 boundary.

## Preconditions

None on representation: `requires.pattern` is `any`. The function runs on any table and is the step every other function's representation check starts from. R also runs on a table with no `id` column, warning (`no_id_column`); Python raises `RepresentationError` there (D3-130). It raises when two input columns would take one hy name and one of them already is that name (`name_alias_conflict`), when three or more columns alias one hy name after the `permanent_identifier` rule (`name_alias_ambiguous`), when two column names are equal ignoring case (`column_name_duplicate`) or a column name is empty (`column_name_empty`), all from `align_names` (`align_names.md`), and when an input with `upmain` and `downmain` breaks the flownetwork main-path invariants (`flownetwork_multiple_downmain`, `flownetwork_multiple_upmain`). R and Python raise when an identifier column arrives as floating-point numbers and a value has a fractional part (`identifier_not_whole`, D3-07) or is infinite (`identifier_not_finite`, I2-3). Python raises `TypeError` for an identifier column holding mixed types (I-3, `identifier_mixed_type`). Python raises on a repeated (`id`, `toid`) pair (`id_toid_pair_repeated`, S-6, D3-57, D3-166); R returns such a table until the M6 §4 row (Edge cases).

R, `add_topo = TRUE` on node input carrying `divergence`: the `toid` build is `add_toids(return_dendritic = TRUE)` (step 8), so it stops when `divergence` is missing on any flowline (`divergence_missing`) or a split has no main path or more than one (`divergence_coding_invalid`), naming the flowlines by `id` (`add_toids.md`).

## Definition

Roxygen: converts a compatible dataset into a fabric s3 class. Automatically detects the network representation and classifies the result as the most specific hydroloom subclass (e.g. `hy_topo`, `hy_node`). With `add_topo = TRUE`, also auto-builds topology (e.g. toid from fromnode/tonode).

As a property of the input: every column that the vocabulary (or a name registered with `hydroloom_names()`) recognizes carries its hy name, every identifier column carries one of the two identifier types decided from its values, every missing `toid` carries the reserved outlet value for that type, and the row set and values are the input's. The representation the output belongs to is a function of which of `id`, `toid`, `fromnode`, `tonode`, `topo_sort`, `levelpath`, `levelpath_outlet_id` are present and whether `id` repeats.

## Algorithm

1. If the input is sf, set the geometry aside and work on the attribute table.
2. Align names (`align_names`; `align_names.md` Algorithm steps 2 to 7 hold here unchanged). Lowercase every column name. Look each up in the alias map: the vocabulary's `aliases` in vocabulary term order and, within a term, alias order, then names registered with `hydroloom_names()` in registration order (a registered alias is lowercased on registration, D3-65, so an uppercase registration takes effect). Tie rules, binding:
   - If an alias column and a column already holding its hy name are both present (`comid` beside `id`), stop (`name_alias_conflict`).
   - If any hy name has two or more candidate columns and `permanent_identifier` is a candidate, `permanent_identifier` drops out and keeps its input name, with a message in R. This holds whether or not `id` is the hy name with several candidates: `permanent_identifier` beside `tocomid` and `to_permanent_identifier` keeps its name rather than becoming `id`.
   - If a hy name still has exactly two candidates, the one later in alias-map order takes it, with a warning (`name_alias_duplicate`); the other keeps its input name. Column order does not enter. For `id` the map order is `id3dhp`, `comid`, `nhdplusid`, `featureid`, `permanent_identifier`, `from_permanent_identifier`, `reconciled_id`, `aggregated_id`, so `featureid` wins over `nhdplusid`. Case `hy_duplicate_alias_tie` asserts the warning only (D3-182); the same tie's output is compared by `align_names_duplicate_alias`.
   - If a hy name still has three or more candidates, stop (`name_alias_ambiguous`); R warns once per extra candidate first. The one exception is the same alias registered twice: both entries rename one column, so R warns and renames.
   - A column whose lowercased name is neither a hy name nor an alias gets its input name back, case preserved. A column whose lowercased name is a hy name keeps the lowercase form (`ID` becomes `id`).
   - On sf input the geometry is already set aside (step 1), so the two sf stops `align_names.md` records (`sf_geometry_name_case`, `sf_mixed_case_names`) do not occur.
3. If `clean = TRUE`, keep only columns that now carry a hy name, in their order; geometry is not reattached. Otherwise reattach the geometry as the last column (R names it `geom`; Python keeps the active geometry column's name, G-14).
4. Python only: reset the row index (I-12).
5. Identifier ingest (I-2). For each identifier column, in vocabulary term order: a `character` column is unchanged; an `integer`, `integer64`, or whole-valued `double` column becomes `integer` when every non-missing value lies in −2147483647 to 2147483647, else the exact decimal string of each value (no exponent, no trailing `.0`); a missing value stays missing in the new type, and `NaN` is missing. `id` and `toid` are typed together, as are `fromnode` and `tonode` (D3-06): if either column of a pair becomes or arrives as `character`, both become `character`. A `double` column holding a value with a fractional part stops (`identifier_not_whole`, D3-07); one holding `Inf` or `-Inf` stops (`identifier_not_finite`, I2-3), checked after the fractional part within each column. Both messages give the input column name (`COMID`, not `id`) and the 1-based row positions, since the identifier value is itself the problem (I-12). A column of any other type (factor, logical) is left as it arrives, except that the pair rule makes a factor `character` when its partner is `character`. Python: every identifier column becomes its exact decimal string, whatever arrives, except that a float column holding a value with a fractional part raises (`identifier_not_whole`), one holding an infinite value raises (`identifier_not_finite`), and a column mixing strings and numbers raises (I-3); `NaN` in a float column is missing.
   Flag ingest (S-2, M-18). For each flag column: an `integer` or `double` column (not `integer64`) whose non-missing values are all 0 or 1 becomes `logical`, 1 `TRUE` and 0 `FALSE`; a missing value stays missing. A `logical` column is unchanged. Python: the same values become `bool`, or the nullable `boolean` dtype when a value is missing. A numeric flag column holding any other value is left as it arrives with a warning (`flag_value_not_binary`), a tolerated coding (Edge cases); a `character`, factor, or `integer64` flag column is left as it arrives without one.
6. If `toid` is present, replace each missing value with the reserved outlet value (I-5): `""` when `id` is `character`, `0L` when `id` is `integer`, so `toid` keeps the type of `id`. Python: always `""`, and the column returns in a non-nullable string dtype.
7. R: convert to a tibble (row names dropped, I-12), set `orig_names`, add class `hy`, and set `dendritic` to `FALSE` when a `divergence` column holds any value 2, else `TRUE`.
8. R, `add_topo = TRUE` only: if `fromnode` and `tonode` are present, `toid` is absent, and `id` is unique, build `toid` with `add_toids(x, return_dendritic = <divergence present>)`. The result has `toid` second, and its values are `id` values or the step-6 outlet value, so `toid` has the type of `id`. Each row keeps its input position, a row with a missing `tonode` included, which takes the outlet value (D3-56). With `divergence` present, the `add_toids.md` coding checks apply first: a missing `divergence` or a split with no main path or more than one stops (`divergence_missing`, `divergence_coding_invalid`); `fromnode` moves to the last attribute position; no flowline points to a minor path (divergence 2) except a minor path that is the only flowline leaving the point where the flowline above it ends, which is connected with a warning (`divergence_minor_only_path`, D3-158); and a main path coded 0 is read as the main path with a warning (`divergence_zero_main_path`, D3-174). Each flowline then has one row, so `id` stays unique. Without `divergence`, a flowline above a divergence appears once per downstream flowline, its rows together in its input position, their `toid` in the input order of the downstream flowlines (`add_toids.md` step 7, D3-37). `orig_names` gets no entry for `toid`. Then, if `toid` is present and `id` repeats, warn (`nonunique_id_with_toid`).
9. Classify (`classify_hy`), first match wins, binding: `id` and `toid` with unique `id` gives `hy_topo`, promoted to `hy_leveled` when `topo_sort`, `levelpath`, and `levelpath_outlet_id` are all present; `id` and `toid` with repeated `id` gives `hy_flownetwork`, which replaces class `hy` and checks the `upmain`/`downmain` invariants when both columns are present; `id`, `fromnode`, and `tonode` with unique `id` gives `hy_node`; anything else stays plain `hy`. `toid` outranks the node columns: a table with both is `hy_topo`.

No sort is performed (D-3 holds vacuously). Output row order is input row order (D-14), except that `add_topo = TRUE` without `divergence` repeats a row in place once per downstream flowline. Nothing depends on randomness or scheduling (D-15). Every value in a row depends only on that row, except the `add_topo` `toid` join and the step-5 type decisions, which depend on the row set but not its order (D-7).

## Postconditions

- Row count equals input row count, except `add_topo = TRUE` on node input without `divergence`, where it is at least the input count.
- Output row order is input row order (D-14); with `add_topo = TRUE` on node input without `divergence`, each input row's copies stand together in its input position, their `toid` in the input order of the downstream flowlines (`add_toids.md` step 7, D3-37).
- Every output column name is either a vocabulary hy name or an input column name that is not a hy name when lowercased; an input column whose lowercased name is an alias keeps its input name only when step 2 gave the hy name to another column (the losing alias of a two-way tie, or `permanent_identifier` under its rule).
- No two output columns share a name.
- `toid`, when present, has no missing value.
- Every `toid` filled in step 6 is `""` when `id` is `character` (always in Python) and `0L` when `id` is `integer` in R.
- I-2: every identifier column that arrived as `integer`, `integer64`, `double`, or `character` is `integer` or `character` in R and `str` in Python; `id` and `toid` share one type; `fromnode` and `tonode` share one type.
- I-2: an identifier column is R `integer` only if every non-missing value lies within ±2147483647.
- I-2: the decimal string of every identifier value equals the decimal string of the arriving value.
- I-1, D3-07, I2-3: no identifier column in the output is R `double` or Python float, and none holds an infinite value.
- M-18: every flag column that arrived as `integer` or `double` (not `integer64`) or `logical` with non-missing values in 0, 1, `TRUE`, `FALSE` is R `logical` and Python boolean; a value is `TRUE` exactly where the arriving value is 1 or `TRUE`, and missing where it is missing.
- Values of every non-identifier, non-flag column, and of `toid` on non-missing rows, equal the input values.
- With `clean = TRUE`, every column carries a hy name and the output has no geometry.
- With `clean = FALSE` and sf input, the output is sf, the geometry is the last column, and geometries equal the input geometries row for row.
- R: `representation` of the output is `hy_topo` or `hy_leveled` when `id` and `toid` are present and `id` is unique; `hy_flownetwork` when `id` repeats; `hy_node` when `toid` is absent and `id`, `fromnode`, `tonode` are present with unique `id`.
- R: attribute `dendritic` is `FALSE` when the output has a `divergence` column holding a value 2 (a minor path) and `TRUE` otherwise. It describes that column, not the topology returned: a repeated `id` without `divergence` is stamped `TRUE`, and an `add_topo = TRUE` result whose `toid` keeps main paths only is stamped `FALSE` when `divergence` holds a 2 (Edge cases).
- R: `hy_reverse(hy(x))` equals `x` for a tibble or sf tibble `x` whose identifier columns are already `integer` or `character`, whose flag columns are already `logical`, and whose `toid` has no missing value.
- Running the function twice gives the same columns and values as running it once.

## Complexity

Name alignment is O(c·a) for c columns and a aliases; identifier typing and the `toid` fill are O(n) per identifier column; space O(n·c) for the copied table. `add_topo` adds the node join, O(n log n) time and O(n + m) space for m connections.

## Edge cases

- One flowline (`single_flowline.gpkg`): returns it unchanged apart from names and types; `toid` filled if missing; R class `hy_topo` when `id` and `toid` are present.
- No connections, all terminals (`all_terminals.gpkg`): every row is a terminal (I-4); missing values filled per step 6; R class `hy_topo`.
- Zero rows (`empty_network.gpkg`): returns zero rows with every column typed (I-11); R class `hy_topo`; identifier columns that arrive as integer or double are `integer`.
- Duplicated row, the same `id` and `toid` twice (`duplicated_edge.gpkg`): R returns both rows as `hy_flownetwork` and does not raise; case `hy_duplicated_edge` is R-only. S-6 makes a repeated (`id`, `toid`) pair an error whether or not the rows differ elsewhere, raised at ingest so every function inherits it (D3-57): Python `as_hy` raises from its first release (`id_toid_pair_repeated`, D3-166), and R follows under the M6 §4 row.
- Duplicated `id` with `toid`: not an error (I-6); R returns `hy_flownetwork`, without class `hy`; with `add_topo = TRUE` R also warns. `hy_reverse()` restores the input names on that result and `is.hy()` returns `TRUE` for it, since it carries `orig_names` (D3-16, D3-64).
- `dendritic` describes the `divergence` column only (Postconditions): `hy_duplicated_id` (flowline 1 flows to both 2 and 3, no `divergence`) is stamped `TRUE`; `hy_new_hope_add_topo` (main-path `toid`, `divergence` holding 2) is stamped `FALSE`.
- `add_topo = TRUE` on node input without `divergence` (`new_hope_no_divergence.gpkg`, 832 rows): R returns `hy_flownetwork` and warns (`nonunique_id_with_toid`). Case `hy_new_hope_add_topo_no_divergence` asserts the warning only (D3-182); case `hy_new_hope_add_topo_no_divergence_rows` makes the same call with the warning suppressed and compares `id` and `toid` in row order (D3-192).
- A `toid` naming a flowline not in the table (a subset cut above its true outlet, or a flow out of a processing unit): left as is; the row is a terminal by I-4.
- No `id` column: R returns plain `hy`; the warning is tibble's "Unknown or uninitialised column", which the `no_id_column` key records. Python raises `RepresentationError` (`no_id_column`, D3-130).
- `id` `integer` and `toid` `character` with a missing `toid` (`hy_id_integer_toid_character`): both become `character` (D3-06) and the fill is `""`.
- Factor `id` and factor `toid` with a missing `toid`: R stops in the fill with vctrs's error (`outlet_fill_type_mismatch`). A factor `id` beside a `character` `toid` becomes `character` with it (D3-06); beside an `integer` `toid` it stays a factor and the fill is `0L`. Python never reaches it, since its identifiers are `str` (D3-105).
- Non-whole `double` identifier (`hy_identifier_not_whole`, `COMID` 3.5 on row 3): R and Python stop (`identifier_not_whole`, D3-07), giving the input column name and the row positions.
- Infinite `double` identifier (`hy_identifier_not_finite`, `COMID` `Inf` on row 3): R and Python stop (`identifier_not_finite`, I2-3), giving the input column name and the row positions. `NaN` is missing, so a `NaN` `toid` takes the outlet value.
- Flag column coded 1/0 (`StartFlag` and `TerminalFl` in `new_hope.gpkg` and `nhdplushr.gpkg`): `logical` (M-18). A missing flag value stays missing (`hy_flag_missing`: `TerminalFl` 0, missing, 1 gives `FALSE`, `NA`, `TRUE` with no warning; the nullable `boolean` branch in Python). Conformance compares the stored flag type (D3-171). `terminal_flag` is the source's coding and is not reconciled with I-4: on `new_hope.gpkg`, cut above its true outlet, every row is `FALSE`, yet the outlet row is a terminal by I-4.
- Numeric flag column holding a value other than 0, 1, or missing: tolerated with a warning (`flag_value_not_binary`, D3-152), once per column, giving the input column name and the rows by `id` (by position only when there is no `id` column). The column is left as it arrives, which is not a supported output. Case `hy_flag_value_not_binary` (`TerminalFl` 2 on `COMID` 30) asserts the warning only (D3-182). One call to any function raises it once per column: a `.data.frame` method calls `hy()` once, and its internal steps work on the `hy` object without calling `hy()` again (I23-2).
- Whole-valued `double` above 2^53: the value is already rounded on read; its decimal string is that of the stored double.
- Input already of class `hy`: names already aligned; `orig_names` is rebuilt from the current names, so the original names of the first call are lost.
- Missing weight (`missing_weight.gpkg`), zero-length line (`zero_length.gpkg`), empty name (`empty_name.gpkg`): `hy` reads no weight, geometry, or name, so each passes through; the missing `da_sqkm` stays missing, the zero-length geometry is returned as read (R names its column `geom`), and the empty name stays `""`. `empty_name.gpkg` has no `toid`, so R returns plain `hy`.
- Real networks: `coastal.gpkg` (NHDPlusV2, 32 coastal terminals with missing `tocomid`, filled with `0L` by step 6); `nhdplushr.gpkg` (`NHDPlusID`, `FromNode`, and `ToNode` beyond 32 bits become `character` identifiers, a branch the case `hy_large_nodes` compares since `hy_nhdplushr` keeps neither node column, `Permanent_Identifier` drops out under the step-2 rule and keeps its name, R class `hy_node`); `new_hope_3dhp.gpkg` (`id3dhp` becomes `id`, 3DHP `hydrosequence` becomes `topo_sort`, and `dnhydrosequence` becomes `dn_topo_sort`, `character` identifiers, `toid` built from `dnhydrosequence` when the fixture was made (`fixtures/index.yaml`), with outlet `toid` already `""`). The layers of the `nhdplushr.gpkg` and `new_hope*.gpkg` cases store only the columns their manifest `columns:` lists give: `id`, `toid` where present, and `start_flag`, `terminal_flag` where present (D3-163, D3-185). Renames of the other columns, `Permanent_Identifier` keeping its name among them, are compared on the synthetic cases (`hy_permanent_identifier_tie` and the `align_names` cases). `hy_new_hope_default` alone keeps its geometry (G-14).

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| alias beside its hy name | an input column renames to a hy name that another column already has, so two columns would carry one name | none; the columns are named in the message | drop or rename one of the two columns | `name_alias_conflict` |
| two aliases for one hy name (warning) | two input columns are aliases of one hy name; the one later in the alias list (a registered alias comes after the vocabulary's) took the name and the other kept its input name | none; the columns are named in the message | drop the column that should not carry the hy name, or register the intended mapping with `hydroloom_names()` | `name_alias_duplicate` |
| three or more aliases for one hy name | three or more input columns are aliases of one hy name after `permanent_identifier` drops out (step 2); with the earliest in the alias list set aside, two or more remain and none can be chosen to carry it | none; the columns are named in the message | drop all but one of the aliasing columns | `name_alias_ambiguous` |
| two column names equal ignoring case | names are compared lowercased, so two names equal ignoring case collide | none; the columns are named in the message | rename one column so the lowercased names differ | `column_name_duplicate` |
| empty column name | a column has no name | none; the column position is in the message | give the column a name | `column_name_empty` |
| repeated `id` after `add_topo` (R warning) | divergences gave a flowline more than one row, so `hy()` returned a flownetwork (one row per downstream connection), not `hy_topo` | the rows with a repeated `id` | `make_node_topology()`, for one row per flowline connected through the points where flowlines meet (nodes) | `nonunique_id_with_toid` |
| more than one `downmain` per `id` | a flowline has more than one main downstream path: more than one of its rows has `downmain` TRUE | the rows with `downmain` TRUE and a repeated `id` | correct `downmain` so each `id` has one main downstream connection | `flownetwork_multiple_downmain` |
| more than one `upmain` per `toid` | a flowline receives more than one main upstream path: more than one row pointing to it has `upmain` TRUE | the rows with `upmain` TRUE and a repeated `toid` | correct `upmain` so each `toid` has one main upstream connection | `flownetwork_multiple_upmain` |
| `id` and `toid` of incompatible types (R, from vctrs) | the reserved outlet value takes the type of `id` (`""` where `id` is text, 0 otherwise), and `toid` has a type that value cannot be written into | the rows with a missing `toid` | convert `id` and `toid` to integer or text before the call | `outlet_fill_type_mismatch` |
| no `id` column (R warning, from tibble; Python error, D3-130) | no input column is `id` or one of its aliases, so the catchments or flowlines have no identifier | all | rename the identifier column to `id`, or register its name as an alias of `id` with `hydroloom_names()` | `no_id_column` |
| identifier column of mixed types (Python, I-3) | one identifier column holds both strings and numbers | the rows whose type differs from the first value's | cast the column to one type before `as_hy()` | `identifier_mixed_type` |
| non-whole floating-point identifier (D3-07) | an identifier column arrives as floating-point numbers and a value has a fractional part, so it cannot be read as a whole-number identifier without changing the value | the row positions whose value has a fractional part | correct the values, or cast the column to text, before `hy()` | `identifier_not_whole` |
| infinite floating-point identifier (I2-3) | an identifier column arrives as floating-point numbers and a value is `Inf` or `-Inf`, which is not a whole number and whose text form would make a different identifier of it | the row positions holding an infinite value | correct the values before `hy()` | `identifier_not_finite` |
| flag column not coded 0/1 (warning, tolerated) | a flag column arrives as numbers and a value is not 0, 1, or missing, so it cannot be read as true and false without changing a value; `hy()` leaves the column as it arrives | the rows, by `id`, whose value is not 0, 1, or missing | recode the values to 0 or 1 before `hy()` | `flag_value_not_binary` |
| missing `divergence` under `add_topo` (R) | `add_topo = TRUE` keeps only the main path at each divergence, and `divergence` is missing on a flowline, so the main path cannot be told from a minor one there | the rows, by `id`, with a missing `divergence` | `add_divergence()`, given the coastal and inland outlets, to compute `divergence` | `divergence_missing` |
| split with no main path or more than one under `add_topo` (R) | at a split every downstream flowline is coded 2, leaving no main path, or two or more are not coded 2 | the downstream flowlines at those splits, by `id` | `add_divergence()`, given the coastal and inland outlets, to recompute `divergence` | `divergence_coding_invalid` |
| main path coded 0 under `add_topo` (R warning, tolerated) | at a split the one downstream flowline not coded 2 is coded 0, the code for a flowline not below a divergence; it is read as the main path | the flowlines coded 0 at a split, by `id` | `add_divergence()`, given the coastal and inland outlets, to recompute `divergence` | `divergence_zero_main_path` |
| lone minor path under `add_topo` (R warning, tolerated) | a flowline coded 2 is the only flowline leaving the point where the flowline above it ends, a shape clipping leaves; it is connected to so the flowline above is not left a terminal | the lone flowlines coded 2, by `id` | `add_divergence()`, given the coastal and inland outlets, to recompute `divergence` on the network as clipped | `divergence_minor_only_path` |
| repeated (`id`, `toid`) pair (Python, S-6, D3-57) | two rows give one flowline the same downstream flowline | the repeated rows, by `id` | drop the repeated rows, keeping the one that is correct | `id_toid_pair_repeated` |

`remedies.json` is read in M6 (D3-153, D3-178), so these rows follow the dispositions where it lags: it lists `no_id_column` as a warning (Python raises, D3-130) and `outlet_fill_type_mismatch` for Python (R only, D3-105); `raised_by` of the four `divergence_*` keys omits `hy()`, which reaches them through `add_topo` (D3-173). The four keys `identifier_not_finite`, `flag_value_not_binary`, `divergence_zero_main_path`, and `divergence_minor_only_path` are agent-drafted and await the maintainer's read (D3-180), as does `id_toid_pair_repeated`, which lists Python alone until the R raise lands (D3-191).

## References

- R source: `R/hy.R` (`hy`), `R/align_names.R` (`align_names`, `hydroloom_names`), `R/hy_classes.R` (`classify_hy`, `new_hy_topo`, `new_hy_leveled`, `new_hy_node`, `new_hy_flownetwork`), `R/add_toids.R` (`add_toids.hy_node`), `R/utils.R` (`get_outlet_value`, `is_outlet`), `R/vocab.R` (`vocab_alias_map`, `vocab_good_names`, `vocab_id_names`, `vocab_flag_names`).
- Tests: `tests/testthat/test_hydroloom.R`, `tests/testthat/test_hy_classes.R`, `tests/testthat/test_outlet_conventions.R`.
- Vocabulary: `inst/extdata/hy_vocab.json` (terms, `type`, `aliases`; each alias's `source` field records the data model it comes from: `nhdplus` for NHDPlusV2 `comid`/`tocomid`, `nhdplushr` for `nhdplusid`, `3dhp` for `id3dhp`, `hydrosequence`, `dnhydrosequence`, and `uphydrosequence`).
- Source data models: McKay and others (2012), NHDPlus Version 2: User Guide; Moore and others (2019), User's Guide for the National Hydrography Dataset Plus (NHDPlus) High Resolution, USGS Open-File Report 2019-1096.
- Descriptions: `align_names.md` (step 2 in full), `add_toids.md` (the `add_topo` build and its divergence coding checks).
- SPEC I-1 to I-6, I-11, I-12, I-14, S-2, S-6, A-1, A-2, A-5, A-9, A-16, A-17, G-14; AGENTS.md "S3 Class Hierarchy".

## Worked example

Fixture `fixtures/hy_worked_example.gpkg`, aspatial, three rows, `COMID` and `ToCOMID` stored as REAL:

| COMID | ToCOMID | Notes |
| --- | --- | --- |
| 101 | 102 | a |
| 102 | 103 | b |
| 103 | NULL | c |

`hy(x)` returns:

| id | toid | Notes |
| --- | --- | --- |
| 101 | 102 | a |
| 102 | 103 | b |
| 103 | 0 | c |

`id` and `toid` are `integer` (R) and `str` (Python, with `""` in place of `0` on the last row); `Notes` keeps its name; R class `hy_topo`, `orig_names` `c(COMID = "id", ToCOMID = "toid", Notes = "Notes")`, `dendritic` `TRUE`. The case is `hy_worked_example`.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording R 1.3.0 behavior. Intended change I-2 (ROADMAP §4, M3) is stated as post-change behavior in Algorithm step 5, step 6 (`0L`), Postconditions, and Edge cases; the pairing of `id` with `toid` and `fromnode` with `tonode` in one type is this description's reading of I-2. A-9 gap: A-16 lists `as_hy(network, *, id_col=None, toid_col=None)`; the R signature `hy(x, clean = FALSE, add_topo = FALSE)` is recorded, `clean` and `add_topo` are A-5 omissions, and `id_col`/`toid_col` are Python-only. Language difference, not a change: R renames the geometry column to `geom` (`hy_reverse` restores it); Python keeps its name (G-14). The A-10 condition-class change does not apply: `hy` raises no representation error.
- 2026-10-26, suite 0.1.0, review fix round: step 2 tie rules brought into line with `align_names.md` as probed (D3-24): `permanent_identifier` drops out when any hy name has several candidates, and three or more candidates stop (`name_alias_ambiguous`, D3-60). The outlet-fill error key is `outlet_fill_type_mismatch` (D3-60). The `dendritic` attribute and the S-6 pass-through and real-network cases are stated; the duplicated-row case is captured as R-only (D3-46, D3-57).
- 2026-10-27, suite 0.1.0, M3 close: D3-07 (ROADMAP §4 identifier-ingest row, I-2): a non-whole floating-point identifier raises `identifier_not_whole` in R and Python (step 5, Preconditions, Postconditions, Edge cases, Remedies); factor and logical identifiers still pass through. M1 M-18 (S-2; no ROADMAP §4 row yet): flag columns coded 1/0 become `logical` in R and boolean in Python on ingest (Signature, step 5, Postconditions, Edge cases); the `hy_reverse` round-trip Postcondition now requires `logical` flags. D3-130: Python raises `RepresentationError` on a table with no `id` column, so the 2026-10-25 line's "no representation error" holds for R only. D3-16 and D3-64 (`hy_reverse.hy_flownetwork`): the repeated-`id` Edge case states the post-change `hy_reverse()` and `is.hy()` results. D3-65: a registered alias is lowercased on registration (step 2). D3-146: Remedies texts aligned with `remedies.json`. H-12: "terminal" for a row whose `toid` is not an `id`.
- 2026-10-28, suite 0.1.0, M3 close unit 2: H-01 (Edge cases, flag bullet: `terminal_flag` is not reconciled with I-4), H-05 (Remedies, `no_id_column` fix adds the `hydroloom_names()` alias route), H-06 (Signature, flag paragraph: Python boolean is `bool` or nullable `boolean`).
- 2026-10-28, suite 0.1.0, M3 close-out run 2 (rule 2 sweep): "post-change", "current R", and "today" wording removed where the change has landed, the behavior stated in present tense (D3-159, D3-172); Signature, step 5, Postconditions, Edge cases, and the Worked example now say what R does. I2-3: an infinite identifier raises `identifier_not_finite` in R and Python, after the fractional check, naming row positions; `NaN` is missing (Preconditions, step 5, Postconditions, Edge cases, Remedies). D3-152: a numeric flag column holding a value other than 0, 1, or missing warns `flag_value_not_binary`, naming rows by `id`, and is left as it arrives; stated as tolerated under Edge cases only (rule 5), and case `hy_flag_value_not_binary` asserts the warning only (D3-182). hy H-02: the missing-flag branch has case `hy_flag_missing` (D3-177). D3-56, D3-173: through `add_topo`, a row with a missing `tonode` keeps its input position (step 8, Postconditions on row order). D3-72, D3-158, D3-174: through `add_topo` with `divergence`, the `add_toids` coding stops and the two tolerated-coding warnings apply (Preconditions, step 8, Remedies). D3-57, D3-166: Python `as_hy` raises on a repeated (`id`, `toid`) pair; R follows in M6 (Preconditions, Edge cases, Remedies, no key yet). add_topo_sort H-04: 3DHP `hydrosequence` becomes `topo_sort` (Edge cases, References). D3-163, D3-185, D3-182, D3-171: Edge cases say which columns the real-network layers store, that `hy_new_hope_add_topo_no_divergence` and `hy_duplicate_alias_tie` assert their warnings only, and that the stored flag type is compared. D3-146, D3-180: Remedies rows match `remedies.json` keys and kinds, with rows for the drafted keys `hy()` raises or reaches through `add_topo` and a note on where the catalog lags the dispositions (D3-153).
- 2026-10-29, suite 0.1.0, M3 close run 3: D3-184, 3DHP `dnhydrosequence` and `uphydrosequence` are `dn_topo_sort` and `up_topo_sort` aliases; the `new_hope_3dhp.gpkg` bullet and the vocabulary reference list them. The `hy_new_hope_3dhp` layer stores `id` and `toid` only and does not move.
- 2026-10-29, suite 0.1.0, M3 close run 3: I23-2, the `flag_value_not_binary` warning is raised once per column per call to any function, not once per internal `hy()` call (Edge cases, flag bullet).
- 2026-10-29, suite 0.1.0, M3 close run 3, D3-191: the repeated (`id`, `toid`) raise cites the agent-drafted key `id_toid_pair_repeated` (Preconditions, Edge cases, Remedies row and note).
- 2026-10-29, suite 0.1.0, M3 close run 3, D3-192: case `hy_new_hope_add_topo_no_divergence_rows` compares the row count and order of `add_topo = TRUE` without `divergence` (Edge cases).
- 2026-10-29 (M3 close run 3, reader round D3-201): H-01 (Remedies, `name_alias_ambiguous` applies after the `permanent_identifier` rule), H-02 (Edge cases and step 5, factor `id` under the pair rule and the fill), H-03 (Remedies, `outlet_fill_type_mismatch` fix converts to integer or text), H-04 (step 8 and the row-order Postcondition, copies in the input order of the downstream flowlines, D3-37), H-05 (Edge cases, `nhdplushr.gpkg` nodes become `character`), H-07 (Edge cases, 3DHP `toid` built from `dnhydrosequence` when the fixture was made).
- 2026-10-30, suite 0.1.0, M3 close-out run 4, D3-214: the Signature identifier paragraph defers the factor and logical pass-through to the step-5 pair rule.
