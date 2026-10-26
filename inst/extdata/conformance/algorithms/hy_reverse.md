---
requires:
  # Narrowed to what R accepts, because the A-4 code table reads this field
  # (D3-42). R runs on any object that inherits `hy` (hy_topo, hy_leveled,
  # hy_node, and a plain hy with neither toid nor nodes, which has no A-2
  # pattern name) and stops on hy_flownetwork, which does not inherit `hy`
  # (D3-16). Renaming reads names, not topology; Python needs nothing on the
  # frame (I-14) and would accept a flownetwork too.
  pattern: [hy_topo, hy_node]
  columns: []
  via: {}
---

# hy_reverse

## Signature

R: `hy_reverse(x)`. Python: `hy_reverse(network, source=None)` (SPEC A-16; ROADMAP §2 naming row).

| argument | type | default | language |
| --- | --- | --- | --- |
| `x` / `network` | table with hydroloom names, as returned by `hy()` / `as_hy()` | none | both |
| `source` | `text`, one of the S-2 alias sources `nhdplus`, `nhdplushr`, `3dhp`, `hydrofabric`, or `None` | `None` | Python only, appended (A-9) |

Returns the same table, same class as the input after its hydroloom subclasses are removed (`sf`, `tbl_df`, `data.frame`; GeoDataFrame or DataFrame in Python, A-8), with column names mapped back to the names the user supplied. No column is added, dropped, or retyped. The function names no kernel (K-6).

## Preconditions

R: `x` must pass `is.hy(x)`, which needs the `hy` class and the `orig_names` attribute. An object that fails stops with the `not_hy_object` remedy, after `is.hy()` emits its own message for the missing class or attribute. A table that `hy()` routed to `hy_flownetwork` (repeated `id` with `toid`, I-6) does not inherit `hy` and so stops here, although it carries `orig_names`; `requires.pattern` lists `hy_topo` and `hy_node` for that reason, and a plain `hy` table (an `id` with neither `toid` nor `fromnode`/`tonode`) also passes although A-2 gives it no pattern name. Once the A-10 change lands, the stop carries the condition class `hydroloom_representation_error`.

Python: any frame. No state rides on the frame (I-14), so the inverse mapping comes from `source` or from `register_names()`. An unknown `source` raises `KeyError` with the `unknown_alias_source` remedy (A-10, vocabulary problems). Until the naming block of M5 (ROADMAP M5 "Do not touch"), only the private `_hy_reverse(out, names)` exists, taking the local mapping `as_hy` returned (A-17).

## Definition

The output is the input with every column that `hy()` renamed carrying its original name again, and nothing else changed. In R the original names are the names of the `orig_names` attribute, whose values are the hydroloom names `hy()` assigned. A column whose current name is a value of `orig_names` takes the matching name; a column absent from `orig_names` (one a hydroloom function added after `hy()`, such as `toid` from `add_toids()` or `topo_sort`) keeps its hydroloom name. In Python the mapping is the inverse of the aliases tagged with `source` in the vocabulary, or of the mapping given to `register_names()`, or the `names` mapping passed to `_hy_reverse`.

## Algorithm

1. R: check `is.hy(x)`; stop with `not_hy_object` if it fails.
2. Read `orig_names`, then remove the attribute from `x`.
3. For each column of `x` in its current order, look up its name among the values of `orig_names`; where found, replace it with the corresponding original name. Unmatched columns keep their names. Column order and row order are untouched (D-14).
4. Remove every class in `hy`, `hy_topo`, `hy_leveled`, `hy_node`, `hy_flownetwork` and the `dendritic` attribute.
5. If `x` is `sf`, set `sf_column` to the original name of the geometry column and re-stamp with `st_sf()`.

Python: build the inverse map (hydroloom name to source alias) from the vocabulary for `source`, or from the registry, and rename with `DataFrame.rename(columns=...)`, returning a new frame (A-8). Geometry follows `GeoDataFrame.rename`, which keeps the active geometry when the column is renamed.

Tie rules: none apply. Each hydroloom name maps to at most one original name, because `hy()` renames one input column to each hydroloom name; in the vocabulary each term carries at most one alias per source tag (S-2). Nothing is sorted, so D-1 and D-3 do not apply; no sentinel (K-4); everything is above the kernel boundary (K-1) and no CSR index is built.

## Postconditions

- Row count and row order equal the input's (D-14, D-7).
- Column count equals the input's; every column's values are identical to the input's, type included.
- Each column whose name is a value of `orig_names` (R) or a key of the inverse map (Python) is renamed to the matching original name; every other column keeps its name.
- The output inherits none of `hy`, `hy_topo`, `hy_leveled`, `hy_node`, `hy_flownetwork` and has no `orig_names` or `dendritic` attribute (R).
- An `sf` input gives an `sf` output whose `sf_column` is the original geometry name (R); a GeoDataFrame input gives a GeoDataFrame with the same active geometry (Python).
- Round trip, R: for a tibble or `sf` tibble `y` with no missing `toid` and identifier columns whose type `hy()` leaves alone, `hy_reverse(hy(y))` equals `y`. A `toid` filled on ingest (I-5) is not restored to missing.
- Round trip, Python: for any frame `df` and source `s`, `as_hy(hy_reverse(as_hy(df), source=s))` has the same columns and values as `as_hy(df)`.
- The input is not modified (A-8).

## Complexity

Time O(c) in the number of columns c for the rename, plus O(n) for the `st_sf()` re-stamp of an `sf` input. Space O(c); columns are not copied.

## Edge cases

One flowline: returns the row with its names restored; on `single_flowline.gpkg`, whose columns already carry hydroloom names, the rename is the identity and the row comes back unchanged. No connections (all terminals): returns the rows with their names restored, and each terminal `toid` comes back as it arrived; on `all_terminals.gpkg` the names are already hydroloom names, so the rows come back unchanged. The renaming path is shown by `hy_reverse_renamed` and the `new_hope` cases. Zero rows: returns zero rows with the original names and column types (I-11). Duplicated `id` with `toid`, either a repeated row or a divergence written as one row per downstream flowline: R stops with `not_hy_object`, because `hy()` returned an `hy_flownetwork` (D3-16). The names can still be restored by hand from `attr(x, "orig_names")`, whose names are the original names and whose values are the hydroloom names. Python restores the vocabulary's alias for `source`, which is lowercase (`comid`, not `COMID`), not the case the user supplied, and leaves a column whose alias carries no source tag (`reachcode`, `ftype`, `fcode`) at its hydroloom name under every `source`; `register_names()` is the route to the exact original names. A data frame passed to `hy()` comes back from `hy_reverse()` as a tibble. With `hy(clean = TRUE)` the dropped columns stay dropped and the geometry is gone. With `hy(add_topo = TRUE)` the added `toid` keeps its hydroloom name and `add_toids()` has already moved `fromnode` to the end of the table; `hy_reverse()` restores names, not order. A column renamed or removed between `hy()` and `hy_reverse()` keeps whatever name it has. Missing weight, zero-length line, and empty name do not apply.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| input is not an hy object (R) | the table did not come from `hy()`, or `hy()` found flowlines repeated at divergences (a flownetwork) and returned an `hy_flownetwork` | all | call `hy_reverse()` on the object `hy()` returned | `not_hy_object` |
| unknown alias source (Python) | `source` is not one of the vocabulary's alias sources | none | pass one of `nhdplus`, `nhdplushr`, `3dhp`, `hydrofabric`, or register the mapping with `register_names()` | `unknown_alias_source` |

## References

- `R/hy.R` (`hy_reverse()`, `hy()`, `is.hy()`); `R/00_hydroloom.R` (`hy_classes`); `R/align_names.R`.
- `inst/extdata/hy_vocab.json`, the `aliases` and their `source` tags (S-2).
- SPEC I-5, I-6, I-14, A-4, A-5, A-8, A-9, A-16, A-17.
- USGS, USGS hydrofabric data theme, section "Hydrofabric datasets", which describes NHDPlusV2, NHDPlusHR, and 3DHP, the datasets whose attribute names the `nhdplus`, `nhdplushr`, and `3dhp` alias sets restore: https://water.usgs.gov/themes/hydrofabric/.

## Worked example

Fixture `fixtures/hy_reverse_renamed.gpkg`, aspatial, three rows:

| ID | toID | extra |
| --- | --- | --- |
| 1 | 2 | a |
| 2 | 3 | b |
| 3 | missing | c |

`hy()` maps `ID` to `id` and `toID` to `toid`, fills the missing `toid` with 0 (I-5), and leaves `extra`. `hy_reverse(hy(x))` returns a tibble:

| ID | toID | extra |
| --- | --- | --- |
| 1 | 2 | a |
| 2 | 3 | b |
| 3 | 0 | c |

## Changes

- 2026-10-25, suite 0.1.0: first description; records hydroloom 1.3.0 behavior. A-10 (ROADMAP §4, M3): the `not_hy_object` stop gains the class `hydroloom_representation_error`. I-2 (ROADMAP §4, M3): `hy()` turns whole-valued double identifiers into `integer`, so after it lands the round trip returns `integer` where a double arrived. Signature gap (A-9): R takes `x` and no `source`; Python's `network` follows the A-9 exemption and `source` is a Python-only argument appended after it.
- 2026-10-25, suite 0.1.0: review round (H-01 to H-08). `requires.pattern` narrowed from `[any]` to `[hy_topo, hy_node]`, what R accepts (D3-16, D3-42); edge cases, the `not_hy_object` remedy, and References revised; a divergence case added.
