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

Python: none on representation; `requires` is `[any]`. The pattern is computed from the
names and identifiers `as_hy` gives the input, so Python raises what `as_hy` raises on
the same input (Remedies) and nothing else.

## Definition

Roxygen: returns the most specific hydroloom class. The class names map to database /
graph representation patterns: `"hy_leveled"`, enriched edge list (+ topo_sort,
levelpath); `"hy_topo"`, self-referencing edge list (id/toid, dendritic); `"hy_node"`,
fromnode/tonode graph (id/fromnode/tonode); `"hy_flownetwork"`, junction table
(id/toid/upmain/downmain); `"hy"`, name-aligned base object.

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
2. `id` and `toid` present and `id` repeated: `"hy_flownetwork"`;
3. `id`, `fromnode`, and `tonode` present and `id` unique: `"hy_node"`;
4. otherwise: `"hy"`.

`toid` outranks the node columns: a table with `id`, `toid`, `fromnode`, and `tonode`
is `"hy_topo"`. Values other than `id` are not read; a missing `toid` does not change
the pattern.

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
and whether `id` repeats, not on row order (D-7), and on nothing random (D-15). No CSR
index is built.

## Postconditions

- The result is a single non-missing string, one of `"hy_leveled"`, `"hy_topo"`,
  `"hy_node"`, `"hy_flownetwork"`, `"hy"`, or, in R only, `"not a hydroloom object"`.
- The result is unchanged by any permutation of the input rows (D-7).
- R: `hy_network_type(hy(x))` is never `"not a hydroloom object"` for a table `x`.
- R: `hy_network_type(hy(x))` equals Python `representation(x)` for every table `x` that
  `hy()` accepts.
- `"hy_leveled"` only if `id`, `toid`, `topo_sort`, `levelpath`, and
  `levelpath_outlet_id` are present and `id` is unique (R: after `hy()`).
- `"hy_flownetwork"` if and only if `id` and `toid` are present and `id` repeats (R:
  after `hy()`).
- `"hy_node"` only if `toid` is absent and `id`, `fromnode`, `tonode` are present with
  unique `id` (R: after `hy()`).
- Python: `representation(as_hy(network)) == representation(network)`.
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
  the function does not raise on it.
- Repeated `id` with `fromnode` and `tonode` and no `toid`: `"hy"`, since neither the
  flownetwork nor the node branch applies.
- No `id`, or `id` with neither `toid` nor both node columns: `"hy"`.
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
| alias beside its hy name (Python) | two columns would carry one hy name | none; columns named in the message | rename or drop one of the two columns | `name_alias_conflict` |
| several aliases for one hy name (Python, warning) | the latest alias in map order was used | none; columns named in the message | drop the unwanted columns, or register the intended mapping with `register_names()` | `name_alias_duplicate` |
| identifier column of mixed types (Python, I-3) | one column holds both strings and numbers | rows whose type differs from the first value's | cast the column to one type | `identifier_mixed_type` |

R raises and warns on nothing. Python's rows are the conditions `as_hy` raises in name
alignment and identifier typing (`hy.md` Remedies); the keys are `hy`'s, not new ones.

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
