---
requires:
  pattern: [any]
  columns: []
  via: {}
---

# is.hy

## Signature

R: `is.hy(x, silent = FALSE)`. Python: `is_hy(network, *, silent=False)` (ROADMAP §2; A-9
renames the first argument).

| argument | type | default |
| --- | --- | --- |
| `x` / `network` | any object; a table in practice | none |
| `silent` | logical / `bool` | `FALSE` / `False` |

Returns one value of vocabulary type `flag`: R `logical` of length one, never `NA`;
Python `bool`. The expectation is the S-7 scalar layer: one row, one column named
`value`.

Roxygen gives the argument and return prose: "test if object is a valid according to the
hy s3 class"; `x` "object to test"; `silent` "logical should messages be emitted?";
returns "logical TRUE if valid".

No kernel is involved (K-1, K-11).

## Preconditions

None. The function accepts any object, including `NULL`, a list, and a zero-row table,
and never raises. No representation check and no conversion is applied; `requires` is
`[any]`.

## Definition

R: `is.hy(x)` is `TRUE` when `x` carries the `hy` class and the `orig_names` attribute
that `hy()` attaches, and `FALSE` otherwise. It answers whether `x` is the output of
`hy()` (or of an operation that kept both markers, such as a dplyr `filter()` on an `hy`
object), not whether the columns are in hydroloom names. A table read from a file is
never `hy` in R, whatever its column names. Column values are not read: a missing `toid`
does not make a table invalid, because a row whose `toid` is not present in `id` is a
terminal (I-4, issue #85).

Three R outputs follow from the class rule. A table `hy()` classifies as
`hy_flownetwork` (repeated `id` with `toid`, I-6) is `FALSE`, because that class does not
inherit from `hy`. A table returned by any hydroloom function called on an `hy` object
is `FALSE`, because the `.data.frame` methods restore user names and strip the hy
classes. A table `hy()` produced with no `id` column is `TRUE`.

Python keeps no state on a frame (I-14), so it cannot test for `hy()` output. `is_hy`
answers the nearest question the frame itself can: whether `as_hy` would leave its
column names unchanged and its identifiers in their ingest type, and whether it is not
in the flownetwork pattern. That is `True` when all three hold:

1. no column name is a vocabulary alias or a registered name that `as_hy` would rename;
2. each of `id`, `toid`, `fromnode`, `tonode` that is present is a string column (I-1,
   I-2); a missing `toid` value is allowed, as in R;
3. `representation(network)` is not `hy_flownetwork` (A-2).

The two definitions agree on a table in source names (both `FALSE`) and on a flownetwork
pattern table (both `FALSE`). They differ on a table already in hydroloom names that was
not produced by `hy()` in the same R session: R returns `FALSE`, Python may return
`True`. Cases on such tables are R only (see Changes).

## Algorithm

R:

1. If `x` does not inherit from `"hy"`, emit the message "no hy class attribute" unless
   `silent`, and return `FALSE`.
2. If `"orig_names"` is not among `names(attributes(x))`, emit the message "no original
   names attribute" unless `silent`, and return `FALSE`.
3. Return `TRUE`.

The class test comes first, so an object missing both markers reports only the class
message. `silent` changes whether a message is emitted, never the value.

Python: evaluate the three conditions of Definition and return their conjunction.
Nothing is printed or logged (A-12), so `silent` is accepted for signature parity and
has no effect.

No tie rule, sentinel, or D rule applies: the output is one value and does not depend on
row order (D-7 holds trivially). No CSR index is built.

## Postconditions

- The result is a single non-missing logical (R) or `bool` (Python).
- The result is the same for `silent = TRUE` and `silent = FALSE`.
- The result is unchanged by any permutation of the input rows (D-7).
- R: `is.hy(hy(x))` is `TRUE` for every table `x` that `hy()` does not classify as
  `hy_flownetwork`.
- R: `is.hy(x)` is `FALSE` for every table read from a file without a call to `hy()`.
- R: the result is unchanged by setting any `toid` value to `NA` on an `hy` object.
- Python: `is_hy(network)` is `False` whenever `representation(network)` is
  `hy_flownetwork`.
- Python: `is_hy(as_hy(network))` is `True` whenever `as_hy(network)` is not in the
  flownetwork pattern.
- The input is not modified (A-8).

## Complexity

R: O(1) time and space; only the class and attribute names are read. Python: O(c) in
the number of columns for the name test plus the cost of `representation`, which is O(n)
for the `id` uniqueness test.

## Edge cases

- One flowline: R `TRUE` after `hy()`, `FALSE` on the raw table; no message when
  `silent`.
- No connections (all terminals): same as one flowline; `toid` values are not read.
- Divergence (repeated `id` with distinct `toid` values, as in `loop.gpkg`): the
  flownetwork pattern I-6 treats as a representation, not an error. R `FALSE` after
  `hy()`, since the result is `hy_flownetwork`, with the message "no hy class attribute"
  unless `silent`. Python `False` by condition 3.
- Duplicated row (the same `id`/`toid` pair listed twice, `duplicated_edge.gpkg`): S-6
  says this raises, which M3 does not apply (D3-46, D3-57). `is.hy` reads no rows and
  does not raise; `hy()` does not raise either and returns `hy_flownetwork`, so R is
  `FALSE` both as read and after `hy()`. Python `False` by condition 3.
- Missing `toid`: accepted; R `TRUE` on an `hy` object whose `toid` holds `NA`.
- Zero-row table (`empty_network.gpkg`): R `FALSE` as read, `TRUE` after `hy()`.
  Python may return `True` (I-11).
- Not a table (`NULL`, a list): `FALSE`, with the class message unless `silent`.
- Missing weight, zero-length line, empty name: not read; the result follows the class
  rule alone.

## Remedies

none

The function raises and warns on nothing. R's two `message()` texts, "no hy class
attribute" and "no original names attribute", are status messages outside A-10; Python
emits neither (A-12).

## References

- `R/hy.R`: `is.hy()`, and `hy()` for the `hy` class and `orig_names` attribute.
- `R/hy_classes.R`: `classify_hy()` and `new_hy_flownetwork()`, which strip the `hy`
  class from a flownetwork-pattern table; `hy_as_dataframe()`, which strips it from
  function outputs.
- `tests/testthat/test_hydroloom.R`, "s3 class creation".
- DOI-USGS/hydroloom issue #85, outlet detection and `NA` `toid`.
- SPEC I-1, I-2, I-4, I-6, I-14, A-2, A-8, A-9, A-10, A-12, A-16, S-7.

## Worked example

Fixture `fixtures/is_hy_source_names.gpkg`, an aspatial table in NHDPlusV2 names:

| comid | tocomid |
| --- | --- |
| 1 | 2 |
| 2 | 3 |
| 3 | NA |

`is.hy(x, silent = TRUE)` on the table as read is `FALSE`: it carries no `hy` class.
`is_hy(network)` is `False`: `comid` and `tocomid` are `nhdplus` aliases that `as_hy`
renames. Expectation layer `is_hy_source_names` holds one row, `value = FALSE`.

In R, `is.hy(hy(x))` is `TRUE` (`hy()` renames to `id`, `toid` and fills the `NA` with
`0`); case `is_hy_after_hy` records this path on `hy_toid_missing_with_nodes.gpkg`, run
through `hy()` by `prepare` (D3-84). Setting `toid[3]` back to `NA` keeps it `TRUE`; that value, on an
object altered after `hy()`, stays in testthat as an interface test.

## Changes

- 2026-10-25, suite 0.1.0: first description, recording hydroloom 1.3.0 behavior. Gap
  with SPEC A-16: the table gives `is_hy(network)`; R has `silent = FALSE`, which A-9
  keeps, and Python accepts it with no effect. The Python definition (name, identifier
  type, and pattern test) stands in for R's class test, which I-14 makes impossible in
  Python; cases on tables already in hydroloom names are `supported_by: [r]` only. No
  ROADMAP §4 change applies.
- 2026-10-26, suite 0.1.0: review round. Divergence and duplicated row separated in Edge
  cases; terminal for a `toid` outside `id`; the zero-row table and `loop.gpkg` gain
  cases. No behavior change.
- 2026-10-26, suite 0.1.0: remedies pass. Worked example states that the `TRUE` path
  after `hy()` is case `is_hy_after_hy` (D3-84). No behavior change.
