---
requires:
  pattern: [none]
  columns: []
  via: {}
---

# hydroloom_names

## Signature

R `hydroloom_names(x = NULL, clear = FALSE)`; Python `register_names` (ROADMAP §2).

Arguments, from R (A-9):

- `x`: named `text` vector, default `NULL`. Each element's name is an input column name
  (an alias) and its value is the hydroloom name that alias renames to.
- `clear`: `flag`, default `FALSE`.

Return: a named `text` vector, the alias registry after the call, element names the
aliases and values the hydroloom names, in registry order. It carries no `hydroloom_names` print class (that class belongs to
`hydroloom_name_definitions`). As an expectation layer (S-7) the vector is a `frame` of
two `text` columns, `name` (the alias) and `value` (the hydroloom name), one row per
entry, in registry order.

Python (SPEC A-5, A-16; RULES B-4): `register_names(mapping, *, replace=False)`.
`mapping` is R's `x` as a mapping `{alias: hydroloom name}`, both `str`; `replace` is R's
`clear`. Python has no call with no mapping: R's `x = NULL` reads the registry, and that
reading is not part of the Python function (B-5).

- Scope: one registry per process, as in R. A registration is seen by every later call
  to `align_names`, `as_hy`, and `hy_reverse` in the process, from any module, until it
  is replaced or restored.
- Effect: the call registers at once. Each alias is lowercased on registration (D3-65)
  and appended after the existing entries in the mapping's iteration order. With
  `replace=True` the registered entries are dropped first and the vocabulary aliases
  stay, as with R `clear = TRUE` (D3-20).
- Return: a context manager, and nothing else; `with register_names(mapping) as r:`
  binds `None` to `r`. R returns the registry because a call with no `x` is how R reads
  it; Python's call always takes a mapping, so the return carries no registry (B-5), and
  the handle is what lets one call also be a `with` block (A-16, "none; also usable as a
  context manager"). Called outside `with`, the registration stays.
- Context manager: `with register_names(mapping):` restores the registry as it was on
  entry, that is before the call registered `mapping`, when the block exits, whether it
  exits normally or by an exception. Registrations made inside the block, `replace=True`
  among them, are undone with it. Nested blocks restore in reverse order.
- Validation: a value that is not a vocabulary column term (a hydroloom name) raises
  `KeyError` (`register_name_unknown`, A-10, a vocabulary problem) and leaves the
  registry as it was (B-4). So does an alias that, lowercased, is a hydroloom name other
  than its value (`register_alias_is_name`, B-1); an identity entry is accepted.

Argument and return prose come from roxygen (A-13). No kernel is involved (K-6).

## Preconditions

The function reads no network (`requires.pattern: [none]`). In R, an `x` with no names at
all stops with `unnamed_name_mapping`, and the registry is left as it was. Python's
`register_names()` takes a mapping, which always carries keys, so the key is R-only. A
partly named `x` passes the check. In R and Python, a value of `x` or `mapping` that is
not a vocabulary column term stops with `register_name_unknown`, giving the alias and the
value, and the registry is left as it was, `clear` or `replace` included: an entry that
can never rename a column is the silent no-op D3-65 fixed (B-4, B-3). In the same way,
an alias that, lowercased, is a hydroloom name other than its value stops with
`register_alias_is_name`, giving the alias and the value: registering `toid` as `id`, or
`id` as `toid`, would rename a column that already carries a hydroloom name to another
one with no warning (B-1). An identity entry, an alias equal to its value (`id` as `id`),
is accepted, since the vocabulary already holds identity pairs (`toid`, `fromnode`,
`tonode`, `divergence`) and such a pair renames nothing.

## Definition

The alias registry is a session-scoped list of (alias, hydroloom name) pairs that
`align_names()`, and through it `hy()`, uses to rename input columns. At package load it
holds one pair for every alias of every vocabulary column term in `hy_vocab.json`
(concept entries excluded), in the file's term order and, within a term, its alias order.
Most of those aliases are NHDPlusV2 attribute names in their 10-character dbf form
(`comid`, `tocomid`, `levelpathi`, `arbolatesu`). The rest come from NHDPlusHR
(`nhdplusid`), NHD (`permanent_identifier`, `reachcode`, `ftype`, `fcode`), 3DHP
(`id3dhp`, `hydrosequence`, `dnhydrosequence`, `uphydrosequence`), Mainstems
(`mainstemid`), and hydrofabric products (`reconciled_id`, `aggregated_id`), and a few are
identity pairs (`toid`, `fromnode`, `tonode`, `divergence`). A table whose columns use
other names needs a call to `hydroloom_names()` before `hy()` will recognize them. A call with no `x` and `clear = FALSE` returns the
registry unchanged. A call with `x` returns, and stores, the prior registry followed by
`x` with its names lowercased. With `clear = TRUE` the prior registry is first replaced
by the registry seeded at load, the vocabulary aliases alone, so registered mappings are
dropped and the vocabulary aliases stay.

A registered alias wins over a vocabulary alias for the same column, the vocabulary
alias of another term included: registering `comid` as `toid` makes a `COMID` column
`toid`, not `id` (`align_names.md` steps 2 and 5, B-4, G-11). In Python,
`hy_reverse(source=None)` restores the most recently registered alias for each
hydroloom name, lowercase as the registry holds it (`hy_reverse.md`, D3-65).

## Algorithm

1. Read the registry from the package environment.
2. If `x` is given and has no names, stop (`unnamed_name_mapping`).
3. If `x` is given and any value is not a vocabulary column term, stop
   (`register_name_unknown`). Otherwise, if any name of `x`, lowercased, is a vocabulary
   column term that differs from its value, stop (`register_alias_is_name`, B-1); an
   identity entry passes. Both checks run before `clear` is applied, so the registry is
   unchanged on either stop.
4. If `clear`, rebuild the vocabulary alias map (`vocab_alias_map()`, as at load) and
   store it as the registry.
5. If `x` is `NULL`, return the registry as it now stands.
6. Otherwise lowercase the names of `x` (D3-65), append `x` to the registry with `c()`,
   store the result, and return it.

Python runs steps 3, 4, and 6 on `mapping` with `replace` for `clear`, after taking a
copy of the registry for the context manager to restore, and returns the context
manager in place of the registry.

No sort and no tie rule: order is the vocabulary file's order followed by call order
(D-15, the input order is defined, not hashed). Appending does not deduplicate; a
repeated alias is kept as given.
No sentinel and no CSR index; nothing lies below the kernel boundary (K-1).

## Postconditions

- The return is a character vector whose `names` attribute is not `NULL`.
- After the call, the stored registry equals the return value.
- With `x = NULL` and `clear = FALSE`, the registry before and after the call are equal.
- With `x` given and `clear = FALSE`, the return is the prior registry followed by `x`
  coerced to character with its names lowercased: length is prior length plus `length(x)`, and the leading
  elements equal the prior registry element for element.
- With `x` given and `clear = TRUE`, the return is the registry at load followed by `x`
  coerced to character with its names lowercased; any mapping registered before the call
  is absent unless `x` repeats it.
- With `x = NULL` and `clear = TRUE`, the return equals the registry at load.
- In the registry at load, no alias repeats, every alias is lowercase, and every value is
  a vocabulary column term.
- An unnamed `x` stops, and the registry after the stop equals the registry before it.
- An `x` or `mapping` with a value that is not a vocabulary column term stops
  (`register_name_unknown`), and the registry after the stop equals the registry before
  it, with `clear = TRUE` or `replace=True` too. So every value in the registry is a
  vocabulary column term.
- An `x` or `mapping` with an alias that, lowercased, is a vocabulary column term other
  than its value stops (`register_alias_is_name`), and the registry after the stop equals
  the registry before it, with `clear = TRUE` or `replace=True` too. So every registry
  alias that is a hydroloom name maps to itself.
- Python: on exit from `with register_names(mapping):`, by any route, the registry equals
  the registry before the call.

## Complexity

Time and space O(k + j), k the registry length and j the length of `x`; `c()` copies the
registry on each append. Network size (n, m) does not enter.

## Edge cases

The function reads no network, so one flowline, no connections, a missing weight, and a
zero-length line do not apply. A duplicated alias is appended, not merged; both entries
stay, and `align_names()` counts them as two candidates when a table carries that column,
warning with `name_alias_duplicate` for two candidates of one hydroloom name and stopping
with `name_alias_ambiguous` for three or more (`align_names.md` steps 2 and 5, D3-24).
Because names are lowercased on registration (D3-65), an alias that lowercases to a
seeded alias repeats it: registering `COMID = "id"` as printed in the NHDPlusV2 User
Guide adds a second `comid` entry, and every NHDPlusV2 table then warns with
`name_alias_duplicate`. The restore is `hydroloom_names(clear = TRUE)` with no `x`, which
returns the registry to the one set at package load. Passing a saved registry back with
`clear = TRUE` repeats every vocabulary alias, since the reset already holds them, and
every table carrying a seeded alias then warns with `name_alias_duplicate` (H03-2).
Several seeded aliases of one term can also meet in one table: an NHDPlusHR flowline
table carries both `nhdplusid` and `permanent_identifier`, both aliases of `id`;
`align_names()` resolves that by its `permanent_identifier` rule (`align_names.md`
step 3, D3-24), so `nhdplusid` becomes `id` and `permanent_identifier` keeps its name; R also emits an informational message that has no `remedies.json` key
(as with D3-19).
An empty name in a partly named `x` is registered as `""` and never matches a
column. An alias with uppercase letters is registered in lowercase (`FlowlineID` as
`flowlineid`), so it matches the column, which `align_names()` also lowercases before
looking it up. A value that is not a hydroloom name stops with `register_name_unknown`
in R and Python. In R this is a named change (B-3): through hydroloom 1.3.x
`hydroloom_names()` registers such a value, and `align_names()` renames the column and
then restores its original name, so the entry has no effect; the stop lands in R in M5
run 3 unit 3 with a NEWS entry. An alias that is a hydroloom name other than its value
(`toid` as `id`) stops with `register_alias_is_name`, also a named change in R landing in
unit 3: through hydroloom 1.3.x R registers it, and a `toid` column then becomes `id`.
An identity entry (`id` as `id`) is accepted and renames nothing; where it repeats a
seeded identity pair (`toid` as `toid`) it is a duplicated alias and warns with
`name_alias_duplicate`, as above. A registered alias that is also the vocabulary alias of
another term (`comid` registered as `toid`) is a candidate for the registered name only
(`align_names.md` step 2). A non-character `x`
is coerced to character by `c()`.
`clear = TRUE` with no `x` drops every registered mapping and keeps the vocabulary
aliases, so `hy()` still renames the seeded columns. Python's
`register_names(replace=True)` does the same (D3-20). The registry, cleared or extended, returns to the vocabulary seed
the next time the package is loaded, which includes every R restart.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| `x` has no names | the mapping gives hydroloom names without the input column names they apply to | every element of the mapping | `hydroloom_names(c(<input column> = "<hydroloom name>"))` | `unnamed_name_mapping` |
| a registered value is not a hydroloom name (key drafted) | the mapping renames an alias to a name that is not a hydroloom name, so the entry could never rename a column | the aliases whose value is not a hydroloom name, with those values | map each alias to a hydroloom name before registering | `register_name_unknown` |
| a registered alias is a hydroloom name other than its value (key drafted) | the entry would rename a column that already carries a hydroloom name to a different one | the aliases that are hydroloom names other than their values, with those values | use the input column's own name as the alias, or drop the entry, before registering | `register_alias_is_name` |

`register_name_unknown` and `register_alias_is_name` are agent-drafted in
`remedies.json` for the M6 read, as `id_missing` was; `remedies.json` has no in-file
draft marker, so the drafts are recorded here. Each lists Python only until R raises it
in M5 run 3 unit 3, which adds `r` and the R text. Python raises both as `KeyError`
(A-10).

## References

- `R/align_names.R` (`hydroloom_names`, `align_names`), `R/vocab.R` (`vocab_alias_map`),
  `R/zzz.R` (`.onLoad` seeds the registry), `R/00_hydroloom.R` (`hydroloom_env`).
- `inst/extdata/hy_vocab.json`, the `aliases` field of each term (SPEC S-2, S-3).
- `tests/testthat/test_align_names.R`, `tests/testthat/test_vocab.R`.
- Source data models: McKay and others (2012), NHDPlus Version 2: User Guide, the
  NHDPlusV2 attribute names behind the `nhdplus` aliases; Moore and others (2019),
  User's Guide for the National Hydrography Dataset Plus (NHDPlus) High Resolution, USGS
  Open-File Report 2019-1096, behind the `nhdplushr` aliases.

## Worked example

No fixture table is read. With `v` the registry at load (55 entries):

```r
hydroloom_names(clear = TRUE)            # v
hydroloom_names(c(flowline_id = "id"))   # c(v, flowline_id = "id")
hydroloom_names(c(Next_ID = "toid"))     # c(v, flowline_id = "id", next_id = "toid")
hydroloom_names(clear = TRUE)            # v
```

After the third call, a table with columns `flowline_id`, `Next_ID`, `other` aligns to
`id`, `toid`, `other`; after the fourth it keeps those names. The conformance case reads the registry at
load, so its expectation is the vocabulary's alias table as two columns, `name` and
`value`, 55 rows as of `hy_vocab.json` after the 3DHP `dnhydrosequence` and
`uphydrosequence` aliases of `dn_topo_sort` and `up_topo_sort` (D3-184); the first row is `id3dhp`, `id`.

## Changes

- 2026-10-25, suite 0.1.0: first description; records R behavior with the registry
  seeded from `hy_vocab.json` (M1). No ROADMAP §4 change applies. A-9 gap: SPEC A-5 and
  A-16 give Python `register_names(mapping, *, replace=False)`, returning nothing and
  usable as a context manager, where R is `hydroloom_names(x = NULL, clear = FALSE)`
  returning the registry; this description records R's signature.
- 2026-10-25, suite 0.1.0: hydrology review round; Definition names the products the
  seeded aliases come from; Edge cases add seeded aliases meeting in one table, cite the
  `name_alias_duplicate` and `name_alias_ambiguous` keys, and state reload behavior once.
- 2026-10-27, suite 0.1.0: M3 close. D3-65: `hydroloom_names()` lowercases each alias on
  registration, with a NEWS line, so an alias given with uppercase letters matches instead
  of being a silent no-op; Definition, Algorithm step 5, Postconditions, and Edge cases
  state the behavior. ROADMAP §4 has no row for this change (a row is
  proposed). D3-146: the Remedies row's `which rows` matches `remedies.json`. D3-20: Python's
  `register_names(mapping, *, replace=False)` stays a gap against R's signature;
  `replace=True` replaces registered mappings only and never the vocabulary's aliases,
  because S-2 makes the vocabulary file the one copy and a runtime call that hid its
  aliases could not be seen by `capabilities()` or the harness. M3 H-05: References cite
  the NHDPlusV2 and NHDPlusHR user guides.
- 2026-10-28, suite 0.1.0: M3 close, unit 2. H-01: Edge cases state that an alias which
  lowercases to a seeded alias repeats it and warns `name_alias_duplicate`. H-02: Edge
  cases state that R `clear = TRUE` empties the vocabulary aliases and Python
  `replace=True` keeps them.
- 2026-10-28, suite 0.1.0: M3 close run 2. H-03 (with D3-20): R `clear = TRUE` resets
  the registry to the vocabulary aliases seeded at load, followed by `x`, in place of
  emptying it; an R change with a NEWS line and the ROADMAP §4 S-2 row, which also carries
  the D3-65 lowercasing. Signature drops the `NULL` return, since the registry always
  holds the vocabulary aliases; Definition, Algorithm step 3, the `clear = TRUE`
  Postconditions, and the worked example follow. H-02 restated: Edge cases say R
  `clear = TRUE` and Python `replace=True` agree, and that passing a saved registry back
  with `clear = TRUE` repeats the vocabulary aliases. H-04: the vocabulary gains 3DHP
  `hydrosequence` as a `topo_sort` alias, so the load-time registry and the
  `hydroloom_names_default` layer have 53 entries; Definition lists the alias.
- 2026-10-29, suite 0.1.0: M3 close run 3. D3-184: the vocabulary gains 3DHP
  `dnhydrosequence` and `uphydrosequence` as `dn_topo_sort` and `up_topo_sort` aliases,
  each after its NHDPlus alias, so the load-time registry and the
  `hydroloom_names_default` layer have 55 entries; Definition lists the aliases.
- 2026-10-29, suite 0.1.0, M3 close run 3, H03-2: Edge cases state
  `hydroloom_names(clear = TRUE)` with no `x` as the restore, and that passing a saved
  registry back with `clear = TRUE` repeats the vocabulary aliases, beside the H-01
  duplicated-alias sentence.
- 2026-10-29 (M3 close run 3, reader round D3-201): H-01, H-03, H-04, H-05.
- 2026-11-03, suite 0.1.0, M5 run 3 unit 2, REVIEW_11032026 item 4 (RULES B-4, B-5;
  G-7 to G-11): Signature gives the Python `register_names(mapping, *, replace=False)`
  contract: process-wide registry, aliases lowercased on registration (D3-65), a context
  manager as the return (R's registry return dropped under B-5), restore of the registry
  as it was before the call on exit from `with`, and a `KeyError` on a value that is not
  a hydroloom name. Preconditions, Algorithm step 3, Postconditions, Edge cases, and
  Remedies add the `register_name_unknown` stop in R and Python, with the key drafted
  for the M6 read; in R it is a named change (B-3) landing in M5 run 3 unit 3 with
  NEWS. Definition states that a registered alias wins over a vocabulary alias, the
  alias of another term included (G-11), and what `hy_reverse(source=None)` restores.
  Edge cases cite `align_names.md` step 3 for the `permanent_identifier` rule, which
  moved there from step 4 (item 1).
- 2026-11-03, suite 0.1.0, M5 run 3 unit 2 fix round. Hydro H-01 (RULES B-1, B-3,
  D3-65): an alias that, lowercased, is a hydroloom name other than its value stops with
  `register_alias_is_name` (key drafted, Python only until unit 3), since `toid`
  registered as `id` would make a `toid` column `id`; an identity entry is accepted
  because the vocabulary holds identity pairs. Signature, Preconditions, Algorithm step
  3, Postconditions, Edge cases, and Remedies follow; the registry is unchanged on the
  stop. Hydro H-02: the `register_name_unknown` message says "map to" in place of "are
  registered to", and its `condition` drops the anchors D3-65, B-4, and B-3, which a
  user cannot follow; that provenance (the D3-65 silent no-op, B-4, B-3) is recorded
  here, since the `remedies.json` schema has no field users do not read.
