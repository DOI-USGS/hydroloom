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
aliases and values the hydroloom names, in registry order; `NULL` when the registry is
empty. It carries no `hydroloom_names` print class (that class belongs to
`hydroloom_name_definitions`). As an expectation layer (S-7) the vector is a `frame` of
two `text` columns, `name` (the alias) and `value` (the hydroloom name), one row per
entry, in registry order.

Argument and return prose come from roxygen (A-13). No kernel is involved (K-6).

## Preconditions

None on a network; the function takes none (`requires.pattern: [none]`). `x`, when given,
must carry names; an `x` with no names at all stops with `unnamed_name_mapping` and the
registry is left as it was. A partly named `x` passes the check.

## Definition

The alias registry is a session-scoped list of (alias, hydroloom name) pairs that
`align_names()`, and through it `hy()`, uses to rename input columns. At package load it
holds one pair for every alias of every vocabulary column term in `hy_vocab.json`
(concept entries excluded), in the file's term order and, within a term, its alias order.
A call with no `x` and `clear = FALSE` returns the registry unchanged. A call with `x`
returns, and stores, the prior registry followed by `x`; with `clear = TRUE` the prior
registry is first replaced by an empty one.

## Algorithm

1. Read the registry from the package environment.
2. If `x` is given and has no names, stop (`unnamed_name_mapping`).
3. If `clear`, store an empty registry.
4. If `x` is `NULL`, return the registry as it now stands.
5. Otherwise append `x` to the registry with `c()`, store the result, and return it.

No sort and no tie rule: order is the vocabulary file's order followed by call order
(D-15, the input order is defined, not hashed). Appending neither deduplicates nor
validates; a repeated alias or a value that is not a hydroloom name is kept as given.
No sentinel and no CSR index; nothing lies below the kernel boundary (K-1).

## Postconditions

- The return is `NULL` or a character vector whose `names` attribute is not `NULL`.
- After the call, the stored registry equals the return value.
- With `x = NULL` and `clear = FALSE`, the registry before and after the call are equal.
- With `x` given and `clear = FALSE`, the return is the prior registry followed by `x`:
  length is prior length plus `length(x)`, and the leading elements equal the prior
  registry element for element.
- With `x` given and `clear = TRUE`, the return equals `x` coerced to character.
- With `x = NULL` and `clear = TRUE`, the return is `NULL`.
- In the registry at load, no alias repeats, every alias is lowercase, and every value is
  a vocabulary column term.
- An unnamed `x` stops, and the registry after the stop equals the registry before it.

## Complexity

Time and space O(k + j), k the registry length and j the length of `x`; `c()` copies the
registry on each append. Network size (n, m) does not enter.

## Edge cases

The function reads no network, so one flowline, no connections, a missing weight, and a
zero-length line do not apply. A duplicated alias is appended, not merged; both entries
stay, and `align_names()` then warns about duplicate names when a table carries that
column. An empty name in a partly named `x` is registered as `""` and never matches a
column. An alias with uppercase letters is registered as given and never matches, because
`align_names()` lowercases column names before looking them up. A value that is not a
hydroloom name is registered; `align_names()` renames the column and then restores its
original name, so it has no effect. A non-character `x` is coerced to character by `c()`.
`clear = TRUE` with no `x` empties the registry, including the vocabulary aliases, so
`hy()` renames nothing until the package is reloaded. Registrations last until R restarts.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |
| `x` has no names | the mapping gives hydroloom names without the input column names they apply to | every element of `x` | `hydroloom_names(c(<input column> = "<hydroloom name>"))` | `unnamed_name_mapping` |

## References

- `R/align_names.R` (`hydroloom_names`, `align_names`), `R/vocab.R` (`vocab_alias_map`),
  `R/zzz.R` (`.onLoad` seeds the registry), `R/00_hydroloom.R` (`hydroloom_env`).
- `inst/extdata/hy_vocab.json`, the `aliases` field of each term (SPEC S-2, S-3).
- `tests/testthat/test_align_names.R`, `tests/testthat/test_vocab.R`.

## Worked example

No fixture table is read. Starting from a cleared registry:

```r
hydroloom_names(clear = TRUE)            # NULL
hydroloom_names(c(flowline_id = "id"))   # c(flowline_id = "id")
hydroloom_names(c(next_id = "toid"))     # c(flowline_id = "id", next_id = "toid")
```

A table with columns `flowline_id`, `next_id`, `other` then aligns to `id`, `toid`,
`other`. The conformance case reads the registry at load instead, so its expectation is
the vocabulary's alias table as two columns, `name` and `value`, 52 rows as of
`hy_vocab.json` at M1; the first row is `id3dhp`, `id`.

## Changes

- 2026-10-25, suite 0.1.0: first description; records R behavior with the registry
  seeded from `hy_vocab.json` (M1). No ROADMAP §4 change applies. A-9 gap: SPEC A-5 and
  A-16 give Python `register_names(mapping, *, replace=False)`, returning nothing and
  usable as a context manager, where R is `hydroloom_names(x = NULL, clear = FALSE)`
  returning the registry; this description records R's signature.
