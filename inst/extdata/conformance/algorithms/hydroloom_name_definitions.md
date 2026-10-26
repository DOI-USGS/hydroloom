---
requires:
  pattern: [none]
  columns: []
  via: {}
---

# hydroloom_name_definitions

## Signature

R `hydroloom_name_definitions`, an exported object, not a function: it takes no
arguments and is read by name. Python `name_definitions()` (ROADMAP §2, A-16), a function
of no arguments.

Return in R: a named `text` vector with class `c("hydroloom_names", "character")`, one
element per vocabulary column term; element names are the hydroloom names and values are
their definitions. The class exists for its print method. Python returns a frame of two
`text` columns, `name` and `definition`, one row per element in the same order.

As an expectation layer (S-7) the return is a `frame` of the same two `text` columns,
`name` and `definition`, one row per element, in vector order. Neither column name is a
vocabulary term, so the harness applies no coercion beyond text.

Return prose comes from roxygen (A-13). No kernel is involved (K-6).

## Preconditions

None on a network; nothing is passed (`requires.pattern: [none]`). The vocabulary file
`inst/extdata/hy_vocab.json` must be readable and every column term in it must carry a
`definition` (S-2 makes the field required). Nothing is raised by the object itself; a
missing or malformed vocabulary file stops package load, in R in `.onLoad`, before the
object exists.

## Definition

The return lists every hydroloom column name with its definition: for each term in
`hy_vocab.json` that is not a concept entry (`concept: true`), one element whose name is
the term key and whose value is that term's `definition` field, unchanged. Order is the
file's term order. The definitions are the vocabulary's maintainer-authored text (S-2);
this description cites them by term and does not restate them.

## Algorithm

1. Read `hy_vocab.json` once (S-3; R caches it in the package environment).
2. Drop concept entries.
3. For each remaining term, in file order, take its `definition` field; name the result
   by the term key.
4. R: add the `hydroloom_names` class and assign the vector into the package namespace
   in `.onLoad`, so the value is fixed for the session. Python: build the frame on call.

No sort and no tie rule: order is the vocabulary file's order (D-15; the order is
defined by the file, not by hashing or dict iteration of unsorted input). Output row
order follows that order (D-14). No sentinel and no CSR index; nothing lies below the
kernel boundary (K-1).

## Postconditions

- The return has one element (R) or row (Python) per non-concept term of
  `hy_vocab.json`, 46 at vocabulary version 1.0.0.
- Names, in order, equal the vocabulary's column-term keys in file order, which in R is
  `vocab_good_names()`.
- Names are unique, non-empty, and lowercase snake_case.
- Each value equals, character for character, the `definition` field of the term its
  name points to.
- No value is missing or empty.
- No concept entry (`mainstem`) appears.
- The return does not depend on the alias registry: `hydroloom_names()` registrations,
  including `clear = TRUE`, leave it unchanged.
- R: the class is `c("hydroloom_names", "character")`.

## Complexity

Time and space O(t), t the number of vocabulary terms; in R the work is done once at
package load. Network size (n, m) does not enter.

## Edge cases

The return reads no network, so one flowline, no connections, a duplicated row, a
missing weight, a zero-length line, and an empty name do not apply. A column term
without a `definition` stops R package load (`vapply` requires one string per term); the
vocabulary schema test rejects such a file first. In R, subsetting with `[` drops the
`hydroloom_names` class, so a subset prints as a plain named character vector; the
print method numbers each element and puts its definition on the next line, which is
R-local presentation and not contract. Modifying a copy of the R object does not change
the package's copy.

## Remedies

none

## References

- `inst/extdata/hy_vocab.json` (each term's `definition` field) and
  `inst/extdata/hy_vocab.schema.json` (SPEC S-2, S-3).
- `R/vocab.R` (`hy_vocab`, `vocab_columns`, `vocab_name_definitions`), `R/zzz.R`
  (`.onLoad` assigns the object), `R/00_hydroloom.R` (placeholder and
  `print.hydroloom_names`), `R/align_names.R` (roxygen).
- `tests/testthat/test_align_names.R`, `tests/testthat/test_hydroloom.R` ("test print").

## Worked example

No fixture table is read. The first two elements, at vocabulary version 1.0.0:

| name | definition |
| --- | --- |
| `id` | the `definition` field of term `id` in `hy_vocab.json` |
| `toid` | the `definition` field of term `toid` in `hy_vocab.json` |

The conformance case's expectation is the full 46-row layer in file order, starting
`id`, `toid`, `fromnode`, `tonode`, `divergence`.

## Changes

- 2026-10-25, suite 0.1.0: first description; records R behavior with the object built
  from `hy_vocab.json` at load (M1). No ROADMAP §4 change applies. A-9 gap: R exports an
  object read by name where SPEC A-5 and A-16 give Python a function,
  `name_definitions()`, returning a frame; the description takes the frame's columns as
  exactly `name` and `definition`, which A-16 leaves unstated.
