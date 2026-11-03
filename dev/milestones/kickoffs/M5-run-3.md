# Kickoff: M5, run 3

Base `a771b5a4c4d7266dd268153ee1867b3e777307a7` on `python-port`. Effort medium. Model claude-opus-5.5.

Written by the orchestrator from the `REVIEW_11032026` dispositions (all nine filled and
transcribed to `dev/plan/RULES.md` in `a771b5a`) and the open BACKLOG M5 rows (P-5). The
run carries the M5 deliverable (3) — `add_toids` — and the description, R, manifest, and
roxygen follow-ups the review dispositions call for. The effort sweep the M5 brief and the
run 2 kickoff name runs here: effort is medium, against run 1's and run 2's high, measured
on `add_toids` (unit 6).

Who does what. The orchestrator builds nothing (P-5). Units 1 and 4 are plan and R
documentation: `general-task-execution`, reviewed by `hl-style-reviewer` on the unit's
diff, one fix round. Unit 2 is description-side, reads `R/`: `general-task-execution`, a
reader round by `hl-hydro-reviewer` on the binding diff only, findings `record` (P-4,
frozen). Unit 3 is an R change (§4) with regenerated expectations: `general-task-execution`,
`hl-style-reviewer` on the diff, one fix round. Units 5 and 6 are `port-function` runs:
manifest marks by `general-task-execution` where needed, then `hl-implementer`,
`hl-spec-reviewer` and `hl-style-reviewer` on the diff, one fix round; unit 4's roxygen is
read by `hl-modeler-reader` after it composes. Every unit is one Log entry in
`dev/milestones/M5.md`, written by `hl-aggregator` in mode `record`.

## Units, in dependency order

1. Plan commit — the `plan:` commit the dispositions call for. SPEC S-4 gains the
   `r_only:` case field (item 9): a `[r]` case not R-only by design counts against parity,
   one carrying `r_only:` with its description-section anchor counts apart; the M5
   Done-when "the parity table shows the phase-1 functions as `r, python`" is reworded to
   read as P-5's parity line. RULES B-4 already holds the item-4 and item-6 text from
   `a771b5a`; this unit adds no RULES line. The M6 BACKLOG row carrying D3-57 gains the note
   item 3 requires: when R's `hy()` raises on the repeated (`id`, `toid`) pair,
   `hy_network_type_duplicated_edge` becomes a raise case under `hy` or is dropped with that
   regeneration (the row is added if none carries D3-57). The four BACKLOG M5 rows this run
   closes are left in place for the units that close them; no BACKLOG edit here beyond the
   M6 note (plan; RULES P-2).
2. Description corrections (description) — `align_names.md` steps 3 and 4 trade places so
   the `permanent_identifier` rule runs before the conflict check, as the code runs them
   (item 1, B-1, SP-02); the idempotence Postcondition there and in `hy.md` carries the
   two-way-tie exception (the losing alias sits beside the name it lost and a second pass
   raises `name_alias_conflict`). `hydroloom_names.md` gains the `register_names` Python
   signature, return value, context-manager semantics, process-wide scope, and validation
   (item 4, B-4): `with register_names(mapping):` restores the registry as it was on entry;
   a registered value that is not a hydroloom name raises, with a drafted `remedies.json`
   key for the M6 read; a registered alias wins over a vocabulary alias for the same column,
   the alias of another term included. `hy_reverse.md` says `source=None` restores the most
   recently registered alias for each name, lowercase as the registry holds it (G-1 to G-3).
   `align_names.md` says a registered alias that is the vocabulary alias of another term
   resolves to the registered mapping (G-11). Each carries a Changes line citing its item.
   A reader round by `hl-hydro-reviewer` reads the binding diff only; findings `record`
   (P-4, frozen).
3. R change and manifest parity (R change, §4; cases and expectations) —
   `align_names_conflict` and `align_names_conflict_toid` go `[r, python]` and the
   idempotence property asserts the raise on a two-way tie instead of leaving ties out
   (item 1); `register_names()` raises in R on a value that is not a hydroloom name, with a
   NEWS entry and the unit 2 drafted key, the silent no-op D3-65 fixed (item 4, B-3); the
   `r_only:` field from unit 1 is written on the R-only-by-design `[r]` cases of the phase-1
   functions (item 9), and the parity generator counts them apart from `[r]` cases with no
   `r_only:`. Expectations regenerate; R conformance stays green. The ROADMAP §4 row for the
   `register_names` R raise is added if none covers it.
4. Roxygen — the three Python-only facts written back as "In Python," roxygen sentences
   (item 2, P-4): identifiers become `str`; `IndexIds` positions run from 0 and carry
   `uniques`, `indptr`, `indices`; `name_definitions` returns columns `name` and
   `definition`. The shared `network` substitution pair becomes `@param x` roxygen
   (item 8, P-4): `@inheritParams` from one holder where the text is identical, verbatim
   elsewhere, the substitution pair deleted; the agent's sentence is the draft and
   "catchments" gets its gloss. `devtools::document()`, then `docstrings.json` regenerated;
   R conformance stays green. `hl-modeler-reader` reads the composed result against
   `.kiro/context/prose.md`; findings route per P-4.
5. `register_names` and naming-block Python follow-up (Python) — the `register_names`
   registry (process-wide, context-manager restore, raise on a non-hydroloom value under the
   unit 2 key), `hy_reverse(source=None)` restoring the registered alias lowercase, the
   `align_names` registered-alias precedence, and `__all__` plus `supported_by: [r, python]`
   for `register_names` once it has a `generator: [python]` case (A-18, B-4). The Python
   side of item 4.
6. `add_toids` (Python) — the M5 deliverable (3): `add_toids` at the A-16 scope, ingest
   normalization and the I-5 fill, the missing-`divergence` raise (`divergence_missing`),
   the missing-`tonode` terminal that stays a target, the broken-divergence-coding raise
   (`divergence_coding_invalid`), the 0-at-a-split main-path read with a warning, the lone
   downstream-2 connect with a warning, flownetwork input raising (`toid_already_present`),
   and `return_dendritic = FALSE` keeping secondary paths. `__all__` and
   `supported_by: [r, python]` (A-15, A-18). This is the effort-sweep unit: effort medium.

## BACKLOG rows pulled in

- `register_names()` tie rule when several aliases map to one hydroloom name (BACKLOG M5;
  owner maintainer; §4 row not needed) — units 2, 3, 5 close it: `align_names.md` step 5
  decides which alias wins and item 4 decides the one-column, two-names case.
- A-13 language-neutral pass: the shared `network` substitution text becomes roxygen
  (BACKLOG Any time, roxygen; §4 row not needed) — unit 4 takes the `network`-text half;
  the `navigate_network_dfs` half stays in its BACKLOG M5 row for that function's port.

## Last logged read per description

| description | read SHA |
| --- | --- |
| align_names.md | `6df5077`; frozen (P-4); unit 2 binding diff read |
| hy.md | `d1289ac`; frozen (P-4); unit 2 binding diff read |
| hydroloom_names.md | `6ffc110`; frozen (P-4); unit 2 binding diff read |
| hy_reverse.md | `7e21308`; frozen (P-4); unit 2 binding diff read |
| add_toids.md | `8a369ab`; frozen (P-4), no reader round in this run |

## Done-when items the script does not check

- The `add_toids` and naming-block Python cases pass under `pixi run test` on win-64.
  Three platforms, both Python versions, and `py-min-versions` need a push; report as not
  met and name the workflow files (`.github/workflows/py-test.yaml`,
  `.github/workflows/py-min-versions.yaml`).
- The parity table: report which phase-1 functions read `r, python` after the unit 3
  `r_only:` pass, and which still carry `[r]` cases with no `r_only:` and why.
- The effort sweep (medium against high) runs on `add_toids` (unit 6); report the
  comparison the run can make and name what needs a push.
- Not in this run, so report each as not met: permutation invariance (D-7),
  `sort_network` and `add_topo_sort`, `navigate_network_dfs`, `capabilities`,
  `is_dendritic`, hatch-vcs, the Linux `R CMD build` check, and the release.
