# Kickoff: M5, run 2

Base `af8ee99737614fc0a5615dd7391f4092bc309cae` on `python-port`. Effort high. Model claude-opus-5.5.

Written by the orchestrator from the `REVIEW_11022026` dispositions and the open BACKLOG M5 rows (P-5). Who does what. Units 1, 2, 3 and 5 are not Python: they go to `general-task-execution`, and `hl-style-reviewer` reviews the unit's diff, one fix round. Units 2 and 3 are description-side and read `R/`. Units 4 and 6 are `port-function` runs: manifest marks by `general-task-execution` first, then `hl-implementer`, `hl-spec-reviewer` and `hl-style-reviewer` on the diff, one fix round. Unit 6 adds `hl-modeler-reader` on the five composed docstrings. Every unit is one Log entry in `dev/milestones/M5.md`, written by `hl-aggregator` in mode `record`.

## Units, in dependency order

1. Plan commit and Log cells — the `plan:` commit the `REVIEW_11022026` dispositions call for: SPEC A-10 gains the missing-column `KeyError` carve-out (item 7), A-11 says the first frame outside hydroloom (item 5), I-9 gains the nullable `boolean` flag carve-out (item 6); VERIFICATION §2.1 loses its fourth row and gains the B-4 sentence, the BACKLOG hydrofabric row closes, and M2 D2-15 is noted as resolved without a fixture (item 1); `.kiro/context/orchestrator.md` drops "or at a block" to match P-5 (item 1). The eight `—` cells in the M5 exemplar entry are filled from the items that settle them: SP-01, SP-02, SP-07 `B-1`; ST-05, ST-11/SP-09 `B-2`; ST-09 `B-4`; SP-03, SP-04 `P-1` (plan; RULES P-2).
2. Description corrections — `align_names.md` step 3 rewritten so a name-aligning pass on its own output never raises, and `hy.md` step 2 to match (item 2, B-1); `hy_network_type.md` Preconditions and Remedies narrowed to name alignment and `id` typing, as Algorithm step 1 reads (item 3, P-1); `hy.md` Preconditions and Remedies gain the missing-`id` raise at ingest naming rows by position, with a drafted `remedies.json` key for the M6 read (item 4, B-1). Each carries a Changes line citing its item. A reader round by `hl-hydro-reviewer` reads the binding diff only; its findings are `record` (P-4, frozen) (description).
3. Roxygen language-neutral pass — the roxygen of `hy`, `hy_network_type` and `make_index_ids`: the shared sentences true in both languages, the R-only facts (`add_topo`, the S3 class, R row numbers, `"not a hydroloom object"`) in sentences of their own that the substitution table drops (item 8, P-4, A-13). The approved Definition paragraphs of the three descriptions and the applied fixes in `.kiro/workflow-output/M5-run-1/unit-4/docstrings.md` are the text. `devtools::document()`, then `docstrings.json` regenerated; R conformance stays green (R change; §4 row not needed, documentation only).
4. Exemplar follow-ups — `representation` narrowed to the corrected `hy_network_type.md`; `align_names` step 3 as rewritten; the missing-`id` raise under the unit 2 key; a stack-level helper in `errors.py` used by every `warnings.warn`; S-16, S-17, S-18 from the BACKLOG row; the three `PER_FUNCTION` entries in `_docstrings.py` deleted once unit 3's text composes. Marks first: the two-way tie case and `hy_network_type_duplicated_edge` to `[r, python]` (Python).
5. towncrier — `changelog.d/`, the pixi task, and the first fragment, covering `hydroloom.errors`, `as_hy`, `representation` and `make_index_ids` (packaging).
6. Naming block — `hy_reverse` (public, with `source=`), `is_hy`, `register_names`, `name_definitions`, `align_names`, and `__all__` and `supported_by: [r, python]` for each (A-15, A-16, A-18). The `register_names()` tie rule is the BACKLOG row the maintainer owns: where the run cannot land without it, the call follows B-1 as amended by item 2 and goes to the review file (orchestrator.md bin 3) (Python).
7. Shared composer text — the `network` parameter text and See Also in the composed docstrings, once unit 6 makes `register_names` exist: language-neutral roxygen where the sentence is shared, the substitution table where only a name differs (A-13). `hl-modeler-reader` reads the result (R roxygen and Python composer).

## BACKLOG rows pulled in

- Reference hydrofabric subset fixture (BACKLOG M5; §4 row not needed) — unit 1 closes it, per item 1.
- Exemplar follow-ups the second code review left as `should` (BACKLOG M5; §4 row not needed) — unit 4, with the stack-level helper and missing-`id` raise items 4 and 5 add to it.
- Roxygen beyond correctness the M5 exemplar modeler read found (BACKLOG Any time; owner David) — unit 3 takes only the R-only-fact half item 8 assigns; the wording rows M-10, M-11, M-14 to M-16, M-18 stay David's.
- towncrier setup and the first fragment (BACKLOG M5; §4 row not needed) — unit 5.
- `register_names()` tie rule (BACKLOG M5; owner maintainer) — unit 6, as above.
- Shared composer text in public docstrings (BACKLOG M5; §4 row not needed) — unit 7.

Left for later runs: the `sort_network_dups.gpkg` Python-only raise cases (with `sort_network`), the A-13 pass on `navigate_network_dfs` (with its port), the Linux `R CMD build` check (needs CI).

## Last logged read per description

| description | read SHA |
| --- | --- |
| align_names.md | `6387079`; frozen (P-4); unit 2 binding diff read |
| hy.md | `7e21308`; frozen (P-4); unit 2 binding diff read |
| hy_network_type.md | `9cfc1da`; frozen (P-4); unit 2 binding diff read |
| make_index_ids.md | `7e21308`; frozen (P-4), no reader round in this run |
| hy_reverse.md | `7e21308`; frozen (P-4), no reader round in this run |
| is.hy.md | `9cfc1da`; frozen (P-4), no reader round in this run |
| hydroloom_names.md | `7e21308`; frozen (P-4), no reader round in this run |
| hydroloom_name_definitions.md | `02348b5`; frozen (P-4), no reader round in this run |

## Done-when items the script does not check

- The cases marked `[r, python]` pass under `pixi run test` on win-64. Three platforms, both Python versions, and `py-min-versions` need a push; report as not met and name the workflow files.
- The parity table: report which of `hy`, `hy_network_type`, `make_index_ids` and the naming-block functions still carry R-only cases, and why.
- The effort sweep (medium against high) runs on (3), `add_toids`, not here: this run carries the follow-ups and its effort is not comparable. The run 3 kickoff states Effort medium, and the session is started at that setting.
- Not in this run, so report each as not met: permutation invariance (D-7), `add_toids`, `sort_network` and `add_topo_sort`, `navigate_network_dfs`, `capabilities`, `is_dendritic`, hatch-vcs, the Linux `R CMD build` check, and the release.
