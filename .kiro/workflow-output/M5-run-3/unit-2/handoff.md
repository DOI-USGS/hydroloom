# M5 run 3 unit 2 handoff, for unit 3 (R change)

Unit 2 changed descriptions and `remedies.json` only. No case, expectation, or R file moved. `pixi run validate` passes (37), the R conformance, remedies, and align_names tests pass (1961, 0 failed), and `pixi run lint` and `pixi run test` pass (815 passed, 358 skipped).

## R reorder (REVIEW_11032026 item 1, RULES B-1, B-3)

`align_names()` in `R/align_names.R` runs the `name_alias_conflict` stop before the `permanent_identifier` drop. `align_names.md` steps 3 and 4 now run the drop first, and for that count a column already holding the hydroloom name is one of its candidates, once (through the identity pair where the map has one). So `permanent_identifier` beside `id` drops out and the table returns unchanged. R today stops on `data.frame(id = 1:2, Permanent_Identifier = c("a", "b"))` with "Problem aligning names. Permanent_Identifier conflicts with hydroloom name id". Python `_align` (`src/hydroloom/_hy.py`) already has the description's order and counting rule. NEWS entry needed.

## R raise (REVIEW_11032026 item 4, B-4, B-3)

`hydroloom_names(x)` must stop with `register_name_unknown` when any value of `x` is not in `vocab_good_names()`. The check runs after the unnamed check and before `clear` is applied, so the registry is unchanged on the stop (`hydroloom_names.md` Algorithm step 3, Postconditions). NEWS entry needed, and a ROADMAP §4 row if none covers it.

## Second R raise (fix round, hydro H-01, RULES B-1, B-3)

`hydroloom_names(x)` must also stop with `register_alias_is_name` when any name of `x`, lowercased, is in `vocab_good_names()` and differs from its value. It runs in the same step 3, after the `register_name_unknown` check and before `clear` is applied, so the registry is unchanged on the stop. An identity entry (`id = "id"`, `toid = "toid"`) passes, since the vocabulary already holds identity pairs (`toid`, `fromnode`, `tonode`, `divergence`). R today accepts `hydroloom_names(c(toid = "id"))`, and `align_names(data.frame(toid = 1:2))` then gives `id`; after unit 3 the call stops and the registry is unchanged. Draft key `register_alias_is_name` in `remedies.json` (placeholder `{names}`, `python_class: KeyError`, `languages: ["python"]`, `r_text: []`); unit 3 adds `"r"` and the R text as for `register_name_unknown`. NEWS entry needed, in the same line as the first raise.

## Drafted key

`register_name_unknown` in `inst/extdata/remedies.json`, placeholder `{names}` (each alias with its non-hydroloom value), `python_class: KeyError`, `raised_by: ["hydroloom_names"]`. It lists `languages: ["python"]` and `r_text: []` because `test_remedies.R` requires R text exactly when `r` is listed. When R raises it, add `"r"` to `languages` and the R message as `r_text` (R's `stop()` text written as the message with `<names>` for the placeholder, or set the message to match). The draft is recorded in the `hydroloom_names.md` Remedies note, as for `id_missing`.

## Registered alias of another term (G-11), also an R change

`align_names.md` step 2: where one alias has pairs to two or more hydroloom names, only the latest in map order is a candidate, so a registered alias wins over the vocabulary alias of another term. R does not do this today: after `hydroloom_names(c(comid = "toid"))`, `align_names(data.frame(COMID = 1:2))` gives `id`. The kickoff's unit 3 text does not list this; it is the same B-4 line as the raise, so it is either folded into unit 3's R change with NEWS or recorded as a BACKLOG row under M5 for the align_names owner. Python `_align` takes the last candidate in dict order (G-11) and is unit 5's.

## Cases that will change

None disagrees today: every R conformance case passes against the edited descriptions. The order change reaches no current case in R, since no case runs `align_names` on `permanent_identifier` beside `id`. Unit 3's own manifest items (`align_names_conflict`, `align_names_conflict_toid` to `[r, python]`; the idempotence property asserting the raise on a two-way tie) are unaffected by this unit. A new case for the `permanent_identifier`-beside-`id` second pass, and one for the `register_name_unknown` raise, would be the cases that record the two R changes.

## Signature note

`hydroloom_names.md` gives `register_names(mapping, *, replace=False)`, keeping `replace` from SPEC A-16 and D3-20 rather than the bare `register_names(mapping)` in the unit brief.
