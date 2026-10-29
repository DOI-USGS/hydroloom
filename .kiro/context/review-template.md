# Review file — the one thing a run hands the maintainer

File: `dev/milestones/REVIEW_<yyyymmdd>.md`. Written by the orchestrator at the end of a
run from the ledger, `git log`, and the unit notes. Everything else an agent wrote stays
under `.kiro/workflow-output/`.

What goes in: a case no disposition, §4 row, SPEC anchor, or RULES.md principle decided;
an edit to maintainer-authored text; a citation only the maintainer can supply. What stays
out: anything already decided, the Decisions rows and Log cells as such, Done-when
evidence beyond one line, commit tables, and anything a `verify` finding raised on its own.

Size: a reader who knows the plan reads it in a few minutes. One paragraph of status.
Each item two to four sentences. If the file would run past a screen or two, the run has
left decisions in it that a principle covers; look again before writing.

    # Review <date>: <milestone>, run <n>

    <Commits `<first>`..`<last>`, none pushed. Suite and check status in one clause.
    Done-when items met, and the ones not met with the item in this file that holds
    them. Count of new Decisions rows, if any.>

    ## Rules proposed

    ### <short title> (<D<n>-nn> if a row was written)

    <The case, two sentences.> Call taken: <one sentence>. Reverse: <one sentence>.
    Proposed line, under <B-n | P-n | new>: <one or two sentences in RULES.md form>.

    Disposition:

    ## Maintainer-authored edits needed

    - <file> § <where>: <the sentence as it stands>; <what is wrong, one clause>.

    ## Citations needed

    - <description or term>: <the claim that has no source>.

Suggested disposition values: `accept`, `accept with edit: <what>`, `reject: <reason>`,
`defer to M<n>`. `grep -c '^Disposition: $'` counts what is left. The maintainer's text
after `Disposition:` is the rule's text as it enters RULES.md, edited by the next run only
for RULES.md form.
