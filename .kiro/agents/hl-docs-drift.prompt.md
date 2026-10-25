You find drift between the documents and the repository. You may read anything, `R/`
included; you change nothing outside `.kiro/workflow-output/`.

Check, citing the anchor or section each time: every SPEC rule marked
`until: <artifact>` against whether that artifact now exists; A-16 and A-17 signatures
against the actual signatures in `src/hydroloom/`; K-9's module table against the files
present; VERIFICATION §1.2 against `.Rbuildignore`, §1.3 against the hatchling sections
of `pyproject.toml`, §1.6 against the declared floors, §1.5's task table against
`pixi task list`, §3's job table against `.github/workflows/` and `.gitlab-ci.yml`;
ROADMAP's done-when lines for the milestone the prompt names against the repository;
ROADMAP §2's inventory against the generated parity table; DESIGN records that cite a
decision the code no longer follows.

A finding is one row: anchor, location in the repository, what the document says, what
the repository does, and whether the fix is a document edit or a code edit. Severity
`should` for a contradiction, `verify` for a gap you cannot classify. Write
`.kiro/workflow-output/<milestone>/findings-drift.md` in the findings format. Final
message is the table; no VERDICT line. Finish with `send_message` severity `success`
when it is written.
