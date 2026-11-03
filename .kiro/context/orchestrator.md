# Orchestrator — how a run decides, records, and reports

Every kickoff prompt for a milestone run points here instead of restating it. The rules of
the package are in `dev/plan/SPEC.md`; the maintainer's principles for what SPEC and a
description leave open are in `dev/plan/RULES.md`. The orchestrator coordinates and keeps
the record; it writes no code, description, or finding itself.

## Deciding

The maintainer is not available during a run. Do not stop to ask. Every call an agent
would otherwise make alone, and every piece of work the run finds that its kickoff did not
name, goes to exactly one of three bins:

1. **Fix in scope.** A disposition, a ROADMAP §4 row, a SPEC anchor, or a RULES.md
   principle decides it, and the change lands in a unit the kickoff names. Apply it.
   Record it as one citation where the call lands: the commit message, the Log entry, or
   the description's Changes line. No review item. A fix that changes no behavior is in
   scope whatever function it touches: a comment, a fixture-index entry, a plan or
   roxygen sentence that is wrong, a description sentence outside the binding sections,
   an R body edit under which every conformance case still passes and no expectation
   layer changes. These go in the run's one housekeeping commit (RULES P-2), cited like
   any other bin 1 fix, and never to BACKLOG.
2. **Backlog.** The change alters behavior, a binding section, a manifest case, or an
   expectation layer of a function outside the kickoff's units, whether or not something
   decides it. One row in `dev/plan/BACKLOG.md` under the milestone it blocks, with the
   rule that decides it where one exists and `§4: yes` where it is a user-visible R
   change. An accident RULES B-3 plainly decides still goes here when its function is
   outside the run: a run's scope does not grow. Where the call also changes a file a
   later run reads as contract (a description's binding section, the manifest, a plan
   document), the row says how to reverse it.
3. **Elevate.** Nothing decides it and the run's units cannot land without it. Take the
   reversible call that best fits SPEC and the nearest principle, apply it, and write one
   item to the review file (below) with the call taken and how to reverse it.

The maintainer's files stay his (RULES P-4): `plain` and `definition`, roxygen beyond
correctness, `remedies.json` message text, README, articles. A needed edit there is a
BACKLOG row under "Any time", or a review item when a unit cannot land without it.

A description that has had its reader round and fix round is frozen (RULES P-4): it
changes only from an implementer finding or a conformance failure. A reader finding on a
frozen description is `record`, not a fix round.

There is no Decisions table in `M<n>.md` from M4 on; a bin 2 row or a bin 3 item is its
replacement.

A disposition is an instruction (RULES P-1). Where it and a description, the manifest, or
existing R disagree, the disposition wins. Where two disagree, the later wins. Do not
reopen a dispositioned row.

Record and skip, stopping only if nothing else can proceed: a destructive git operation;
a change to an existing expectation that no disposition, §4 row, or principle explains; a
user-visible behavior change nothing names (RULES B-3). Never push.

## Running

- A run starts from a kickoff file in the shape of `.kiro/context/kickoff-template.md`,
  committed under `dev/milestones/kickoffs/` by the orchestrator when the maintainer says
  to continue and the last review file's Disposition lines are filled (RULES P-5). Write
  it from those dispositions and the open BACKLOG rows, commit it, and start; do not
  answer with a list of what it would hold. It names the milestone,
  the units in dependency order, the base SHA, the effort level, the BACKLOG rows pulled
  in, and for each description the SHA of its last logged read.
- The unit list is the kickoff's (RULES P-5). A run ends on its last unit,
  never by adding a unit; what a run finds goes to a bin.
- A reader round reads a diff (RULES P-4): the binding sections changed since the
  description's last-read SHA, the whole file only on a first read. A description with no
  binding diff is not sent to a reader.
- No phase-2 description is written before the M5 exemplar slice has landed (RULES P-5).

## Recording

- Plan edits only through a `plan:` commit, in the run that needs them (RULES P-2).
- Every commit carries `--trailer "Assisted-by: <agent> (<model-id>)"`.
- Log entries are appended by `hl-aggregator` in mode `record`, run before the unit's
  last commit so the entry is in the unit's commit set, not a commit after it. A
  description-review entry records the SHA the reader read. The disposition column is
  filled by mapping (RULES P-2): an applied finding `accept`, a reported row `accept`, a
  cell a Decisions row (M3 and earlier) settles `see D<n>-nn`, a BACKLOG row `backlog M<n>`,
  a cell a principle settles its id (`B-2`, `P-3`), a `verify` row `record`. `—` stays only on a cell the review file carries, and
  the review item names the cell.
- The ledger `.kiro/workflow-output/<run>/state.md` holds one line per unit: status,
  commit SHA, decision ids. On restart, read it and `git log --oneline -20` first.
- Sub-agent output goes under `.kiro/workflow-output/<run>/<unit>/`; a final message is at
  most 15 lines; test and check output reaches the orchestrator through filters only.

## Reporting

A milestone's done-check is `Rscript dev/done_check.R M<n>` at the end of a run, plus the
Done-when items it cannot check (a reader round dispositioned, `R CMD check`), each read
once and reported in one clause. When every item passes, the orchestrator writes the
milestone's `## Summary` marked `DRAFT:` for the maintainer to edit, and no review file. When it fails, the orchestrator writes
`dev/milestones/REVIEW_<date>.md` in the shape of `.kiro/context/review-template.md`, and
nothing else for the maintainer to read. There is no separate report. The file holds: one
status paragraph with the Done-when items not met and the count of BACKLOG rows the run
added; the bin 3 items, each with the call taken, how to reverse it, and a proposed
RULES.md line; the citations needed. An item the maintainer has already answered, in a
disposition or a principle, does not appear. If the orchestrator cannot propose a line,
the item says so and proposes nothing.

The review file is the only channel. The first unit of the next run reads its
`Disposition:` lines, and transcribes into it any disposition the maintainer gave in chat
instead: an accepted proposal is added to RULES.md under the principle it extends, with
the review file as its source; a rejected one is applied as a change in the following
unit; both in one `rules:` commit.
