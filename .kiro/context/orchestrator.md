# Orchestrator — how a run decides, records, and reports

Every kickoff prompt for a milestone run points here instead of restating it. The rules of
the package are in `dev/plan/SPEC.md`; the maintainer's principles for what SPEC and a
description leave open are in `dev/plan/RULES.md`. The orchestrator coordinates and keeps
the record; it writes no code, description, or finding itself.

## Deciding

The maintainer is not available during a run. Do not stop to ask. For every call an agent
would otherwise make alone:

1. Find what decides it: a disposition, a ROADMAP §4 row, a SPEC anchor, or a RULES.md
   principle. Apply it. Record it as one citation where the call lands: the commit
   message, the Log entry, or the description's Changes line. No Decisions row, no cell
   left for the maintainer, no review item.
2. Nothing decides it. Take the reversible call that best fits SPEC and the nearest
   principle, apply it, and write one item to the run's review file (below). Add a
   Decisions row only when the call changes a file a later run reads as contract, which is
   a description's binding section, the manifest, or a plan document, so the row's "how to
   reverse" column is on record; the review item cites the row.
3. The maintainer's files stay his (RULES P-4): `plain` and `definition`, roxygen beyond
   correctness, `remedies.json` message text, README, articles, Summary sections. A needed
   edit there is one line in the review file, naming the file and the sentence.

A disposition is an instruction (RULES P-1). Where it and a description, the manifest, or
existing R disagree, the disposition wins. Where two disagree, the later wins. Do not
reopen a dispositioned row.

Record and skip, stopping only if nothing else can proceed: a destructive git operation;
a change to an existing expectation that no disposition, §4 row, or principle explains; a
user-visible behavior change nothing names (RULES B-3). Never push.

## Recording

- Plan edits only through a `plan:` commit, in the run that needs them (RULES P-2).
- Every commit carries `--trailer "Assisted-by: <agent> (<model-id>)"`.
- Log entries are appended by `hl-aggregator` in mode `record`. The disposition column is
  filled by mapping (RULES P-2): an applied finding `accept`, a reported row `accept`, a
  cell a Decisions row settles `see D<n>-nn`, a cell a principle settles its id (`B-2`,
  `P-3`), a `verify` row `record`. `—` stays only on a cell the review file carries, and
  the review item names the cell.
- The ledger `.kiro/workflow-output/<run>/state.md` holds one line per unit: status,
  commit SHA, decision ids. On restart, read it and `git log --oneline -20` first.
- Sub-agent output goes under `.kiro/workflow-output/<run>/<unit>/`; a final message is at
  most 15 lines; test and check output reaches the orchestrator through filters only.

## Reporting

The run ends by writing `dev/milestones/REVIEW_<date>.md` in the shape of
`.kiro/context/review-template.md`, and nothing else for the maintainer to read. There is
no separate report. The file holds: one status paragraph; the cases nothing decided, each
with the call taken, how to reverse it, and a proposed RULES.md line; the maintainer-
authored edits needed; the citations needed. An item the maintainer has already answered,
in a disposition or a principle, does not appear. If the orchestrator cannot propose a
line, the item says so and proposes nothing.

The first unit of the next run reads that file's `Disposition:` lines: an accepted
proposal is added to RULES.md under the principle it extends, with the review file as its
source; a rejected one is applied as a change in the following unit; both in one `rules:`
commit.
