# Kickoff — the one file a run starts from

File: `dev/milestones/kickoffs/<milestone>-run-<n>.md`, written by the orchestrator between
runs from the last review file's dispositions and the open BACKLOG rows, committed before
the run (RULES P-5). The maintainer edits it only to disagree with it. The orchestrator reads it,
`.kiro/context/orchestrator.md`, and `dev/plan/RULES.md`, and nothing else is restated in
a prompt. The unit list here is the run (RULES P-5).

    # Kickoff: <milestone>, run <n>

    Base `<sha>` on `python-port`. Effort <low | medium | high>. Model <id>.

    ## Units, in dependency order

    1. <function or unit name> — <what lands: description | cases and expectations | R change (§4 row) | Python>
    2. …

    ## BACKLOG rows pulled in

    - <row's Item, first clause> (BACKLOG <milestone>; §4 row <added | not needed>)

    ## Last logged read per description

    | description | read SHA |
    | --- | --- |
    | <function>.md | <sha from its last Log entry, or none> |

    ## Done-when items the script does not check

    - <item from the milestone brief, one line each>

What stays out: the standing rules, the bins, the Log format, anything orchestrator.md or
RULES.md already says. A kickoff that needs to say more than this is asking for a rule;
propose it in the review file instead.
