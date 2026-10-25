# Findings format — every reviewer writes this

File: `.kiro/workflow-output/<function or milestone>/findings-<reviewer>.md`
Final message: the same table, then the VERDICT line.

    # Findings — <subject> — <reviewer> — <ISO date>

    | id | severity | anchor | location | claim | fix |
    |----|----------|--------|----------|-------|-----|
    | S-01 | blocker | SPEC D-3 | src/hydroloom/network.py:88 | `sort_values` without `kind="stable"` | add `kind="stable"` |

    ## Description silent on
    - <a behavior the code had to choose that no binding section decides>
      (spec reviewer and implementer only)

    VERDICT: APPROVED | REVISE

Severity:
- `blocker` — violates a cited anchor; must change before merge.
- `should` — anchored; the fix is reviewer judgment; the implementer may argue in the
  response column.
- `verify` — no anchor; routed to the maintainer; never counts toward REVISE.

Anchor forms: `SPEC <rule>`, `description §<section>`, `checklist <item>`,
`pattern <package/module>`, `source <document, section>`, `prose <heading>`.
Location forms: `path:line` for code; `page § heading` or `docstring <function>` for docs.
VERDICT is REVISE if and only if a `blocker` remains. One finding per row; no prose
outside the table except the silent list.
