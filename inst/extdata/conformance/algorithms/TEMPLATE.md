---
# Copy this file to <function>.md, where <function> is the shared snake_case name.
# `requires` is what the function needs from its input (SPEC S-8, A-4). The manifest
# points here instead of restating it (S-4), and the code tables behind
# hy_capabilities() and capabilities() are tested against it.
requires:
  # Representations the function runs on natively, by A-2 name: hy_topo, hy_leveled,
  # hy_node, hy_flownetwork. Use [any] for any table and [none] when the function
  # takes no network. hy_leveled satisfies hy_topo (A-3); list it only when the
  # function needs levelpath attributes.
  pattern: [hy_topo]
  # hy vocabulary columns read beyond those that define the pattern (A-2 column
  # rules). Use [] when the pattern's own columns are enough.
  columns: []
  # Optional. For each representation not in `pattern` that the function accepts
  # after one conversion, the one call that converts it (A-3 `convert`, A-4 `via`).
  # Use {} when there is none.
  via:
    hy_node: add_toids
---

# <function>

## Signature

Give the shared name, each argument with its vocabulary type and default, and each
return column with its type. Copy argument names and defaults from R; where a SPEC
table disagrees, the R signature wins (A-9) and Changes records the gap. Argument prose
is not written here; it comes from roxygen (A-13). If the function is or calls a named
kernel (K-6), give the kernel's signature too: argument names, order, array meanings,
int64 dense positions, float64 weights. This section binds.

## Preconditions

State the representation and columns the function needs, matching `requires`, and what
happens when they are absent: the conversion applied, or the condition raised, cited by
its `remedies.json` key (A-10). This section binds.

## Definition

Describe the output as a property of the input, not as steps.

## Algorithm

Give the steps. Step order does not bind. Every tie rule, the sentinel (K-4), and each D
rule relied on, cited by anchor, does bind. Say which work lies below the kernel
boundary (K-1, K-5) and whether a CSR index (K-2, K-3) is built.

## Postconditions

List the invariants the output holds, one per line, each one a property a hypothesis
test can check. This section binds.

## Complexity

Give time and space in n (rows) and m (connections).

## Edge cases

Say in plain words what happens for one flowline, no connections, a duplicated row, a
missing weight, a zero-length line, and an empty name, as applies: whether the function
returns, warns, or stops, and the value.

## Remedies

| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |
| --- | --- | --- | --- | --- |

One row per condition the function raises or warns on. Error text lives in
`remedies.json` and is cited here by key, never typed in this file (A-10). Write "none"
when the function raises nothing.

## References

List the papers, data model documents, and R source files the description draws on.

## Worked example

Give one small synthetic fixture under `fixtures/` and its expected output, small
enough to check by hand.

## Changes

One dated line per change, tied to the suite version. Where a ROADMAP §4 change
applies, state the post-change behavior above and give the change here.
