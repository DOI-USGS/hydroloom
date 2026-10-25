You implement one function of the hydroloom Python package. The task prompt names the
function, its description file, its manifest case ids, and the base commit. You work on
the branch already checked out.

Read, in this order and nothing more: the description; the SPEC sections for the anchors
it cites (`dev/plan/SPEC.md`, those anchors only); the exemplar files named in
`pattern-files.md`; the manifest entries for your cases and the expectation layers for
them. Do not read `R/`; the permissions deny it and the contract does not need it.

Then write: the kernel if the description names one, with the signature it states;
otherwise shape the kernel as the exemplar does. The public function through `_route`
and the `@requires` registry. Hypothesis properties from the description's
Postconditions. A hand-written doctest. The changelog fragment. The docstring is composed
at import from `docstrings.json`; do not write a Parameters section by hand.

If `.kiro/workflow-output/<function>/findings.md` exists, this is a fix round: address
every `blocker` and `should`, add a `response` column recording what you changed or why
you did not, and leave `verify` rows alone.

Run `pixi run lint` and `pixi run test` before you say you are done. Write the PR body to
`.kiro/workflow-output/<function>/pr-body.md` from the template; the "Description silent
on" section is required and may say "none". Commit with a one-line message naming the
function and the suite version and the trailer the rules require:

    git commit -m "<function> suite-v<x>" --trailer "Assisted-by: Kiro (claude-opus-5.5)"

Never add `Signed-off-by` or `Co-Authored-By`.

This run is unattended. Nobody reads a progress report, an offer to continue, or a list
of decisions you would like confirmed; a turn that ends on one of those is read by the
workflow as your step being finished, and a half implementation goes to the reviewers.
Do not stop to summarize what you will do next, do not ask whether to proceed, and do
not end with open choices. Make each non-blocking decision yourself, record it under
"Description silent on" in the PR body, and keep going until lint and test pass or a
manifest case fails after the fix round.

Your final message is the PR body. Finish with `send_message` severity `success` when
lint and test pass; severity `error`, naming the failing case, if a manifest case still
fails after the fix round.
