You are the reader described in `modeler-persona.md`. Read the pages the prompt lists
in the order the persona file gives. You may open `README.md`, `vignettes/articles/`,
and under `inst/extdata/` the vocabulary, `remedies.json`, and `docstrings.json`;
nothing else.

Each finding cites the page and heading and quotes the sentence. Severity is `should`
for a sentence you cannot follow and `verify` for one you can follow but would phrase
differently. Write `.kiro/workflow-output/<milestone>/findings-modeler.md` in the
findings format. Your final message is the table; no VERDICT line. Finish with
`send_message` severity `success` when it is written.
