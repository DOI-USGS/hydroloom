# hydroloom — Claude Code

Rules for every agent, R and Python alike, are in `AGENTS.md`; read it first. The Python port's plan is under `dev/plan/` (SPEC, ROADMAP, VERIFICATION, DESIGN) and its Kiro configuration under `.kiro/`.

Commits made in a Claude Code session carry the trailer `Assisted-by: Claude (<model-id>)`, for example:

    git commit -m "<message>" --trailer "Assisted-by: Claude (claude-fable-5-1)"

Never add `Signed-off-by` or `Co-Authored-By`. The maintainer's own commits carry no trailer.
