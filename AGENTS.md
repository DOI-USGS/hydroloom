## Commands

- `devtools::test()`
- `devtools::check()` — R CMD check
- `devtools::document()` — rebuild roxygen docs

## CI and release

GitHub (DOI-USGS/hydroloom) is the development platform; code.usgs.gov
(`usgs` remote) is the release platform. GitHub Actions runs `R CMD check` on
push/PR to main and deploys pkgdown. The GitLab pipeline (`.gitlab-ci.yml`)
runs only on `rc/*` branches and produces a verified source tarball in the
GitLab package registry — that tarball is what goes to CRAN. Local builds are
not part of the release workflow.

Vignettes: only `vignettes/hydroloom.Rmd` ships to CRAN. `vignettes/articles/`
is website-only, excluded via `^vignettes/articles$` in `.Rbuildignore`. All
vignettes gate chunk evaluation on `Sys.getenv("BUILD_VIGNETTES") == "TRUE"`,
so nothing evaluates and no service is contacted during a CRAN build. Articles
are not installed vignettes — cross-reference them with a pkgdown URL, never
`vignette("name")`. Vignette-only dependencies live in
`Config/Needs/website`, not `Suggests`.

## Style

- 2-space indent, snake_case for functions and variables
- `<-` for assignment (not `=`)
- `|>` native pipe in new code (`%>%` still in legacy code — don't convert)
- `if (` with space before paren; K&R braces (`} else {` on same line)
- One-liner `if` without braces OK: `if (is.null(x)) stop("msg")`
- Validation: `if (...) stop()` — no assertion library. Concatenate with multiple args to `stop()`, not `paste0()`.
- `message()` for progress/status; `warning(..., call. = FALSE)` for empty results
- Spread out code: blank line after opening function brace, between logical sections, and before return values
- Roxygen: `@title` sentence-case, `@param type. Description`, `@inherit`/`@inheritParams` to share docs
- Visibility: `@export` for public API, `@noRd` for private helpers (most common), `@keywords internal` for internal-but-documented workers
- Imports: `@importFrom pkg fn` in roxygen
- `.data$col` pronoun inside dplyr verbs; `!!` for injection
- Function args: continuation lines use 2-space indent (Positron/tidyverse style, not aligned to `(`) in new code
- Minimal diffs: don't restyle existing code; only apply new style to new/changed lines
- No trailing commas in lists or function signatures
- Tests: `testthat` + `expect_*`

## S3 Class Hierarchy (Issue #73)

### Class Hierarchy

```
hy                        # base: id + orig_names attribute
├── hy_topo               # self-referencing edge list: id, toid (unique id)
│   └── hy_leveled        # + topo_sort, levelpath, levelpath_outlet_id
└── hy_node               # bipartite graph: id, fromnode, tonode (unique id)

hy_flownetwork            # non-dendritic edge list: id, toid (non-unique id ok)
                          # optionally: upmain, downmain (full junction table)
```

### Key Design Decisions

1. `.data.frame` methods are UNCHANGED -- `hy()` auto-classifies, dispatch hits subclass methods
2. `hy_leveled` inherits from `hy_topo` -- `.hy_topo` methods work on leveled objects automatically
3. `hy_flownetwork` is NOT a subclass of `hy` -- it's a separate junction table
4. `attr(x, "dendritic")` is a property, not a class -- orthogonal to representation
5. dplyr operations strip custom classes -- `put_hyg()` and producer functions must restore them
6. `hy()` with `add_topo = TRUE` auto-builds toid from nodes via internal `add_toids()` call

### Dispatch Helpers (in `R/hy_classes.R`)

Boilerplate S3 method patterns are centralized in 5 internal helpers:
- `hy_classify_and_redispatch()` -- `.hy` methods: classify then re-dispatch or guided error.
  Uses `utils::getS3method()` to check method existence before dispatching (prevents
  infinite recursion when no typed method exists).
- `hy_as_dataframe()` -- `.data.frame` methods: hy() -> dispatch -> restore user names
  -> strip hy classes (handles `sf` re-stamp inline; no longer calls `hy_reverse()`).
- `hy_node_to_topo()` -- `.hy_node` methods: message + add_toids() + re-dispatch
- `hy_node_to_flownetwork()` -- `.hy_node` methods: to_flownetwork() + re-dispatch
- `hy_topo_to_node()` -- `.hy_topo` methods: as_hy_node() + re-dispatch or error

Guidance constants: `hy_guidance_topo`, `hy_guidance_leveled`, `hy_guidance_node`

### Unique id enforced in hy_topo

`hy_topo` requires unique `id`. `classify_hy()` auto-routes non-unique id+toid data
to `hy_flownetwork`. The old `allow_non_unique` bypass is removed.

**How non-dendritic data flows:**
- `classify_hy()` detects duplicated `id` → stamps `hy_flownetwork` (not `hy_topo`)
- `hy_flownetwork` now only requires `id` + `toid`; `upmain`/`downmain` are optional
  (presence checked by column name, no subclass needed — only 2 places use them:
  `navigate_network_dfs()` direction args and `print.hy_flownetwork()`)
- Internal call sites (add_divergence, add_streamorder, accumulate_downstream, etc.)
  produce non-unique id+toid → `classify_hy` → `hy_flownetwork` → dispatch to
  `.hy_flownetwork` methods automatically

**Key internal patterns:**
- `sort_network_impl()` / `make_index_ids_impl()` -- extracted algorithm bodies in
  `R/sort_network.R` and `R/make_index_ids.R`. Both `.hy_topo` and `.hy_flownetwork`
  methods call these to avoid circular S3 dispatch.
- `.hy_flownetwork` delegate methods -- for functions where the algorithm already
  works on non-dendritic data, `.hy_flownetwork` methods call `.hy_topo` directly:
  `add_streamorder`, `accumulate_downstream`, `get_bridge_flowlines`, `add_levelpaths`,
  `make_node_topology`
- `sort_network.hy_topo` runs `check_hy_outlets` + `check_hy_graph` before `_impl`;
  `sort_network.hy_flownetwork` runs `check_hy_outlets` only (graph check not
  applicable to non-dendritic data)

### Nuances

- **Catchment vs flowline duality** -- `hy_topo` and `hy_node` each represent
  *either* a catchment topology or a flowline topology. Functionally separate
  graphs: catchments carry local drainage area and are 1:1 with their flowpath;
  flowlines are linear features without a committed local drainage area. The
  class is agnostic; the caller must know which graph they have. Don't conflate.
- **Query helpers** -- prefer these over re-deriving from class names:
  - `hy_network_type(x)` returns the most-specific class name (`"hy_leveled"`,
    `"hy_topo"`, `"hy_node"`, `"hy_flownetwork"`, `"hy"`, or
    `"not a hydroloom object"`). Used inside `hy_classify_and_redispatch()` to
    drive `getS3method()` lookups.
  - `hy_capabilities(x)` returns a named logical vector of which hydroloom
    functions are callable on `x` given current class + columns.
    `include_conversions = TRUE` adds functions reachable after one
    representation conversion. **Authoritative programmatic answer** to "what
    can I do with this object?"
- **`is_dendritic()` semantics** -- returns `FALSE` if `divergence` column has
  any value of 2, OR (when `validate_data = TRUE`, default) if `toid` is
  present and `id` has duplicates. Otherwise `TRUE`. Dendritic-ness is a
  property, not a class -- a `hy_topo` is always dendritic by virtue of
  unique `id`, but a `hy_node` may or may not be.
- **Conversion routes** (which function moves between representations):
  - `hy_node` -> `hy_topo`: `add_toids()`. With `return_dendritic = FALSE`,
    keeps secondary paths and produces an `id`/`toid` table that
    `classify_hy()` routes to `hy_flownetwork`.
  - `hy_topo` -> `hy_node`: `make_node_topology()`.
  - `hy_topo` -> `hy_leveled`: `add_levelpaths()` (adds `topo_sort`,
    `levelpath`, `levelpath_outlet_id`).
  - `hy_leveled` -> `hy_flownetwork`: `to_flownetwork()`. Uses `levelpath`
    to set `upmain`/`downmain` at each divergence.
  - `hy_node` -> `hy_flownetwork`: `to_flownetwork()` (warns; produces a
    topology-only edge list with no `upmain`/`downmain`).
  - `hy_flownetwork` -> `hy_node`: `make_node_topology()` (delegate method).
- **`as_hy_node()` downcast** -- when a table has *both* `toid` and
  `fromnode`/`tonode`, `classify_hy()` prioritizes `hy_topo`. Node-based S3
  methods call the internal `as_hy_node()` helper to re-classify so dispatch
  lands on `.hy_node`. `hy_topo_to_node()` wraps this in the dispatch-helper
  layer.
- **`hy_flownetwork` `upmain`/`downmain` invariants** -- when both columns are
  present, `new_hy_flownetwork()` enforces: rows with `downmain == TRUE` must
  have unique `id` (one main downstream connection per upstream feature);
  rows with `upmain == TRUE` must have unique `toid` (one main upstream
  connection per downstream feature). These are the only structural
  invariants beyond `id`/`toid` presence.
- **`hy_flownetwork` is the only edge-list-shaped representation that
  preserves divergences directly** -- the other three either forbid
  duplicated `id` (`hy_topo`, `hy_leveled`) or encode connectivity through
  node identifiers rather than edges (`hy_node`).
- **Outlet detection (issue #85)** -- a row is an outlet if its `toid` does
  not refer to any `id` in the table. Use `is_outlet(x)` for reads;
  `get_outlet_value(x)` only when *writing* the reserved outlet `toid` value
  (`""` for character, `0` otherwise). Tolerates NA, foreign reserved values,
  and unique-per-outlet ids. `is.hy()` accepts NA toids.
- **`hy_flownetwork` class isolation** -- `new_hy_flownetwork()` strips any
  `hy`/`hy_topo`/`hy_leveled`/`hy_node` classes from its input. The
  `orig_names` attribute may still be carried internally so name-aligned
  `.data.frame` dispatch can round-trip user column names; the user-facing
  `to_flownetwork()` drops it at the boundary.

## Python port — rules for every agent

This repository holds the hydroloom R package at its root and the Python package under
`src/hydroloom/`. Shared artifacts live under `inst/extdata/`: the vocabulary
(`hy_vocab.json`, `abbreviations.json`), the remedies catalog (`remedies.json`), the
composed docstrings (`docstrings.json`), and the conformance suite (`conformance/`:
`manifest.yaml`, `algorithms/`, `fixtures/`, `expected/`). The planning documents are
`dev/plan/SPEC.md` (rules, cited by anchor), `ROADMAP.md`, `VERIFICATION.md`, `DESIGN.md`.
The review record is `dev/milestones/M<n>.md`, one file per milestone.

One maintainer directs every agent on both sides of the port. "Maintainer" in these documents
names that role when it decides: approving a description, filling the disposition column,
merging. "Author" names a session, not a person: a description-side session reads `R/` and
writes descriptions, cases, and R edits; a Python-side session never opens `R/`. Text called
maintainer-authored is written in a session the maintainer reviews as prose rather than as
code. The co-maintainer named in DESIGN §9 is a second person and the one place a second
human is meant.

Rules that hold in every session:

1. The contract for a Python function is its algorithm description
   (`inst/extdata/conformance/algorithms/<function>.md`) and the SPEC anchors it cites.
   The expectation GeoPackage (`conformance/expected/<function>.gpkg`, one layer per
   case) is the oracle. Python-side agents do not open `R/` or `tests/testthat/`.
2. Binding sections of a description: Signature, Preconditions, Postconditions, and every
   tie rule and sentinel in Algorithm. Algorithm step order does not bind. A kernel the
   description names binds by signature (SPEC K-6); a kernel it does not name is the
   author's to shape.
3. Error text comes from `inst/extdata/remedies.json` by key, never typed at a raise
   site (SPEC A-10).
4. A review finding cites an anchor: a SPEC rule, a description section, a checklist
   item, or a named source document. A finding with no anchor is severity `verify`; it
   goes to the maintainer and never blocks.
5. Done means `pixi run lint` and `pixi run test` pass. Run them yourself; hook triggers
   do not fire in sub-agents.
6. Write only where your agent's permissions allow. Findings, PR bodies, and reports go
   under `.kiro/workflow-output/<function or milestone>/`.
7. Hand off through files and your final message. Do not paste another agent's
   transcript or reasoning into your output; pass findings, not deliberation.
8. Every commit an agent makes carries `Assisted-by: <agent> (<model-id>)` as a git
   trailer (`git commit --trailer "Assisted-by: Kiro (claude-opus-5.5)"`) and never
   `Signed-off-by` or `Co-Authored-By`. Agents never write the disposition column of a
   milestone file; that column is the maintainer's.
9. Prose follows `.kiro/context/prose.md`. Existing roxygen, vignette, and
   name-definition text is reused, not rewritten. Agents author user-facing prose only
   in changelog fragments, doctests, and PR bodies; README, articles, `plain` and
   `definition` fields, and `remedies.json` are maintainer-authored and agent-reviewed.
