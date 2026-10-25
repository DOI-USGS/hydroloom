# hydroloom SPEC — conventions binding on both implementations

Rules only. Each rule has an anchor (`K-3`, `D-7`) that algorithm descriptions, ROADMAP briefs, and code comments cite. Reasoning lives in DESIGN.md under the same anchor; verification of each rule lives in VERIFICATION.md. A rule marked `until: <artifact>` is prose standing in for a repository file that replaces it when the file exists.

Sections: K kernel boundary · S shared artifacts · I identifiers and missingness · D determinism · G geometry · A API shape.

## L. Layout and build facts

Moved to VERIFICATION §1 (tree, `.Rbuildignore`, hatchling, environments, dependencies, version mechanics). The letter is held so older citations resolve: L-1 tree → §1.1, L-2 → §1.2, L-3 → §1.3, L-5 → §1.4, L-6 → §1.5, L-7 → §1.6, L-8 → §1.7.

## K. Kernel boundary

**K-1 The boundary.** Above: frames, identifiers, the vocabulary, representation checks, errors. Below: `indptr`, `indices`, attribute arrays, scalars, nothing else. Every graph computation, vectorized level-synchronous walks included, lives below the boundary, held by the import-linter contract (K-9), so profiling and replacement are per kernel. Only named kernels (K-6) carry a cross-language signature; everything else below the boundary is plain numpy shaped as the author sees fit.

**K-2 CSR is canonical.** `indptr` length n+1, `indices` length m; neighbors of dense node i in the direction `mode` are `indices[indptr[i]:indptr[i+1]]`. The padded ELLPACK form (R's `make_index_ids` matrix plus `lengths`) is a view, convertible by repeat and reshape. Build: `factorize` (pandas) or a data.table join (R) for the id-to-index map, then argsort, bincount, cumsum; no per-row loop.

**K-3 `IndexIds`.** Frozen dataclass: `indptr` (int64, n+1), `indices` (int64, m), `uniques` (pandas Index in the identifier's own dtype, maps dense position to id), `mode` (`"to"` or `"from"`); optional `main` (per-edge main-path flag) and `edge_id` (per-edge identifier, flownetwork pattern) arrays, length m, aligned with `indices`, default `None`, the CSR idiom for edge data, so a pre-built index stays self-contained. `make_index_ids(mode="both")` returns a pair. `transpose()` builds the other direction from CSR (argsort, bincount). `to_scipy()` returns a `csr_array` sharing buffers; `ell()` returns the padded view with `lengths`. No frame reference is kept. It is the only object that crosses the boundary as-is and the only cached-graph type; a function that accepts a pre-built graph in R accepts `IndexIds` via an `isinstance` branch before name alignment. R keeps its `make_index_ids()` list (`to`, `lengths`, `to_list`, `froms` variants).

**K-4 Sentinel.** CSR has none: a terminal has an empty neighbor range. In dense per-node arrays (ELL padding, a single-successor vector) the sentinel is "first valid index minus one": −1 in Python's 0-based kernels, 0 in R's 1-based ones. A Python kernel never uses 0 as a sentinel.

**K-5 A kernel never reaches upward.** It does not look up an identifier, read a column name, consult `attrs`, or raise a domain error. The caller validates before and interprets after.

**K-6 Identical signatures across languages, for named kernels.** The named kernels are the compiled-kernel candidates DESIGN §4 lists (level path, Pfafstetter, many-start DFS, bridge detection, the attribute-guided `navigate_hydro_network` modes) plus `project_point_to_line` (G-6) and the Levenshtein kernel (G-16): the places where tie rules run on numbers both languages must sort identically. For these, argument names, order, array meanings, and dtype width match. Every kernel, named or not, uses `int64` indices and `float64` weights and the K-4 sentinel; R passes `double` for both and the R kernel casts. Kernel index arrays hold dense positions (0 to n−1), never identifier values, so this int64 is numpy's native position width and a Python-internal detail; identifiers themselves are strings in Python, `integer` or `character` in R (I-1), and never enter a kernel. An unnamed kernel is language-local and the manifest marks it so; its shape is the author's.

**K-7 No compiled code in the first port.** Sequential kernels are plain loops over arrays. numba is not used in v1. numba or Rust for a kernel is a later, per-kernel, asv-measured decision (VERIFICATION §5). When a Rust core lands, the numpy and base-R kernels are kept as the unshipped conformance oracle.

**K-8 Single-threaded, deterministic.** A kernel is never parallel. Parallelism, if ever, is joblib above the boundary, with results identical to serial (D-15).

**K-9 Module layout.** An import-linter contract holds that `hydroloom._kernels` imports neither pandas, geopandas, shapely, nor `hydroloom._hy`.

| Path | Side of boundary | Contents |
| --- | --- | --- |
| `hydroloom/_kernels/` | below | arrays in, arrays out; no pandas, no identifiers, no vocabulary |
| `hydroloom/_kernels/csr.py` | below, language-local | build, transpose, ELL view (K-2) |
| `hydroloom/_kernels/walk.py` | below, language-local | level-synchronous walks: frontier expansion, strongly connected components (D-8), topological depth |
| `hydroloom/_kernels/accumulate.py` | below, language-local | downstream accumulation: triangular solve or sorted pass |
| `hydroloom/_kernels/sequential.py` | below, named (K-6) | level path, Pfafstetter, many-start DFS, bridge detection, attribute-guided navigation |
| `hydroloom/_kernels/string.py` | below, named (K-6) | Levenshtein for `string_score` (G-16) |
| `hydroloom/_kernels/geom.py` | below, named (K-6) | `project_point_to_line` (G-6) |
| `hydroloom/index_ids.py` | crosses | `IndexIds` (K-3), the only object that crosses as-is |
| `hydroloom/_hy.py` | above | `as_hy`, `hy_reverse`, `representation`, `_route`, vocabulary access |
| `hydroloom/network.py` | above | public functions: frame → `_route` → body → `IndexIds` and arrays → kernel → frame |

**K-10 R layering.** R kernel extraction is deferred, per kernel, to the moment a compiled-kernel decision fires for it (K-7), and is proven then by the conformance cases that have existed for the function since M3. Only the named kernels (K-6: `sequential`, `geom`, `string`) will ever be extracted; the rest of R stays as it is. When extracted, a kernel lives in `R/kernel-*.R` with the same name, argument order, and array semantics as its Python counterpart; 0-based dense indices at the boundary, 1-based inside, the interface documenting which. New R code written for the intended changes (ROADMAP §4: the Tarjan component check, the identifier-ordered sibling sort) follows K-4 and K-6 as it is written; existing bodies are left alone. There is no extraction step in the unit of work.

**K-11 Description.** Every function's algorithm description lives in the conformance suite (S-8). Only a named kernel's description carries a kernel signature.

## S. Shared artifacts

**S-1 Five artifacts, one copy each**, under `inst/extdata/`: the vocabulary (`hy_vocab.json` with `abbreviations.json`), the algorithm descriptions (`conformance/algorithms/<function>.md`), the manifest (`conformance/manifest.yaml`), the fixtures and expectations (`conformance/fixtures/`, `conformance/expected/`), and the remedies catalog (`remedies.json`, authored; keyed by condition, `str.format` placeholders, read by both packages at the raise site, A-10). No derived or generated form of any of these is committed. One derived file is committed beside them: `docstrings.json`, generated from `man/` by `parse_Rd` (A-13), CI-diffed against a fresh run, with `man/` itself as the precedent for a committed generated file. Test code and API shape are language-local and not shared. Neither package hard-codes an attribute name; an addition is a JSON change with a version bump.

**S-2 Vocabulary schema** (`until: hy_vocab.schema.json`, validated in pre-commit and CI). Top level: `version`, `terms` map keyed by hy name. Each term: `role` (HY_Features concept or none), `type`, `definition` (the precise paragraph), `unit` where the type implies one (`km`, `km2`, `m`, `percent`; UDUNITS-compatible), `aliases` (map from source `nhdplus`, `nhdplushr`, `3dhp`, `hydrofabric` to that source's column name), `computed_by` (function name, on computed terms; read by `capabilities()`), `tier` (`core`, `standard`, `derived`), `plain` (one sentence for a reader who does not know the term). Eight types, each a set of harness checks:

| type | value | checks |
| --- | --- | --- |
| `id` | opaque identifier | section I |
| `flag` | boolean | never missing once computed |
| `count` | non-negative integer | complete on every returned row; the definition states the domain (`stream_calc` 0 is a domain value) |
| `measure` | float, percent along a flowline | float; NaN is the missing marker |
| `length`, `area` | float with unit | tolerance per D-13; a `length` may be signed when its definition says so (`distance_km`: negative toward the outlet, positive toward the head) |
| `code` | integer from an enumerated set | `values` map from integer to meaning (`divergence`: `{0: none, 1: main, 2: minor}`); every value checked; a source alias may carry its own coding, converted on ingest |
| `fraction` | float in [0, 1] | optional `sums_to_one_over` naming a grouping column, checked per group at the D-13 tolerance (`divergence_fraction` over `fromnode`) |

`core` tier is the six the pipeline article uses: `id`, `toid`, `topo_sort`, `levelpath`, `stream_order`, `pathlength_km`; `derived` is the `up_`, `dn_`, `terminal_` family. `mainstem` gets a concept entry, marked as a concept, not a column. Absent by design: per-source type overrides beyond a `code`'s `values`; any grouping or hierarchy. `abbreviations.json` is a sibling file, versioned the same way, schema-validated the same way.

**S-3 Reading the vocabulary.** R: jsonlite on first use via `system.file("extdata", ..., package = "hydroloom")`, cached in a package environment; jsonlite in Imports. No `sysdata.rda`, no `data-raw/` step, no sync test. Python: `importlib.resources` from the wheel copy (VERIFICATION §1.3).

**S-4 Manifest schema** (`until: manifest.schema.json`). Top level: `suite_version`, `functions` map, `cases` list. `functions[<name>].requires`: the representation pattern and columns, referencing the description's YAML front matter (S-8) rather than restating it; CI asserts both packages' code-declared `requires` tables equal it (A-4); the parity generator reads it; a case naming an unlisted function fails validation. `functions[<name>].returns`: one of `frame`, `graph`, `traversal`, `scalar` (the layer convention in S-7). `functions[<name>].key`: the natural-key columns the harness sorts on before comparing a long table, so `order: ignore` has a defined meaning for non-frame returns. Each case: `id` (unique slug, stem of its expected layer), `function` (shared snake_case name), `fixture` (path under `fixtures/`), `args` (keyword map, literal values only; a fixture path where a second table is needed, never code), `expected` (path under `expected/`, default `<function>.gpkg` layer `<id>`), `compare` (`exact` or `tolerance`, with per-case `atol`, `rtol` overriding suite defaults), `order` (`exact` or `ignore`; `sort_network` is `exact`; `navigate_network_dfs` carries its order in the `order` column of its traversal layer, so `ignore` suffices), `supported_by` (list from `r`, `python`), `generator` (which of them writes the expectation). Nothing inlined: every input and every expectation is a file.

**S-5 Fixtures.** GeoPackage throughout, one file per fixture; attribute-only fixtures are aspatial tables; geometry fixtures stored projected in EPSG:5070; each under a megabyte. The fixture inventory (real subsets, synthetic networks, the state-plane copy) is VERIFICATION §2.1; the basin-scale fixture is VERIFICATION §5.

**S-6 Boundary fixtures**, one to three rows, each a decided behavior, each run as a manifest case through every function whose `requires` it satisfies:

| fixture | behavior |
| --- | --- |
| single-row network | defined values on every function's n = 1 path (`indptr` length 2, one-node DFS, level path of one) |
| all terminals (five flowlines, no edges) | identifier order from `sort_network`; five singleton level paths; zero path length |
| duplicated edge in a flownetwork | raises |
| missing value in `weight_attribute` | raises naming the row, in `accumulate_downstream` and `add_levelpaths` alike |
| zero-length geometry | measure 0 with a warning |
| empty name in `string_score` | score 0 |

**S-7 Expectations.** Generated, never typed. `expected/<function>.gpkg`, one layer per case, written by the testthat harness as an explicit step (not `_snaps/`). R is the sole generator until a function reaches parity; then the manifest may name Python as co-generator and CI checks both write expectations equal in value and dtype. Comparison is after coercing each column to the dtype the vocabulary type declares for that language (I-table), never the stored SQLite type and never bytes. Identifier columns are stored as TEXT and compared as text in both harnesses; the R harness casts an integer identifier to its decimal string on write and on compare, so a co-generated expectation is equal in value and stored type while the in-memory dtypes differ by language (I-1). An expectation about a computed value lives in the manifest, never inline in testthat or pytest.

Non-frame returns are stored as long tables, one layer each, by `returns` kind (S-4): a **graph** (`make_index_ids`) is `id`, `position` (0-based rank within the node's neighbor list, D-5 order), `neighbor`, with optional per-edge `main` and `edge_id` when present; identifier columns in the identifier type; one row per edge; terminals have no rows and the node set is the fixture's `id` by I-4; dense node position is not contract and does not appear; `mode="both"` is one layer with a `mode` column. A **traversal** (`navigate_network_dfs`) is `start`, `order` (0-based), `id`. A **scalar** (`representation`, `is_dendritic`) is a one-row, one-column layer named `value`, typed by the vocabulary type. Each harness has one converter per kind, checked by round trip.

**S-8 Algorithm description template** (`until: algorithms/TEMPLATE.md`; the schema validator checks every heading is present). Sections, all required, none long. YAML front matter holds `requires` (the representation pattern and columns), which the manifest references and both code registries are checked against (S-4, A-4). **Signature** (shared name; argument names with vocabulary types; return columns with types; argument prose is not here, it comes from roxygen, A-13; a named kernel's signature, K-6, K-11), **Preconditions** (representation and columns required; what is raised; this is `_route`'s `needs`), **Definition** (the output as a property, not steps), **Algorithm** (steps, every tie rule, the sentinel, the D rules relied on, cited by anchor), **Postconditions** (the invariants; the hypothesis list), **Complexity** (time and space in n and m), **Edge cases** (plain words: one flowline, no connections, duplicated row, missing weight, zero-length line, empty name; returns, warns, or stops, and the value), **Remedies** (table keyed by condition: what is wrong, which rows, the one call that fixes it; each row cites its `remedies.json` key, A-10, or is rendered from the catalog at docs build), **References**, **Worked example** (one synthetic fixture with its expected output), **Changes** (dated, tied to suite versions).

Binding sections for the Python-side session: Signature, Preconditions, Postconditions, and every tie rule and sentinel in Algorithm bind; Algorithm's step order does not. A kernel the description names binds by signature (K-6); a kernel it does not name is the author's to shape.

**S-9 Versioning of shared artifacts.** `suite_version` is independent of both package versions; the Python package pins a suite version, never an R release; an R patch release does not imply a Python release; a suite bump implies a release of whichever package's expectations changed. Before Python 1.0, the Python package may pin a suite version whose expectations were generated from an unreleased R `main`; the R release that carries them follows when it happens (no CRAN release is scheduled against the port's milestones; 1.3.0 is the last one the plan assumes). The vocabulary carries its own `version`; a vocabulary change is a minor version bump in both packages.

**S-10 Conformance directory and CRAN.** `inst/extdata/conformance/` is Rbuildignored (VERIFICATION §1.2). The R harness locates it with `testthat::test_path()` and `skip_if_not(dir.exists(...))`; tests that ship to CRAN are minimal interface tests, plus the collation test (D-1) and the vocabulary schema test. The Python sdist keeps the directory, so conda-forge runs conformance.

## I. Identifiers and missingness

Every rule is asserted by the harness on dtype, not only value.

| # | Topic | Rule | R | Python |
| --- | --- | --- | --- | --- |
| I-1 | Identifier type | Identifiers are opaque: never assumed numeric, contiguous, or ordered. R has two identifier types, 32-bit integer and string, fixed on ingest (I-2) and preserved through every function; Python has one, string (2026-10-25, DESIGN §7). Never float, never 64-bit on either side. Across languages an identifier compares as its decimal string (S-7). | `integer` or `character` | `str`/`object`, always |
| I-2 | Identifier type on ingest | R: an integer-valued column whose values all fit in 32 bits becomes the 32-bit integer type; one with any value outside that range becomes its exact decimal string. The rule is on values, not on the arriving dtype. Python: every identifier column becomes its exact decimal string on ingest, whatever arrives. The same file therefore yields R `integer` and Python `str` for New Hope, and the harness compares identifiers as text (S-7). No 64-bit integer identifier exists on either side. | `integer64` or whole-valued `double` (how sf reads a 64-bit column) → `integer` if within range, else `character`; bit64 not a dependency | `int64`, `int32`, or whole-valued float → `str` (exact decimal); `str` unchanged; no cast on return |
| I-3 | Mixed types in one column | Not supported; raises. | `stop()` | `TypeError` |
| I-4 | Terminal detection | A terminal is a row whose `toid` is not present in `id`. The value is not consulted. | `!toid %in% id` | `~toid.isin(id)` |
| I-5 | Canonical `toid` fill | On ingest, missing `toid` becomes the reserved value for the identifier's type: `0` integer, `""` character; in Python always `""` (I-2). Downstream functions never depend on it. | `hy()` replaces `NA` | `as_hy()` replaces `pd.NA` / `None` / `NaN`; a column left without missing values returns in its non-nullable dtype |
| I-6 | Repeated `id` | A representation signal, not an error: `id` with `toid` where `id` repeats is the flownetwork pattern. Functions needing unique `id` raise through the representation check, naming the pattern needed. | subclass stamped by `hy()` | `representation(df)` (A-2) |
| I-7 | Missing measures | Missing, not `0` or `-1`. | `NA_real_` | `NaN` in float64 |
| I-8 | Missing flags | Never missing once computed; unknown means not yet run. | logical, no `NA` | `bool`, never nullable `boolean` |
| I-9 | Computed integer attributes | Never missing on a returned row (stream order starts at 1, stream level at 0, stream calc 0 is a domain value). Zero-row input types the column with no rows. A user-supplied integer column with missing values passes through in its dtype; a function that consumes it raises naming the column. No nullable extension dtype anywhere in the public API. | `integer`, complete | `int32`, complete |
| I-10 | Node identifiers | Follow I-1 to I-3. Package-created node ids from an edge list are a deduplication device, discardable, not stable across calls unless the input carried them. | | |
| I-11 | Empty network | Zero-row input returns zero-row output with all computed columns present and typed. | | |
| I-12 | Index vs identifier | The row index is never load-bearing. | row names ignored | index reset on ingest, never consulted; output keeps input row order unless the function is a sort |
| I-13 | pandas 3 strings | `as_hy()` does not cast around the `str`/NaN default. I-1 and I-5 keep missing markers out of kernels; the harness compares after coercion (S-7), so pandas 2 and 3 results compare equal. | | |
| I-14 | Original names | `as_hy()` keeps no state on the frame; nothing rides on `df.attrs`. `hy_reverse(df, source=)` inverts from the vocabulary's `aliases` or `register_names()`; an ad hoc mapping is passed back explicitly. | `orig_names` attribute | no frame state |

**I-15 Type policy.** Every vocabulary term carries one of the eight S-2 types; the harness maps each to its R and Python dtype from this table. A term cannot be added without a type.

## D. Determinism

Wherever an algorithm underdetermines its output, the function's description states the rule and a conformance fixture exercises the tie.

| # | Point | Rule |
| --- | --- | --- |
| D-1 | String collation | Byte order (code point), never locale. R: every sort goes through `dplyr::arrange` or `data.table::setorder`/`setorderv`, both C-locale; guaranteed by version floor `dplyr (>= 1.1.0)`, checked by a CRAN-shipped testthat test sorting `"a"`, `"B"`, `"_c"` and asserting C order. Python default is code point. |
| D-2 | R version floors | Moved to VERIFICATION §1.6; number held. |
| D-3 | Sort stability | All sorts stable. pandas `sort_values(kind="stable")`, numpy `argsort(kind="stable")`; data.table and dplyr are stable already. |
| D-4 | Sibling order in `sort_network` | Among flowlines sharing a `toid`, by identifier byte order and nothing else. `sort_network` reads `id` and `toid` only. Output order is part of the expected output, compared exactly. Idempotent and permutation-invariant. Intended change from R's DFS-visit order; NEWS entry. |
| D-5 | Adjacency order in `make_index_ids` | Every node's CSR neighbor list is in identifier byte order (one argsort on (row, id) after the bincount), so every walking kernel inherits a canonical order. |
| D-6 | Traversal order in `navigate_network_dfs` | Depth-first, siblings in identifier byte order (from D-5); visited identifiers are returned in traversal order, so the order is output and is contract through the `order` column of the traversal layer (S-7). Level-path-following walks (UM, DM) are canonical because `levelpath` is. |
| D-7 | Permutation invariance | No computed value depends on input row order. For every manifest function: shuffle input rows, run, sort both outputs by `id`, assert equal. The master property. For `sort_network` and `navigate_network_dfs`, whose output order is itself contract (D-4, D-6), an un-sorted variant asserts the outputs equal without the sort. |
| D-8 | Cycle detection in `check_hy_graph` | One check, always: strongly connected components over the `id`/`toid` graph. Any component larger than one fails the network; its rows are returned labeled `component`; a size-two component is additionally labeled `reciprocal`; a self-loop (`toid == id`) is a cycle. `loop_check` argument removed. Python `scipy.sparse.csgraph.connected_components(connection="strong")`; R a Tarjan kernel in base R. `sort_network` calls it first. Intended change; NEWS entry. |
| D-9 | Divergence tie-breaking | With no source `divergence` attribute, the main path follows the rule sequence `add_divergence` implements today, codified verbatim in its description; identifier byte order is appended as the only fallthrough. The source attribute always wins when present. |
| D-10 | Point-to-line ties | The kd-tree yields candidates only; the kernel sorts by (projected distance rounded to `tie_tol`, flowline identifier byte order) and takes the first `max_matches`. `tie_tol` = 1 m, a documented constant, not an argument. Same rule for endpoint matching in `make_attribute_topology`. Cases in G-7. |
| D-11 | Level path at confluences | Name match, then drainage area, then arbolate sum; ties fall through to identifier byte order. |
| D-12 | Accumulation order | Path length and arbolate sum summed in sorted network order, downstream to upstream, float64. R's extended-precision `sum` differs in the last bits; invisible at D-13. |
| D-13 | Floating-point comparison | `abs(a − b) <= atol`, `rtol = 0`, `atol` per vocabulary type from meter-scale source precision: 1 m for `length` (10⁻³ in km), 1 m² for `area` (10⁻⁶ in km²), for `measure` the meter along the flowline, 100 / length_m percent, computed per row. `aggregate_id_measure` rounded to four decimals is compared exactly. Identifiers, counts, orders, codes, flags exact. Per-case manifest overrides for a description that declares a value exact. |
| D-14 | Row and column order | Non-sorting functions preserve input row order. Computed columns append in the description's order; expected outputs compare by column name, not position. |
| D-15 | No randomness; parallel equals serial | No dependence on `set.seed`, hash randomization, dict insertion order of unsorted input, or scheduling. Parallel paths are tested against the same expectations. |
| D-16 | Capture then change | A function's first cases assert current R behavior; an intended change is a named step: new cases, the R change on `main` with a NEWS entry (the CRAN release that carries it follows on its own schedule), then the Python port against the new expectations. Exception: an accident of implementation no user could have relied on (locale collation, unstable sort order, an undecided tie) gets the intended rule directly, with a NEWS entry and no capture step. |
| D-17 | Nearest hydro location | In `add_hydro_location_distance`, a position equidistant from two hydro locations on its aggregate flowpath takes the smaller `hydro_location_id` compared as a string (byte order, D-1). Shipped in 1.3.0; captured, not an intended change. |

## G. Geometry

| # | Topic | Rule |
| --- | --- | --- |
| G-1 | GEOS version | The `docs` and `r` pixi environments pin one GEOS. The Python matrix does not; D-13 absorbs minor-version differences. |
| G-2 | Units, geometry side | Every geometry-derived argument and output (`search_radius`, `precision`, `gap_tolerance`, `tie_tol`, `offset`, `snap_m`, `get_partial_length`; `max_hole_area` in square units) is in meters at the API boundary, converted with the exact CRS factor (G-4). Outputs are bare floats, never wrapped. |
| G-3 | Units, attribute side | An argument compared against a column (`distance` in `navigate_hydro_network` against `length_km`, `pathlength_km`) is in the vocabulary's `unit` for that column. A bare number is taken as that unit with a warning (`UnitsAssumedWarning` in Python, a classed `warning()` in R), never an error. A unit-bearing value (`units` in R; duck-typed `.to`/`.magnitude` in Python) is converted on entry with no warning; a dimension mismatch is the unit library's own error. No package imports a unit library. Attribute arithmetic that combines one column with itself or a fraction is unit-free and untouched. Conformance case: `distance` bare, as km, as miles; equal results; only the bare call warns. |
| G-4 | CRS admission | A distance-taking function inspects the CRS on entry (pyproj `is_projected` and axis unit with exact factor; `sf::st_crs()`). Projected metres: as-is. Projected other linear unit: arguments converted in and outputs out by the reported factor. Geographic: raise, naming the CRS, saying to project first, with the `to_crs` / `st_transform` line in the docs. R warns for one minor release then raises; Python raises from its first release. The 0.01° default is gone; `search_radius = 200` m is the only default. Case: one network in EPSG:5070 and in a feet-based state plane zone, equal to tolerance. |
| G-5 | Reprojection regimes | Point-indexing functions (`index_points_to_lines`, `index_points_to_waterbodies`) reproject the network to the points' CRS (which must pass G-4) and return offsets in meters. Functions measuring the network (`add_pathlength`, `add_measures`, `get_partial_length`) require the network's CRS to pass G-4 and never reproject. `dissolve_polygons` works in `work_crs` and transforms back; `work_crs` is required in Python; R deprecates the 5070 default in the next minor release, then requires it; the error names 5070 as the usual CONUS choice; `work_crs = NULL` with geographic input raises. |
| G-6 | Point-to-line algorithm | Two stages. Candidates: lines cast to `LINESTRING`, optionally densified by `segmentize(precision)` after unit conversion, vertices exploded, a kd-tree (`scipy.spatial.KDTree`; `RANN::nn2`) returns every vertex within `search_radius`, reduced to distinct flowlines. Selection and measure: the `project_point_to_line` kernel (`_kernels/geom.py`, `R/kernel-geom.R`, same signature; point array and each candidate's coordinate array in) computes per segment the clamped scalar projection `t = dot(p − a, b − a) / |b − a|²`, `t` in [0, 1], minimum over segments; returns perpendicular distance and fraction along the line, on the original undensified geometry. Smallest projected distance wins; `offset` is that distance; the measure is that fraction. `ids` restricts the search to one named flowline per point and overrides `max_matches`; `aggregate_ids` restricts it to the flowlines carrying each point's `aggregate_id`; the two are exclusive. Intended change from R's nearest-vertex answer; NEWS entry; the R change on `main` generates the expectations (D-16). |
| G-7 | Point-to-line cases | An exact tie from integer coordinates where tree order and id order disagree; a near-tie inside `tie_tol`; `max_matches = 2` asserting the order of both; a point whose nearest densified vertex is on one line while its projection is nearer another. |
| G-8 | `precision` | Meters; densifies for the candidate search only; recall lever. Recall is complete when `search_radius >= precision`; the function warns when that does not hold. No effect on returned offset or measure. |
| G-9 | Measures | Percent from downstream end: 0 at outlet, 100 at inlet. Aggregate-id measures require `frommeas`/`tomeas` on every flowline. `aggregate_id` and `aggregate_id_measure` output columns appear only when the network carries them; compared by present columns. Fraction along line (0 to 1) internally; conversion at the boundary. The aggregate flowpath axis (1.3.0) is cumulative `length_km` per `aggregate_id` from the downstream end, built from flowline lengths and independent of `frommeas`/`tomeas`; it is refused when the flowlines of an `aggregate_id` do not link one to the next. `id_from_measure` and `id_to_measure` bound a piece of one flowline. A distance from a hydro location is signed: negative toward the outlet, positive toward the head (HY_DistanceDescription names the same two directions). A confluence hydro location sits on the receiving flowpath at `id_measure` 100 of the first flowline below the junction, never on the tributary. A position resolving exactly onto a flowline boundary belongs to the lower flowline at `id_measure` 100. |
| G-10 | Geometry types | Flowlines cast to `LINESTRING` on entry to a geometry-requiring function, as R's `st_cast` sites do (`as_hy` carries the geometry column through untouched, G-14); multipart measures documented as possibly wrong, as in R. Points are `POINT`; other types raise. Polygons for dissolve are `POLYGON` or `MULTIPOLYGON`. |
| G-11 | Splitting and rescaling | `substring` follows GEOS `LengthIndexedLine`. A split at a fraction yields two `LINESTRING`s sharing the split vertex exactly; the vertex is inserted, not snapped. |
| G-12 | Precision model | None applied; coordinates pass through. Snapping, if ever, is an explicit function. |
| G-13 | Empty and invalid geometry | Empty geometry raises on ingest for geometry-requiring functions. Invalid geometry is not repaired silently; `check_valid()` is the explicit step. |
| G-14 | Geometry column | geopandas' active geometry column and sf's `sf_column`, whatever the name; output preserves it. |

**G-15** moved to ROADMAP §2 (linear-referencing group and its primitives); the number is held.

**G-16 `string_score`.** Unit-cost Levenshtein on normalized names, scored `1 − d / max(len(a), len(b))`. Normalization: lowercase, punctuation stripped, whitespace collapsed, abbreviations expanded from `abbreviations.json` (adopted from USGS hydrolink's `clean_water_name`). No stop-word removal. Empty name scores 0. Kernel with shared behavior and language-local implementation: `utils::adist()` default costs in R; a Wagner–Fischer loop in Python (`difflib` is a different ratio). Ties fall through to identifier byte order. `disambiguate_indexes` selects the closest candidate scoring ≥ 0.6, else the closest. Case: point names against flowline names with a known best match and one deliberate tie.

**G-17 Geometry utilities** are kept by name as thin wrappers over shapely 2 and geopandas primitives, with the native equivalent named in each docstring. The function-to-primitive mapping is in ROADMAP §2.

## A. API shape

| # | Topic | R | Python |
| --- | --- | --- | --- |
| A-1 | Entry point | generic functions taking and returning a data.frame or sf; `hy()` stamps a subclass | module-level functions taking and returning a DataFrame or GeoDataFrame; `_route` dispatches on `representation(df)`; no subclass, no stamping, no user-adopted class |
| A-2 | Representation | `hy_topo`, `hy_leveled`, `hy_node`, `hy_flownetwork` stamped by `hy()`; `classify_hy` re-derives on every entry | `representation(df)` returns one of the four pattern names from the column rules of `classify_hy` (presence of `toid`, `fromnode`/`tonode`, uniqueness of `id`), computed on demand at every entry |
| A-3 | `_route` | `hy_classify_and_redispatch`, `hy_node_to_topo`, `hy_dispatch_error` | A registration decorator, `@requires(Representation.TOPO, convert={Representation.NODE: add_toids})`, declares each public function's `needs` and `convert` in a module-level registry (the xarray-backend / scikit-learn-tags pattern). `_route(df, fn, native, **kw)` reads that entry: computes the representation; applies `convert[label]` if present (node → topo via `add_toids`, silently); calls `native[label]`; else raises `RepresentationError` with the guidance text from `remedies.json` (A-10). `LEVELED` satisfies `needs=TOPO` by a one-line subsumption rule. `native` bodies are the `*_impl` layer, directly above the kernels. Geometry-dependent behavior branches on `isinstance(x, GeoDataFrame)` inside a body. |
| A-4 | Capabilities | `hy_capabilities(x)`: a frame (function, available, via) from a hand-written table in code, today's vector plus the test it lacked | `capabilities(network)`: frame with `function`, `available`, `via` (the one call that makes an unavailable function callable), iterated from the decorator registry (A-3). Neither package reads the manifest at runtime; CI asserts both code tables equal the manifest's `requires` (`test_api.py`; a testthat test; the `conformance-parity` job across the two). |
| A-5 | Naming | `hy()` / `hy_reverse()`; `hydroloom_names()` registers mappings. `hy(x, clean = FALSE, add_topo = FALSE)` | `as_hy(network, *, id_col=None, toid_col=None)` / `hy_reverse(network, source=)`; `register_names(mapping, *, replace=False)`, also a context manager on the `pandas.option_context` pattern; `name_definitions()`, `align_names(network)`. `clean` and `add_topo` are deliberate omissions, not gaps: pandas users select columns themselves, and `_route` converts node to topo on demand; the rename map and the docstring generator do not carry their text over. |
| A-6 | Graph representation | `make_index_ids(x, mode=)` list; `mode` is `"to"`, `"from"`, or `"both"` already, with `main` and `edge_id` carried when the network has them; no intended change | `make_index_ids(network, *, mode="to")` → `IndexIds` (K-3), optional `main` and `edge_id` |
| A-7 | Accessor | — | optional and later: `df.hy.<function>()` delegating to module functions; no logic in it |
| A-8 | Mutation and return type | never mutates; same class as input | never mutates; no `inplace=`; same class as input; geometry never dropped or added as a side effect |
| A-9 | Names and arguments | `snake_case`; `x` for the network | identical function names; identical argument names where the concept is identical, with one exemption: the first positional argument is `network` wherever R's `x` is a network table (phase 1–2; `navigate_network_dfs` takes an `IndexIds` under the same name) and the operand noun (`points`, `lines`, `polygons`) in phase 3, since it is positional with `*` after it and its keyword is almost never written; Python-only arguments appended, never inserted; data first, then keyword-only (`*`); where a default disagrees with R, the R signature wins; bodies use `net` or `network`, ruff `PD901` on |
| A-10 | Errors | `stop()` / `rlang::abort()`; one condition class, `hydroloom_representation_error`; message text from `remedies.json` through a small helper | `errors.py`, three classes and no more: `HydroloomError` (base), `RepresentationError` (from `_route`, naming pattern and columns), `CRSError` (geographic or missing CRS), each also a `ValueError`. Vocabulary problems `KeyError`; mixed-type identifiers `TypeError`; `check_hy_graph` returns a table, never raises. Text for both languages comes from `inst/extdata/remedies.json` by key with `str.format` placeholders (`raise RepresentationError(msg("needs_topo", rows=...))`), never typed at the raise site; each entry says the condition, the rows, the one call that resolves it, and the description's Remedies section cites the key (S-8). |
| A-11 | Warnings | `warning()`, classed | `warnings.warn` with a package warning class and `stacklevel=2` on every call; deprecations `FutureWarning` for at least one minor release before removal |
| A-12 | Logging and progress | `message()` behind `verbose` | none in version one; nothing printed, nothing logged |
| A-13 | Docstrings | roxygen2 is the authored documentation source; `man/` is generated from it as today, with no `man-roxygen/` step. A `dev/` script runs `tools::parse_Rd()` over `man/*.Rd`, indexed by `\alias`, and writes `inst/extdata/docstrings.json` (committed, CI-diffed, force-included in the wheel): title, description, details, each `@param`, `@returns`, with `\code` and `\verb` as backticks, `\link` as the bare name, lists as `- ` and `1. `; lines of the form `Required attributes: ...` dropped (`capabilities()` is the structured answer); `@examples` not transferred. roxygen inheritance in use as of 1.3.0: `@inheritSection` four times (the Measures section of `index_points_to_lines`), resolved into `man/`; `@inheritParams`, `@template`, `@family` none, so no roclet is needed. | numpydoc; a decorator composes the docstring at import from `docstrings.json` (pandas `_shared_docs`/`@doc`, matplotlib `_docstring.Substitution`, scipy `doccer` precedent), with a substitution table for R phrasing (`data.frame network compatible with hydroloom_names` → a table; `logical if TRUE` → if true) and the language-neutral pass done per function in its description PR; every public function has a `See Also` naming the R function and a hand-written doctest. Validation runs on the imported objects with `numpydoc.validate` inside a pytest test; ruff `D417` and the numpydoc pre-commit hook are exempted for decorated functions. Rule for both: an argument taking a column name is documented by what the column has to mean (`weight_attribute`: "a column where larger means more upstream"), never by an example column from one dataset. |
| A-14 | Type hints | — | on every public function; `py.typed`; mypy strict on `_kernels` and `index_ids.py` (kernel signatures `NDArray[np.int64]`, `NDArray[np.float64]`), standard elsewhere; no `pandas-stubs` |
| A-15 | Parity table | generated from the manifest's `supported_by`; a Python function that exists without conformance cases is listed as experimental; `hydroloom.__all__` is the set of manifest functions whose `supported_by` includes `python` (A-18) | |

**A-16 Phase-1 signatures.** `network` first, everything else keyword-only, argument names copied from R (A-9).

| Function | Signature | Returns |
| --- | --- | --- |
| `as_hy` | `as_hy(network, *, id_col=None, toid_col=None)` | frame with hy names; the exemplar's scope is vocabulary-alias renaming, I-2 coercion, I-3 raise, I-5 fill, I-12 index reset, G-14 pass-through, plus the private `_hy_reverse(out, names)` A-17 needs |
| `hy_reverse` | `hy_reverse(network, source=None)` | frame with source names (I-14) |
| `is_hy` | `is_hy(network)` | bool |
| `representation` | `representation(network)` | one of the four pattern names (A-2) |
| `capabilities` | `capabilities(network)` | frame: `function`, `available`, `via` (A-4) |
| `register_names` | `register_names(mapping, *, replace=False)` | none; also usable as a context manager |
| `name_definitions` | `name_definitions()` | frame from the vocabulary |
| `align_names` | `align_names(network)` | frame |
| `add_toids` | `add_toids(network, *, return_dendritic=True)` | frame |
| `make_index_ids` | `make_index_ids(network, *, mode="to")` | `IndexIds`; a pair for `mode="both"` (K-3) |
| `sort_network` | `sort_network(network, *, split=False, outlets=None)` | frame, row order per D-4 |
| `add_topo_sort` | `add_topo_sort(network, *, split=False, outlets=None)` | frame |
| `navigate_network_dfs` | `navigate_network_dfs(network, starts, *, direction="down", reset=False)` | list aligned with `starts`; each element the visited identifiers in traversal order (D-6), as a pandas Index in the identifier dtype |

`mode` is one of `"to"`, `"from"`, `"both"`. `direction` is one of `"up"`, `"down"`. `network` in `navigate_network_dfs` is a frame or an `IndexIds`.

**A-17 Routing pattern.** How a public function is written, showing the `@requires` registration (A-3), `_route`, and the one real type branch:

```python
@requires(Representation.TOPO, convert={Representation.NODE: add_toids})
def sort_network(network, *, split=False, outlets=None):
    df, names = _as_hy(network)                # names is a local, never frame state
    out = _route(df, sort_network,             # needs and convert come from the registry entry
                 native={Representation.TOPO: _sort_network_topo,
                         Representation.FLOWNETWORK: _sort_network_fn},
                 split=split, outlets=outlets)
    return _hy_reverse(out, names)

def navigate_network_dfs(network, starts, *, direction="down", reset=False):
    if isinstance(network, IndexIds):
        return _navigate_dfs_index(network, starts, direction=direction, reset=reset)
    df, _ = _as_hy(network)
    return navigate_network_dfs(make_index_ids(df, mode=direction), starts,
                                direction=direction, reset=reset)
```

`capabilities()` iterates the same registry the decorator fills; `test_api.py` asserts the registry equals the manifest's `requires` (A-4).

**A-18 `__all__`.** `hydroloom.__all__` equals the set of manifest functions whose `supported_by` includes `python`; `tests/python/test_api.py` asserts it.
