# Changelog

## hydroloom 1.2.0

This release introduces an S3 class hierarchy (`hy_topo`, `hy_leveled`,
`hy_node`, `hy_flownetwork`) that lets hydroloom functions validate
input at dispatch time and provide guided error messages when the wrong
network representation is passed. Existing code that passes `data.frame`
or `hy` objects continues to work without changes – the new classes are
assigned automatically and are transparent to downstream consumers.
Package developers who depend on hydroloom should note that returned
objects now carry subclass attributes (e.g. `hy_topo`) which are
stripped by
[`hy_reverse()`](https://doi-usgs.github.io/hydroloom/reference/hy_reverse.md)
and by standard dplyr operations.

- Outlet detection is explicit
  ([\#85](https://github.com/DOI-USGS/hydroloom/issues/85)): a row is an
  outlet when its `toid` is not in `id`. Canonical reserved values (`0`
  / `""`), `NA`, implicit absence, foreign reserved values, and
  unique-per-outlet ids are all accepted.
- [`hy()`](https://doi-usgs.github.io/hydroloom/reference/hy.md) still
  normalizes `NA` `toid` to the canonical reserved outlet value, so
  `x$toid == 0` after
  [`hy()`](https://doi-usgs.github.io/hydroloom/reference/hy.md) keeps
  working.
- [`make_node_topology()`](https://doi-usgs.github.io/hydroloom/reference/make_node_topology.md)
  and
  [`is.hy()`](https://doi-usgs.github.io/hydroloom/reference/is.hy.md)
  accept `NA` and orphan `toid`; the related errors are removed.
- `check_hy_outlets()` warns only on `id`/`toid` type mismatch.
- [`sort_network()`](https://doi-usgs.github.io/hydroloom/reference/sort_network.md)
  warns only when a network has zero outlets.
- Improve performance of
  [`add_levelpaths()`](https://doi-usgs.github.io/hydroloom/reference/add_levelpaths.md)
  by converting to data.table
- Add S3 class hierarchy: `hy_topo`, `hy_leveled`, `hy_node`,
  `hy_flownetwork` –
  [\#73](https://github.com/DOI-USGS/hydroloom/issues/73)
- [`hy()`](https://doi-usgs.github.io/hydroloom/reference/hy.md) gains
  `add_topo` parameter to auto-build toid from fromnode/tonode
- New exported helpers:
  [`hy_network_type()`](https://doi-usgs.github.io/hydroloom/reference/hy_network_type.md),
  [`is_dendritic()`](https://doi-usgs.github.io/hydroloom/reference/is_dendritic.md),
  [`hy_capabilities()`](https://doi-usgs.github.io/hydroloom/reference/hy_capabilities.md)
- Print methods for `hy_topo`, `hy_node`, `hy_flownetwork`
- Producer functions now stamp output classes:
  [`add_toids()`](https://doi-usgs.github.io/hydroloom/reference/add_toids.md)
  -\> `hy_topo`,
  [`sort_network()`](https://doi-usgs.github.io/hydroloom/reference/sort_network.md)
  -\> `hy_topo`,
  [`add_levelpaths()`](https://doi-usgs.github.io/hydroloom/reference/add_levelpaths.md)
  -\> `hy_leveled`,
  [`make_node_topology()`](https://doi-usgs.github.io/hydroloom/reference/make_node_topology.md)
  -\> `hy_node`,
  [`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md)
  -\> `hy_flownetwork`
- [`add_divergence()`](https://doi-usgs.github.io/hydroloom/reference/add_divergence.md)
  sets `attr(x, "dendritic") <- FALSE` on output
- `add_toids(return_dendritic = FALSE)` is deprecated; use
  [`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md)
- S3 method dispatch: functions now dispatch on subclass
  (e.g. `.hy_topo`, `.hy_leveled`) with guided error messages for wrong
  input class
- Functions that require `hy_leveled`
  (e.g. [`add_pfafstetter()`](https://doi-usgs.github.io/hydroloom/reference/add_pfafstetter.md),
  [`navigate_hydro_network()`](https://doi-usgs.github.io/hydroloom/reference/navigate_hydro_network.md),
  [`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md))
  fall through from `hy_topo`/`hy_node` when required columns are
  already present
- Fix pre-existing bug in `make_to_dt()` where dendritic branch failed
  on tibble input (data.table `with = FALSE` syntax on plain data.frame)
- Fix
  [`get_bridge_flowlines()`](https://doi-usgs.github.io/hydroloom/reference/get_bridge_flowlines.md)
  correctness on networks with independent terminals:
  `make_nondendritic_topology()` collapsed all rows carrying the
  reserved outlet value into one synthetic node, misclassifying bridges
  and exhausting memory on continental networks
- [`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md)
  and
  [`get_bridge_flowlines()`](https://doi-usgs.github.io/hydroloom/reference/get_bridge_flowlines.md)
  now accept `hy_node` input without `divergence`/`levelpath`,
  auto-converting to a non-dendritic edge list with a warning that
  main-path info is dropped
- `hy_flownetwork` no longer inherits from `hy` – it is a separate
  junction table where `id` is not guaranteed to be a primary key, and
  it does not pass
  [`is.hy()`](https://doi-usgs.github.io/hydroloom/reference/is.hy.md).
  Documented behavior already; the implementation now matches
- Documentation:
  - Add class-level roxygen pages for `hy_topo`, `hy_leveled`,
    `hy_node`, and `hy_flownetwork` describing representation pattern,
    required columns, supported functions, and conversion paths
  - Add divergence case study to
    [`vignette("non-dendritic")`](https://doi-usgs.github.io/hydroloom/articles/non-dendritic.md)
    showing how a secondary path is dropped in `hy_topo` form and
    preserved in `hy_flownetwork` form
  - Add
    [`hy_capabilities()`](https://doi-usgs.github.io/hydroloom/reference/hy_capabilities.md)
    pipeline walkthrough to
    [`vignette("network_navigation")`](https://doi-usgs.github.io/hydroloom/articles/network_navigation.md)
    demonstrating the `hy` -\> `hy_node` -\> `hy_topo` -\> `hy_leveled`
    -\> `hy_flownetwork` progression
  - Style and clarity pass across all vignettes
- Domain decomposition gains a containment relationship that lets
  callers declare, post-decomposition, that one domain (a catchment
  aggregate, in HY_Features terms) is enclosed by another – endorheic
  basins, drainage-divide remnants, or any isolated component the caller
  wants treated as contained. New
  [`set_containment()`](https://doi-usgs.github.io/hydroloom/reference/set_containment.md)
  records the declaration;
  [`get_containing_domain()`](https://doi-usgs.github.io/hydroloom/reference/get_containing_domain.md)
  reads it back;
  [`get_domain_graph()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_graph.md)
  returns one row per declaration (with `nexus_id = NA` and
  `relation_type = "contained"`);
  [`recompose()`](https://doi-usgs.github.io/hydroloom/reference/recompose.md)
  gains a `containment` argument that defaults to `"ignore"` (each
  basin’s accumulated value stops at its own outlet) and optionally
  `"accumulate"` (the contained basin’s accumulated value is added at
  the containing domain’s outlet and routed downstream through the
  containing basin’s extensive network). Containment does not appear in
  `nexus_registry` because no flow crosses a hydro nexus between the two
  basins.
- **Deprecation notice:** A future release will require that `hy_topo`
  objects have unique `id` values (one row per catchment). Non-dendritic
  connectivity with duplicated ids in a toid-based edge list will need
  to be represented as `hy_flownetwork` (via
  [`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md)).
  Developers who currently pass non-dendritic toid tables through
  hydroloom functions should migrate to
  [`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md)
  or
  [`make_node_topology()`](https://doi-usgs.github.io/hydroloom/reference/make_node_topology.md)
  for non-dendritic workflows.

## hydroloom 1.1.3

CRAN release: 2026-02-20

Update test tolerances for failing Fedora CRAN tests

## hydroloom 1.1.2

CRAN release: 2026-02-20

Hydroloom 1.1.2 introduces new functionality in
[`accumulate_downstream()`](https://doi-usgs.github.io/hydroloom/reference/accumulate_downstream.md),
reworks the
[`make_index_ids()`](https://doi-usgs.github.io/hydroloom/reference/make_index_ids.md),
and deprecates
[`make_fromids()`](https://doi-usgs.github.io/hydroloom/reference/make_fromids.md)
and
[`format_index_ids()`](https://doi-usgs.github.io/hydroloom/reference/format_index_ids.md)
for clarity of package function. Deprecated functions will be removed in
the next major version release.

- [`subset_network()`](https://doi-usgs.github.io/hydroloom/reference/subset_network.md)
  has been added to support subsetting networks to include all
  diversions that emanate from the basin. –
  [\#60](https://github.com/DOI-USGS/hydroloom/issues/60)
- [`accumulate_downstream()`](https://doi-usgs.github.io/hydroloom/reference/accumulate_downstream.md)
  now supports “total upstream” and “divergence routed” accumulation. –
  [\#17](https://github.com/DOI-USGS/hydroloom/issues/17)
- [`make_index_ids()`](https://doi-usgs.github.io/hydroloom/reference/make_index_ids.md)
  has been rewritten. It now uses four modes (“to”, “from”, and “both”).
- [`make_fromids()`](https://doi-usgs.github.io/hydroloom/reference/make_fromids.md)
  is deprecated in favor of
  [`make_index_ids()`](https://doi-usgs.github.io/hydroloom/reference/make_index_ids.md)
  with mode = “from”.
- [`format_index_ids()`](https://doi-usgs.github.io/hydroloom/reference/format_index_ids.md)
  is deprecated. The \*\_list element of
  [`make_index_ids()`](https://doi-usgs.github.io/hydroloom/reference/make_index_ids.md)
  can be unnested instead.

## hydroloom 1.1.1

CRAN release: 2025-10-01

- fix bug with sort_network when duplicate entries are in the extended
  attributes – [\#52](https://github.com/DOI-USGS/hydroloom/issues/52)
- add specific id search to index_points_to_lines for
  [\#24](https://github.com/DOI-USGS/hydroloom/issues/24)
- error handling [\#49](https://github.com/DOI-USGS/hydroloom/issues/49)

## hydroloom 1.1.0

CRAN release: 2024-08-26

- Add new vignette for network navigation
  [`vignette("network_navigation")`](https://doi-usgs.github.io/hydroloom/articles/network_navigation.md)
- Add support for upmain and downmain navigation in
  [`navigate_network_dfs()`](https://doi-usgs.github.io/hydroloom/reference/navigate_network_dfs.md)
- Add support for upmain and downmain in
  [`make_index_ids()`](https://doi-usgs.github.io/hydroloom/reference/make_index_ids.md)
  and
  [`make_fromids()`](https://doi-usgs.github.io/hydroloom/reference/make_fromids.md).
- Add function to create a “flownetwork” representation of the network
  with a `to_flownetwork` function.
- Improved handling / fix bugs with edge cases in
  [`add_toids()`](https://doi-usgs.github.io/hydroloom/reference/add_toids.md)
- Improved error conditions when missing suggested packages.
- [`navigate_hydro_network()`](https://doi-usgs.github.io/hydroloom/reference/navigate_hydro_network.md)
  will now navigate from a diverted path to a main path where it
  previously only followed traditional tributaries..

## hydroloom 1.0.1 and 1.0.2

CRAN release: 2024-01-09

- Add checks if nhdplusTools is available.

## hydroloom 1.0.0

CRAN release: 2023-09-29

- Initialized new project
- Create basic `hy` s3 object handling
  [`hy()`](https://doi-usgs.github.io/hydroloom/reference/hy.md)
- Ensure tibble is used throughout
- Support generic depth first search navigation
  [`navigate_network_dfs()`](https://doi-usgs.github.io/hydroloom/reference/navigate_network_dfs.md)
- Complete migration of nhdplusTools functions to hydroloom.
  [\#1](https://github.com/DOI-USGS/hydroloom/issues/1)
- Support sorting non-dendritic network in
  [`sort_network()`](https://doi-usgs.github.io/hydroloom/reference/sort_network.md)
- Implemented non-dendritic network support in
  [`add_toids()`](https://doi-usgs.github.io/hydroloom/reference/add_toids.md)
- Implemented complete stream order / stream calculator attribute
  [`add_streamorder()`](https://doi-usgs.github.io/hydroloom/reference/add_streamorder.md)
  and `add_streamcalculator()`
  <https://github.com/DOI-USGS/nhdplusTools/issues/188>
- Support for both numeric and character identifiers
  [\#2](https://github.com/DOI-USGS/hydroloom/issues/2)
- Support for NHDPlus and NHD 24k naming schemes
  [\#5](https://github.com/DOI-USGS/hydroloom/issues/5)
  [`hydroloom_names()`](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md)
- Support for linear indexing for lines that do not have measure
  attributes [\#6](https://github.com/DOI-USGS/hydroloom/issues/6)
  [`index_points_to_lines()`](https://doi-usgs.github.io/hydroloom/reference/index_points_to_lines.md)
- Implemented creation of an attribute topology from a geometric network
  [`make_attribute_topology()`](https://doi-usgs.github.io/hydroloom/reference/make_attribute_topology.md)
- Added vignette showing how to work with the NHD flow table.
  <https://github.com/DOI-USGS/nhdplusTools/issues/340>
  [`vignette("flow-table")`](https://doi-usgs.github.io/hydroloom/articles/flow-table.md)
- Implement method to add divergence attribute to a non-dendritic
  network.
  [`add_divergence()`](https://doi-usgs.github.io/hydroloom/reference/add_divergence.md)
- Implement basic graph and sophisticate loop detection algorithm
  [\#7](https://github.com/DOI-USGS/hydroloom/issues/7) and
  [\#9](https://github.com/DOI-USGS/hydroloom/issues/9).
  [`check_hy_graph()`](https://doi-usgs.github.io/hydroloom/reference/check_hy_graph.md)
