# Changelog

## hydroloom 1.1.2

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
- Add function tp create a “flownetwork” representation of the network
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
