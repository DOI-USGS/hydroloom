hydroloom 1.1.0
==========

- Add new vignette for network navigation `vignette("network_navigation")`
- Add support for upmain and downmain navigation in `navigate_network_dfs()`
- Add support for upmain and downmain in `make_index_ids()` and `make_fromids()`.
- Add function tp create a "flownetwork" representation of the network with a `to_flownetwork` function.
- Improved handling / fix bugs with edge cases in `add_toids()`
- Improved error conditions when missing suggested packages.
- `navigate_hydro_network()` will now navigate from a diverted path to a main path where it previously only followed traditional tributaries..

hydroloom 1.0.1 and 1.0.2
==========

- Add checks if nhdplusTools is available.

hydroloom 1.0.0
==========

- Initialized new project
- Create basic `hy` s3 object handling `hy()`
- Ensure tibble is used throughout
- Support generic depth first search navigation `navigate_network_dfs()`
- Complete migration of nhdplusTools functions to hydroloom. #1
- Support sorting non-dendritic network in `sort_network()`
- Implemented non-dendritic network support in `add_toids()`
- Implemented complete stream order / stream calculator attribute `add_streamorder()` and `add_streamcalculator()` https://github.com/DOI-USGS/nhdplusTools/issues/188
- Support for both numeric and character identifiers #2
- Support for NHDPlus and NHD 24k naming schemes #5 `hydroloom_names()`
- Support for linear indexing for lines that do not have measure attributes #6 `index_points_to_lines()`
- Implemented creation of an attribute topology from a geometric network `make_attribute_topology()`
- Added vignette showing how to work with the NHD flow table. https://github.com/DOI-USGS/nhdplusTools/issues/340 `vignette("flow-table")`
- Implement method to add divergence attribute to a non-dendritic network. `add_divergence()`
- Implement basic graph and sophisticate loop detection algorithm #7 and #9. `check_hy_graph()`
