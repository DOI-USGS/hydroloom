hydroloom 1.0.1
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
