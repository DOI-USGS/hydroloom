# Package index

## hy S3 Object

- [`hy()`](https://doi-usgs.github.io/hydroloom/reference/hy.md) :
  Create a hy Fabric S3 Object

- [`is.hy()`](https://doi-usgs.github.io/hydroloom/reference/is.hy.md) :

  Is Valid `hy` Class?

- [`hy_topo`](https://doi-usgs.github.io/hydroloom/reference/hy_topo.md)
  : hy_topo: self-referencing edge list

- [`hy_leveled`](https://doi-usgs.github.io/hydroloom/reference/hy_leveled.md)
  : hy_leveled: enriched self-referencing edge list

- [`hy_node`](https://doi-usgs.github.io/hydroloom/reference/hy_node.md)
  : hy_node: bipartite feature-and-nexus graph

- [`hy_flownetwork`](https://doi-usgs.github.io/hydroloom/reference/hy_flownetwork.md)
  : hy_flownetwork: non-dendritic junction table

- [`hy_network_type()`](https://doi-usgs.github.io/hydroloom/reference/hy_network_type.md)
  : What representation pattern does this network use?

- [`hy_capabilities()`](https://doi-usgs.github.io/hydroloom/reference/hy_capabilities.md)
  : What operations are available for this network?

- [`is_dendritic()`](https://doi-usgs.github.io/hydroloom/reference/is_dendritic.md)
  : Is the network dendritic?

- [`hy_reverse()`](https://doi-usgs.github.io/hydroloom/reference/hy_reverse.md)
  :

  Reverse `hy` to Original Names

- [`hydroloom_names()`](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md)
  : Get or Set Hydroloom Names

- [`hydroloom_name_definitions`](https://doi-usgs.github.io/hydroloom/reference/hydroloom_name_definitions.md)
  : Hydroloom Name Definitions

- [`align_names()`](https://doi-usgs.github.io/hydroloom/reference/align_names.md)
  : Align Names to Hydroloom Convention

## Utility Functions

- [`check_hy_graph()`](https://doi-usgs.github.io/hydroloom/reference/check_hy_graph.md)
  : Check hy Graph
- [`check_valid()`](https://doi-usgs.github.io/hydroloom/reference/check_valid.md)
  : Check and Repair Geometry Validity
- [`dissolve_polygons()`](https://doi-usgs.github.io/hydroloom/reference/dissolve_polygons.md)
  : Dissolve Polygons
- [`fix_flowdir()`](https://doi-usgs.github.io/hydroloom/reference/fix_flowdir.md)
  : Fix Flow Direction
- [`get_node()`](https://doi-usgs.github.io/hydroloom/reference/get_node.md)
  : Get Line Node
- [`st_compatibalize()`](https://doi-usgs.github.io/hydroloom/reference/st_compatibalize.md)
  : Make Spatial Inputs Compatible
- [`rename_geometry()`](https://doi-usgs.github.io/hydroloom/reference/rename_geometry.md)
  : Rename Geometry

## Network Graph and Topology Functions

- [`sort_network()`](https://doi-usgs.github.io/hydroloom/reference/sort_network.md)
  : Sort Network
- [`subset_network()`](https://doi-usgs.github.io/hydroloom/reference/subset_network.md)
  : Subset Network
- [`add_topo_sort()`](https://doi-usgs.github.io/hydroloom/reference/add_topo_sort.md)
  : Add topo_sort
- [`add_toids()`](https://doi-usgs.github.io/hydroloom/reference/add_toids.md)
  : Add Downstream IDs
- [`make_attribute_topology()`](https://doi-usgs.github.io/hydroloom/reference/make_attribute_topology.md)
  : Make Attribute Topology
- [`make_node_topology()`](https://doi-usgs.github.io/hydroloom/reference/make_node_topology.md)
  : Make Node Topology from Edge Topology
- [`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md)
  : To Flownetwork
- [`make_index_ids()`](https://doi-usgs.github.io/hydroloom/reference/make_index_ids.md)
  : Make Index ids
- [`get_bridge_flowlines()`](https://doi-usgs.github.io/hydroloom/reference/get_bridge_flowlines.md)
  : Get Bridge Flowlines

## Network Attributes

- [`add_divergence()`](https://doi-usgs.github.io/hydroloom/reference/add_divergence.md)
  : Add Divergence Attribute
- [`add_return_divergence()`](https://doi-usgs.github.io/hydroloom/reference/add_return_divergence.md)
  : Add Return Divergence
- [`add_levelpaths()`](https://doi-usgs.github.io/hydroloom/reference/add_levelpaths.md)
  : Add Level Paths
- [`add_pathlength()`](https://doi-usgs.github.io/hydroloom/reference/add_pathlength.md)
  : Add Path Length
- [`add_pfafstetter()`](https://doi-usgs.github.io/hydroloom/reference/add_pfafstetter.md)
  : Add Pfafstetter Codes
- [`add_streamlevel()`](https://doi-usgs.github.io/hydroloom/reference/add_streamlevel.md)
  : Add Streamlevel
- [`add_streamorder()`](https://doi-usgs.github.io/hydroloom/reference/add_streamorder.md)
  : Add Streamorder

## Network Navigation and Accumulation

- [`navigate_network_dfs()`](https://doi-usgs.github.io/hydroloom/reference/navigate_network_dfs.md)
  : Navigate all Paths Depth First
- [`navigate_connected_paths()`](https://doi-usgs.github.io/hydroloom/reference/navigate_connected_paths.md)
  : Navigate Connected Paths
- [`navigate_hydro_network()`](https://doi-usgs.github.io/hydroloom/reference/navigate_hydro_network.md)
  : Navigate Hydro Network
- [`accumulate_downstream()`](https://doi-usgs.github.io/hydroloom/reference/accumulate_downstream.md)
  : Accumulate Variable Downstream

## Indexing and Linear Referencing

- [`index_points_to_lines()`](https://doi-usgs.github.io/hydroloom/reference/index_points_to_lines.md)
  : Index Points to Lines
- [`index_points_to_waterbodies()`](https://doi-usgs.github.io/hydroloom/reference/index_points_to_waterbodies.md)
  : Index Points to Waterbodies
- [`disambiguate_indexes()`](https://doi-usgs.github.io/hydroloom/reference/disambiguate_indexes.md)
  : Disambiguate Flowline Indexes
- [`get_hydro_location()`](https://doi-usgs.github.io/hydroloom/reference/get_hydro_location.md)
  : Get Hydro Location
- [`get_partial_length()`](https://doi-usgs.github.io/hydroloom/reference/get_partial_length.md)
  : Get Partial Flowpath Length
- [`rescale_measures()`](https://doi-usgs.github.io/hydroloom/reference/rescale_measures.md)
  : Rescale Aggregate id Measure to id Measure
- [`add_measures()`](https://doi-usgs.github.io/hydroloom/reference/add_measures.md)
  : Add aggregate id measures to flowlines

## Domain Decomposition

Experimental. Decompose a flowline network into per-segment domains for
distributed accumulation and recompose results back to the source
network. API may change.

- [`decompose_network()`](https://doi-usgs.github.io/hydroloom/reference/decompose_network.md)
  : Decompose a network into domains
- [`recompose()`](https://doi-usgs.github.io/hydroloom/reference/recompose.md)
  : Recompose a domain decomposition by accumulating an attribute
  downstream
- [`hy_domain()`](https://doi-usgs.github.io/hydroloom/reference/hy_domain.md)
  : Construct a hy_domain
- [`domain_decomposition`](https://doi-usgs.github.io/hydroloom/reference/domain_decomposition.md)
  : Domain decomposition object
- [`print(`*`<domain_decomposition>`*`)`](https://doi-usgs.github.io/hydroloom/reference/print.domain_decomposition.md)
  : Print a domain_decomposition
- [`validate_decomposition()`](https://doi-usgs.github.io/hydroloom/reference/validate_decomposition.md)
  : Validate a domain decomposition
- [`get_domain()`](https://doi-usgs.github.io/hydroloom/reference/get_domain.md)
  : Get a domain by id
- [`get_domain_for_catchment()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_for_catchment.md)
  : Look up the domain containing a catchment
- [`get_domain_graph()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_graph.md)
  : Get the inter-domain edge list from a decomposition
- [`get_domain_connectivity()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_connectivity.md)
  : Get a basin's extensive network overlay
- [`get_nexus_registry()`](https://doi-usgs.github.io/hydroloom/reference/get_nexus_registry.md)
  : Get the nexus registry from a decomposition
- [`get_overrides()`](https://doi-usgs.github.io/hydroloom/reference/get_overrides.md)
  : Get the overrides table from a decomposition
- [`set_containment()`](https://doi-usgs.github.io/hydroloom/reference/set_containment.md)
  : Declare containment between domains
- [`get_containing_domain()`](https://doi-usgs.github.io/hydroloom/reference/get_containing_domain.md)
  : Look up a domain's containing domain
- [`is_leaf_domain()`](https://doi-usgs.github.io/hydroloom/reference/is_leaf_domain.md)
  : Test whether a domain is a leaf
- [`is_root_domain()`](https://doi-usgs.github.io/hydroloom/reference/is_root_domain.md)
  : Test whether a domain is the root of its basin
- [`is_stem_domain()`](https://doi-usgs.github.io/hydroloom/reference/is_stem_domain.md)
  : Test whether a domain is a stem

## Deprecated

- [`make_fromids()`](https://doi-usgs.github.io/hydroloom/reference/make_fromids.md)
  : DEPRECATED Convert "to" index ids to "from" index ids
- [`format_index_ids()`](https://doi-usgs.github.io/hydroloom/reference/format_index_ids.md)
  : DEPRECATED: Format Index ids
