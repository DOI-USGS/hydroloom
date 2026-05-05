# hy_topo: self-referencing edge list

A `hy_topo` object is a self-referencing edge list representing either a
catchment topology or a flowline topology: one row per feature, carrying
an `id` and a `toid` that points at the immediately downstream feature.
This is the main representation for downstream traversal, topological
sorting, and most accumulation algorithms in hydroloom.

Catchment topology and flowline topology are functionally separate
graphs. A catchment carries a known local drainage area and is 1:1 with
its flowpath (the linear realization of the catchment). A flowline is a
linear feature without a committed local drainage area — it may coincide
with a catchment's flowpath, but that relationship is not guaranteed.
`hy_topo` represents either graph; the two are practically related but
should not be conflated.

`hy_topo` inherits from `hy`. The enriched subclass `hy_leveled` extends
`hy_topo` with topo-sort and levelpath columns, so methods written for
`hy_topo` apply to `hy_leveled` objects without modification.

## Details

`hy_topo` requires unique `id`. A divergence would need two downstream
connections from the same upstream feature — that is, two rows with the
same `id` — and `hy_topo` does not permit that. Networks with
divergences belong in
[hy_node](https://doi-usgs.github.io/hydroloom/reference/hy_node.md) (a
bipartite graph through `fromnode`/`tonode`) or in
[hy_flownetwork](https://doi-usgs.github.io/hydroloom/reference/hy_flownetwork.md)
(a junction table that annotates main and diverted paths). When the
internal classifier called from
[`hy()`](https://doi-usgs.github.io/hydroloom/reference/hy.md) sees a
duplicated `id` in an `id`/`toid` table, it produces an `hy_flownetwork`
rather than an `hy_topo`.

For the authoritative, programmatic view of which functions are callable
on a particular object, use
[`hy_capabilities()`](https://doi-usgs.github.io/hydroloom/reference/hy_capabilities.md).

## Required columns

- `id` — catchment or flowline identifier, unique across rows

- `toid` — `id` of the immediately downstream feature; network outlets
  carry the reserved outlet value (`0` for numeric ids, `""` for
  character)

See
[hydroloom_name_definitions](https://doi-usgs.github.io/hydroloom/reference/hydroloom_name_definitions.md)
for the canonical column definitions and accepted aliases.

## Functions that operate on hy_topo

- Topology and sorting:
  [`sort_network()`](https://doi-usgs.github.io/hydroloom/reference/sort_network.md),
  [`add_topo_sort()`](https://doi-usgs.github.io/hydroloom/reference/add_topo_sort.md),
  [`check_hy_graph()`](https://doi-usgs.github.io/hydroloom/reference/check_hy_graph.md)

- Path enrichment:
  [`add_levelpaths()`](https://doi-usgs.github.io/hydroloom/reference/add_levelpaths.md),
  [`add_pathlength()`](https://doi-usgs.github.io/hydroloom/reference/add_pathlength.md),
  [`add_streamorder()`](https://doi-usgs.github.io/hydroloom/reference/add_streamorder.md)

- Accumulation and traversal:
  [`accumulate_downstream()`](https://doi-usgs.github.io/hydroloom/reference/accumulate_downstream.md),
  [`navigate_hydro_network()`](https://doi-usgs.github.io/hydroloom/reference/navigate_hydro_network.md),
  [`navigate_network_dfs()`](https://doi-usgs.github.io/hydroloom/reference/navigate_network_dfs.md),
  [`make_index_ids()`](https://doi-usgs.github.io/hydroloom/reference/make_index_ids.md)

Call
[`hy_capabilities()`](https://doi-usgs.github.io/hydroloom/reference/hy_capabilities.md)
on a specific object to see which functions are callable on it given its
current columns.

## Conversions to other representations

- To
  [hy_node](https://doi-usgs.github.io/hydroloom/reference/hy_node.md)
  (bipartite graph):
  [`make_node_topology()`](https://doi-usgs.github.io/hydroloom/reference/make_node_topology.md)

- To
  [hy_leveled](https://doi-usgs.github.io/hydroloom/reference/hy_leveled.md)
  (enriched edge list):
  [`add_levelpaths()`](https://doi-usgs.github.io/hydroloom/reference/add_levelpaths.md),
  which adds `topo_sort`, `levelpath`, and `levelpath_outlet_id`

- To
  [hy_flownetwork](https://doi-usgs.github.io/hydroloom/reference/hy_flownetwork.md)
  (junction table):
  [`add_levelpaths()`](https://doi-usgs.github.io/hydroloom/reference/add_levelpaths.md)
  then
  [`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md)

## See also

[hy](https://doi-usgs.github.io/hydroloom/reference/hy.md),
[hy_leveled](https://doi-usgs.github.io/hydroloom/reference/hy_leveled.md),
[hy_node](https://doi-usgs.github.io/hydroloom/reference/hy_node.md),
[hy_flownetwork](https://doi-usgs.github.io/hydroloom/reference/hy_flownetwork.md),
[`hy_capabilities()`](https://doi-usgs.github.io/hydroloom/reference/hy_capabilities.md),
[`hy_network_type()`](https://doi-usgs.github.io/hydroloom/reference/hy_network_type.md),
[`is_dendritic()`](https://doi-usgs.github.io/hydroloom/reference/is_dendritic.md),
[`add_toids()`](https://doi-usgs.github.io/hydroloom/reference/add_toids.md),
[`sort_network()`](https://doi-usgs.github.io/hydroloom/reference/sort_network.md),
[`make_node_topology()`](https://doi-usgs.github.io/hydroloom/reference/make_node_topology.md)

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

z <- add_toids(hy(x))

hy_network_type(z)
#> [1] "hy_topo"

z
#> # hydroloom non-dendritic edge list (self-referencing): 746 features, sorted
#> Simple feature collection with 746 features and 36 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1496152 ymin: 1551203 xmax: 1527383 ymax: 1577303
#> Projected CRS: +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#> # A tibble: 746 × 37
#>         id    toid GNIS_ID GNIS_NAME  length_km aggregate_id   wbid feature_type
#>      <int>   <dbl> <chr>   <chr>          <dbl> <chr>         <int> <chr>       
#>  1 8893864 8894334 991288  Northeast…     3.24  03030002000… 0      StreamRiver 
#>  2 8894490 8894336 991288  Northeast…     0.002 03030002000… 0      Connector   
#>  3 8894494 8894490 991288  Northeast…     0.102 03030002000… 0      Connector   
#>  4 8894334 8894492 991288  Northeast…     0.073 03030002000… 8.89e6 ArtificialP…
#>  5 8894492 8894494 991288  Northeast…     0.008 03030002000… 8.89e6 ArtificialP…
#>  6 8893850 8893864 991288  Northeast…     0.954 03030002000… 0      StreamRiver 
#>  7 8893842 8893850 991288  Northeast…     0.219 03030002000… 0      StreamRiver 
#>  8 8894192 8893842 991288  Northeast…     3.09  03030002000… 0      StreamRiver 
#>  9 8894310 8894192 991288  Northeast…     0.045 03030002000… 8.89e6 ArtificialP…
#> 10 8893810 8894308 991288  Northeast…     0.583 03030002000… 0      StreamRiver 
#> # ℹ 736 more rows
#> # ℹ 29 more variables: feature_type_code <int>, stream_level <int>,
#> #   stream_order <int>, stream_calculator <int>, tonode <dbl>, topo_sort <dbl>,
#> #   levelpath <dbl>, pathlength_km <dbl>, terminal_topo_sort <dbl>,
#> #   arbolate_sum <dbl>, divergence <int>, start_flag <int>,
#> #   terminal_flag <int>, dn_stream_level <int>, up_levelpath <dbl>,
#> #   up_topo_sort <dbl>, dn_levelpath <dbl>, dn_minor_topo_sort <dbl>, …
```
