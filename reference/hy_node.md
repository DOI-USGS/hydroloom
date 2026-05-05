# hy_node: bipartite feature-and-nexus graph

A `hy_node` object is a bipartite graph over catchments or flowlines and
nexuses: each catchment (or flowline) is one row, carrying a `fromnode`
(the upstream nexus it leaves) and a `tonode` (the downstream nexus it
enters). Connectivity is recovered by matching `tonode` to `fromnode`
across rows. This is the representation that natively supports
divergences without duplicating rows — a divergence is simply a nexus
with more than one outgoing feature.

As with `hy_topo`, a `hy_node` represents either a catchment topology or
a flowline topology. Catchments carry a known local drainage area and
are 1:1 with their flowpaths; flowlines are linear features without a
committed local drainage area. The two graphs are practically related
but functionally distinct.

`hy_node` inherits from `hy`.

## Details

Because divergences are encoded through shared node identifiers rather
than row duplication, `hy_node` requires unique `id` even on
non-dendritic networks. The `fromnode`/`tonode` pair carries the
connectivity that an `id`/`toid` edge list cannot.

`hy_node` is the entry point for hydroloom's divergence-aware
operations:
[`add_divergence()`](https://doi-usgs.github.io/hydroloom/reference/add_divergence.md)
flags main and diverted paths from geometry,
[`add_return_divergence()`](https://doi-usgs.github.io/hydroloom/reference/add_return_divergence.md)
marks where diverted paths return to the main, and
[`subset_network()`](https://doi-usgs.github.io/hydroloom/reference/subset_network.md)
walks the bipartite graph during subsetting.

## Required columns

- `id` — catchment or flowline identifier, unique across rows

- `fromnode` — upstream nexus identifier

- `tonode` — downstream nexus identifier

See
[hydroloom_name_definitions](https://doi-usgs.github.io/hydroloom/reference/hydroloom_name_definitions.md)
for the canonical column definitions.

## Functions that operate on hy_node

- Divergence handling:
  [`add_divergence()`](https://doi-usgs.github.io/hydroloom/reference/add_divergence.md),
  [`add_return_divergence()`](https://doi-usgs.github.io/hydroloom/reference/add_return_divergence.md)

- Subsetting:
  [`subset_network()`](https://doi-usgs.github.io/hydroloom/reference/subset_network.md)

- Edge-list construction:
  [`add_toids()`](https://doi-usgs.github.io/hydroloom/reference/add_toids.md)

Call
[`hy_capabilities()`](https://doi-usgs.github.io/hydroloom/reference/hy_capabilities.md)
on a specific object for the authoritative list given its current
columns.

## Conversions to other representations

- To
  [hy_topo](https://doi-usgs.github.io/hydroloom/reference/hy_topo.md)
  (self-referencing edge list):
  [`add_toids()`](https://doi-usgs.github.io/hydroloom/reference/add_toids.md).
  With `return_dendritic = TRUE` (the default) the resulting `toid`
  drops secondary paths at divergences; set the option to `FALSE` and
  route through
  [`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md)
  to keep them.

## See also

[hy](https://doi-usgs.github.io/hydroloom/reference/hy.md),
[hy_topo](https://doi-usgs.github.io/hydroloom/reference/hy_topo.md),
[hy_leveled](https://doi-usgs.github.io/hydroloom/reference/hy_leveled.md),
[hy_flownetwork](https://doi-usgs.github.io/hydroloom/reference/hy_flownetwork.md),
[`hy_capabilities()`](https://doi-usgs.github.io/hydroloom/reference/hy_capabilities.md),
[`hy_network_type()`](https://doi-usgs.github.io/hydroloom/reference/hy_network_type.md),
[`make_node_topology()`](https://doi-usgs.github.io/hydroloom/reference/make_node_topology.md),
[`add_toids()`](https://doi-usgs.github.io/hydroloom/reference/add_toids.md),
[`add_divergence()`](https://doi-usgs.github.io/hydroloom/reference/add_divergence.md),
[`subset_network()`](https://doi-usgs.github.io/hydroloom/reference/subset_network.md)

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

z <- hy(x)

hy_network_type(z)
#> [1] "hy_node"

z
#> # hydroloom non-dendritic fromnode/tonode graph: 746 features, 663 nexuses
#> Simple feature collection with 746 features and 35 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1496152 ymin: 1551203 xmax: 1527383 ymax: 1577303
#> Projected CRS: +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#> # A tibble: 746 × 36
#>         id GNIS_ID GNIS_NAME       length_km aggregate_id      wbid feature_type
#>      <int> <chr>   <chr>               <dbl> <chr>            <int> <chr>       
#>  1 8893864 991288  Northeast Creek     3.24  03030002000018       0 StreamRiver 
#>  2 8894490 991288  Northeast Creek     0.002 03030002000018       0 Connector   
#>  3 8894494 991288  Northeast Creek     0.102 03030002000018       0 Connector   
#>  4 8894334 991288  Northeast Creek     0.073 03030002000018 8892958 ArtificialP…
#>  5 8894492 991288  Northeast Creek     0.008 03030002000018 8892958 ArtificialP…
#>  6 8893850 991288  Northeast Creek     0.954 03030002000019       0 StreamRiver 
#>  7 8893842 991288  Northeast Creek     0.219 03030002000020       0 StreamRiver 
#>  8 8894192 991288  Northeast Creek     3.09  03030002000021       0 StreamRiver 
#>  9 8894310 991288  Northeast Creek     0.045 03030002000021 8892932 ArtificialP…
#> 10 8893810 991288  Northeast Creek     0.583 03030002000022       0 StreamRiver 
#> # ℹ 736 more rows
#> # ℹ 29 more variables: feature_type_code <int>, stream_level <int>,
#> #   stream_order <int>, stream_calculator <int>, fromnode <dbl>, tonode <dbl>,
#> #   topo_sort <dbl>, levelpath <dbl>, pathlength_km <dbl>,
#> #   terminal_topo_sort <dbl>, arbolate_sum <dbl>, divergence <int>,
#> #   start_flag <int>, terminal_flag <int>, dn_stream_level <int>,
#> #   up_levelpath <dbl>, up_topo_sort <dbl>, dn_levelpath <dbl>, …
```
