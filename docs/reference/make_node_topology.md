# Make Node Topology from Edge Topology

creates a node topology table from an edge topology

## Usage

``` r
make_node_topology(x, add_div = NULL, add = TRUE)

# S3 method for class 'data.frame'
make_node_topology(x, add_div = NULL, add = TRUE)

# S3 method for class 'hy'
make_node_topology(x, add_div = NULL, add = TRUE)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- add_div:

  data.frame of logical containing id and toid diverted paths to add.
  Should have id and toid fields. If TRUE, the network will be
  interpreted as a directed acyclic graph with downstream diversions
  included in the edge topology.

- add:

  logical if TRUE, node topology will be added to x in return.

## Value

data.frame containing id, fromnode, and tonode attributes or all
attributes provided with id, fromnode and tonode in the first three
columns.

If `add_div` is TRUE, will also add a `divergence` attribute where the
provided diverted paths are assigned value 2, existing main paths that
emanate from a divergence are assigned value 1, and all other paths are
assigned value 0.

## Details

Required attributes: `id`, `toid`

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

y <- dplyr::select(add_toids(x), -ToNode, -FromNode)

y <- make_node_topology(y)
#> Warning: nhdplusTools make_node_topology is deprecated. Use hydroloom version.

# just the divergences which have unique fromids in x but don't in new hope.
div <- add_toids(dplyr::select(x, COMID, FromNode, ToNode),
  return_dendritic = FALSE)
div <- div[div$toid %in%
  x$COMID[x$Divergence == 2], ]

y <- dplyr::select(add_toids(x), -ToNode, -FromNode)

y <- make_node_topology(y, add_div = div)
#> Warning: nhdplusTools make_node_topology is deprecated. Use hydroloom version.
```
