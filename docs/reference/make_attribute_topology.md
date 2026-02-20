# Make Attribute Topology

given a set of lines with starting and ending nodes that form a
geometric network, construct an attribute topology.

## Usage

``` r
make_attribute_topology(x, min_distance)

# S3 method for class 'data.frame'
make_attribute_topology(x, min_distance)

# S3 method for class 'hy'
make_attribute_topology(x, min_distance)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- min_distance:

  numeric distance in units compatible with the units of the projection
  of `lines`. If no nodes are found within this distance, no connection
  will be returned.

## Value

data.frame with id and toid

## Details

Required attributes: `id` and sf linestring geometry

If a `future` plan is set up, node distance calculations will be applied
using future workers.

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

y <- dplyr::select(x, COMID)

y <- sf::st_transform(y, 5070)

z <- make_attribute_topology(y, 10)

x <- add_toids(hy(x), return_dendritic = FALSE)

x[x$id == x$id[1], ]$toid
#> [1] 8894334
z[z$COMID == x$id[1], ]$toid
#> [1] 8894334
```
