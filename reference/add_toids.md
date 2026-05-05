# Add Downstream IDs

Generates a toid attribute from node topology by joining tonode and
fromnode attributes.

## Usage

``` r
add_toids(x, return_dendritic = TRUE)

# S3 method for class 'data.frame'
add_toids(x, return_dendritic = TRUE)

# S3 method for class 'hy'
add_toids(x, return_dendritic = TRUE)

# S3 method for class 'hy_topo'
add_toids(x, return_dendritic = TRUE)

# S3 method for class 'hy_node'
add_toids(x, return_dendritic = TRUE)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- return_dendritic:

  logical remove non dendritic paths if TRUE. Requires a "divergence"
  flag where 1 is main and 2 is secondary.

## Value

hy object with toid attribute

## Details

Required attributes: `fromnode`, `tonode`

Conditionally: `divergence` (if `return_dendritic = TRUE`)

## See also

[hy_node](https://doi-usgs.github.io/hydroloom/reference/hy_node.md),
[hy_topo](https://doi-usgs.github.io/hydroloom/reference/hy_topo.md),
[`make_node_topology()`](https://doi-usgs.github.io/hydroloom/reference/make_node_topology.md)

## Examples

``` r
g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

x <- add_toids(hy(g))

y <- add_toids(g)

names(g)[1:4]
#> [1] "COMID"     "GNIS_ID"   "GNIS_NAME" "LENGTHKM" 

names(x)[1:4]
#> [1] "id"        "toid"      "GNIS_ID"   "GNIS_NAME"

names(y)[1:4]
#> [1] "COMID"     "toid"      "GNIS_ID"   "GNIS_NAME"
```
