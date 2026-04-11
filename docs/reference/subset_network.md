# Subset Network

Subsets a network to features upstream of a given outlet. For
non-dendritic networks, the function identifies flowpaths connected to
the upstream basin via fromnode/tonode that are not reachable by
upstream navigation alone. This is useful for networks where an upstream
navigation returns a basin that contains a nested basin (closed or
intersecting) connected through diversions. If `only_up` is `FALSE`,
those nested basins are captured by navigating downstream from missed
diversions to find their outlets, then navigating upstream from those
outlets.

Note: If a diversion leaves a basin entirely, the subset will include
the entire basin upstream of where the diversion terminates. this
function will return both closed basins and intersecting basins.

## Usage

``` r
subset_network(x, outlet, only_up = FALSE)

# S3 method for class 'data.frame'
subset_network(x, outlet, only_up = FALSE)

# S3 method for class 'hy'
subset_network(x, outlet, only_up = FALSE)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- outlet:

  identifier of the outlet flowpath to subset upstream from.

- only_up:

  logical if `TRUE`, only upstream navigation is used and any missed
  diversion connections are disconnected. If `FALSE` (default), nested
  endorheic basins reachable through diversions are also included.

## Value

data.frame subset of `x` containing flowpaths upstream of the outlet.

Note: if "toid" is included in the input, it will be returned without
modification. This may result in one or more "toid" entries that contain
ids that are not part of the subset.

## Details

Required attributes: `id`, `fromnode`, `tonode`

Conditionally: `divergence` (if non-dendritic)

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

sub <- subset_network(x, 8893420)

nrow(sub)
#> [1] 24
```
