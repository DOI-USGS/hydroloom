# Get Partial Flowpath Length

Finds the upstream and downstream lengths along a given flowline.
Internally, the function rescales the aggregate_id_measure to a
id_measure and applies that rescaled measure to the length of the
flowline.

## Usage

``` r
get_partial_length(hydro_location, network = NULL, flowpath = NULL)
```

## Arguments

- hydro_location:

  list containing a hydrologic locations with names aggregate_id
  (reachcode) and aggregate_id_measure (reachcode measure).

- network:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- flowpath:

  data.frame containing one flowpath that corresponds to the
  `hydro_location`. Not required if `x` is provided. `x` is not required
  if `flowpath` is provided.

## Value

list containing `up` and `dn` elements with numeric length in km.

## Examples

``` r
x <- sf::read_sf(system.file("extdata", "walker.gpkg", package = "hydroloom"))

hydro_location <- list(comid = 5329339,
  reachcode = "18050005000078",
  reach_meas = 30)

(pl <- get_partial_length(hydro_location, x))
#> $dn
#> [1] 1.4358
#> 
#> $up
#> [1] 3.3502
#> 
```
