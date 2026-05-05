# Get a domain by id

Returns the
[`hy_domain()`](https://doi-usgs.github.io/hydroloom/reference/hy_domain.md)
object stored under `domain_id` in a decomposition. Errors when
`domain_id` is not a key of `decomposition$domains`.

## Usage

``` r
get_domain(decomposition, domain_id)
```

## Arguments

- decomposition:

  object of class `domain_decomposition`.

- domain_id:

  character(1). Domain id to look up.

## Value

object of class `hy_domain` — the named list with all six slots
described in
[`hy_domain()`](https://doi-usgs.github.io/hydroloom/reference/hy_domain.md).

## See also

[`hy_domain()`](https://doi-usgs.github.io/hydroloom/reference/hy_domain.md)
for the per-domain object,
[`get_domain_connectivity()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_connectivity.md),
[`get_domain_for_catchment()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_for_catchment.md),
[`get_domain_graph()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_graph.md).

## Examples

``` r
g <- sf::read_sf(system.file("extdata/walker.gpkg", package = "hydroloom"))

h <- hy(g) |>
  add_toids() |>
  add_levelpaths(name_attribute = "GNIS_ID",
    weight_attribute = "arbolate_sum")

d <- decompose_network(h)

get_domain(d, names(d$domains)[[1]])
#> $domain_id
#> [1] "domain_5329303_5329303"
#> 
#> $outlet_nexus_id
#> [1] "nx_5329303_outlet"
#> 
#> $inlet_nexus_ids
#> character(0)
#> 
#> $containing_domain_id
#> [1] NA
#> 
#> $catchments
#> # hydroloom dendritic leveled edge list (self-referencing): 62 features, sorted
#> Simple feature collection with 62 features and 42 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -122.9355 ymin: 38.09752 xmax: -122.6844 ymax: 38.25057
#> Geodetic CRS:  GRS 1980(IUGG, 1980)
#> # A tibble: 62 × 43
#>         id    toid levelpath_outlet_id topo_sort levelpath FDATE              
#>  *   <int>   <dbl>               <int>     <dbl>     <dbl> <dttm>             
#>  1 5329871 5329383             5329391  10133844  10052429 2008-03-19 00:00:00
#>  2 5329383 5329849             5329391  10087243  10052429 1999-07-03 00:00:00
#>  3 5329849 5329393             5329391  10069226  10052429 2008-03-19 00:00:00
#>  4 5329393 5329391             5329391  10059179  10052429 2000-09-08 00:00:00
#>  5 5329407 5329391             5329407  10133901  10133901 1999-07-03 00:00:00
#>  6 5329391 5329389             5329391  10052429  10052429 1999-07-03 00:00:00
#>  7 5329435       0             5329303  10133889  10022949 2009-08-24 00:00:00
#>  8 5329389       0             5329303  10047501  10022949 2009-08-24 00:00:00
#>  9 5329387 5329397             5329387  10133904  10133904 1999-07-03 00:00:00
#> 10 5329397       0             5329303  10043663  10022949 2009-08-24 00:00:00
#> # ℹ 52 more rows
#> # ℹ 37 more variables: RESOLUTION <chr>, GNIS_ID <chr>, GNIS_NAME <chr>,
#> #   length_km <dbl>, aggregate_id <chr>, FLOWDIR <chr>, wbid <int>,
#> #   feature_type <chr>, feature_type_code <int>, Shape_Length <dbl>,
#> #   stream_level <int>, stream_order <int>, stream_calculator <int>,
#> #   tonode <dbl>, pathlength_km <dbl>, terminal_topo_sort <dbl>,
#> #   arbolate_sum <dbl>, divergence <int>, start_flag <int>, …
#> 
#> attr(,"class")
#> [1] "hy_domain"
```
