# Get a basin's extensive network overlay

Returns the `hy_leveled` overlay stored under `basin_id` in
`decomposition$domain_connectivity`, or — when `basin_id` is `NULL` (the
default) — the full named list of overlays. The overlay is the basin's
*extensive network*: a `hy_leveled` view of the connecting flowlines
with `toid`s intact except at the basin outlet, which carries the
reserved outlet `toid` value.

## Usage

``` r
get_domain_connectivity(decomposition, basin_id = NULL)
```

## Arguments

- decomposition:

  object of class `domain_decomposition`.

- basin_id:

  character(1) or `NULL`. Basin id to look up. `NULL` (default) returns
  the full named list.

## Value

single `hy_leveled` overlay when `basin_id` is supplied; named list of
overlays when `basin_id` is `NULL`.

## See also

[`hy_domain()`](https://doi-usgs.github.io/hydroloom/reference/hy_domain.md)
for the dual-ownership rule that motivates the overlay,
[domain_decomposition](https://doi-usgs.github.io/hydroloom/reference/domain_decomposition.md),
[`get_domain()`](https://doi-usgs.github.io/hydroloom/reference/get_domain.md),
[`get_domain_graph()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_graph.md).

## Examples

``` r
g <- sf::read_sf(system.file("extdata/walker.gpkg", package = "hydroloom"))

h <- hy(g) |>
  add_toids() |>
  add_levelpaths(name_attribute = "GNIS_ID",
    weight_attribute = "arbolate_sum")

d <- decompose_network(h)

get_domain_connectivity(d, names(d$domain_connectivity)[[1]])
#> # hydroloom dendritic leveled edge list (self-referencing): 18 features, sorted
#> Simple feature collection with 18 features and 42 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -122.9228 ymin: 38.09752 xmax: -122.6844 ymax: 38.23326
#> Geodetic CRS:  GRS 1980(IUGG, 1980)
#> # A tibble: 18 × 43
#>         id    toid levelpath_outlet_id topo_sort levelpath FDATE              
#>  *   <int>   <dbl>               <int>     <dbl>     <dbl> <dttm>             
#>  1 5329435 5329389             5329303  10133889  10022949 2009-08-24 00:00:00
#>  2 5329389 5329397             5329303  10047501  10022949 2009-08-24 00:00:00
#>  3 5329397 5329395             5329303  10043663  10022949 2009-08-24 00:00:00
#>  4 5329395 5329821             5329303  10040577  10022949 2009-08-24 00:00:00
#>  5 5329821 5329385             5329303  10038010  10022949 2009-08-24 00:00:00
#>  6 5329385 5329847             5329303  10035832  10022949 2009-08-24 00:00:00
#>  7 5329847 5329843             5329303  10033949  10022949 2008-03-19 00:00:00
#>  8 5329843 5329373             5329303  10032318  10022949 2008-03-19 00:00:00
#>  9 5329373 5329365             5329303  10030898  10022949 2009-08-24 00:00:00
#> 10 5329365 5329357             5329303  10029628  10022949 1999-07-03 00:00:00
#> 11 5329357 5329343             5329303  10028504  10022949 1999-07-03 00:00:00
#> 12 5329343 5329339             5329303  10027485  10022949 1999-07-03 00:00:00
#> 13 5329339 5329315             5329303  10026563  10022949 1999-07-03 00:00:00
#> 14 5329315 5329317             5329303  10025719  10022949 1999-07-03 00:00:00
#> 15 5329317 5329305             5329303  10024932  10022949 1999-07-03 00:00:00
#> 16 5329305 5329293             5329303  10024217  10022949 1999-07-03 00:00:00
#> 17 5329293 5329303             5329303  10023557  10022949 1999-07-03 00:00:00
#> 18 5329303       0             5329303  10022949  10022949 1999-07-03 00:00:00
#> # ℹ 37 more variables: RESOLUTION <chr>, GNIS_ID <chr>, GNIS_NAME <chr>,
#> #   length_km <dbl>, aggregate_id <chr>, FLOWDIR <chr>, wbid <int>,
#> #   feature_type <chr>, feature_type_code <int>, Shape_Length <dbl>,
#> #   stream_level <int>, stream_order <int>, stream_calculator <int>,
#> #   tonode <dbl>, pathlength_km <dbl>, terminal_topo_sort <dbl>,
#> #   arbolate_sum <dbl>, divergence <int>, start_flag <int>,
#> #   terminal_flag <int>, dn_stream_level <int>, up_levelpath <dbl>, …
```
