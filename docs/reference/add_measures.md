# Add aggregate id measures to flowlines

given a set of connected flowlines that have ids and aggregate ids, adds
from_aggregate_id_measure and to_aggregate_id_measure for use with
[index_points_to_lines](https://doi-usgs.github.io/hydroloom/reference/index_points_to_lines.md)

Aggregate ids, such as mainstem ids or reachcodes span multiple
flowlines. Linear referencing along these features requires knowledge of
the portion of the aggregate line a given flowline makes up. This
function assumes that the complete aggregate feature is included and
calculates the measure of the top and bottom of each flowline along each
aggregate line.

## Usage

``` r
add_measures(x)

# S3 method for class 'data.frame'
add_measures(x)

# S3 method for class 'hy'
add_measures(x)
```

## Arguments

- x:

  sf data.frame compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md)
  with at least id and aggregate_id attributes. A pre-populated toid
  attribute will be used if present.

## Value

x with aggregate measures added to it

## Details

If no "toid" attribute is included,
[make_attribute_topology](https://doi-usgs.github.io/hydroloom/reference/make_attribute_topology.md)
is used to to create one. This is required to ensure the flowlines
making up each aggregate line are sorted in a known upstream to
downstream order.

This function assumes that all flowlines that make up an aggregate
feature are included. Returned measures will be incomplete and incorrect
if aggregate features (mainstems of reaches) are truncated.

## Examples

``` r
g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

d <- dplyr::select(g, COMID, REACHCODE) |>
  sf::st_cast("LINESTRING")
#> Warning: repeating attributes for all sub-geometries for which they may not be constant

add_measures(d)
#> no toid found, attempting to add one from geometry.
#> Simple feature collection with 746 features and 4 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1496152 ymin: 1551203 xmax: 1527383 ymax: 1577303
#> Projected CRS: +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#> # A tibble: 746 × 5
#>      COMID REACHCODE      REACHCODE_from_measure REACHCODE_to_measure
#>  *   <int> <chr>                           <dbl>                <dbl>
#>  1 8893864 03030002000018                 5.38               100     
#>  2 8894490 03030002000018                 0                    0.0529
#>  3 8894494 03030002000018                 0.0529               3.03  
#>  4 8894334 03030002000018                 3.25                 5.38  
#>  5 8894492 03030002000018                 3.03                 3.25  
#>  6 8893850 03030002000019                 0                  100     
#>  7 8893842 03030002000020                 0                  100     
#>  8 8894192 03030002000021                 0                   99.5   
#>  9 8894310 03030002000021                99.5                100     
#> 10 8893810 03030002000022                43.6                100     
#> # ℹ 736 more rows
#> # ℹ 1 more variable: geom <LINESTRING [m]>
```
