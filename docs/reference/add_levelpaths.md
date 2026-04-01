# Add Level Paths

Assigns level paths using the stream-leveling approach of NHD and
NHDPlus. If arbolate sum is provided in the weight column, this will
match the behavior of NHDPlus. Any numeric value can be included in this
column and the largest value will be followed when no nameid is
available.

x must include id, toid, and conditionally divergence attributes. If a
"topo_sort" (hydrosequence in nhdplus terms) attribute is included, it
will be used instead of recreation.

If a future plan is set, it will be used for a preprocess step of the
function.

## Usage

``` r
add_levelpaths(
  x,
  name_attribute,
  weight_attribute,
  override_factor = NULL,
  status = FALSE
)

# S3 method for class 'data.frame'
add_levelpaths(
  x,
  name_attribute,
  weight_attribute,
  override_factor = NULL,
  status = FALSE
)

# S3 method for class 'hy'
add_levelpaths(
  x,
  name_attribute,
  weight_attribute,
  override_factor = NULL,
  status = FALSE
)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- name_attribute:

  character attribute to be used as name identifiers.

- weight_attribute:

  character attribute to be used as weight.

- override_factor:

  numeric multiplier to use to override `name_attribute`. See details.

- status:

  boolean if status updates should be printed.

## Value

data.frame with id, levelpath_outlet_id, topo_sort, and levelpath
columns. See details for more info.

## Details

The levelpath algorithm defines upstream mainstem paths through a
network. At a given junction with two or more upstream flowlines, the
main path is either 1) the path with the same name, 2) the path with any
name, 3) or the path with the larger weight. If the `weight_attribute`
is `override_factor` times larger on a path, it will be followed
regardless of the name_attribute indication.

If id and toid are non-dendritic so id:toid is many to one and id is
non-unique, a divergence attribute must be included such that the
dendritic network can be extracted after the network is sorted.

## Examples

``` r
g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

test_flowline <- add_toids(g)

# use NHDPlus attributes directly
add_levelpaths(test_flowline,
  name_attribute = "GNIS_ID",
  weight_attribute = "ArbolateSu")
#> Simple feature collection with 746 features and 37 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1496152 ymin: 1551203 xmax: 1527383 ymax: 1577303
#> Projected CRS: +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#> # A tibble: 746 × 38
#>      COMID    toid levelpath_outlet_id  Hydroseq LevelPathI
#>  *   <int>   <dbl>               <int>     <dbl>      <dbl>
#>  1 8897784       0             8897784 250010365  250010365
#>  2 8894360 8897784             8897784 250010419  250010365
#>  3 8894356 8894360             8897784 250010476  250010365
#>  4 8894354 8894356             8897784 250010545  250010365
#>  5 8894352 8894354             8897784 250010614  250010365
#>  6 8894344 8894352             8897784 250010678  250010365
#>  7 8894332 8894344             8897784 250010743  250010365
#>  8 8894324 8894332             8897784 250010806  250010365
#>  9 8894322 8894324             8894322 250010877  250010877
#> 10 8893808 8894322             8894322 250010956  250010877
#> # ℹ 736 more rows
#> # ℹ 33 more variables: geom <MULTILINESTRING [m]>, GNIS_ID <chr>,
#> #   GNIS_NAME <chr>, LENGTHKM <dbl>, REACHCODE <chr>, WBAREACOMI <int>,
#> #   FTYPE <chr>, FCODE <int>, StreamLeve <int>, StreamOrde <int>,
#> #   StreamCalc <int>, ToNode <dbl>, Pathlength <dbl>, TerminalPa <dbl>,
#> #   ArbolateSu <dbl>, Divergence <int>, StartFlag <int>, TerminalFl <int>,
#> #   DnLevel <int>, UpLevelPat <dbl>, UpHydroseq <dbl>, DnLevelPat <dbl>, …

# use hy attributes where they can be mapped
add_levelpaths(hy(test_flowline),
  name_attribute = "GNIS_ID",
  weight_attribute = "arbolate_sum")
#> Simple feature collection with 746 features and 37 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1496152 ymin: 1551203 xmax: 1527383 ymax: 1577303
#> Projected CRS: +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#> # A tibble: 746 × 38
#>         id    toid levelpath_outlet_id topo_sort levelpath
#>      <int>   <dbl>               <int>     <dbl>     <dbl>
#>  1 8897784       0             8897784 250010365 250010365
#>  2 8894360 8897784             8897784 250010419 250010365
#>  3 8894356 8894360             8897784 250010476 250010365
#>  4 8894354 8894356             8897784 250010545 250010365
#>  5 8894352 8894354             8897784 250010614 250010365
#>  6 8894344 8894352             8897784 250010678 250010365
#>  7 8894332 8894344             8897784 250010743 250010365
#>  8 8894324 8894332             8897784 250010806 250010365
#>  9 8894322 8894324             8894322 250010877 250010877
#> 10 8893808 8894322             8894322 250010956 250010877
#> # ℹ 736 more rows
#> # ℹ 33 more variables: geom <MULTILINESTRING [m]>, GNIS_ID <chr>,
#> #   GNIS_NAME <chr>, length_km <dbl>, aggregate_id <chr>, wbid <int>,
#> #   feature_type <chr>, feature_type_code <int>, stream_level <int>,
#> #   stream_order <int>, stream_calculator <int>, tonode <dbl>,
#> #   pathlength_km <dbl>, terminal_topo_sort <dbl>, arbolate_sum <dbl>,
#> #   divergence <int>, start_flag <int>, terminal_flag <int>, …
```
