# Reverse `hy` to Original Names

renames hy object to original names and removes hy object attributes.

## Usage

``` r
hy_reverse(x)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

## Value

returns x with attribute names converted to original names provided to
[hy](https://doi-usgs.github.io/hydroloom/reference/hy.md)

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
x <- hy(x)

hy_reverse(x)
#> Simple feature collection with 746 features and 35 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1496152 ymin: 1551203 xmax: 1527383 ymax: 1577303
#> Projected CRS: +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#> # A tibble: 746 × 36
#>     COMID GNIS_ID GNIS_NAME LENGTHKM REACHCODE WBAREACOMI FTYPE FCODE StreamLeve
#>  *  <int> <chr>   <chr>        <dbl> <chr>          <int> <chr> <int>      <int>
#>  1 8.89e6 991288  Northeas…    3.24  03030002…          0 Stre… 46006          3
#>  2 8.89e6 991288  Northeas…    0.002 03030002…          0 Conn… 33400          3
#>  3 8.89e6 991288  Northeas…    0.102 03030002…          0 Conn… 33400          3
#>  4 8.89e6 991288  Northeas…    0.073 03030002…    8892958 Arti… 55800          3
#>  5 8.89e6 991288  Northeas…    0.008 03030002…    8892958 Arti… 55800          3
#>  6 8.89e6 991288  Northeas…    0.954 03030002…          0 Stre… 46006          3
#>  7 8.89e6 991288  Northeas…    0.219 03030002…          0 Stre… 46006          3
#>  8 8.89e6 991288  Northeas…    3.09  03030002…          0 Stre… 46006          3
#>  9 8.89e6 991288  Northeas…    0.045 03030002…    8892932 Arti… 55800          3
#> 10 8.89e6 991288  Northeas…    0.583 03030002…          0 Stre… 46006          3
#> # ℹ 736 more rows
#> # ℹ 27 more variables: StreamOrde <int>, StreamCalc <int>, FromNode <dbl>,
#> #   ToNode <dbl>, Hydroseq <dbl>, LevelPathI <dbl>, Pathlength <dbl>,
#> #   TerminalPa <dbl>, ArbolateSu <dbl>, Divergence <int>, StartFlag <int>,
#> #   TerminalFl <int>, DnLevel <int>, UpLevelPat <dbl>, UpHydroseq <dbl>,
#> #   DnLevelPat <dbl>, DnMinorHyd <dbl>, DnDrainCou <int>, DnHydroseq <dbl>,
#> #   FromMeas <dbl>, ToMeas <dbl>, RtnDiv <int>, VPUIn <int>, VPUOut <int>, …
```
