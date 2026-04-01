# Create a hy Fabric S3 Object

converts a compatible dataset into a fabric s3 class

## Usage

``` r
hy(x, clean = FALSE)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- clean:

  logical if TRUE, geometry and non-hydroloom compatible attributes will
  be removed.

## Value

hy object with attributes compatible with the hydroloom package.

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

hy(x)
#> Simple feature collection with 746 features and 35 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1496152 ymin: 1551203 xmax: 1527383 ymax: 1577303
#> Projected CRS: +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
#> # A tibble: 746 × 36
#>         id GNIS_ID GNIS_NAME       length_km aggregate_id      wbid feature_type
#>      <int> <chr>   <chr>               <dbl> <chr>            <int> <chr>       
#>  1 8893864 991288  Northeast Creek     3.24  03030002000018       0 StreamRiver 
#>  2 8894490 991288  Northeast Creek     0.002 03030002000018       0 Connector   
#>  3 8894494 991288  Northeast Creek     0.102 03030002000018       0 Connector   
#>  4 8894334 991288  Northeast Creek     0.073 03030002000018 8892958 ArtificialP…
#>  5 8894492 991288  Northeast Creek     0.008 03030002000018 8892958 ArtificialP…
#>  6 8893850 991288  Northeast Creek     0.954 03030002000019       0 StreamRiver 
#>  7 8893842 991288  Northeast Creek     0.219 03030002000020       0 StreamRiver 
#>  8 8894192 991288  Northeast Creek     3.09  03030002000021       0 StreamRiver 
#>  9 8894310 991288  Northeast Creek     0.045 03030002000021 8892932 ArtificialP…
#> 10 8893810 991288  Northeast Creek     0.583 03030002000022       0 StreamRiver 
#> # ℹ 736 more rows
#> # ℹ 29 more variables: feature_type_code <int>, stream_level <int>,
#> #   stream_order <int>, stream_calculator <int>, fromnode <dbl>, tonode <dbl>,
#> #   topo_sort <dbl>, levelpath <dbl>, pathlength_km <dbl>,
#> #   terminal_topo_sort <dbl>, arbolate_sum <dbl>, divergence <int>,
#> #   start_flag <int>, terminal_flag <int>, dn_stream_level <int>,
#> #   up_levelpath <dbl>, up_topo_sort <dbl>, dn_levelpath <dbl>, …

hy(x, clean = TRUE)[1:10, ]
#> # A tibble: 10 × 29
#>         id length_km aggregate_id      wbid feature_type   feature_type_code
#>      <int>     <dbl> <chr>            <int> <chr>                      <int>
#>  1 8893864     3.24  03030002000018       0 StreamRiver                46006
#>  2 8894490     0.002 03030002000018       0 Connector                  33400
#>  3 8894494     0.102 03030002000018       0 Connector                  33400
#>  4 8894334     0.073 03030002000018 8892958 ArtificialPath             55800
#>  5 8894492     0.008 03030002000018 8892958 ArtificialPath             55800
#>  6 8893850     0.954 03030002000019       0 StreamRiver                46006
#>  7 8893842     0.219 03030002000020       0 StreamRiver                46006
#>  8 8894192     3.09  03030002000021       0 StreamRiver                46006
#>  9 8894310     0.045 03030002000021 8892932 ArtificialPath             55800
#> 10 8893810     0.583 03030002000022       0 StreamRiver                46006
#> # ℹ 23 more variables: stream_level <int>, stream_order <int>,
#> #   stream_calculator <int>, fromnode <dbl>, tonode <dbl>, topo_sort <dbl>,
#> #   levelpath <dbl>, pathlength_km <dbl>, terminal_topo_sort <dbl>,
#> #   arbolate_sum <dbl>, divergence <int>, start_flag <int>,
#> #   terminal_flag <int>, dn_stream_level <int>, up_levelpath <dbl>,
#> #   up_topo_sort <dbl>, dn_levelpath <dbl>, dn_minor_topo_sort <dbl>,
#> #   dn_topo_sort <dbl>, aggregate_id_from_measure <dbl>, …

attr(hy(x), "orig_names")
#>                       COMID                     GNIS_ID 
#>                        "id"                   "GNIS_ID" 
#>                   GNIS_NAME                    LENGTHKM 
#>                 "GNIS_NAME"                 "length_km" 
#>                   REACHCODE                  WBAREACOMI 
#>              "aggregate_id"                      "wbid" 
#>                       FTYPE                       FCODE 
#>              "feature_type"         "feature_type_code" 
#>                  StreamLeve                  StreamOrde 
#>              "stream_level"              "stream_order" 
#>                  StreamCalc                    FromNode 
#>         "stream_calculator"                  "fromnode" 
#>                      ToNode                    Hydroseq 
#>                    "tonode"                 "topo_sort" 
#>                  LevelPathI                  Pathlength 
#>                 "levelpath"             "pathlength_km" 
#>                  TerminalPa                  ArbolateSu 
#>        "terminal_topo_sort"              "arbolate_sum" 
#>                  Divergence                   StartFlag 
#>                "divergence"                "start_flag" 
#>                  TerminalFl                     DnLevel 
#>             "terminal_flag"           "dn_stream_level" 
#>                  UpLevelPat                  UpHydroseq 
#>              "up_levelpath"              "up_topo_sort" 
#>                  DnLevelPat                  DnMinorHyd 
#>              "dn_levelpath"        "dn_minor_topo_sort" 
#>                  DnDrainCou                  DnHydroseq 
#>                "DnDrainCou"              "dn_topo_sort" 
#>                    FromMeas                      ToMeas 
#> "aggregate_id_from_measure"   "aggregate_id_to_measure" 
#>                      RtnDiv                       VPUIn 
#>                    "RtnDiv"                     "VPUIn" 
#>                      VPUOut                    AreaSqKM 
#>                    "VPUOut"                   "da_sqkm" 
#>                   TotDASqKM                        geom 
#>             "total_da_sqkm"                      "geom" 
```
