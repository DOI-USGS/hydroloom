# Make Spatial Inputs Compatible

makes sf1 compatible with sf2 by projecting into the projection of 2 and
ensuring that the geometry columns are the same name.

## Usage

``` r
st_compatibalize(sf1, sf2)
```

## Arguments

- sf1:

  sf data.frame

- sf2:

  sf data.frame

## Value

sf1 transformed and renamed to be compatible with sf2

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

one <- dplyr::select(x)
two <- sf::st_transform(one, 5070)

attr(one, "sf_column") <- "geotest"
names(one)[names(one) == "geom"] <- "geotest"

st_compatibalize(one, two)
#> Simple feature collection with 746 features and 0 fields
#> Geometry type: MULTILINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 1496152 ymin: 1551203 xmax: 1527383 ymax: 1577303
#> Projected CRS: NAD83 / Conus Albers
#> # A tibble: 746 × 1
#>                                                                             geom
#>  *                                                         <MULTILINESTRING [m]>
#>  1 ((1518702 1557298, 1518643 1557297, 1518632 1557288, 1518631 1557209, 151861…
#>  2                                          ((1517194 1556000, 1517192 1555999))
#>  3 ((1517288 1556038, 1517252 1556023, 1517215 1556010, 1517200 1556004, 151719…
#>  4                         ((1517349 1556090, 1517341 1556077, 1517295 1556041))
#>  5                                          ((1517295 1556041, 1517288 1556038))
#>  6 ((1518668 1557990, 1518699 1557904, 1518722 1557890, 1518753 1557831, 151880…
#>  7 ((1518694 1558172, 1518702 1558142, 1518729 1558118, 1518738 1558086, 151866…
#>  8 ((1519790 1560148, 1519772 1560132, 1519738 1560110, 1519545 1560059, 151949…
#>  9                         ((1519833 1560160, 1519803 1560151, 1519790 1560148))
#> 10 ((1520229 1560983, 1520199 1560918, 1520183 1560868, 1520134 1560844, 152012…
#> # ℹ 736 more rows
```
