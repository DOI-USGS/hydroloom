# Drop Geometry

drops geometry if present, does nothing otherwise.

## Usage

``` r
drop_geometry(x)
```

## Arguments

- x:

  data.frame that may contain a geometry column

## Value

data.frame without geometry column

## Examples

``` r
(g <- sf::st_sf(a=3, geo = sf::st_sfc(sf::st_point(1:2))))
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#>   a         geo
#> 1 3 POINT (1 2)
drop_geometry(g)
#>   a
#> 1 3
```
