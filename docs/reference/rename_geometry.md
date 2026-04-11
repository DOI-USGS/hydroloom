# Rename Geometry

correctly renames the geometry column of a sf object.

## Usage

``` r
rename_geometry(g, name)
```

## Arguments

- g:

  sf data.table

- name:

  character name to be used for geometry

## Value

sf data.frame with geometry column renamed according to name parameter

## Examples

``` r
(g <- sf::st_sf(a = 3, geo = sf::st_sfc(sf::st_point(1:2))))
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#>   a         geo
#> 1 3 POINT (1 2)
rename_geometry(g, "geometry")
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#>   a    geometry
#> 1 3 POINT (1 2)
```
