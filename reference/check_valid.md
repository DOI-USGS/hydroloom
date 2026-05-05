# Check and Repair Geometry Validity

Validates sf geometry, repairs invalid features, unifies geometry types,
and optionally reprojects. Handles GEOMETRYCOLLECTION artifacts from
`st_make_valid()` by extracting the dominant geometry type.

## Usage

``` r
check_valid(x, ...)

# S3 method for class 'sf'
check_valid(x, out_prj = sf::st_crs(x), ...)

# S3 method for class 'sfc'
check_valid(x, out_prj = sf::st_crs(x), ...)
```

## Arguments

- x:

  sf data.frame, sfc geometry, or NULL.

- ...:

  additional arguments passed to methods.

- out_prj:

  crs. Target CRS for the output, passed to
  [`st_transform`](https://r-spatial.github.io/sf/reference/st_transform.html).
  Defaults to the CRS of `x`.

## Value

Same class as input: sf data.frame, sfc, or NULL.

## Examples

``` r
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
p1 <- st_polygon(list(
  rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)),
  rbind(c(2, 2), c(2, 8), c(8, 8), c(8, 2), c(2, 2))
))
x <- st_sf(geometry = st_sfc(p1, crs = 5070))
result <- check_valid(x)
```
