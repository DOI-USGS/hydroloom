# Get Hydro Location

given a flowline index, returns the hydrologic location (point) along
the specific linear element referenced by the index.

## Usage

``` r
get_hydro_location(indexes, flowpath)
```

## Arguments

- indexes:

  data.frame as output from
  [index_points_to_lines](https://doi-usgs.github.io/hydroloom/reference/index_points_to_lines.md).

- flowpath:

  data.frame with three columns: id, frommeas, and tomeas as well as
  geometry.

## Value

sfc_POINT simple feature geometry list of length `nrow(indexes)`

## Examples

``` r
if (require(nhdplusTools)) {
  source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))

  indexes <- index_points_to_lines(sample_flines,
    sf::st_sfc(sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328)),
      sf::st_point(c(-76.91711, 39.40884)),
      sf::st_point(c(-76.88081, 39.36354))),
    crs = 4326)))

  get_hydro_location(indexes, sample_flines)
}
#> Warning: converting to LINESTRING, this may be slow, check results
#> Geometry set for 3 features 
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -76.91761 ymin: 39.3633 xmax: -76.8694 ymax: 39.49326
#> Geodetic CRS:  WGS 84
#> POINT (-76.8694 39.49326)
#> POINT (-76.91761 39.40909)
#> POINT (-76.881 39.3633)
```
