# Index Points to Lines

given an sf point geometry column, return id, aggregate_id (e.g.
reachcode), and aggregate id measure for each point.

## Usage

``` r
index_points_to_lines(
  x,
  points,
  search_radius = NULL,
  precision = NA,
  max_matches = 1,
  ids = NULL
)

# S3 method for class 'data.frame'
index_points_to_lines(
  x,
  points,
  search_radius = NULL,
  precision = NA,
  max_matches = 1,
  ids = NULL
)

# S3 method for class 'hy'
index_points_to_lines(
  x,
  points,
  search_radius = NULL,
  precision = NA,
  max_matches = 1,
  ids = NULL
)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- points:

  sf or sfc of type POINT in analysis projection. NOTE: x will be
  projected to the projection of the points layer.

- search_radius:

  units distance for the nearest neighbor search to extend in analysis
  projection. If missing or NULL, and points are in a lon lat
  projection, a default of 0.01 degree is used, otherwise 200 m is used.
  Conversion to the linear unit used by the provided crs of points is
  attempted. See RANN nn2 documentation for more details.

- precision:

  numeric the resolution of measure precision in the output in meters.

- max_matches:

  numeric the maximum number of matches to return if multiple are found
  in search_radius

- ids:

  vector of ids corresponding to flowline ids from `x` of the same
  length as and order as `points`. If included, index searching will be
  constrained to one and only one flowline per point.

  `search radius` is still used with this option but `max_matches` is
  overridden.

## Value

data.frame with up to five columns, point_id, id, aggregate_id,
aggregate_id_measure, and offset. point_id is the row or list element in
the point input. If an aggregate_id (e.g. mainstem or reachcode) is not
included in x. it will not be included in the output. If from and to
measures are not included for each id in x, measures will not be
included in the output.

## Details

Required attributes: `id` and sf linestring geometry

Note 1: Inputs are cast into LINESTRINGS. Because of this, the measure
output of inputs that are true multipart lines may be in error.

Note 2: This algorithm finds the nearest node in the input flowlines to
identify which flowline the point should belong to. As a second pass, it
can calculate the measure to greater precision than the nearest flowline
geometry node.

Note 3: Offset is returned in units consistent with the projection of
the input points.

Note 4: See `dfMaxLength` input to sf::st_segmentize() for details of
handling of precision parameter.

Note 5: "from" is downstream – 0 is the outlet "to" is upstream – 100 is
the inlet

Note 6: This function does not assume that it has access to the complete
aggregate feature. From and to aggregate id measures must be included
for each flowline in order to have aggregate id measures (reachcode or
mainstem measures) in the output.

## Examples

``` r
# \donttest{
if (require(nhdplusTools)) {
  source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))

  if (!any(lengths(sf::st_geometry(sample_flines)) > 1))
    sample_flines <- sf::st_cast(sample_flines, "LINESTRING", warn = FALSE)

  point <- sf::st_sfc(sf::st_point(c(-76.87479, 39.48233)),
    crs = 4326)

  index_points_to_lines(sample_flines, point)

  point <- sf::st_transform(point, 5070)

  index_points_to_lines(sample_flines, point,
    search_radius = units::set_units(200, "m"))

  index_points_to_lines(sample_flines, point, precision = 30)

  points <- sf::st_sfc(list(sf::st_point(c(-76.86934, 39.49328)),
    sf::st_point(c(-76.91711, 39.40884)),
    sf::st_point(c(-76.88081, 39.36354))),
  crs = 4326)

  index_points_to_lines(sample_flines, points,
    search_radius = units::set_units(0.2, "degrees"),
    max_matches = 10)

  index_points_to_lines(sample_flines, points,
    search_radius = units::set_units(0.2, "degrees"),
    ids = c(11689926, 11690110, 11688990))

}
#> Warning: crs of lines and points don't match. attempting st_transform of lines
#> Warning: crs of lines and points don't match. attempting st_transform of lines
#>   point_id    COMID      REACHCODE REACHCODE_measure       offset
#> 1        1 11689926 02060003001467          100.0000 6.026811e-05
#> 2        2 11690110 02060003001493          100.0000 7.424781e-03
#> 3        3 11688990 02060003000515            1.9144 8.652186e-03
# }
```
