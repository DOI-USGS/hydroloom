# Index Points to Waterbodies

given an sf point geometry column, return waterbody id, and id of
dominant artificial path

## Usage

``` r
index_points_to_waterbodies(
  waterbodies,
  points,
  flines = NULL,
  search_radius = NULL
)
```

## Arguments

- waterbodies:

  sf data.frame of type POLYGON or MULTIPOLYGON including a "wbid"
  attribute.

- points:

  sfc of type POINT

- flines:

  sf data.frame (optional) of type LINESTRING or MULTILINESTRING
  including id, wbid, and topo_sort attributes. If omitted, only
  waterbody indexes are returned.

- search_radius:

  units class with a numeric value indicating how far to search for a
  waterbody boundary in units of provided projection. Set units with
  [set_units](https://r-quantities.github.io/units/reference/units.html).

## Value

data.frame with columns `in_wb_COMID` (or `in_wbid`), `near_wb_COMID`
(or `near_wbid`), `near_wb_dist`, and `outlet_fline_COMID` (or
`wb_outlet_id`). Column names use COMID when input contains a COMID
attribute, otherwise hydroloom names (wbid) are used. Distance is in
units of provided projection.

## Examples

``` r
if (require(nhdplusTools)) {

  source(system.file("extdata/sample_data.R", package = "nhdplusTools"))

  waterbodies <- sf::st_transform(
    sf::read_sf(sample_data, "NHDWaterbody"), 5070)

  points <- sf::st_transform(
    sf::st_sfc(sf::st_point(c(-89.356086, 43.079943)),
      crs = 4326), 5070)

  index_points_to_waterbodies(waterbodies, points,
    search_radius = units::set_units(500, "m"))

}
#>   near_wb_COMID near_wb_dist in_wb_COMID
#> 1     167120949     272.8278   167120949
```
