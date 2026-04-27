# Add Divergence Attribute

Given a non-dendritic flow network and required attributes, adds a
divergence attribute according to NHDPlus data model methods.

## Usage

``` r
add_divergence(
  x,
  coastal_outlet_ids,
  inland_outlet_ids,
  name_attr,
  type_attr,
  major_types
)

# S3 method for class 'data.frame'
add_divergence(
  x,
  coastal_outlet_ids,
  inland_outlet_ids,
  name_attr,
  type_attr,
  major_types
)

# S3 method for class 'hy'
add_divergence(
  x,
  coastal_outlet_ids,
  inland_outlet_ids,
  name_attr,
  type_attr,
  major_types
)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- coastal_outlet_ids:

  vector of identifiers for network outlets that terminate at the coast.

- inland_outlet_ids:

  vector of identifiers for network outlets that terminate inland.

- name_attr:

  character attribute name of attribute containing a feature name or
  name identifier.

- type_attr:

  character attribute name of attribute containing a feature type
  indicator.

- major_types:

  vector of values of `type_attr` that should be interpreted as being
  "major". e.g. river might be major and canal might be minor.

## Value

returns x with a `divergence` attribute appended

## Details

Required attributes: `id`, `fromnode`, `tonode`

When considering downstream connections with diversions, there are three
factors considered to determine which is primary.  
1a) same name  
1b) is named  
2) feature type (type_attr controls this)  
3) flows to coast (has a coastal connection is preferred)  

The following list describes the order of precedence for tests  
1: 1a, 2, 3  
2: 1a, 2  
3: The NHDPlus uses diverted fraction this is not used currently.  
4: 1b, 2, 3  
5: 2, 3  
6: 1b, 3  
7: 3,  
8: 1b, 2  
9: 2  
10: 1b  

If all checks return and no primary connection has been identified, the
connection with a smaller id is chosen.

In the case that there are two or more upstream connections, the
upstream name to use is chosen 1) if there is only one upstream flowline
with a name 2) if one of the upstream flowlines with a name matches the
downstream line, 3) if one of the upstream flowlines is of a "major"
type and others are not, and, 4) if no criteria exist to select one, the
smallest id value otherwise.

## Examples

``` r
f <- system.file("extdata/coastal_example.gpkg", package = "hydroloom")

g <- sf::read_sf(f)
g <- g[g$FTYPE != "Coastline", ]

outlets <- g$COMID[!g$ToNode %in% g$FromNode]

g <- dplyr::select(g, COMID, gnis_id, FTYPE,
  FromNode, ToNode)

add_divergence(g,
  coastal_outlet_ids = outlets,
  inland_outlet_ids = c(),
  name_attr = "gnis_id",
  type_attr = "FTYPE",
  major_types = c("StreamRiver", "ArtificialPath", "Connector"))
#> Simple feature collection with 535 features and 6 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -124.3627 ymin: 39.82399 xmax: -123.7742 ymax: 40.41246
#> Geodetic CRS:  NAD83
#> # A tibble: 535 × 7
#>      COMID gnis_id FTYPE    FromNode ToNode                      geom divergence
#>  *   <int> <chr>   <chr>       <dbl>  <dbl>          <LINESTRING [°]>      <dbl>
#>  1 2544239 NA      StreamR… 10092685 1.00e7 (-124.2181 40.41246, -12…          0
#>  2 2544241 229695  StreamR… 10004575 1.00e7 (-124.2256 40.39929, -12…          0
#>  3 2544243 NA      StreamR… 10092686 1.00e7 (-124.2439 40.40495, -12…          0
#>  4 2544263 229695  StreamR… 10004579 1.00e7 (-124.2273 40.39801, -12…          0
#>  5 2544287 NA      StreamR… 10092696 1.00e7 (-124.1699 40.39544, -12…          0
#>  6 2544289 229695  StreamR… 10004588 1.00e7 (-124.1867 40.38665, -12…          0
#>  7 2544301 229695  StreamR… 10092697 1.00e7 (-124.1666 40.38338, -12…          0
#>  8 2544303 229695  StreamR… 10004594 1.00e7 (-124.2376 40.39235, -12…          0
#>  9 2544309 NA      StreamR… 10092700 1.00e7 (-124.3126 40.38182, -12…          0
#> 10 2544311 NA      StreamR… 10092701 1.00e7 (-124.2285 40.37687, -12…          0
#> # ℹ 525 more rows
```
