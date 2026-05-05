# Recompose a domain decomposition by accumulating an attribute downstream

Accumulates a numeric variable on `decomposition$source_network` through
the decomposed fabric and returns the source network with the per-row
downstream-accumulated value populated.

## Usage

``` r
recompose(decomposition, var, containment = c("ignore", "accumulate"))
```

## Arguments

- decomposition:

  A `domain_decomposition` returned by
  [`decompose_network()`](https://doi-usgs.github.io/hydroloom/reference/decompose_network.md).

- var:

  character(1). Name of a numeric column on
  `decomposition$source_network` to accumulate downstream through the
  fabric.

- containment:

  character(1). One of `"ignore"` (default – a contained basin's
  accumulated value stops at its own outlet) or `"accumulate"` (the
  contained basin's accumulated value is added at the containing
  domain's outlet and routed downstream through the containing basin's
  extensive network).

## Value

The `source_network` tibble with the per-row downstream-accumulated
value of `var` populated. Class and row order match the input
`source_network`.

## Details

Two passes. The decomposed form is engineered so each pass is a single
[accumulate_downstream()](https://doi-usgs.github.io/hydroloom/reference/accumulate_downstream.md)
call.

Pass 1 – per-domain accumulation produces, for every extensive network
row, the locally-incremental value (own + all laterals draining
transitively to that row). Lateral rows receive their full
upstream-cumulative within the domain, which equals the basin-wide
cumulative for that row since laterals never cross a domain boundary.

Pass 2 – per-basin extensive-network accumulation walks the basin's
`domain_connectivity` overlay end-to-end, seeded with the
locally-incremental values from pass 1, to produce the basin-wide
cumulative at every extensive network row.

Sub-threshold basins (no `domain_connectivity` overlay) carry their
single domain's pass 1 values straight through.

**Containment.** With `containment = "accumulate"` (opt-in), each
contained basin's accumulated value at its outlet is added at the
containing domain's outlet – the most-downstream row of the containing
domain's segment of the extensive network – and routed downstream from
there through the containing basin's extensive network. Transitive
containment (A inside B inside C) is processed from the most-contained
outward, so A's contribution is added to B before B's value is read, and
B's combined value is added to C. With the default
`containment = "ignore"` a contained basin's accumulated value stops at
its own outlet – the correct behavior for a true endorheic basin and the
only behavior before this argument was added. See
[`set_containment()`](https://doi-usgs.github.io/hydroloom/reference/set_containment.md)
for how containment is declared.

## See also

[`accumulate_downstream()`](https://doi-usgs.github.io/hydroloom/reference/accumulate_downstream.md),
[`decompose_network()`](https://doi-usgs.github.io/hydroloom/reference/decompose_network.md),
[`set_containment()`](https://doi-usgs.github.io/hydroloom/reference/set_containment.md).

## Examples

``` r
g <- sf::read_sf(system.file("extdata/walker.gpkg", package = "hydroloom"))

h <- hy(g) |>
  add_toids() |>
  add_levelpaths(name_attribute = "GNIS_ID",
    weight_attribute = "arbolate_sum")

d <- decompose_network(h)

rec <- recompose(d, "da_sqkm")

head(rec[, c("id", "da_sqkm")])
#> Simple feature collection with 6 features and 2 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -122.9228 ymin: 38.18986 xmax: -122.8391 ymax: 38.23326
#> Geodetic CRS:  GRS 1980(IUGG, 1980)
#> # hydroloom dendritic leveled edge list (self-referencing): 6 features
#> # A tibble: 6 × 3
#>        id da_sqkm                                                           geom
#>     <int>   <dbl>                                               <LINESTRING [°]>
#> 1 5329303    194.   (-122.9161 38.22958, -122.9174 38.22452, -122.9228 38.22129)
#> 2 5329293    190.                       (-122.9136 38.23326, -122.9161 38.22958)
#> 3 5329305    177. (-122.8951 38.22816, -122.8959 38.2226, -122.9042 38.22214, -…
#> 4 5329317    169. (-122.8758 38.20836, -122.8801 38.21294, -122.8907 38.21282, …
#> 5 5329315    161. (-122.86 38.21232, -122.8688 38.2166, -122.8775 38.21424, -12…
#> 6 5329339    104. (-122.8391 38.18986, -122.8483 38.19682, -122.8571 38.2, -122…
```
