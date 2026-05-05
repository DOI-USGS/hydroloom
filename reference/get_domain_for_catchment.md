# Look up the domain containing a catchment

Returns the `domain_id` of the domain that owns a given catchment in a
decomposition. Accepts a scalar or vector of catchment ids.

## Usage

``` r
get_domain_for_catchment(decomposition, catchment_id)
```

## Arguments

- decomposition:

  object of class `domain_decomposition`.

- catchment_id:

  scalar or vector of catchment ids.

## Value

character vector of domain ids, same length as `catchment_id`.

## See also

[domain_decomposition](https://doi-usgs.github.io/hydroloom/reference/domain_decomposition.md)
for the wrapper object,
[`decompose_network()`](https://doi-usgs.github.io/hydroloom/reference/decompose_network.md).

## Examples

``` r
g <- sf::read_sf(system.file("extdata/walker.gpkg", package = "hydroloom"))

h <- hy(g) |>
  add_toids() |>
  add_levelpaths(name_attribute = "GNIS_ID",
    weight_attribute = "arbolate_sum")

d <- decompose_network(h)

get_domain_for_catchment(d, h$id[1])
#> [1] "domain_5329303_5329303"
```
