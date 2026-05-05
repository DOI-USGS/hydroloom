# Get the overrides table from a decomposition

Returns `decomposition$overrides` – the non-dendritic inter-domain
transfer table passed through from
[`decompose_network()`](https://doi-usgs.github.io/hydroloom/reference/decompose_network.md).
`NULL` when no overrides were supplied.

## Usage

``` r
get_overrides(decomposition)
```

## Arguments

- decomposition:

  object of class `domain_decomposition`.

## Value

data.frame or `NULL`.

## See also

[domain_decomposition](https://doi-usgs.github.io/hydroloom/reference/domain_decomposition.md),
[`decompose_network()`](https://doi-usgs.github.io/hydroloom/reference/decompose_network.md).

## Examples

``` r
g <- sf::read_sf(system.file("extdata/walker.gpkg", package = "hydroloom"))

h <- hy(g) |>
  add_toids() |>
  add_levelpaths(name_attribute = "GNIS_ID",
    weight_attribute = "arbolate_sum")

d <- decompose_network(h)

get_overrides(d)
#> NULL
```
