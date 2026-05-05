# Test whether a domain is the root of its basin

A domain is the *root* when its outlet nexus has no downstream domain —
the registry row for its `outlet_nexus_id` carries `to_domain_id = NA`.
The root is the basin's most-downstream domain.

## Usage

``` r
is_root_domain(decomposition, domain_id)
```

## Arguments

- decomposition:

  object of class `domain_decomposition`.

- domain_id:

  character(1). Domain id to test.

## Value

logical(1).

## See also

[`is_leaf_domain()`](https://doi-usgs.github.io/hydroloom/reference/is_leaf_domain.md),
[`is_stem_domain()`](https://doi-usgs.github.io/hydroloom/reference/is_stem_domain.md),
[`hy_domain()`](https://doi-usgs.github.io/hydroloom/reference/hy_domain.md).

## Examples

``` r
g <- sf::read_sf(system.file("extdata/walker.gpkg", package = "hydroloom"))

h <- hy(g) |>
  add_toids() |>
  add_levelpaths(name_attribute = "GNIS_ID",
    weight_attribute = "arbolate_sum")

d <- decompose_network(h)

is_root_domain(d, names(d$domains)[[1]])
#> [1] TRUE
```
