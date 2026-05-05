# Test whether a domain is a stem

A domain is a *stem* when it is neither a leaf nor a root — both
upstream domains feed into it and its outlet hands off to a downstream
domain.

## Usage

``` r
is_stem_domain(decomposition, domain_id)
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
[`is_root_domain()`](https://doi-usgs.github.io/hydroloom/reference/is_root_domain.md),
[`hy_domain()`](https://doi-usgs.github.io/hydroloom/reference/hy_domain.md).

## Examples

``` r
g <- sf::read_sf(system.file("extdata/walker.gpkg", package = "hydroloom"))

h <- hy(g) |>
  add_toids() |>
  add_levelpaths(name_attribute = "GNIS_ID",
    weight_attribute = "arbolate_sum")

d <- decompose_network(h)

is_stem_domain(d, names(d$domains)[[1]])
#> [1] FALSE
```
