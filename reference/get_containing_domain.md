# Look up a domain's containing domain

Returns the `containing_domain_id` slot for one or more domains in a
decomposition. Containment is declared via
[`set_containment()`](https://doi-usgs.github.io/hydroloom/reference/set_containment.md);
domains without a container return `NA_character_`.

## Usage

``` r
get_containing_domain(decomposition, domain_id)
```

## Arguments

- decomposition:

  object of class `domain_decomposition`.

- domain_id:

  scalar or vector of domain ids (keys of `decomposition$domains`).

## Value

character vector of containing domain ids, same length as `domain_id`.
`NA_character_` for domains that have no container.

## See also

[`set_containment()`](https://doi-usgs.github.io/hydroloom/reference/set_containment.md),
[`get_domain_graph()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_graph.md),
[domain_decomposition](https://doi-usgs.github.io/hydroloom/reference/domain_decomposition.md).

## Examples

``` r
g <- sf::read_sf(system.file("extdata/walker.gpkg", package = "hydroloom"))

h <- hy(g) |>
  add_toids() |>
  add_levelpaths(name_attribute = "GNIS_ID",
    weight_attribute = "arbolate_sum")

d <- decompose_network(h)

get_containing_domain(d, names(d$domains)[1])
#> [1] NA
```
