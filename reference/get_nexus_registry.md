# Get the nexus registry from a decomposition

Returns `decomposition$nexus_registry` – the table that records each
synthetic hydro nexus produced by
[`decompose_network()`](https://doi-usgs.github.io/hydroloom/reference/decompose_network.md)
and which domains it connects. One row per nexus; the columns are
`nexus_id`, `from_domain_id`, and `to_domain_id` (NA at basin outlets
where no downstream domain receives the handoff).

## Usage

``` r
get_nexus_registry(decomposition)
```

## Arguments

- decomposition:

  object of class `domain_decomposition`.

## Value

data.frame.

## See also

[domain_decomposition](https://doi-usgs.github.io/hydroloom/reference/domain_decomposition.md),
[`get_domain_graph()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_graph.md).

## Examples

``` r
g <- sf::read_sf(system.file("extdata/walker.gpkg", package = "hydroloom"))

h <- hy(g) |>
  add_toids() |>
  add_levelpaths(name_attribute = "GNIS_ID",
    weight_attribute = "arbolate_sum")

d <- decompose_network(h)

head(get_nexus_registry(d))
#>            nexus_id         from_domain_id to_domain_id
#> 1 nx_5329303_outlet domain_5329303_5329303         <NA>
```
