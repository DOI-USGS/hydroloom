# Get the inter-domain edge list from a decomposition

Returns the inter-domain edge list as a hydroloom edge list (`hy_topo`
or `hy_flownetwork`). Two kinds of relationships appear in the result,
selected by `relations`:

- `"flow"` – pulled from `nexus_registry`. Each row whose `to_domain_id`
  is non-NA contributes one row to the result, recording where one
  domain hands off to the next at a hydro nexus. The result row carries
  that nexus's id in `nexus_id` and `relation_type = "flow"`.

- `"contained"` – pulled from each domain's `containing_domain_id` slot.
  Each non-NA value contributes one row, from the contained domain to
  its container. Containment is declared post-decomposition via
  [`set_containment()`](https://doi-usgs.github.io/hydroloom/reference/set_containment.md)
  and does not pass through a hydro nexus, so the result row carries
  `nexus_id = NA_character_` and `relation_type = "contained"`.

The default returns both kinds; pass `relations = "flow"` for the
inter-domain flow graph only, or `relations = "contained"` for
containment relationships only.

The returned object is passed through `classify_hy()` so it carries the
most-specific hydroloom class (`hy_topo` when the inter-domain graph is
dendritic, `hy_flownetwork` when it is not). This lets downstream
hydroloom functions like
[`check_hy_graph()`](https://doi-usgs.github.io/hydroloom/reference/check_hy_graph.md)
and
[`sort_network()`](https://doi-usgs.github.io/hydroloom/reference/sort_network.md)
dispatch on it without extra conversion.

## Usage

``` r
get_domain_graph(decomposition, relations = c("flow", "contained"))
```

## Arguments

- decomposition:

  object of class `domain_decomposition`.

- relations:

  character vector. Which `relation_type` values to include. Default is
  both `"flow"` and `"contained"`.

## Value

hydroloom edge list (`hy_topo` or `hy_flownetwork`) with columns `id`,
`toid`, `nexus_id`, `relation_type`.

## See also

[domain_decomposition](https://doi-usgs.github.io/hydroloom/reference/domain_decomposition.md)
for the wrapper object,
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

get_domain_graph(d, relations = "flow")
#> # hydroloom dendritic edge list (self-referencing): 0 features
#> [1] id            toid          nexus_id      relation_type
#> <0 rows> (or 0-length row.names)
```
