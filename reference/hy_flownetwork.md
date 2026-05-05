# hy_flownetwork: non-dendritic junction table

A `hy_flownetwork` object is a junction table — a non-dendritic edge
list of `id` and `toid` rows where `id` may repeat. Each row records one
connection between an upstream catchment or flowline and a downstream
one, and optional `upmain`/`downmain` logical columns annotate which of
the connections at a divergence is the main path. This is the only
edge-list-shaped representation in hydroloom that preserves divergences
directly.

Unlike the other class pages on this index, `hy_flownetwork` does
**not** inherit from `hy` — `id` is not guaranteed to be a primary key,
and a `hy_flownetwork` does not pass
[`is.hy()`](https://doi-usgs.github.io/hydroloom/reference/is.hy.md).
The user-facing producer
[`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md)
drops the `orig_names` round-trip metadata that `hy` carries; a
`hy_flownetwork` produced by
[`hy()`](https://doi-usgs.github.io/hydroloom/reference/hy.md) or
`classify_hy()` from a non-dendritic data.frame may still have
`orig_names` attached internally so name-aligned dispatch can restore
the user's column names on return.

## Details

`hy_flownetwork` is what hydroloom's internal classifier produces (via
[`hy()`](https://doi-usgs.github.io/hydroloom/reference/hy.md)) when an
`id`/`toid` table has duplicated `id` values, and what
[`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md)
produces from a `hy_leveled` input. The `upmain`/`downmain` annotation,
when present, lets divergence-aware operations choose a main path
without having to re-derive it from levelpath identity each time.

Several hydroloom operations that were originally written against
`hy_topo` accept `hy_flownetwork` directly through delegate methods,
because the underlying algorithm already tolerates non-dendritic input:
[`add_streamorder()`](https://doi-usgs.github.io/hydroloom/reference/add_streamorder.md),
[`accumulate_downstream()`](https://doi-usgs.github.io/hydroloom/reference/accumulate_downstream.md),
[`get_bridge_flowlines()`](https://doi-usgs.github.io/hydroloom/reference/get_bridge_flowlines.md),
[`add_levelpaths()`](https://doi-usgs.github.io/hydroloom/reference/add_levelpaths.md),
and
[`make_node_topology()`](https://doi-usgs.github.io/hydroloom/reference/make_node_topology.md)
all work on `hy_flownetwork` without conversion.

## Required columns

- `id` — catchment or flowline identifier; may be non-unique

- `toid` — downstream `id`

Optional:

- `upmain` — logical, `TRUE` for the main upstream connection at each
  junction

- `downmain` — logical, `TRUE` for the main downstream connection at
  each junction

See
[hydroloom_name_definitions](https://doi-usgs.github.io/hydroloom/reference/hydroloom_name_definitions.md)
for the canonical column definitions.

## Functions that operate on hy_flownetwork

- Native operations:
  [`navigate_network_dfs()`](https://doi-usgs.github.io/hydroloom/reference/navigate_network_dfs.md),
  [`make_index_ids()`](https://doi-usgs.github.io/hydroloom/reference/make_index_ids.md)

- Delegated to the `hy_topo` algorithm:
  [`add_streamorder()`](https://doi-usgs.github.io/hydroloom/reference/add_streamorder.md),
  [`accumulate_downstream()`](https://doi-usgs.github.io/hydroloom/reference/accumulate_downstream.md),
  [`get_bridge_flowlines()`](https://doi-usgs.github.io/hydroloom/reference/get_bridge_flowlines.md),
  [`add_levelpaths()`](https://doi-usgs.github.io/hydroloom/reference/add_levelpaths.md),
  [`make_node_topology()`](https://doi-usgs.github.io/hydroloom/reference/make_node_topology.md)

Call
[`hy_capabilities()`](https://doi-usgs.github.io/hydroloom/reference/hy_capabilities.md)
on a specific object for the authoritative list given its current
columns.

## Conversions to other representations

- To
  [hy_node](https://doi-usgs.github.io/hydroloom/reference/hy_node.md)
  (bipartite graph):
  [`make_node_topology()`](https://doi-usgs.github.io/hydroloom/reference/make_node_topology.md)

Going the other direction — from `hy_node` or `hy_leveled` into
`hy_flownetwork` — is what
[`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md)
is for.

## See also

[hy](https://doi-usgs.github.io/hydroloom/reference/hy.md),
[hy_topo](https://doi-usgs.github.io/hydroloom/reference/hy_topo.md),
[hy_leveled](https://doi-usgs.github.io/hydroloom/reference/hy_leveled.md),
[hy_node](https://doi-usgs.github.io/hydroloom/reference/hy_node.md),
[`hy_capabilities()`](https://doi-usgs.github.io/hydroloom/reference/hy_capabilities.md),
[`hy_network_type()`](https://doi-usgs.github.io/hydroloom/reference/hy_network_type.md),
[`is_dendritic()`](https://doi-usgs.github.io/hydroloom/reference/is_dendritic.md),
[`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md),
[`navigate_network_dfs()`](https://doi-usgs.github.io/hydroloom/reference/navigate_network_dfs.md),
[`make_index_ids()`](https://doi-usgs.github.io/hydroloom/reference/make_index_ids.md)

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

z <- to_flownetwork(x)

hy_network_type(z)
#> [1] "hy_flownetwork"

z
#> # hydroloom flow network (junction table): 832 connections, 86 diversions
#> # A tibble: 832 × 4
#>         id    toid upmain downmain
#>      <int>   <dbl> <lgl>  <lgl>   
#>  1 8893864 8894334 TRUE   TRUE    
#>  2 8894490 8894336 TRUE   TRUE    
#>  3 8894494 8894490 TRUE   TRUE    
#>  4 8894334 8894492 TRUE   TRUE    
#>  5 8894492 8894494 TRUE   TRUE    
#>  6 8893850 8893864 TRUE   TRUE    
#>  7 8893842 8893850 TRUE   TRUE    
#>  8 8894192 8893842 TRUE   TRUE    
#>  9 8894192 8893844 FALSE  FALSE   
#> 10 8894310 8894192 TRUE   TRUE    
#> # ℹ 822 more rows
```
