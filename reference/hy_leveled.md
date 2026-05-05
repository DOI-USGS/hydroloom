# hy_leveled: enriched self-referencing edge list

A `hy_leveled` object is a `hy_topo` carrying the additional columns
produced by stream leveling: `topo_sort`, `levelpath`, and
`levelpath_outlet_id`. These columns are what mainstem-aware operations
(Pfafstetter coding, stream level, junction-table conversion) need in
order to run.

`hy_leveled` is a strict subclass of `hy_topo`, so every function that
accepts a `hy_topo` accepts a `hy_leveled` as well.

## Details

`hy_leveled` exists to mark a `hy_topo` as having been through
[`add_levelpaths()`](https://doi-usgs.github.io/hydroloom/reference/add_levelpaths.md)
so downstream functions can dispatch on the presence of leveling without
re-checking column names. The leveling columns encode mainstem path
identity (`levelpath`), the outlet that closes that path
(`levelpath_outlet_id`), and the topological order along the network
(`topo_sort`).

Like `hy_topo`, `hy_leveled` requires unique `id` and cannot represent
divergences as duplicated rows. Convert to
[hy_flownetwork](https://doi-usgs.github.io/hydroloom/reference/hy_flownetwork.md)
via
[`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md)
to preserve main and diverted paths in junction-table form.

## Required columns

- `id` — catchment or flowline identifier, unique across rows

- `toid` — `id` of the immediately downstream feature

- `topo_sort` — topological sort order (NHDPlus hydrosequence)

- `levelpath` — mainstem path identifier

- `levelpath_outlet_id` — outlet `id` that closes each levelpath

See
[hydroloom_name_definitions](https://doi-usgs.github.io/hydroloom/reference/hydroloom_name_definitions.md)
for the canonical column definitions.

## Functions that operate on hy_leveled

All `hy_topo` methods, plus the leveling-aware operations:

- Mainstem coding:
  [`add_pfafstetter()`](https://doi-usgs.github.io/hydroloom/reference/add_pfafstetter.md),
  [`add_streamlevel()`](https://doi-usgs.github.io/hydroloom/reference/add_streamlevel.md)

- Junction-table conversion:
  [`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md)

Call
[`hy_capabilities()`](https://doi-usgs.github.io/hydroloom/reference/hy_capabilities.md)
on a specific object for the authoritative list given its current
columns.

## Conversions to other representations

- To
  [hy_node](https://doi-usgs.github.io/hydroloom/reference/hy_node.md)
  (bipartite graph):
  [`make_node_topology()`](https://doi-usgs.github.io/hydroloom/reference/make_node_topology.md)

- To
  [hy_flownetwork](https://doi-usgs.github.io/hydroloom/reference/hy_flownetwork.md)
  (junction table):
  [`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md),
  which uses `levelpath` to decide which connection at a divergence is
  the main path

## See also

[hy](https://doi-usgs.github.io/hydroloom/reference/hy.md),
[hy_topo](https://doi-usgs.github.io/hydroloom/reference/hy_topo.md),
[hy_node](https://doi-usgs.github.io/hydroloom/reference/hy_node.md),
[hy_flownetwork](https://doi-usgs.github.io/hydroloom/reference/hy_flownetwork.md),
[`hy_capabilities()`](https://doi-usgs.github.io/hydroloom/reference/hy_capabilities.md),
[`hy_network_type()`](https://doi-usgs.github.io/hydroloom/reference/hy_network_type.md),
[`add_levelpaths()`](https://doi-usgs.github.io/hydroloom/reference/add_levelpaths.md),
[`add_pfafstetter()`](https://doi-usgs.github.io/hydroloom/reference/add_pfafstetter.md),
[`to_flownetwork()`](https://doi-usgs.github.io/hydroloom/reference/to_flownetwork.md)

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

z <- add_levelpaths(add_toids(hy(x)),
                    name_attribute = "GNIS_ID",
                    weight_attribute = "arbolate_sum")

hy_network_type(z)
#> [1] "hy_leveled"
```
