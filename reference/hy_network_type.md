# What representation pattern does this network use?

Returns the most specific hydroloom class. The class names map to
database / graph representation patterns:

- `"hy_leveled"`: enriched edge list (+ topo_sort, levelpath)

- `"hy_topo"`: self-referencing edge list (id/toid, dendritic)

- `"hy_node"`: fromnode/tonode graph (id/fromnode/tonode)

- `"hy_flownetwork"`: junction table (id/toid/upmain/downmain)

- `"hy"`: name-aligned base object

## Usage

``` r
hy_network_type(x)
```

## Arguments

- x:

  object to query.

## Value

character. Most specific hydroloom class name, or
`"not a hydroloom object"` if x has no hydroloom class.

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
hy_network_type(hy(x))
#> [1] "hy_node"
```
