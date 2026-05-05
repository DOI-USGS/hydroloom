# What operations are available for this network?

Returns a named logical vector indicating which hydroloom functions can
operate on the input object given its current class and columns.

Functions marked TRUE are directly callable. Use
`include_conversions = TRUE` to see what becomes available after a
single representation conversion.

## Usage

``` r
hy_capabilities(x, include_conversions = FALSE)
```

## Arguments

- x:

  object to query.

- include_conversions:

  logical. If TRUE, also marks functions reachable via a single
  representation conversion.

## Value

named logical vector.

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
hy_capabilities(hy(x))
#>               add_toids      make_node_topology          to_flownetwork 
#>                    TRUE                   FALSE                   FALSE 
#>            sort_network           add_topo_sort          add_levelpaths 
#>                   FALSE                   FALSE                   FALSE 
#>          add_pathlength         add_streamorder   accumulate_downstream 
#>                   FALSE                   FALSE                   FALSE 
#>          check_hy_graph  navigate_hydro_network         add_pfafstetter 
#>                   FALSE                   FALSE                   FALSE 
#>         add_streamlevel          add_divergence   add_return_divergence 
#>                   FALSE                    TRUE                    TRUE 
#>          subset_network    navigate_network_dfs          make_index_ids 
#>                    TRUE                   FALSE                   FALSE 
#> make_attribute_topology   index_points_to_lines 
#>                    TRUE                    TRUE 
```
