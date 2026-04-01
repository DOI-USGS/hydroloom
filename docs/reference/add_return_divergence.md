# Add Return Divergence

Adds a return divergence attribute to the provided network. The method
implemented matches that of the NHDPlus except in the rare case that a
diversion includes more than one secondary path.

See
[add_divergence](https://doi-usgs.github.io/hydroloom/reference/add_divergence.md)
and
[make_node_topology](https://doi-usgs.github.io/hydroloom/reference/make_node_topology.md).

## Usage

``` r
add_return_divergence(x, status = TRUE)

# S3 method for class 'data.frame'
add_return_divergence(x, status = TRUE)

# S3 method for class 'hy'
add_return_divergence(x, status = TRUE)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- status:

  boolean if status updates should be printed.

## Value

data.frame containing `return_divergence` attribute

## Details

Required attributes: `id`, `fromnode`, `tonode`, `divergence`

Algorithm:

All network connections with more than one downstream feature are
considered.

[navigate_network_dfs](https://doi-usgs.github.io/hydroloom/reference/navigate_network_dfs.md)
is used to find all downstream features emanating from the primary
(`divergence == 1`) outlet of the diversion in question and secondary
(`divergence == 2`) outlet(s) starting with the primary outlet.

[navigate_network_dfs](https://doi-usgs.github.io/hydroloom/reference/navigate_network_dfs.md)
is called with `reset = FALSE` such that the secondary diversion paths
terminate where they combine with a previously visited feature.

If the diverted paths result in only one outlet, the feature it flows to
is marked as a return divergence.

If the diverted paths result in more than one outlet, the one that flows
to the most upstream feature in the set of features downstream of the
primary outlet of the diversion is marked as the return divergence.

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

x <- hy(x)

x <- add_return_divergence(x)

sum(x$return_divergence == x$RtnDiv)
#> [1] 745

# see description for documentation of one that does not match
```
