# Get Bridge Flowlines

Identifies bridge flowlines (cut edges) in the network. Bridge flowlines
are those whose removal would disconnect the network. Flowlines are
edges in the underlying node graph, so bridge detection correctly
identifies the sole-path flowlines that separate parts of the network –
including flowlines within diversion systems that do not rejoin.

## Usage

``` r
get_bridge_flowlines(x, quiet = FALSE)

# S3 method for class 'data.frame'
get_bridge_flowlines(x, quiet = FALSE)

# S3 method for class 'hy'
get_bridge_flowlines(x, quiet = FALSE)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- quiet:

  logical quiet messages?

## Value

vector of flowline ids that are bridge flowlines in the network

## Details

Required attributes: `id`, `toid`

## Examples

``` r
x <- data.frame(
  id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  toid = c(2, 3, 4, 5, 0, 7, 8, 9, 4)
)

# 1 -> 2 -> 3 -> 4 -> 5
#               ^
#               |
# 6 -> 7 -> 8 -> 9
#
# Dendritic tree: all flowlines are bridges
get_bridge_flowlines(x)
#> [1] 1 2 3 4 5 6 7 8 9
```
