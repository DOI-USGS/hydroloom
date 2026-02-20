# Check hy Graph

check that a network graph doesn't contain localized loops.

## Usage

``` r
check_hy_graph(x, loop_check = FALSE)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- loop_check:

  logical if TRUE, the entire network is walked from top to bottom
  searching for loops. This loop detection algorithm visits a node in
  the network only once all its upstream neighbors have been visited. A
  complete depth first search is performed at each node, searching for
  paths that lead to an already visited (upstream) node. This algorithm
  is often referred to as "recursive depth first search".

## Value

if no localized loops are found, returns TRUE. If localized loops are
found, problem rows with a row number added.

## Details

Required attributes: `id`, `toid`

## Examples

``` r
# notice that row 4 (id = 4, toid = 9) and row 8 (id = 9, toid = 4) is a loop.
test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
  toid = c(2, 3, 4, 9, 7, 8, 9, 4))
check_hy_graph(test_data)
#> # A tibble: 2 × 4
#>    toid    id   row toid_check
#>   <dbl> <dbl> <int>      <dbl>
#> 1     4     9     8          9
#> 2     9     4     4          4
```
