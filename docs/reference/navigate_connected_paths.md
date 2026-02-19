# Navigate Connected Paths

Given a dendritic network and set of ids, finds paths or lengths between
all identified flowpath outlets. This algorithm finds paths between
outlets regardless of flow direction.

## Usage

``` r
navigate_connected_paths(x, outlets, status = FALSE)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- outlets:

  vector of ids from data.frame

- status:

  logical print status and progress bars?

## Value

data.frame containing the distance between pairs of network outlets and
a list column containing flowpath identifiers along path that connect
outlets. For a network with one terminal outlet, the data.frame will
have `nrow(x)^2` rows.

## Details

Required attributes: `id`, `toid`, `length_km`

## Examples

``` r
x <- sf::read_sf(system.file("extdata", "walker.gpkg", package = "hydroloom"))

outlets <- c(5329303, 5329357, 5329317, 5329365, 5329435, 5329817)

x <- add_toids(hy(x))

navigate_connected_paths(x, outlets)
#>       id_1    id_2 network_distance_km
#> 1  5329303 5329357              18.742
#> 2  5329303 5329317               5.797
#> 3  5329303 5329365              22.345
#> 4  5329303 5329435              35.130
#> 5  5329303 5329817              16.043
#> 6  5329357 5329317              12.945
#> 7  5329357 5329365               3.603
#> 8  5329357 5329435              16.388
#> 9  5329357 5329817               8.937
#> 10 5329317 5329365              16.548
#> 11 5329317 5329435              29.333
#> 12 5329317 5329817              10.246
#> 13 5329365 5329435              12.785
#> 14 5329365 5329817              12.540
#> 15 5329435 5329817              25.325
#>                                                         path
#> 1                                        7, 6, 5, 4, 3, 2, 1
#> 2                                                    3, 2, 1
#> 3                                     8, 7, 6, 5, 4, 3, 2, 1
#> 4  15, 14, 13, 12, 11, 60, 62, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1
#> 5                                      18, 17, 5, 4, 3, 2, 1
#> 6                                                 7, 6, 5, 4
#> 7                                                          8
#> 8                       15, 14, 13, 12, 11, 60, 62, 10, 9, 8
#> 9                                               7, 6, 18, 17
#> 10                                             8, 7, 6, 5, 4
#> 11          15, 14, 13, 12, 11, 60, 62, 10, 9, 8, 7, 6, 5, 4
#> 12                                              18, 17, 5, 4
#> 13                         15, 14, 13, 12, 11, 60, 62, 10, 9
#> 14                                           8, 7, 6, 18, 17
#> 15        15, 14, 13, 12, 11, 60, 62, 10, 9, 8, 7, 6, 18, 17
```
