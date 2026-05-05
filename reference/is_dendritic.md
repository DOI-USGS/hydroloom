# Is the network dendritic?

Checks whether the network contains any divergences. Returns `FALSE` if
a `divergence` column exists with any value of 2, or if the table has a
`toid` column and duplicated `id` values (indicating row duplication
from divergences). Otherwise returns `TRUE`.

## Usage

``` r
is_dendritic(x, validate_data = TRUE)
```

## Arguments

- x:

  data.frame or hy object to test.

- validate_data:

  logical. If `FALSE`, only checks the `divergence` column (fast). If
  `TRUE` (default), also checks for duplicated ids.

## Value

logical. TRUE if the network is dendritic.

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
is_dendritic(hy(x))
#> [1] FALSE
```
