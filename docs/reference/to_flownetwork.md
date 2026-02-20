# to flownetwork

converts an `hy` object into a flownetwork with "id", "toid", "upmain"
and "downmain attributes.

## Usage

``` r
to_flownetwork(x, warn_dendritic = TRUE)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- warn_dendritic:

  logical if TRUE and a dendritic `toid` attribute is provided, a
  warning will be emitted as toid is expected to be non-dendritic for
  any `downmain` to be `FALSE`.

## Value

data.frame "id", "toid", "upmain" and "downmain attributes. A check is
run to ensure upmain and downmain are valid with one and only one upmain
and one and only one downmain from any given network element.

## Details

Required attributes:

`id` and `toid` or `fromnode` and `tonode`

`divergence` an attribute containing 0, 1, or 2 where 0 indicates there
is only one downstream connection, 1 is the main connection downstream
of a diversion and 2 is secondary connection downstream of a diversion.

`levelpath`, integer attribute which will have one and only one matching
value upstream at a confluence.

## Examples

``` r
f <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
to_flownetwork(f)
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
