# Add Path Length

Generates the main path length to a basin's terminal path.

## Usage

``` r
add_pathlength(x)

# S3 method for class 'data.frame'
add_pathlength(x)

# S3 method for class 'hy'
add_pathlength(x)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

## Value

data.frame containing pathlength_km

## Details

Required attributes: `id`, `toid`, `length_km`

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

x <- add_toids(x)

x <- add_pathlength(x)

plot(x["Pathlength"])

```
