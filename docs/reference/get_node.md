# Get Line Node

Given one or more lines, returns a particular node from the line.

## Usage

``` r
get_node(x, position = "end")
```

## Arguments

- x:

  sf sf data.frame with one or more LINESTRING features

- position:

  character either "start" or "end"

## Value

sf data.frame containing requested nodes

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

start <- get_node(x, "start")
end <- get_node(x, "end")

plot(sf::st_zm(sf::st_geometry(x)),
  lwd = x$StreamOrde, col = "blue")
plot(sf::st_geometry(start), add = TRUE)


plot(sf::st_zm(sf::st_geometry(x)),
  lwd = x$StreamOrde, col = "blue")
plot(sf::st_geometry(end), add = TRUE)

```
