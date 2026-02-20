# add Streamorder

Adds a strahler stream order.

Algorithm: If more than one upstream flowline has an order equal to the
maximum upstream order then the downstream flowline is assigned the
maximum upstream order plus one. Otherwise it is assigned the maximum
upstream order.

To match the NHDPlus algorithm, non-dendritic network connectivity must
be included. All secondary paths will have the `stream_order` of
upstream primary paths and a `stream_calculator` value of 0. Secondary
paths have no affect on the order of downstream paths.

## Usage

``` r
add_streamorder(x, status = TRUE)

# S3 method for class 'data.frame'
add_streamorder(x, status = TRUE)

# S3 method for class 'hy'
add_streamorder(x, status = TRUE)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- status:

  boolean if status updates should be printed.

## Value

data.frame containing added `stream_order` and `stream_calculator`
attribute.

## Details

Required attributes: `id` and `toid` or `fromnode`, `tonode`, and
`divergence`

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

x <- dplyr::select(x, COMID, FromNode, ToNode, Divergence)

x <- add_streamorder(x)

plot(sf::st_geometry(x), lwd = x$stream_order, col = "blue")

plot(sf::st_geometry(x), lwd = x$stream_calculator, col = "blue")

```
