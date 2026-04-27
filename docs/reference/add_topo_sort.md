# Add topo_sort

calls
[sort_network](https://doi-usgs.github.io/hydroloom/reference/sort_network.md)
without support for splitting the network and adds a `nrow:1` topo_sort
attribute.

## Usage

``` r
add_topo_sort(x, outlets = NULL)

# S3 method for class 'data.frame'
add_topo_sort(x, outlets = NULL)

# S3 method for class 'hy'
add_topo_sort(x, outlets = NULL)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- outlets:

  same as id in x. if specified, only the network emanating from these
  outlets will be considered and returned.

## Value

data.frame containing a topo_sort attribute.

## Details

Required attributes: `id`, `toid`
