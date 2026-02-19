# DEPRECATED: Format Index ids

format_index_ids is deprecated and will be removed in a future release.

## Usage

``` r
format_index_ids(g, return_list = FALSE)
```

## Arguments

- g:

  data.frame graph with `id`, `inid` and `toindid` as returned by
  [make_index_ids](https://doi-usgs.github.io/hydroloom/reference/make_index_ids.md)

- return_list:

  logical if TRUE, the returned list will include a "froms_list" element
  containing all from ids in a list form.

## Value

list containing an adjacency matrix and a lengths vector indicating the
number of connections from each node. If `return_list` is `TRUE`, return
will also include a data.frame with an `indid` column and a `toindid`
list column.
