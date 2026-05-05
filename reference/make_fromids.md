# DEPRECATED Convert "to" index ids to "from" index ids

make_fromids is deprecated and will be removed in a future release. Use
[make_index_ids](https://doi-usgs.github.io/hydroloom/reference/make_index_ids.md)
with `mode = "from"` instead.

## Usage

``` r
make_fromids(index_ids, return_list = FALSE, upmain = NULL)
```

## Arguments

- index_ids:

  data.frame as returned by
  [make_index_ids](https://doi-usgs.github.io/hydroloom/reference/make_index_ids.md)

- return_list:

  logical if TRUE, the returned list will include a "froms_list" element
  containing all from ids in a list form.

- upmain:

  data.frame containing `id` and `upmain` columns. `upmain` should be a
  logical value indicating if the id is the upmain connection from its
  downstream neighbors.

## Value

list containing a "froms" matrix, "lengths" vector, and optionally
"froms_list" elements.
