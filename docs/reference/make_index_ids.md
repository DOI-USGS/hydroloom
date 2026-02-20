# Make Index ids

makes index ids for the provided hy object. These can be used for graph
traversal algorithms such that the row number and id are equal.

## Usage

``` r
make_index_ids(x, mode = "to", long_form = NULL)

# S3 method for class 'data.frame'
make_index_ids(x, mode = "to", long_form = NULL)

# S3 method for class 'hy'
make_index_ids(x, mode = "to", long_form = NULL)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- mode:

  character indicating the mode of the graph. Choose from "to", "from",
  or "both". Default is "to". Se Details for more information.

- long_form:

  logical DEPRECATED

## Value

list containing named elements:

- to:

  adjacency matrix with columns that correspond to `unqiue(x$id)`

- lengths:

  vector indicating the number of connections from each node

- to_list:

  a data.frame with an `id`, `indid` and a `toindid` list column.

List will have names `froms`, `lengths`, and `froms_list` for mode
"from".

NOTE: the long_form output is deprecated and will be removed in a future
release.

## Details

Required attributes: `id`, `toid`

mode determines the direction of the graph. If "to", the graph will be
directed from the `id` to the `toid`. If "from", the graph will be
directed from the `toid` to the `id`. If "both", the list will contain
both a "from" and a "to" element containing each version of the graph.

## Examples

``` r
x <- data.frame(
  id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  toid = c(2, 3, 4, 5, 0, 7, 8, 9, 4)
)

make_index_ids(x, mode = "to")
#> $to
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#> [1,]    2    3    4    5    0    7    8    9    4
#> 
#> $lengths
#> 1 2 3 4 5 6 7 8 9 
#> 1 1 1 1 1 1 1 1 1 
#> 
#> $to_list
#>   id indid toindid
#> 1  1     1       2
#> 2  2     2       3
#> 3  3     3       4
#> 4  4     4       5
#> 5  5     5       0
#> 6  6     6       7
#> 7  7     7       8
#> 8  8     8       9
#> 9  9     9       4
#> 

make_index_ids(x, mode = "from")
#> $froms
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#> [1,]   NA    1    2    3    4   NA    6    7    8
#> [2,]   NA   NA   NA    9   NA   NA   NA   NA   NA
#> 
#> $lengths
#> [1] 0 1 1 2 1 0 1 1 1
#> 
#> $froms_list
#> Key: <indid>
#>       id indid fromindid
#>    <num> <int>    <list>
#> 1:     1     1        NA
#> 2:     2     2         1
#> 3:     3     3         2
#> 4:     4     4       3,9
#> 5:     5     5         4
#> 6:     6     6        NA
#> 7:     7     7         6
#> 8:     8     8         7
#> 9:     9     9         8
#> 

make_index_ids(x, mode = "both")
#> $to
#> $to$to
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#> [1,]    2    3    4    5    0    7    8    9    4
#> 
#> $to$lengths
#> 1 2 3 4 5 6 7 8 9 
#> 1 1 1 1 1 1 1 1 1 
#> 
#> $to$to_list
#>   id indid toindid
#> 1  1     1       2
#> 2  2     2       3
#> 3  3     3       4
#> 4  4     4       5
#> 5  5     5       0
#> 6  6     6       7
#> 7  7     7       8
#> 8  8     8       9
#> 9  9     9       4
#> 
#> 
#> $from
#> $from$froms
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#> [1,]   NA    1    2    3    4   NA    6    7    8
#> [2,]   NA   NA   NA    9   NA   NA   NA   NA   NA
#> 
#> $from$lengths
#> [1] 0 1 1 2 1 0 1 1 1
#> 
#> $from$froms_list
#> Key: <indid>
#>       id indid fromindid
#>    <num> <int>    <list>
#> 1:     1     1        NA
#> 2:     2     2         1
#> 3:     3     3         2
#> 4:     4     4       3,9
#> 5:     5     5         4
#> 6:     6     6        NA
#> 7:     7     7         6
#> 8:     8     8         7
#> 9:     9     9         8
#> 
#> 

x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))

x <- add_toids(x, return_dendritic = FALSE)

x <- make_index_ids(x)

names(x)
#> [1] "to"      "lengths" "to_list"
class(x$to)
#> [1] "matrix" "array" 
class(x$lengths)
#> [1] "numeric"
class(x$to_list)
#> [1] "data.frame"
is.list(x$to_list$toindid)
#> [1] TRUE
```
