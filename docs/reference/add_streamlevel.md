# Add Streamlevel

Applies a topological sort and calculates stream level. Algorithm:
Terminal level paths are assigned level 1 (see note 1). Paths that
terminate at a level 1 are assigned level 2. This pattern is repeated
until no paths remain.

If a TRUE/FALSE coastal attribute is included, coastal terminal paths
begin at 1 and internal terminal paths begin at 4 as is implemented by
the NHD stream leveling rules.

## Usage

``` r
add_streamlevel(x, coastal = NULL)

# S3 method for class 'data.frame'
add_streamlevel(x, coastal = NULL)

# S3 method for class 'hy'
add_streamlevel(x, coastal = NULL)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- coastal:

  character attribute name containing a logical flag indicating if a
  given terminal catchment flows to the coast of is an inland sink. If
  no coastal flag is included, all terminal paths are assumed to be
  coastal.

## Value

data,frame containing added `stream_level` attribute

## Details

Required attributes: `levelpath`, `dn_levelpath`

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

x <- add_toids(x)

x <- dplyr::rename(x, orig_stream_level = StreamLeve)

y <- add_streamlevel(x)
#> Warning: Outlets don't follow hydroloom convention of 0 or '', not fixing.
#> Warning: no outlet found -- will start from outlets that go no where.

plot(sf::st_geometry(y), lwd = y$stream_level, col = "blue")


x$coastal <- rep(FALSE, nrow(x))

y <- add_streamlevel(x, coastal = "coastal")
#> Warning: Outlets don't follow hydroloom convention of 0 or '', not fixing.
#> Warning: no outlet found -- will start from outlets that go no where.

unique(y$stream_level)
#> [1]  4 NA  5  6  7  8

x$coastal[!x$Hydroseq == min(x$Hydroseq)] <- TRUE

y <- add_streamlevel(x)
#> Warning: Outlets don't follow hydroloom convention of 0 or '', not fixing.
#> Warning: no outlet found -- will start from outlets that go no where.

unique(y$stream_level)
#> [1]  1 NA  2  3  4  5
```
