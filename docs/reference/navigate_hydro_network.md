# Navigate Hydro Network

Navigates a network of connected catchments using NHDPlus style network
attributes.

## Usage

``` r
navigate_hydro_network(x, start, mode, distance = NULL)

# S3 method for class 'data.frame'
navigate_hydro_network(x, start, mode, distance = NULL)

# S3 method for class 'hy'
navigate_hydro_network(x, start, mode, distance = NULL)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

- start:

  character or numeric to match identifier attribute. The starting
  catchment is included.

- mode:

  character chosen from c(UM, DM, UT, DD) or equivalently c(upmain,
  downmain, up, down).

  1.  UM / upmain: upstream mainstem

  2.  DM / downmain: downstream mainstem

  3.  UT / up: upstream with tributaries

  4.  DD / down: downstream with diversions

- distance:

  numeric distance in km to limit navigation. The first catchment that
  exceeds the provided distance is included.

## Value

vector of identifiers found along navigation

## Details

if only `mode` is supplied, require network attributes are displayed.

NOTE: for "Upstream with tributaries" navigation, if a tributary
emanates from a diversion and is the minor path downstream of that
diversion, it will be included. This can have a very large impact when a
diversion between two large river systems. To strictly follow the
dendritic network, set the "dn_minor_topo_sort" attribute to all 0 in x.

## Examples

``` r
plot_fun <- function(x, s, n) {
  plot(sf::st_geometry(x), col = "grey")
  plot(sf::st_geometry(x[x$id %in% n, ]), add = TRUE)
  plot(sf::st_geometry(x[x$id %in% s, ]), col = "red", lwd = 3, add = TRUE)
}

x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))

start <- 8891126
dm <- navigate_hydro_network(x, start, "DM")

plot_fun(x, start, dm)


dd <- navigate_hydro_network(x, start, "DD")

plot_fun(x, start, dd)


start <- 8894356

um <- navigate_hydro_network(x, start, "UM")

plot_fun(x, start, um)


ut <- navigate_hydro_network(x, start, "UT")

plot_fun(x, start, ut)

```
