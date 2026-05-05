# Decompose a network into domains

Partitions a hydrologic network into `hy_domain` objects for independent
or parallel computation. Each drainage basin is split into domains along
its extensive network; the extensive network itself is returned
separately as the basin's *domain_connectivity* overlay. See
[`hy_domain()`](https://doi-usgs.github.io/hydroloom/reference/hy_domain.md)
and
[domain_decomposition](https://doi-usgs.github.io/hydroloom/reference/domain_decomposition.md)
for details.

## Usage

``` r
decompose_network(
  x,
  stem_metric = "drainage_area",
  stem_threshold = NULL,
  stem_levelpaths = NULL,
  domain_breaks = NULL,
  overrides = NULL
)
```

## Arguments

- x:

  `hy_leveled` object (dendritic network already enriched with
  levelpaths).

- stem_metric:

  character. Metric evaluated at each levelpath outlet to decide stem
  eligibility. `"drainage_area"` reads `total_da_sqkm`; `"arbolate_sum"`
  reads `arbolate_sum`. Only consulted when `stem_threshold` is
  non-NULL.

- stem_threshold:

  numeric scalar or `NULL`. Value of `stem_metric` at a levelpath outlet
  above which the levelpath is a stem candidate. `NULL` (default) falls
  back to one stem per drainage basin (the basin's outlet levelpath).
  When non-NULL, the input must carry a `stream_calculator` column; call
  [`add_streamorder()`](https://doi-usgs.github.io/hydroloom/reference/add_streamorder.md)
  to add it if the source data does not already provide one (NHDPlus
  `StreamCalc`, canonicalized by
  [`hy()`](https://doi-usgs.github.io/hydroloom/reference/hy.md),
  counts).

- stem_levelpaths:

  vector of levelpath ids or `NULL`. When non-NULL, bypasses the
  threshold rule and forces these levelpaths to be stems (the basin's
  terminal-outlet levelpath is always unioned in). Every id must exist
  in `x$levelpath`.

- domain_breaks:

  vector of catchment ids or `NULL`. When non-NULL, these stem catchment
  ids define where the stem network is segmented into domain groups.
  Each break id becomes a segment terminal in addition to the
  auto-detected confluences and outlets. Breaks that are not stem
  catchments in a given basin are silently ignored. When `NULL`
  (default), segmentation is determined automatically from stem
  confluences and (if available) bridge flowlines.

- overrides:

  data.frame. Non-dendritic inter-domain transfer table; pass-through to
  `decomposition$overrides`.

## Value

a
[domain_decomposition](https://doi-usgs.github.io/hydroloom/reference/domain_decomposition.md)
object.

## Details

**Input.** Input must be `hy_leveled` – the network must already carry
`levelpath`, `levelpath_outlet_id`, and `topo_sort` columns. Call
[`add_levelpaths()`](https://doi-usgs.github.io/hydroloom/reference/add_levelpaths.md)
to add. Non-dendritic sources (`hy_flownetwork`) are not supported at
this time.

**Extensive network selection.** Each drainage basin's extensive network
is selected from `stem_metric`, `stem_threshold`, and `stem_levelpaths`
(see arguments). The basin's extensive connectivity (the regional
relationship that ties its drainage pieces together) is materialized as
the basin's `domain_connectivity[[basin_id]]` overlay — a `hy_leveled`
view of the extensive network. The `stem_*` naming reflects the
cross-scale view: a basin's outlet levelpath is a trunk, but with a
threshold or explicit override the selection can pull in branches as
well — every selected path is a *stem* in the cross-scale sense (trunks
and branches are both stems).

**Containment.** This function does not detect containment. A drainage
basin that the caller wants treated as belonging inside another –
typically an endorheic basin or a drainage-divide remnant – is
partitioned here as an independent basin like any other. After
decomposition, the caller declares the relationship with
[`set_containment()`](https://doi-usgs.github.io/hydroloom/reference/set_containment.md);
[`recompose()`](https://doi-usgs.github.io/hydroloom/reference/recompose.md)
applies it when called with `containment = "accumulate"`. The
relationship is recorded on the contained domain's
`containing_domain_id` slot and surfaced by
[`get_domain_graph()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_graph.md)
with `relations = "contained"`. It does not appear in `nexus_registry`
because no flow crosses a hydro nexus between the two basins.

## See also

[domain_decomposition](https://doi-usgs.github.io/hydroloom/reference/domain_decomposition.md)
for the returned object's slots,
[`hy_domain()`](https://doi-usgs.github.io/hydroloom/reference/hy_domain.md)
for the per-domain object,
[`validate_decomposition()`](https://doi-usgs.github.io/hydroloom/reference/validate_decomposition.md),
[`get_domain_graph()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_graph.md),
[`get_domain_for_catchment()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_for_catchment.md).

## Examples

``` r
g <- sf::read_sf(system.file("extdata/walker.gpkg", package = "hydroloom"))

h <- hy(g) |>
  add_toids() |>
  add_levelpaths(name_attribute = "GNIS_ID",
    weight_attribute = "arbolate_sum")

d <- decompose_network(h)

length(d$domains)
#> [1] 1
```
