# Construct a hy_domain

A `hy_domain` is the unit of independent computation in a network
decomposition. Every `hy_domain` is compact — a partitioned piece of a
drainage basin that bundles the segment's lateral tributaries with the
extensive network rows flowing through the segment (the latter in their
decomposed form, with reserved-`toid` values that mark each as a local
outlet). The basin's *extensive network* — a `hy_leveled` view of the
connecting flowlines with `toid`s intact — is stored separately in
`domain_decomposition$domain_connectivity`.

## Usage

``` r
hy_domain(
  domain_id,
  outlet_nexus_id,
  inlet_nexus_ids,
  containing_domain_id,
  catchments
)
```

## Arguments

- domain_id:

  character(1). Unique identifier for this domain.

- outlet_nexus_id:

  character(1). Identifier of the outlet hydro nexus where this domain
  discharges.

- inlet_nexus_ids:

  character. Hydro nexus ids where upstream domains feed into this one.
  `character(0)` for leaf domains; populated for stem and root domains.

- containing_domain_id:

  character(1). For contained domains, the id of the enclosing domain;
  `NA_character_` if not contained. Covers endorheic basins as well as
  drainage-divide remnants and any other case where a domain is to be
  treated as belonging inside another. Declared post-decomposition via
  [`set_containment()`](https://doi-usgs.github.io/hydroloom/reference/set_containment.md).

- catchments:

  hydroloom object carrying the domain's catchment network. Must be
  `hy_topo`, `hy_leveled`, or `hy_flownetwork`.

## Value

object of class `hy_domain` — a list with the five named slots above.

## Details

The `catchments` slot may be `hy_topo` (or `hy_leveled`) for dendritic
internal connectivity, or `hy_flownetwork` to preserve internal
divergences.

**Compact and extensive duality.** The extensive network rows in a
domain's `catchments` carry the reserved outlet `toid` value (the value
`get_outlet_value()` returns), so each becomes a local outlet of its own
contributing sub-basin. Those same ids appear, with `toid`s intact, in
the basin's `domain_connectivity[[basin_id]]` overlay so the basin's
extensive network stays addressable end-to-end. With the two ownerships
kept distinct, per-domain processing runs in parallel and recomposition
lands in a single pass. Extensive network membership inside a domain is
recoverable by intersecting the domain's `catchments$id` with the parent
basin's connectivity-overlay `id`.

**Decomposed and recomposed modes.** The decomposed form is the default
mode: each extensive network row is an outlet of its own contributing
sub-basin, so a single
[accumulate_downstream()](https://doi-usgs.github.io/hydroloom/reference/accumulate_downstream.md)
call on the domain's catchments produces, for every extensive network
catchment in the segment, the locally-incremental drainage area (or any
other accumulable) that belongs there. To switch to recomposed mode,
join `source_network[, c("id", "toid")]` onto the domain's catchments
where `toid` carries the reserved outlet value, replacing it with the
original `toid`; the segment is a connected sub-basin again. See
[`vignette("domain_decomposition")`](https://doi-usgs.github.io/hydroloom/articles/domain_decomposition.md)
for the full framing.

The constructor returns a plain S3 list. Slot mutation after
construction is permitted; downstream invariants are re-checked by
[`validate_decomposition()`](https://doi-usgs.github.io/hydroloom/reference/validate_decomposition.md).

## Examples

``` r
lev <- hy(data.frame(
  id = 1:3, toid = c(2L, 3L, 0L),
  topo_sort = 3:1, levelpath = c(1L, 1L, 1L),
  levelpath_outlet_id = c(3L, 3L, 3L)))

hy_domain(
  domain_id = "T1",
  outlet_nexus_id = "n_out",
  inlet_nexus_ids = character(0),
  containing_domain_id = NA_character_,
  catchments = lev)
#> $domain_id
#> [1] "T1"
#> 
#> $outlet_nexus_id
#> [1] "n_out"
#> 
#> $inlet_nexus_ids
#> character(0)
#> 
#> $containing_domain_id
#> [1] NA
#> 
#> $catchments
#> # hydroloom dendritic leveled edge list (self-referencing): 3 features, sorted
#> # A tibble: 3 × 5
#>      id  toid topo_sort levelpath levelpath_outlet_id
#>   <int> <int>     <int>     <int>               <int>
#> 1     1     2         3         1                   3
#> 2     2     3         2         1                   3
#> 3     3     0         1         1                   3
#> 
#> attr(,"class")
#> [1] "hy_domain"
```
