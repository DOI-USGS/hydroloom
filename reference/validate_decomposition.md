# Validate a domain decomposition

Runs structural checks against a `domain_decomposition` object and
returns a list of `valid` (logical) and `issues` (character vector of
human-readable problem descriptions).

## Usage

``` r
validate_decomposition(decomposition)
```

## Arguments

- decomposition:

  object of class `domain_decomposition`.

## Value

list with elements `valid` (logical scalar) and `issues` (character
vector — empty when `valid` is TRUE).

## Details

Structural checks, run in order:

1.  **Outlet count** — each basin's `domain_connectivity` overlay
    resolves to exactly one outlet sub-network via
    [`sort_network()`](https://doi-usgs.github.io/hydroloom/reference/sort_network.md)
    with `split = TRUE`. Domains may have multiple outlets by design and
    are not checked.

2.  **Coverage / partition** — every `source_network` id appears in
    exactly one domain's `catchments` slot.

3.  **Connectivity membership** — each domain's rows that carry the
    reserved outlet `toid` value (other than genuine basin outlets,
    whose `source_network` `toid` carries the same reserved value)
    appear, with `toid`s intact, in some basin's `domain_connectivity`
    overlay. See
    [`hy_domain()`](https://doi-usgs.github.io/hydroloom/reference/hy_domain.md)
    for the dual-ownership rule behind this duplication.

4.  **Inter-domain cycle** — the derived domain graph
    ([`get_domain_graph()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_graph.md)
    with `relations = "flow"`) is acyclic; checked by delegating to
    [`check_hy_graph()`](https://doi-usgs.github.io/hydroloom/reference/check_hy_graph.md).

5.  **Nexus existence** — every `nexus_id` referenced by a flow row in
    the derived inter-domain edge list is registered in
    `nexus_registry`. Containment relationships are not checked here
    because no flow crosses a hydro nexus between contained and
    containing domains.

6.  **Containment resolution** — every non-NA `containing_domain_id`
    resolves to a key of `decomposition$domains`, no domain is contained
    by itself, and the sequence of containers (A inside B inside C, ...)
    does not loop back on itself. The acyclic check runs
    [`check_hy_graph()`](https://doi-usgs.github.io/hydroloom/reference/check_hy_graph.md)
    on the containment relationships pulled from each domain.

7.  **Override references** — every row in `overrides` (when present)
    names a known source/sink domain via `id`/`toid` and a known
    source/sink nexus via `source_nexus_id`/`sink_nexus_id`.

## See also

[domain_decomposition](https://doi-usgs.github.io/hydroloom/reference/domain_decomposition.md)
for the object's slots,
[`hy_domain()`](https://doi-usgs.github.io/hydroloom/reference/hy_domain.md)
for the per-domain object,
[`decompose_network()`](https://doi-usgs.github.io/hydroloom/reference/decompose_network.md).

## Examples

``` r
lev <- hy(data.frame(
  id = 1:3, toid = c(2L, 3L, 0L),
  topo_sort = 3:1, levelpath = c(1L, 1L, 1L),
  levelpath_outlet_id = c(3L, 3L, 3L)))

domain <- hy_domain(
  domain_id = "T1",
  outlet_nexus_id = "n_out",
  inlet_nexus_ids = character(0),
  containing_domain_id = NA_character_,
  catchments = lev)

d <- structure(
  list(
    domains = list(T1 = domain),
    domain_connectivity = list(),
    overrides = NULL,
    catchment_domain_index = setNames(rep("T1", 3), c("1", "2", "3")),
    nexus_registry = data.frame(nexus_id = "n_out"),
    source_network = lev),
  class = "domain_decomposition")

validate_decomposition(d)
#> $valid
#> [1] TRUE
#> 
#> $issues
#> character(0)
#> 
```
