# Declare containment between domains

Records that one domain in an existing `domain_decomposition` is
contained by another. In HY_Features terms a domain corresponds to a
catchment aggregate – a set of catchments grouped together that is
itself a catchment. Declaring containment says one aggregate (typically
an endorheic basin, a drainage-divide remnant, or any other isolated
component) belongs inside another. The relationship is supplied by the
caller, not detected:
[`decompose_network()`](https://doi-usgs.github.io/hydroloom/reference/decompose_network.md)
partitions disconnected components into independent basins, and this
function tells the package which of those basins are to be treated as
contained.

## Usage

``` r
set_containment(decomposition, contained, containing)
```

## Arguments

- decomposition:

  object of class `domain_decomposition`.

- contained:

  character. One or more domain ids (keys of `decomposition$domains`) to
  mark as contained.

- containing:

  character. The domain ids each `contained` belongs inside. Length 1
  (recycled) or the same length as `contained`.

## Value

the updated `domain_decomposition`. Errors if any id is unknown, if a
domain would be contained by itself, or if the resulting decomposition
fails
[`validate_decomposition()`](https://doi-usgs.github.io/hydroloom/reference/validate_decomposition.md).

## Details

[`recompose()`](https://doi-usgs.github.io/hydroloom/reference/recompose.md)
applies `containing_domain_id` when called with
`containment = "accumulate"`: the contained basin's accumulated value at
its outlet is added at the containing domain's outlet (the
most-downstream row of the containing domain's segment of the extensive
network) and routed downstream from there through the containing basin's
extensive network. The default `containment = "ignore"` leaves a
contained basin's accumulated value at its own outlet, which is correct
for a true endorheic basin.
[`get_domain_graph()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_graph.md)
surfaces every non-NA `containing_domain_id` as one row in its returned
edge list, with `nexus_id = NA` and `relation_type = "contained"`.
Containment does not appear in `nexus_registry` because no flow crosses
a hydro nexus between the two domains.

Containment is transitive: if A is contained by B and B is contained by
C,
[`recompose()`](https://doi-usgs.github.io/hydroloom/reference/recompose.md)
processes A before B and B before C so that C ends up carrying the
combined contributions of all three. The `containing_domain_id` slot
itself is scalar – one direct container per domain. A domain may not be
contained by itself, and the sequence of containers may not loop back on
itself;
[`validate_decomposition()`](https://doi-usgs.github.io/hydroloom/reference/validate_decomposition.md)
catches both.

Multiple pairs may be set in one call: `contained` and `containing` must
be the same length, or `containing` may be length 1 and is then recycled
across all `contained` ids.

## See also

[domain_decomposition](https://doi-usgs.github.io/hydroloom/reference/domain_decomposition.md),
[`decompose_network()`](https://doi-usgs.github.io/hydroloom/reference/decompose_network.md),
[`validate_decomposition()`](https://doi-usgs.github.io/hydroloom/reference/validate_decomposition.md),
[`get_domain_graph()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_graph.md),
[`recompose()`](https://doi-usgs.github.io/hydroloom/reference/recompose.md).

## Examples

``` r
src <- hy(data.frame(
  id = 1:5, toid = c(2L, 3L, 0L, 5L, 0L),
  topo_sort = c(3L, 2L, 1L, 2L, 1L),
  levelpath = c(1L, 1L, 1L, 2L, 2L),
  levelpath_outlet_id = c(3L, 3L, 3L, 5L, 5L)))

d <- structure(
  list(
    domains = list(
      T1 = hy_domain("T1", "n_t1", character(0), NA_character_,
        hy(src[1:3, ])),
      E1 = hy_domain("E1", "n_e1", character(0), NA_character_,
        hy(src[4:5, ]))),
    domain_connectivity = list(),
    overrides = NULL,
    catchment_domain_index = c(setNames(rep("T1", 3), 1:3),
      setNames(rep("E1", 2), 4:5)),
    nexus_registry = data.frame(
      nexus_id = c("n_t1", "n_e1"),
      from_domain_id = c("T1", "E1"),
      to_domain_id = c(NA_character_, NA_character_),
      stringsAsFactors = FALSE),
    source_network = src),
  class = "domain_decomposition")

d <- set_containment(d, contained = "E1", containing = "T1")

get_domain_graph(d, relations = "contained")
#> # hydroloom dendritic edge list (self-referencing): 1 features
#>   id toid nexus_id relation_type
#> 1 E1   T1     <NA>     contained
```
