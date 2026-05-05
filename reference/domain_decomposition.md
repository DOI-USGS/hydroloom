# Domain decomposition object

A `domain_decomposition` is the wrapper object returned by
[`decompose_network()`](https://doi-usgs.github.io/hydroloom/reference/decompose_network.md).
It bundles a list of
[`hy_domain()`](https://doi-usgs.github.io/hydroloom/reference/hy_domain.md)
objects with the basin-level extensive network overlays and the nexus
metadata that recomposition needs.

## Details

The object is a plain S3 list with six slots:

- `domains`:

  named list of `hy_domain` objects, one per sub-network.

- `domain_connectivity`:

  named list of `hy_leveled` overlays keyed by basin id. Each overlay is
  the basin's *extensive network* — a `hy_leveled` view of the
  connecting flowlines with `toid`s intact except at the basin outlet,
  which carries the reserved outlet `toid` value. Sub-threshold basins
  have no overlay.

- `overrides`:

  non-dendritic inter-domain transfer table, or `NULL`.

- `catchment_domain_index`:

  named character vector mapping each catchment id to its domain id.

- `nexus_registry`:

  synthetic nexus identifiers and the domains they connect.

- `source_network`:

  the original enriched input network.

Inter-domain topology is not stored as a slot;
[`get_domain_graph()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_graph.md)
is the canonical derived accessor and rebuilds the edge list from
`nexus_registry` on demand.

## See also

[`decompose_network()`](https://doi-usgs.github.io/hydroloom/reference/decompose_network.md)
for construction,
[`hy_domain()`](https://doi-usgs.github.io/hydroloom/reference/hy_domain.md)
for the per-domain object,
[`validate_decomposition()`](https://doi-usgs.github.io/hydroloom/reference/validate_decomposition.md)
for structural checks,
[`get_domain_graph()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_graph.md)
and
[`get_domain_for_catchment()`](https://doi-usgs.github.io/hydroloom/reference/get_domain_for_catchment.md)
for accessors,
[`print.domain_decomposition()`](https://doi-usgs.github.io/hydroloom/reference/print.domain_decomposition.md)
for the print method.
