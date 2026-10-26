hydroloom (development version)
==========

- The attribute vocabulary (hydroloom names, their definitions, and the
  input column names that rename to them) is now read from
  `inst/extdata/hy_vocab.json` rather than defined in R code.
  `hydroloom_names()`, `hydroloom_name_definitions`, and `align_names()`
  are served from that file. jsonlite is now in Imports.
- The `outletID` alias for `levelpath_outlet_id` is now `outletid`. Input
  names are lowercased before matching, so the camelCase entry never
  matched.
- `hy()` now fixes the type of identifier columns (`id`, `toid`,
  `fromnode`, `tonode`, `levelpath`, and the other identifier names) on
  ingest. A whole-valued `double` or `integer64` column becomes `integer`
  when every value fits in 32 bits and `character` (the exact decimal
  string) otherwise, so NHDPlusV2 `COMID` read from a file comes back
  `integer` and NHDPlus HR `NHDPlusID` comes back `character`. `id` and
  `toid` share one type, as do `fromnode` and `tonode`; an integer `id`
  beside a character `toid` no longer errors. The outlet `toid` is `0L`
  for integer identifiers, and `add_toids()` returns `toid` in the type of
  `id`. Functions called on a `data.frame` return these types under the
  input column names.
- `sort_network()` and `add_topo_sort()` now return one order for a
  network whatever the order of its input rows. Flowlines that flow to
  the same flowline, and separate outlet networks, are placed in
  ascending order of their identifiers compared as text (so `100` comes
  before `2`); rows that share an `id` are ordered by `toid` the same
  way. Previously the order followed the input rows. Row order and
  `topo_sort` values change on most networks with a confluence or more
  than one outlet, and so do values built from `topo_sort`: `levelpath`
  from `add_levelpaths()`, and `add_pfafstetter()` codes where two
  tributaries enter the same flowline.

hydroloom 1.3.0
==========

Distances from confluences and subpaths along identified mainstems. Four
new functions place and recover positions relative to the confluences of
identified mainstems (`aggregate_id` as a mainstem URI), working kilometer
values along each aggregate flowpath axis, built from flowline lengths
rather than coordinates. A mainstem is one kind of aggregate flowpath (the
flowlines that share an `aggregate_id` and link end to end), so any
`aggregate_id` whose flowlines form one unbranched path works the same way.

- `get_confluences()` returns each confluence of two identified mainstems as
  a hydro location on the receiving mainstem just below the tributary.
- `add_hydro_location_distance()` records the signed distance along a mainstem
  from the nearest hydro location; negative toward the outlet, positive toward
  the head.
- `index_hydro_location_distance()` is the inverse, mapping a distance from a
  hydro location back to a flowline `id` and `id_measure`.
- `get_subpath()` returns the subpath between two positions on one aggregate
  flowpath (such as an identified mainstem) as ordered flowline pieces, with
  piece geometry cut by `lwgeom::st_linesubstring()` when the input is `sf`
  and `lwgeom` is installed.
- `index_points_to_lines()` returns `id_measure` on every call and accepts
  `aggregate_ids` to constrain a point to one mainstem's flowlines.
- `hy_capabilities()` reports the four new functions. A new website article,
  "Distances from confluences along mainstems", walks a worked example.

hydroloom 1.2.2
==========

Correctness fix on `make_node_topology()` for dendritic networks. Outlet
`tonode`s were seeded from `max(non-outlet tonode) + 1`. When the highest
`fromnode`s belonged to outlets (whose `tonode` is NA and excluded from that
max), the seed fell inside the `fromnode` range and a terminal flowline's
`tonode` collided with another feature's `fromnode`, fabricating a connection
that `add_toids()` would then rediscover. Outlet `tonode`s are now seeded above
every assigned node (`max(c(fromnode, tonode))`), so terminals keep independent
pendant nodes. Observed on the Alaska reference network (2 of 2,122 outlets).

Performance fix on `accumulate_downstream()`. No user-facing behavior change; output is identical.

Correctness fix on `add_pathlength()` for networks with character ids. The
terminal test was `toid != 0`, which recognizes only the numeric outlet
sentinel. With character ids the sentinel is `""`, the comparison is made as
strings and is `TRUE`, so terminals took the accumulating branch, indexed with
`NA`, and the `NA` propagated up every path above them — the whole
`pathlength_km` column came back `NA`. Now uses `get_outlet_value()`, as
`add_streamorder()` and `add_levelpaths()` do. Networks with numeric ids are
unaffected.

`nhdplusTools` has been renamed `hydrogeofetch`. Examples, tests, and articles that pull sample data now suggest and source from `hydrogeofetch`, and package URLs point at the new repository.

hydroloom 1.2.1
==========

Maintenance release completing the USGS official software release
review (#72). No package code changed.

- Cleaned up vignette messages noted in review (#96).
- Web service walkthroughs are now website-only articles at
  https://doi-usgs.github.io/hydroloom/; `hydroloom.Rmd` is the only
  installed vignette.
- Citation updated to the software release DOI,
  https://doi.org/10.5066/P16RS3UZ.

hydroloom 1.2.0
==========

This release introduces an S3 class hierarchy (`hy_topo`,
`hy_leveled`, `hy_node`, `hy_flownetwork`) that lets hydroloom
validate input at dispatch time and emit guided error messages.
Existing code that passes `data.frame` or `hy` objects keeps
working -- the new classes are assigned automatically by `hy()`
and by producer functions. See `?hy_topo`, `?hy_leveled`,
`?hy_node`, and `?hy_flownetwork` for representation patterns,
required columns, and conversion paths;
the non-dendritic networks and network navigation articles at
https://doi-usgs.github.io/hydroloom/ provide end-to-end walkthroughs. Package developers should note
that returned objects now carry subclass attributes which are
stripped by standard dplyr operations.

- Outlet detection is explicit (#85): a row is an outlet when its
  `toid` is not in `id`. Canonical reserved values (`0` / `""`),
  `NA`, implicit absence, foreign reserved values, and
  unique-per-outlet ids are all accepted. Calls that previously
  errored on `NA` or orphan `toid` now succeed; warnings are
  narrower (`check_hy_outlets()` only on type mismatch,
  `sort_network()` only on zero outlets).
- S3 class hierarchy (#73). Producer functions stamp output
  classes (`add_toids()`, `sort_network()`, `add_levelpaths()`,
  `make_node_topology()`, `to_flownetwork()`); dispatch is on
  subclass with guided errors for wrong input. `hy_flownetwork`
  is a separate junction table -- it does not inherit from `hy`.
- New exported helpers: `hy_network_type()`, `is_dendritic()`,
  `hy_capabilities()`. Print methods for `hy_topo`, `hy_node`,
  `hy_flownetwork`.
- `hy()` gains `add_topo` to auto-build `toid` from
  `fromnode`/`tonode`.
- `add_toids(return_dendritic = FALSE)` is deprecated; use
  `to_flownetwork()`.
- `add_levelpaths()` is faster (data.table conversion).
- New `check_valid()` for sf/sfc geometry repair, including
  `GEOMETRYCOLLECTION` artifacts from `sf::st_make_valid()`. See
  `?check_valid`.
- New `dissolve_polygons()` for unioning catchment or HUC-style
  polygon coverages, with optional grouping and interior-hole
  fill. Uses `geos` (added to Suggests) for accelerated unions
  when installed. See `?dissolve_polygons`.
- Fix `get_bridge_flowlines()` on networks with independent
  terminals -- `make_nondendritic_topology()` had collapsed all
  rows carrying the reserved outlet value into one synthetic
  node, misclassifying bridges and exhausting memory on
  continental networks.
- Fix `make_to_dt()` dendritic branch on tibble input.
- Welcome new contributor Andrew Psoras.
- **Deprecation notice:** A future release will require that
  `hy_topo` objects have unique `id` values (one row per
  catchment). Non-dendritic connectivity with duplicated ids in a
  toid-based edge list will need to be represented as
  `hy_flownetwork` (via `to_flownetwork()`). Developers passing
  non-dendritic toid tables through hydroloom functions should
  migrate to `to_flownetwork()` or `make_node_topology()`.

hydroloom 1.1.3
==========

Update test tolerances for failing Fedora CRAN tests

hydroloom 1.1.2
==========

Hydroloom 1.1.2 introduces new functionality in `accumulate_downstream()`, reworks the `make_index_ids()`, 
and deprecates `make_fromids()` and `format_index_ids()` for clarity of package function. Deprecated functions
will be removed in the next major version release.

- `subset_network()` has been added to support subsetting networks to include all diversions that emanate from the basin. -- #60
- `accumulate_downstream()` now supports "total upstream" and "divergence routed" accumulation. -- #17
- `make_index_ids()` has been rewritten. It now uses four modes ("to", "from", and "both"). 
- `make_fromids()` is deprecated in favor of `make_index_ids()` with mode = "from".
- `format_index_ids()` is deprecated. The *_list element of `make_index_ids()` can be unnested instead.

hydroloom 1.1.1
==========

- fix bug with sort_network when duplicate entries are in the extended attributes -- #52
- add specific id search to index_points_to_lines for #24
- error handling #49

hydroloom 1.1.0
==========

- Add new vignette for network navigation `vignette("network_navigation")`
- Add support for upmain and downmain navigation in `navigate_network_dfs()`
- Add support for upmain and downmain in `make_index_ids()` and `make_fromids()`.
- Add function to create a "flownetwork" representation of the network with a `to_flownetwork` function.
- Improved handling / fix bugs with edge cases in `add_toids()`
- Improved error conditions when missing suggested packages.
- `navigate_hydro_network()` will now navigate from a diverted path to a main path where it previously only followed traditional tributaries..

hydroloom 1.0.1 and 1.0.2
==========

- Add checks if nhdplusTools is available.

hydroloom 1.0.0
==========

- Initialized new project
- Create basic `hy` s3 object handling `hy()`
- Ensure tibble is used throughout
- Support generic depth first search navigation `navigate_network_dfs()`
- Complete migration of nhdplusTools functions to hydroloom. #1
- Support sorting non-dendritic network in `sort_network()`
- Implemented non-dendritic network support in `add_toids()`
- Implemented complete stream order / stream calculator attribute `add_streamorder()` and `add_streamcalculator()` https://github.com/DOI-USGS/hydrogeofetch/issues/188
- Support for both numeric and character identifiers #2
- Support for NHDPlus and NHD 24k naming schemes #5 `hydroloom_names()`
- Support for linear indexing for lines that do not have measure attributes #6 `index_points_to_lines()`
- Implemented creation of an attribute topology from a geometric network `make_attribute_topology()`
- Added vignette showing how to work with the NHD flow table. https://github.com/DOI-USGS/hydrogeofetch/issues/340 `vignette("flow-table")`
- Implement method to add divergence attribute to a non-dendritic network. `add_divergence()`
- Implement basic graph and sophisticate loop detection algorithm #7 and #9. `check_hy_graph()`
