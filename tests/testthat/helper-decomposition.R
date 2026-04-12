# Helpers for the decomposition test layers (0 through 9).
#
# This file is loaded automatically by testthat for every test in
# tests/testthat/. It is the only shared state across the
# test_decomposition_*.R files.
#
# Helpers fall into three groups:
#   1. Loaders / enrichment chains that use only existing hydroloom
#      functions and so work today.
#   2. Skip-if-pending guards for Layers 1-9 that exercise the
#      not-yet-implemented decomposition API.
#   3. Reusable assertion helpers that the layer test files call.
#
# When the decomposition implementation lands, replace the
# `decomposition_pending()` calls inside individual layer files with
# real fixture construction. The structural test files exist now so
# that authoring the implementation is purely a matter of removing
# skips one assertion at a time.

# ---- shared loaders -----------------------------------------------------

#' Load walker.gpkg as a simple-features data.frame.
#' @returns sf data.frame.
load_walker <- function() {

  sf::read_sf(system.file("extdata/walker.gpkg", package = "hydroloom"))

}

#' Load new_hope.gpkg as a simple-features data.frame.
#' @returns sf data.frame.
load_new_hope <- function() {

  sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

}

#' Read the flowlines layer from a (possibly multi-layer) gpkg.
#'
#' Subset_nhdplus output gpkgs contain multiple layers (NHDFlowline_Network,
#' CatchmentSP, NHDWaterbody, ...). sf::read_sf() defaults to the first
#' layer with a warning, which often picks the wrong one. This helper
#' looks for a flowline-shaped layer by name and falls back to the
#' default if no obvious candidate exists.
#'
#' @param path character. Path to the geopackage.
#' @returns sf data.frame.
read_flowlines_layer <- function(path) {

  layers <- sf::st_layers(path)$name

  candidates <- c("NHDFlowline_Network", "NHDFlowline", "Flowline",
    "flowlines", "nhdflowline_network", "nhdflowline")

  match <- intersect(candidates, layers)

  if (length(match) > 0) {
    return(sf::read_sf(path, layer = match[1]))
  }

  sf::read_sf(path)

}

#' Load a tests/testthat/data/*.rds fixture by short name.
#' @param name character. e.g. "network", "coastal", "hr", "loop", "loop2".
#' @returns the deserialized object.
load_test_rds <- function(name) {

  # Anchor the pattern: "loop" must not match "loop2".
  pat <- paste0("^", name, "\\.rds$")

  path <- list.files("data", pattern = pat, recursive = TRUE,
    full.names = TRUE)

  if (length(path) == 0)
    testthat::skip(paste0("test fixture ", name, ".rds not found"))

  readRDS(path[[1]])

}

# ---- canonical enrichment chain ----------------------------------------

#' Run the canonical enrichment chain that produces a hy_leveled.
#'
#' Loads a dataset (or accepts one already loaded), classifies it, builds
#' toid from fromnode/tonode, runs add_levelpaths to produce a hy_leveled,
#' and adds streamorder. add_measures is left out by default because it
#' requires an aggregate_id column that not every fixture carries.
#'
#' The name and weight attributes are auto-selected by probing the
#' enriched object for the canonical (`gnis_id`, `arbolate_sum`) form
#' first and falling back to NHDPlusV2 raw names (`GNIS_ID`, `ArbolateSu`).
#' `hy()` only lowercases names that it recognizes as canonical — GNIS
#' isn't in `good_names`, so walker/new_hope keep `GNIS_ID`, while the
#' NLDI/subset service delivers `gnis_id`.
#'
#' @param x sf data.frame, data.frame, or character path/short-name.
#' @param name_attribute character. Column to use as name attribute for
#'   add_levelpaths. Defaults to NULL (auto-detect).
#' @param weight_attribute character. Column to use as weight attribute
#'   for add_levelpaths. Defaults to NULL (auto-detect).
#' @returns hy_leveled object ready for downstream decomposition use.
enrich_for_decomposition <- function(x,
                                     name_attribute = NULL,
                                     weight_attribute = NULL) {

  if (is.character(x) && length(x) == 1) {

    if (file.exists(x)) {
      x <- read_flowlines_layer(x)
    } else {
      x <- load_test_rds(x)
    }

  }

  h <- hydroloom::hy(x)

  # Strip any pre-existing decomposition outputs so add_levelpaths /
  # add_streamorder do not balk at columns they themselves produce.
  # NHDPlusV2 fixtures (e.g. the at-scale gpkg) carry these from the
  # source service. After hy() the column names are canonical, so a
  # single drop list against canonical names is sufficient.
  drop_canonical <- c("stream_order", "stream_calculator", "topo_sort",
    "levelpath", "levelpath_outlet_id", "pathlength_km", "stream_level")

  to_drop <- intersect(drop_canonical, names(h))

  if (length(to_drop) > 0) {
    h[to_drop] <- NULL
  }

  h <- hydroloom::add_toids(h, return_dendritic = TRUE)

  # Auto-detect name/weight columns when the caller did not specify.
  name_attribute <- name_attribute %||%
    pick_first_present(c("gnis_id", "GNIS_ID"), names(h))

  weight_attribute <- weight_attribute %||%
    pick_first_present(c("arbolate_sum", "ArbolateSu"), names(h))

  if (is.null(name_attribute)) {
    testthat::skip(
      "no GNIS-like name column in fixture; skipping enrichment-dependent test")
  }

  if (is.null(weight_attribute)) {
    testthat::skip(
      "no arbolate-like weight column in fixture; skipping enrichment-dependent test")
  }

  h <- hydroloom::add_levelpaths(h,
    name_attribute = name_attribute,
    weight_attribute = weight_attribute)

  h <- hydroloom::add_streamorder(h, status = FALSE)

  h

}

#' Return the first element of `candidates` that is present in `pool`.
#' @noRd
pick_first_present <- function(candidates, pool) {

  hit <- intersect(candidates, pool)

  if (length(hit) == 0) return(NULL)

  hit[1]

}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- pending-implementation guards -------------------------------------

#' Skip the calling test if a decomposition function does not yet exist.
#'
#' Used by Layers 1-9 to keep the test files structurally complete while
#' the decomposition API is being implemented. As `decompose_network`,
#' `hy_domain`, `validate_decomposition`, etc. land, the corresponding
#' guards in the layer files become no-ops.
#'
#' @param fn character. Name(s) of decomposition functions required.
decomposition_pending <- function(fn) {

  missing_fns <- fn[!vapply(fn,
    \(f) exists(f, envir = asNamespace("hydroloom"), inherits = FALSE),
    logical(1))]

  if (length(missing_fns) > 0)
    testthat::skip(paste0("decomposition API not yet implemented: ",
      paste(missing_fns, collapse = ", ")))

}

# ---- reusable assertions on decomposition objects ----------------------

#' Assert that a decomposition's catchments form a partition of the source.
#'
#' Every source catchment id appears in exactly one domain's catchments
#' slot. No orphans, no duplicates.
#'
#' @param decomposition object returned by decompose_network.
#' @param src the un-decomposed network passed in.
assert_partition_coverage <- function(decomposition, src) {

  domain_ids <- unlist(lapply(decomposition$domains,
    \(d) d$catchments$id),
    use.names = FALSE)

  source_ids <- src$id

  testthat::expect_setequal(domain_ids, source_ids)

  testthat::expect_equal(sum(duplicated(domain_ids)), 0L,
    label = "duplicated catchment ids across domains")

}

#' Assert that each domain's catchments slot has exactly one outlet.
#'
#' Uses sort_network(split = TRUE) which adds a terminal_id column —
#' one per outlet sub-network. A valid decomposition domain must contain
#' exactly one terminal_id, i.e. one outlet sub-network.
#'
#' @param decomposition object returned by decompose_network.
assert_one_outlet_per_domain <- function(decomposition) {

  for (d in decomposition$domains) {

    # Compact domains may have multiple outlets (disconnected
    # tributary groups in a trunk-segment-based compact).
    if (d$domain_type != "trunk") next

    catch <- d$catchments

    if (is.null(catch) || nrow(catch) == 0) next

    sorted <- hydroloom::sort_network(catch, split = TRUE)

    testthat::expect_equal(length(unique(sorted$terminal_id)), 1L,
      label = paste0("domain ", d$domain_id, " outlet count"))

  }

}

#' Assert that the inter-domain flow graph is dendritic and acyclic.
#'
#' Calls check_hy_graph and sort_network on
#' get_domain_graph(d, relations = "flow"). Both must succeed.
#'
#' @param decomposition object returned by decompose_network.
assert_dendritic_inter_domain <- function(decomposition) {

  decomposition_pending(c("get_domain_graph"))

  g <- hydroloom::get_domain_graph(decomposition, relations = "flow")

  testthat::expect_true(isTRUE(hydroloom::check_hy_graph(g)),
    label = "inter-domain flow graph is acyclic")

  # sort_network must run without erroring on the domain graph.
  testthat::expect_no_error(hydroloom::sort_network(g))

}

#' Boolean form of assert_dendritic_inter_domain for in-helper use.
#' @param decomposition object returned by decompose_network.
#' @returns logical(1).
domain_graph_is_dag <- function(decomposition) {

  if (!exists("get_domain_graph",
    envir = asNamespace("hydroloom"), inherits = FALSE)) return(NA)

  g <- hydroloom::get_domain_graph(decomposition, relations = "flow")

  res <- try(hydroloom::check_hy_graph(g), silent = TRUE)

  isTRUE(res)

}

# ---- recomposition oracles ---------------------------------------------

#' Mass-balance check: recomposed values match the un-decomposed oracle.
#'
#' Computes accumulate_downstream(src, var) on the un-decomposed network
#' and joins it against recompose(decomposition)$<var>. Both come from
#' the same hydroloom function so they should be bit-identical aside
#' from join order; the default tolerance is correspondingly tight.
#'
#' @param decomposition object returned by decompose_network.
#' @param src the un-decomposed network (as passed to decompose_network).
#' @param var character. Variable to accumulate. Default `da_sqkm` is
#'   the canonical local-area name produced by `hy()`.
#' @param tolerance numeric. Absolute tolerance for expect_equal.
expect_recomposes_to_source <- function(decomposition, src,
                                        var = "da_sqkm",
                                        tolerance = 1e-9) {

  decomposition_pending("recompose")

  oracle <- hydroloom::accumulate_downstream(src, var, quiet = TRUE)

  rec <- hydroloom::recompose(decomposition,
    domain_results = NULL,
    apply_overrides = FALSE,
    check_mass_balance = FALSE)

  ord <- match(src$id, rec$id)

  testthat::expect_false(any(is.na(ord)),
    label = "every source id is present in recomposed output")

  testthat::expect_equal(rec[[var]][ord], oracle, tolerance = tolerance,
    label = paste0("recomposed ", var, " matches accumulate_downstream"))

}

#' Integer-equality recomposition check on stream order.
#' @inheritParams expect_recomposes_to_source
expect_recomposes_streamorder <- function(decomposition, src) {

  decomposition_pending("recompose")

  oracle <- hydroloom::add_streamorder(src, status = FALSE)

  rec <- hydroloom::recompose(decomposition,
    domain_results = NULL,
    apply_overrides = FALSE,
    check_mass_balance = FALSE)

  ord <- match(src$id, rec$id)

  testthat::expect_identical(
    as.integer(rec$stream_order[ord]),
    as.integer(oracle$stream_order),
    label = "recomposed stream order matches add_streamorder")

}

#' Integer-equality recomposition check on levelpath identity.
#' @inheritParams expect_recomposes_to_source
expect_recomposes_levelpath <- function(decomposition, src) {

  decomposition_pending("recompose")

  rec <- hydroloom::recompose(decomposition,
    domain_results = NULL,
    apply_overrides = FALSE,
    check_mass_balance = FALSE)

  ord <- match(src$id, rec$id)

  testthat::expect_identical(
    rec$levelpath[ord],
    src$levelpath,
    label = "recomposed levelpath matches source")

}

# ---- domain-graph wiring helpers ---------------------------------------

#' Unit-value accumulation through the domain graph.
#'
#' Calls accumulate_domains with all-ones input. Returns the value at
#' the global outlet. Used as a topology-wiring check independent of
#' real attribute data.
#'
#' @param decomposition object returned by decompose_network.
#' @param include_contained logical. Whether to traverse contained edges.
#' @returns numeric(1).
unit_accumulation_check <- function(decomposition,
                                    include_contained = FALSE) {

  decomposition_pending("accumulate_domains")

  domain_ids <- names(decomposition$domains)

  values <- setNames(rep(1, length(domain_ids)), domain_ids)

  res <- hydroloom::accumulate_domains(decomposition,
    domain_values = values,
    fun = sum,
    include_contained = include_contained)

  # outlet domain: the one whose to_domain_id is the sentinel
  # (or NA), or — if the helper output is positional — the maximum.
  max(res, na.rm = TRUE)

}

#' Build a unit-value compact_results list for inject_lateral testing.
#'
#' For each compact domain in the decomposition, produce a single
#' "outlet contribution = 1" entry keyed by domain_id. The exact shape
#' of compact_results is design-dependent; this helper assumes a list
#' of numeric scalars and will need updating once the contract firms up.
#'
#' @param decomposition object returned by decompose_network.
#' @param value_per_domain numeric scalar.
#' @returns named list.
synthetic_compact_results <- function(decomposition, value_per_domain = 1) {

  domain_ids <- vapply(decomposition$domains, \(d) d$domain_id, character(1))

  types <- vapply(decomposition$domains, \(d) d$domain_type, character(1))

  compacts <- domain_ids[types == "compact"]

  setNames(as.list(rep(value_per_domain, length(compacts))), compacts)

}

#' Look up the trunk catchment ids that should receive lateral inflow
#' for a given trunk, according to the nexus registry.
#'
#' @param decomposition object returned by decompose_network.
#' @param trunk_id character. Domain id of the trunk.
#' @returns vector of catchment ids.
trunk_lateral_recipients <- function(decomposition, trunk_id) {

  decomposition_pending("get_nexus_registry")

  reg <- hydroloom::get_nexus_registry(decomposition)

  reg <- reg[reg$to_domain_id == trunk_id, , drop = FALSE]

  unique(reg$trunk_catchment_id)

}

# ---- structural fixtures (Layer 1) -------------------------------------

#' Build a minimal hy_domain for constructor tests.
#'
#' Centralizes the 8-keyword `hy_domain()` invocation used across Layer 1
#' so individual tests only need to override the field that's relevant
#' to what they're asserting. Defaults wire a 1-trunk decomposition with
#' no inlets and no containment.
#'
#' @param catchments hy_topo / hy_leveled / hy_flownetwork to wrap.
#' @param domain_type "trunk" or "compact".
#' @param ... fields to override (domain_id, outlet_nexus_id, etc.).
#' @returns hy_domain object (whatever the constructor returns).
make_minimal_hy_domain <- function(catchments,
                                   domain_type = "trunk",
                                   ...) {

  defaults <- list(
    domain_id = "T1",
    domain_type = domain_type,
    outlet_nexus_id = "n_out",
    inlet_nexus_ids = character(0),
    trunk_domain_id = NA_character_,
    containing_domain_id = NA_character_,
    catchments = catchments,
    topo_sort_offset = 0L
  )

  args <- modifyList(defaults, list(...))

  do.call(hydroloom::hy_domain, args)

}

#' Build a minimal hand-rolled `domain_decomposition` for validator tests.
#'
#' Layer 1 needs to feed `validate_decomposition()` a structurally
#' complete decomposition with one invariant intentionally broken at a
#' time. This helper produces a baseline valid object; tests mutate one
#' field, pass it to `validate_decomposition`, and assert it's flagged.
#'
#' Built as a plain `structure(list(...), class = "domain_decomposition")`
#' on purpose: Layer 1 tests should not depend on a (not-yet-existing)
#' real constructor, only on `validate_decomposition`'s tolerance for
#' the documented shape.
#'
#' @param domains named list of domain entries.
#' @param domain_graph data.frame with id/toid/relation_type rows.
#' @param nexus_registry data.frame with nexus_id rows.
#' @param source_network the original enriched network.
#' @param ... extra slots merged into the returned structure.
#' @returns object inheriting from "domain_decomposition".
make_minimal_decomposition <- function(domains,
                                       domain_graph = data.frame(
                                         id = character(0),
                                         toid = character(0),
                                         relation_type = character(0),
                                         stringsAsFactors = FALSE),
                                       nexus_registry = data.frame(
                                         nexus_id = character(0),
                                         stringsAsFactors = FALSE),
                                       source_network = NULL,
                                       ...) {

  catchment_index <- unlist(lapply(names(domains),
    \(dn) setNames(rep(dn, nrow(domains[[dn]]$catchments)),
      domains[[dn]]$catchments$id)))

  base <- list(
    domains = domains,
    domain_graph = domain_graph,
    overrides = NULL,
    catchment_domain_index = catchment_index,
    nexus_registry = nexus_registry,
    source_network = source_network
  )

  structure(modifyList(base, list(...)),
    class = "domain_decomposition")

}

# ---- Layer 9 fixture provisioning --------------------------------------

#' Idempotent fetch + cache of the Layer 9 at-scale basin.
#'
#' Cached under nhdplusTools::nhdplusTools_data_dir() so the package
#' source tree stays small. The first call fetches; subsequent calls
#' reuse. Returns the path to the cached gpkg, or signals to skip if
#' the network is offline or the fetch fails. The Layer 9 test file
#' already file-level-skips on `skip_if_not_installed("nhdplusTools")`,
#' so the helper does not re-check namespace availability.
#'
#' @returns character path to the cached geopackage.
prepare_at_scale_fixture <- function() {

  cache_dir <- nhdplusTools::nhdplusTools_data_dir()

  gpkg <- file.path(cache_dir, "hydroloom_decomp_at_scale.gpkg")

  if (file.exists(gpkg)) return(gpkg)

  start <- list(featureSource = "comid", featureID = "13968830")

  basin <- tryCatch(
    nhdplusTools::get_nldi_basin(start),
    error = \(e) {
      testthat::skip(paste0("could not fetch NLDI basin: ", conditionMessage(e)))
    })

  basin_buf <- sf::st_buffer(basin, units::set_units(500, "m"))

  res <- tryCatch({
    nhdplusTools::subset_nhdplus(
      bbox = sf::st_bbox(basin_buf),
      output_file = gpkg,
      nhdplus_data = "download",
      flowline_only = FALSE,
      return_data = FALSE,
      overwrite = FALSE)
    sf::write_sf(basin, gpkg, "basin")
  },
    error = \(e) {
      testthat::skip(paste0("could not fetch NHDPlusV2 subset: ",
        conditionMessage(e)))
    })

  if (!file.exists(gpkg))
    testthat::skip("at-scale fixture not produced by subset_nhdplus")

  gpkg

}

#' Count closed sub-basins in an enriched network.
#'
#' Uses `sort_network(x, split = TRUE)`, which annotates each row with
#' a `terminal_id` — the id of the terminal outlet of its connected
#' sub-network. The number of distinct terminal_ids is the number of
#' independent outlet components in the input. Subtracting one for the
#' global outlet leaves the count of closed sub-basins.
#'
#' This is the canonical hydroloom answer to "how many disconnected
#' sub-networks are there", same as the regression coverage in
#' test_sort_network.R for coastal.rds.
#'
#' @param x hy_topo or hy_leveled.
#' @returns integer count.
count_closed_subbasins <- function(x) {

  sorted <- hydroloom::sort_network(x, split = TRUE)

  max(0L, length(unique(sorted$terminal_id)) - 1L)

}
