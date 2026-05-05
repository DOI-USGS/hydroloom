# Layer 7 -- contained-basin policy.
#
# No real contained-basin example exists in the package's small test
# data, so the fixture is built inline here. The fixture is one
# 7-feature outlet basin plus a 3-feature endorheic subgraph; a
# hand-built domain_decomposition wraps both as separate domains and
# links them via the contained domain's `containing_domain_id`.
#
# Pinned contract: the fixture validates structurally, and
# `get_domain_graph()` emits one containment edge per non-NA
# `containing_domain_id` (with `nexus_id = NA` and
# `relation_type = "contained"`).

# ---- inline fixture builders ------------------------------------------

#' Build the contained-basin fixture: outlet basin (7 rows) + endorheic
#' subgraph (3 rows). Returns the source hy_leveled.
make_contained_source <- function() {

  outlet_basin <- data.frame(
    id = 1:7,
    toid = c(2L, 3L, 4L, 5L, 6L, 7L, 0L),
    topo_sort = 7:1,
    levelpath = rep(1L, 7),
    levelpath_outlet_id = rep(7L, 7),
    da_sqkm = rep(1.0, 7))

  endo <- data.frame(
    id = 8:10,
    toid = c(9L, 10L, 0L),
    topo_sort = 3:1,
    levelpath = rep(2L, 3),
    levelpath_outlet_id = rep(10L, 3),
    da_sqkm = rep(1.0, 3))

  hydroloom::hy(rbind(outlet_basin, endo))

}

#' Build the contained-basin decomposition: an outlet domain (T1) and
#' an endorheic domain (E1). `containing_domain_id` on E1 defaults to
#' `"T1"` (pre-wired) so the fixture is structurally valid as-is; pass
#' `wire = FALSE` for the bare decomposition that `set_containment()`
#' tests start from. The registry carries only outlet nexuses --
#' containment is not a registered nexus.
make_contained_decomposition <- function(wire = TRUE) {

  src <- make_contained_source()

  outlet <- make_minimal_hy_domain(
    hydroloom::hy(src[1:7, ]),
    outlet_nexus_id = "n_outlet")

  endo <- make_minimal_hy_domain(
    hydroloom::hy(src[8:10, ]),
    domain_id = "E1",
    outlet_nexus_id = "n_endo",
    containing_domain_id = if (wire) "T1" else NA_character_)

  make_minimal_decomposition(
    domains = list(T1 = outlet, E1 = endo),
    nexus_registry = data.frame(
      nexus_id = c("n_outlet", "n_endo"),
      from_domain_id = c("T1", "E1"),
      to_domain_id = c(NA_character_, NA_character_),
      stringsAsFactors = FALSE),
    source_network = src)

}

# ---- assertions on the fixture ----------------------------------------

test_that("hand-built contained decomposition validates", {

  d <- make_contained_decomposition()

  expect_true(hydroloom::validate_decomposition(d)$valid)

})

test_that("get_domain_graph emits flow and containment edges", {

  d <- make_contained_decomposition()

  flow_only <- hydroloom::get_domain_graph(d, relations = "flow")
  contained_only <- hydroloom::get_domain_graph(d,
    relations = "contained")
  with_contained <- hydroloom::get_domain_graph(d,
    relations = c("flow", "contained"))

  # The registry carries only outlet nexuses (no inter-domain
  # to_domain_id), so the flow graph is empty.
  expect_equal(nrow(flow_only), 0L)

  # E1 declares T1 as its container, so containment emits exactly
  # one edge with nexus_id = NA and relation_type = "contained".
  expect_equal(nrow(contained_only), 1L)
  expect_equal(contained_only$id, "E1")
  expect_equal(contained_only$toid, "T1")
  expect_true(is.na(contained_only$nexus_id))
  expect_equal(contained_only$relation_type, "contained")

  # The combined call returns the union.
  expect_equal(nrow(with_contained), nrow(flow_only) + nrow(contained_only))

})

# ---- set_containment -------------------------------------------------

test_that("set_containment wires a single contained/containing pair", {

  d <- make_contained_decomposition(wire = FALSE)

  expect_true(is.na(d$domains$E1$containing_domain_id))

  d2 <- hydroloom::set_containment(d, contained = "E1", containing = "T1")

  expect_equal(d2$domains$E1$containing_domain_id, "T1")
  expect_true(hydroloom::validate_decomposition(d2)$valid)

})

test_that("set_containment recycles a length-1 containing argument", {

  # Build a 3-domain fixture by extending the contained fixture with a
  # second endorheic basin (E2) so the recycle path has something to
  # do.
  d <- make_contained_decomposition(wire = FALSE)

  src2 <- data.frame(
    id = 11:12, toid = c(12L, 0L),
    topo_sort = 2:1,
    levelpath = c(3L, 3L),
    levelpath_outlet_id = c(12L, 12L),
    da_sqkm = c(1.0, 1.0))

  d$domains$E2 <- make_minimal_hy_domain(
    hydroloom::hy(src2),
    domain_id = "E2",
    outlet_nexus_id = "n_endo2")

  d$source_network <- hydroloom::hy(rbind(
    as.data.frame(d$source_network),
    src2))

  d$catchment_domain_index <- c(d$catchment_domain_index,
    setNames(rep("E2", 2), as.character(11:12)))

  d$nexus_registry <- rbind(d$nexus_registry,
    data.frame(nexus_id = "n_endo2", from_domain_id = "E2",
      to_domain_id = NA_character_, stringsAsFactors = FALSE))

  d2 <- hydroloom::set_containment(d,
    contained = c("E1", "E2"), containing = "T1")

  expect_equal(d2$domains$E1$containing_domain_id, "T1")
  expect_equal(d2$domains$E2$containing_domain_id, "T1")

})

test_that("set_containment rejects unknown domain ids", {

  d <- make_contained_decomposition(wire = FALSE)

  expect_error(
    hydroloom::set_containment(d, contained = "nope", containing = "T1"),
    "unknown domain id")

  expect_error(
    hydroloom::set_containment(d, contained = "E1", containing = "nope"),
    "unknown domain id")

})

test_that("set_containment rejects self-containment", {

  d <- make_contained_decomposition(wire = FALSE)

  expect_error(
    hydroloom::set_containment(d, contained = "T1", containing = "T1"),
    "cannot contain itself")

})

test_that("validator catches a containment cycle", {

  # Hand-wire a two-cycle: T1 -> E1 -> T1. set_containment() goes
  # through the validator on each call, so the first call rejects it;
  # mutate the slots directly to expose the validator.
  d <- make_contained_decomposition(wire = TRUE)

  d$domains$T1$containing_domain_id <- "E1"

  v <- hydroloom::validate_decomposition(d)

  expect_false(v$valid)
  expect_true(any(grepl("containment cycle", v$issues)))

})

# ---- get_containing_domain -------------------------------------------

test_that("get_containing_domain returns NA for uncontained, id for contained", {

  d <- make_contained_decomposition(wire = TRUE)

  # Vector form: T1 has no container; E1 is contained by T1.
  expect_equal(
    hydroloom::get_containing_domain(d, c("T1", "E1")),
    c(NA_character_, "T1"))

  # Scalar form returns a length-1 vector.
  expect_equal(hydroloom::get_containing_domain(d, "E1"), "T1")

})

test_that("get_containing_domain rejects unknown domain ids", {

  d <- make_contained_decomposition(wire = FALSE)

  expect_error(
    hydroloom::get_containing_domain(d, "nope"),
    "unknown domain id")

})

# ---- recompose containment mode --------------------------------------

test_that("recompose containment = 'ignore' is the default and unchanged", {

  d <- make_contained_decomposition(wire = TRUE)

  rec_default <- hydroloom::recompose(d, "da_sqkm")
  rec_ignore  <- hydroloom::recompose(d, "da_sqkm", containment = "ignore")

  expect_equal(rec_default$da_sqkm, rec_ignore$da_sqkm)

  # Per-basin baseline: T1's outlet (id=7) is the basin total of 7.0;
  # E1's outlet (id=10) is the basin total of 3.0. Ignore mode keeps
  # E1's mass inside E1 -- it does not appear on T1's outlet.
  expect_equal(rec_ignore$da_sqkm[rec_ignore$id == 7L], 7.0)
  expect_equal(rec_ignore$da_sqkm[rec_ignore$id == 10L], 3.0)

})

test_that("recompose containment = 'accumulate' adds contained mass at containing outlet", {

  d <- make_contained_decomposition(wire = TRUE)

  rec_accum <- hydroloom::recompose(d, "da_sqkm",
    containment = "accumulate")

  # T1's outlet now carries E1's basin total in addition to T1's own
  # accumulation (7 + 3 = 10). E1's own outlet is unchanged.
  expect_equal(rec_accum$da_sqkm[rec_accum$id == 7L], 10.0)
  expect_equal(rec_accum$da_sqkm[rec_accum$id == 10L], 3.0)

  # Upstream rows of T1 are unaffected -- the contained value is
  # added at T1's outlet (the most-downstream extensive-network row
  # of T1's segment), and there is nothing downstream of it in this
  # sub-threshold basin.
  expect_equal(rec_accum$da_sqkm[rec_accum$id == 1L], 1.0)

})

test_that("recompose containment = 'accumulate' is a no-op without wiring", {

  d <- make_contained_decomposition(wire = FALSE)

  rec_ignore <- hydroloom::recompose(d, "da_sqkm", containment = "ignore")
  rec_accum  <- hydroloom::recompose(d, "da_sqkm",
    containment = "accumulate")

  expect_equal(rec_ignore$da_sqkm, rec_accum$da_sqkm)

})

# ---- walker-cut end-to-end -------------------------------------------
# The strong contract pin: take walker.gpkg, sever a tributary so it
# becomes its own basin, decompose, declare the orphan as contained by
# the main basin, and verify that recompose with `containment =
# "accumulate"` recovers the un-cut accumulated value at the main
# basin's outlet bit-for-bit.

test_that("cut walker tributary recomposes back to the un-cut total under containment", {

  walker <- sf::read_sf(system.file("extdata/walker.gpkg",
    package = "hydroloom"))

  h_full <- hydroloom::hy(walker) |>
    hydroloom::add_toids() |>
    hydroloom::add_levelpaths(name_attribute = "GNIS_ID",
      weight_attribute = "arbolate_sum")

  # Reference values from the un-cut network.
  reference <- hydroloom::accumulate_downstream(h_full, "da_sqkm",
    quiet = TRUE)

  basin_outlet_id <- 5329303L  # walker's only outlet (toid == 0).
  cut_id          <- 5329313L  # outlet of levelpath 10038012, a 9-row tributary.

  ref_at_basin_outlet <- reference[h_full$id == basin_outlet_id]
  ref_at_cut          <- reference[h_full$id == cut_id]

  # Sever the tributary: rewrite cut_id's toid to 0 (walker's reserved
  # outlet value) so it becomes the outlet of an orphan sub-basin.
  # levelpath / topo_sort / levelpath_outlet_id were derived from the
  # un-cut topology and must be dropped so add_levelpaths can rebuild
  # them to match the new shape.
  h_cut <- hydroloom::hy(walker) |> hydroloom::add_toids()

  h_cut$toid[h_cut$id == cut_id] <- 0

  for (col in c("topo_sort", "levelpath", "levelpath_outlet_id"))
    h_cut[[col]] <- NULL

  h_cut <- hydroloom::add_levelpaths(h_cut,
    name_attribute = "GNIS_ID",
    weight_attribute = "arbolate_sum")

  expect_equal(sum(h_cut$toid == 0), 2L,
    info = "after the cut the network has exactly two basin outlets")

  # Decompose. The default produces one domain per basin (the basin's
  # outlet levelpath is selected as a single-segment stem); look the
  # owning domain up by catchment id rather than reconstruct the
  # internally-formatted name.
  d <- hydroloom::decompose_network(h_cut)

  expect_equal(length(d$domains), 2L)

  main_domain   <- hydroloom::get_domain_for_catchment(d, basin_outlet_id)
  orphan_domain <- hydroloom::get_domain_for_catchment(d, cut_id)

  expect_false(identical(main_domain, orphan_domain))

  # Declare the orphan as contained by the main basin.
  d <- hydroloom::set_containment(d,
    contained = orphan_domain, containing = main_domain)

  expect_true(hydroloom::validate_decomposition(d)$valid)

  graph_c <- hydroloom::get_domain_graph(d, relations = "contained")

  expect_equal(nrow(graph_c), 1L)
  expect_equal(graph_c$id, orphan_domain)
  expect_equal(graph_c$toid, main_domain)

  # `containment = "ignore"` carries each basin's own accumulation
  # only -- the orphan's mass dies at the cut row, and the main basin
  # is missing exactly that contribution.
  rec_ignore <- hydroloom::recompose(d, "da_sqkm",
    containment = "ignore")

  expect_equal(rec_ignore$da_sqkm[rec_ignore$id == cut_id], ref_at_cut)
  expect_equal(rec_ignore$da_sqkm[rec_ignore$id == basin_outlet_id],
    ref_at_basin_outlet - ref_at_cut)

  # `containment = "accumulate"` recovers the un-cut total at the main
  # basin's outlet bit-for-bit (within accumulate_downstream's join
  # tolerance).
  rec_accum <- hydroloom::recompose(d, "da_sqkm",
    containment = "accumulate")

  expect_equal(rec_accum$da_sqkm[rec_accum$id == basin_outlet_id],
    ref_at_basin_outlet, tolerance = 1e-9)

  # The orphan's outlet still reports its own basin total -- containment
  # propagates down into the containing basin, never back up.
  expect_equal(rec_accum$da_sqkm[rec_accum$id == cut_id], ref_at_cut)

})
