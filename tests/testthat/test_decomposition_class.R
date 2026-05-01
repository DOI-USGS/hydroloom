# Layer 1 — hy_domain constructor + validate_decomposition unit tests.
#
# Drives the constructor and validator API entirely from hand-built
# fixtures. No decompose_network involvement. Tests are skipped via
# decomposition_pending() until the corresponding API surface lands.
#
# Why hand-built fixtures: each test isolates exactly one invariant,
# making the failing message point at the rule the implementation
# violated.
#
# The 3-row leveled / topo / flownetwork fixtures live inline at the
# top of this file rather than in helper-decomposition.R because the
# pattern is already canonical in test_hy_classes.R:36-47 and the
# helpers add no behavior — just data.frame construction.

make_lev <- function() {
  hydroloom::hy(data.frame(
    id = 1:3, toid = c(2L, 3L, 0L),
    topo_sort = 3:1, levelpath = c(1L, 1L, 1L),
    levelpath_outlet_id = c(3L, 3L, 3L)))
}

make_topo <- function() {
  hydroloom::hy(data.frame(
    id = 1:3, toid = c(2L, 3L, 0L)))
}

make_fn <- function() {
  hydroloom::hy(data.frame(
    id = c(1, 1, 2), toid = c(2, 3, 0),
    upmain = c(TRUE, FALSE, TRUE),
    downmain = c(TRUE, FALSE, TRUE)))
}

# ---- hy_domain() constructor ------------------------------------------

test_that("hy_domain accepts a hy_leveled domain", {

  decomposition_pending("hy_domain")

  d <- make_minimal_hy_domain(make_lev())

  expect_s3_class(d, "hy_domain")
  expect_s3_class(d$catchments, "hy_leveled")

})

test_that("hy_domain accepts a hy_topo domain", {

  decomposition_pending("hy_domain")

  d <- make_minimal_hy_domain(make_topo(), domain_id = "C1")

  expect_s3_class(d, "hy_domain")
  expect_s3_class(d$catchments, "hy_topo")

})

test_that("hy_domain accepts a flownetwork domain", {

  decomposition_pending("hy_domain")

  d <- make_minimal_hy_domain(make_fn(), domain_id = "C1")

  expect_s3_class(d, "hy_domain")
  expect_s3_class(d$catchments, "hy_flownetwork")

})

test_that("hy_domain carries all required slots", {

  decomposition_pending("hy_domain")

  d <- make_minimal_hy_domain(make_lev())

  required <- c("domain_id", "outlet_nexus_id", "inlet_nexus_ids",
    "containing_domain_id", "catchments", "topo_sort_offset")

  expect_named(d, required, ignore.order = TRUE)

  caps <- hydroloom::hy_capabilities(d$catchments)

  expect_true(caps[["accumulate_downstream"]],
    label = "domain catchments support accumulate_downstream")

})

# ---- validate_decomposition() unit cases ------------------------------

test_that("validate_decomposition accepts a hand-built valid decomposition", {

  decomposition_pending(c("hy_domain", "validate_decomposition"))

  # build the smallest possible valid decomposition: one domain wrapping
  # the 3-row leveled fixture and the same fixture as the basin's
  # extensive connectivity overlay.
  lev <- make_lev()

  domain <- make_minimal_hy_domain(lev)

  d <- make_minimal_decomposition(
    domains = list(T1 = domain),
    domain_connectivity = list("3" = lev),
    nexus_registry = data.frame(nexus_id = "n_out"),
    source_network = lev)

  res <- hydroloom::validate_decomposition(d)

  expect_true(res$valid)
  expect_length(res$issues, 0)

})

test_that("validate_decomposition flags coverage failure", {

  decomposition_pending(c("hy_domain", "validate_decomposition"))

  # drop one catchment row from the domain so the partition no longer
  # covers the source network. validate must detect that the missing
  # id is not in any domain. Re-wrap with hy() because data.frame `[`
  # subsetting strips the hy_leveled / hy_topo / hy classes.
  lev <- make_lev()

  partial_lev <- hydroloom::hy(lev[1:2, ])

  domain <- make_minimal_hy_domain(partial_lev)

  d <- make_minimal_decomposition(
    domains = list(T1 = domain),
    nexus_registry = data.frame(nexus_id = "n_out"),
    source_network = lev)

  res <- hydroloom::validate_decomposition(d)

  expect_false(res$valid)
  expect_true(any(grepl("coverage|partition|missing",
    res$issues, ignore.case = TRUE)))

})

test_that("validate_decomposition flags an inter-domain cycle", {

  decomposition_pending(c("hy_domain", "validate_decomposition"))

  # two domains whose nexus_registry rows form a cycle when projected
  # through get_domain_graph(): T1 -> T2 via n1, T2 -> T1 via n2.
  lev <- make_lev()

  t1 <- make_minimal_hy_domain(lev, domain_id = "T1",
    outlet_nexus_id = "n1")

  t2 <- make_minimal_hy_domain(lev, domain_id = "T2",
    outlet_nexus_id = "n2")

  d <- make_minimal_decomposition(
    domains = list(T1 = t1, T2 = t2),
    nexus_registry = data.frame(
      nexus_id = c("n1", "n2"),
      from_domain_id = c("T1", "T2"),
      to_domain_id = c("T2", "T1"),
      stringsAsFactors = FALSE),
    source_network = lev)

  res <- hydroloom::validate_decomposition(d)

  expect_false(res$valid)
  expect_true(any(grepl("cycle|loop|dag",
    res$issues, ignore.case = TRUE)))

})

test_that("validate_decomposition flags a multi-outlet basin overlay", {

  decomposition_pending(c("hy_domain", "validate_decomposition"))

  # two unrelated rows carrying the reserved outlet toid value -> two
  # outlets in the basin's extensive connectivity overlay.
  bad_lev <- hydroloom::hy(data.frame(
    id = 1:4, toid = c(2L, 0L, 4L, 0L),
    topo_sort = 4:1, levelpath = c(1L, 1L, 2L, 2L),
    levelpath_outlet_id = c(2L, 2L, 4L, 4L)))

  domain <- make_minimal_hy_domain(bad_lev)

  d <- make_minimal_decomposition(
    domains = list(T1 = domain),
    domain_connectivity = list("4" = bad_lev),
    nexus_registry = data.frame(nexus_id = "n_out"),
    source_network = bad_lev)

  res <- hydroloom::validate_decomposition(d)

  expect_false(res$valid)
  expect_true(any(grepl("outlet", res$issues, ignore.case = TRUE)))

})

test_that("validate_decomposition flags an unknown containing_domain_id", {

  decomposition_pending(c("hy_domain", "validate_decomposition"))

  lev <- make_lev()

  contained <- make_minimal_hy_domain(lev,
    domain_id = "C1",
    containing_domain_id = "T_phantom")

  d <- make_minimal_decomposition(
    domains = list(C1 = contained),
    nexus_registry = data.frame(nexus_id = "n_out"),
    source_network = lev)

  res <- hydroloom::validate_decomposition(d)

  expect_false(res$valid)
  expect_true(any(grepl("contain", res$issues, ignore.case = TRUE)))

})

# ---- print.domain_decomposition ---------------------------------------

test_that("print.domain_decomposition cheap mode (default) is snapshot-stable", {

  decomposition_pending("decompose_network")

  d <- hydroloom::decompose_network(enrich_for_decomposition(load_walker()))

  expect_snapshot(print(d))

})

test_that("print.domain_decomposition full mode is snapshot-stable", {

  decomposition_pending("decompose_network")

  d <- hydroloom::decompose_network(enrich_for_decomposition(load_walker()))

  expect_snapshot(print(d, full = TRUE))

})

test_that("print.domain_decomposition returns x invisibly", {

  decomposition_pending("decompose_network")

  d <- hydroloom::decompose_network(enrich_for_decomposition(load_walker()))

  # capture.output drains the cat() side-effect; the value should be d.
  capture.output(res <- print(d))

  expect_identical(res, d)

})

test_that("print.domain_decomposition handles empty decomposition", {

  decomposition_pending("decompose_network")

  empty <- structure(
    list(
      domains = list(),
      domain_connectivity = list(),
      overrides = NULL,
      catchment_domain_index = setNames(character(0), character(0)),
      nexus_registry = data.frame(
        nexus_id = character(0),
        from_domain_id = character(0),
        to_domain_id = character(0),
        stem_catchment_id = character(0)),
      source_network = make_lev()),
    class = "domain_decomposition")

  out_cheap <- capture.output(print(empty))

  expect_match(out_cheap, "0 basins", all = FALSE, fixed = TRUE)
  expect_match(out_cheap, "0 domains", all = FALSE, fixed = TRUE)

  # Full mode should not error on an empty decomposition either.
  out_full <- capture.output(print(empty, full = TRUE))

  expect_match(out_full, "Empty decomposition", all = FALSE, fixed = TRUE)

})
