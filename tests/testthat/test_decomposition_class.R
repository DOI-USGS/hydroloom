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

test_that("hy_domain accepts a hy_leveled trunk", {

  decomposition_pending("hy_domain")

  d <- make_minimal_hy_domain(make_lev(), domain_type = "trunk")

  expect_s3_class(d, "hy_domain")
  expect_equal(d$domain_type, "trunk")
  expect_s3_class(d$catchments, "hy_leveled")

})

test_that("hy_domain rejects a non-leveled trunk and points at add_levelpaths", {

  decomposition_pending("hy_domain")

  expect_error(
    make_minimal_hy_domain(make_topo(), domain_type = "trunk"),
    "hy_leveled|add_levelpaths")

})

test_that("hy_domain accepts a leveled compact", {

  decomposition_pending("hy_domain")

  d <- make_minimal_hy_domain(make_lev(),
    domain_type = "compact",
    domain_id = "C1",
    trunk_domain_id = "T1")

  expect_s3_class(d, "hy_domain")
  expect_equal(d$domain_type, "compact")

})

test_that("hy_domain rejects a flownetwork trunk", {

  decomposition_pending("hy_domain")

  expect_error(
    make_minimal_hy_domain(make_fn(), domain_type = "trunk"),
    "hy_leveled|trunk")

})

test_that("hy_domain accepts a flownetwork compact", {

  decomposition_pending("hy_domain")

  d <- make_minimal_hy_domain(make_fn(),
    domain_type = "compact",
    domain_id = "C1",
    trunk_domain_id = "T1")

  expect_s3_class(d, "hy_domain")
  expect_s3_class(d$catchments, "hy_flownetwork")

})

test_that("hy_domain carries all required slots", {

  decomposition_pending("hy_domain")

  d <- make_minimal_hy_domain(make_lev(), domain_type = "trunk")

  required <- c("domain_id", "domain_type", "outlet_nexus_id",
    "inlet_nexus_ids", "trunk_domain_id", "containing_domain_id",
    "catchments", "topo_sort_offset")

  expect_named(d, required, ignore.order = TRUE)

  caps <- hydroloom::hy_capabilities(d$catchments)

  expect_true(caps[["accumulate_downstream"]],
    label = "trunk domain catchments support accumulate_downstream")

})

# ---- validate_decomposition() unit cases ------------------------------

test_that("validate_decomposition accepts a hand-built valid decomposition", {

  decomposition_pending(c("hy_domain", "validate_decomposition"))

  # build the smallest possible valid decomposition: one trunk wrapping
  # the 3-row leveled fixture, no compacts, no overrides.
  lev <- make_lev()

  trunk <- make_minimal_hy_domain(lev, domain_type = "trunk")

  d <- make_minimal_decomposition(
    domains = list(T1 = trunk),
    nexus_registry = data.frame(nexus_id = "n_out"),
    source_network = lev)

  res <- hydroloom::validate_decomposition(d)

  expect_true(res$valid)
  expect_length(res$issues, 0)

})

test_that("validate_decomposition flags coverage failure", {

  decomposition_pending(c("hy_domain", "validate_decomposition"))

  # drop one catchment row from the trunk so the partition no longer
  # covers the source network. validate must detect that the missing
  # id is not in any domain. Re-wrap with hy() because data.frame `[`
  # subsetting strips the hy_leveled / hy_topo / hy classes.
  lev <- make_lev()

  partial_lev <- hydroloom::hy(lev[1:2, ])

  trunk <- make_minimal_hy_domain(partial_lev, domain_type = "trunk")

  d <- make_minimal_decomposition(
    domains = list(T1 = trunk),
    nexus_registry = data.frame(nexus_id = "n_out"),
    source_network = lev)

  res <- hydroloom::validate_decomposition(d)

  expect_false(res$valid)
  expect_true(any(grepl("coverage|partition|missing",
    res$issues, ignore.case = TRUE)))

})

test_that("validate_decomposition flags an inter-domain cycle", {

  decomposition_pending(c("hy_domain", "validate_decomposition"))

  # two trunks pointing at each other in domain_graph -> cycle
  lev <- make_lev()

  t1 <- make_minimal_hy_domain(lev, domain_type = "trunk",
    domain_id = "T1", outlet_nexus_id = "n1")

  t2 <- make_minimal_hy_domain(lev, domain_type = "trunk",
    domain_id = "T2", outlet_nexus_id = "n2")

  d <- make_minimal_decomposition(
    domains = list(T1 = t1, T2 = t2),
    domain_graph = data.frame(
      id = c("T1", "T2"), toid = c("T2", "T1"),
      nexus_id = c("n1", "n2"), nexus_position = c(0, 0),
      relation_type = c("flow", "flow")),
    nexus_registry = data.frame(nexus_id = c("n1", "n2")),
    source_network = lev)

  res <- hydroloom::validate_decomposition(d)

  expect_false(res$valid)
  expect_true(any(grepl("cycle|loop|dag",
    res$issues, ignore.case = TRUE)))

})

test_that("validate_decomposition flags a multi-outlet domain", {

  decomposition_pending(c("hy_domain", "validate_decomposition"))

  # two unrelated rows pointing at sentinel -> two outlets in one domain
  bad_lev <- hydroloom::hy(data.frame(
    id = 1:4, toid = c(2L, 0L, 4L, 0L),
    topo_sort = 4:1, levelpath = c(1L, 1L, 2L, 2L),
    levelpath_outlet_id = c(2L, 2L, 4L, 4L)))

  trunk <- make_minimal_hy_domain(bad_lev, domain_type = "trunk")

  d <- make_minimal_decomposition(
    domains = list(T1 = trunk),
    nexus_registry = data.frame(nexus_id = "n_out"),
    source_network = bad_lev)

  res <- hydroloom::validate_decomposition(d)

  expect_false(res$valid)
  expect_true(any(grepl("outlet", res$issues, ignore.case = TRUE)))

})

test_that("validate_decomposition flags an inter-domain edge with unknown nexus", {

  decomposition_pending(c("hy_domain", "validate_decomposition"))

  lev <- make_lev()

  t1 <- make_minimal_hy_domain(lev, domain_type = "trunk",
    outlet_nexus_id = "n1")

  d <- make_minimal_decomposition(
    domains = list(T1 = t1),
    domain_graph = data.frame(
      id = "T1", toid = "T2",
      nexus_id = "n_missing", nexus_position = 0,
      relation_type = "flow"),
    nexus_registry = data.frame(nexus_id = "n1"),
    source_network = lev)

  res <- hydroloom::validate_decomposition(d)

  expect_false(res$valid)
  expect_true(any(grepl("nexus|connectivity|unknown",
    res$issues, ignore.case = TRUE)))

})

test_that("validate_decomposition flags an unknown containing_domain_id", {

  decomposition_pending(c("hy_domain", "validate_decomposition"))

  lev <- make_lev()

  contained <- make_minimal_hy_domain(lev, domain_type = "compact",
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

test_that("validate_decomposition flags a non-leveled trunk", {

  decomposition_pending(c("hy_domain", "validate_decomposition"))

  # build a normally-constructed valid trunk first, then mutate the
  # catchments slot to a hy_topo to simulate a corrupted decomposition
  # — the constructor's class check would otherwise reject this up front.
  topo <- make_topo()
  lev <- make_lev()

  trunk <- make_minimal_hy_domain(lev, domain_type = "trunk")

  trunk$catchments <- topo

  d <- make_minimal_decomposition(
    domains = list(T1 = trunk),
    nexus_registry = data.frame(nexus_id = "n_out"),
    source_network = topo)

  res <- hydroloom::validate_decomposition(d)

  expect_false(res$valid)
  expect_true(any(grepl("hy_leveled|trunk|class",
    res$issues, ignore.case = TRUE)))

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
      domain_graph = data.frame(
        id = character(0), toid = character(0),
        nexus_id = character(0), nexus_position = numeric(0),
        relation_type = character(0)),
      overrides = NULL,
      catchment_domain_index = setNames(character(0), character(0)),
      nexus_registry = data.frame(
        nexus_id = character(0),
        from_domain_id = character(0),
        to_domain_id = character(0),
        trunk_catchment_id = character(0),
        aggregate_id_measure = numeric(0)),
      source_network = make_lev()),
    class = "domain_decomposition")

  out_cheap <- capture.output(print(empty))

  expect_match(out_cheap, "0 trunks", all = FALSE, fixed = TRUE)
  expect_match(out_cheap, "0 compacts", all = FALSE, fixed = TRUE)

  # Full mode should not error on an empty decomposition either.
  out_full <- capture.output(print(empty, full = TRUE))

  expect_match(out_full, "Empty decomposition", all = FALSE, fixed = TRUE)

})
