test_that("hy() auto-classifies to hy_topo with add_topo = TRUE", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  h <- hy(x, add_topo = TRUE)

  # auto-builds toid from fromnode/tonode -> hy_topo
  expect_s3_class(h, "hy_topo")
  expect_s3_class(h, "hy")
  expect_true("toid" %in% names(h))
  expect_true(inherits(h, "sf"))
})

test_that("hy() default produces hy_node for node data", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  h <- hy(x)

  # default add_topo = FALSE, data has fromnode/tonode -> hy_node
  expect_s3_class(h, "hy_node")
  expect_s3_class(h, "hy")
  expect_false(inherits(h, "hy_topo"))
  expect_true("fromnode" %in% names(h))
  expect_true("tonode" %in% names(h))
})

test_that("hy() classifies toid-only data as hy_topo", {
  x <- data.frame(id = 1:3, toid = c(2L, 3L, 0L))

  h <- hy(x)

  expect_s3_class(h, "hy_topo")
  expect_s3_class(h, "hy")
})

test_that("hy() classifies leveled data as hy_leveled", {
  x <- data.frame(
    id = 1:3, toid = c(2L, 3L, 0L),
    topo_sort = 3:1, levelpath = c(1L, 1L, 1L),
    levelpath_outlet_id = c(3L, 3L, 3L)
  )

  h <- hy(x)

  expect_s3_class(h, "hy_leveled")
  expect_s3_class(h, "hy_topo")
  expect_s3_class(h, "hy")
})

test_that("hy() warns on non-unique id with toid when add_topo = TRUE", {
  x <- data.frame(id = c(1, 1, 2), toid = c(2, 2, 0))

  expect_warning(h <- hy(x, add_topo = TRUE), "Non-unique id")
  expect_s3_class(h, "hy")
  expect_false(inherits(h, "hy_topo"))

  # with default add_topo = FALSE, no warning, stays hy (no subclass)
  h2 <- hy(x)
  expect_s3_class(h2, "hy")
  expect_false(inherits(h2, "hy_topo"))
})

test_that("hy_reverse strips all extended classes", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  h <- hy(x, add_topo = TRUE)
  rev <- hy_reverse(h)

  expect_false(any(hy_classes %in% class(rev)))
  expect_null(attr(rev, "dendritic"))
})

test_that("is_dendritic works correctly", {
  # data with no divergence == 2 should be dendritic
  x <- data.frame(id = 1:3, toid = c(2L, 3L, 0L))
  h <- hy(x)
  expect_true(is_dendritic(h))

  # data with divergence == 2 should not be dendritic
  x2 <- data.frame(id = 1:3, toid = c(2L, 3L, 0L),
                   divergence = c(0L, 2L, 0L))
  h2 <- hy(x2)
  expect_false(is_dendritic(h2))
})

test_that("hy_network_type returns correct type", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  expect_equal(hy_network_type(hy(x)), "hy_node")
  expect_equal(hy_network_type(hy(x, add_topo = TRUE)), "hy_topo")

  lev <- data.frame(
    id = 1:3, toid = c(2L, 3L, 0L),
    topo_sort = 3:1, levelpath = c(1L, 1L, 1L),
    levelpath_outlet_id = c(3L, 3L, 3L)
  )
  expect_equal(hy_network_type(hy(lev)), "hy_leveled")

  expect_equal(hy_network_type(data.frame(x = 1)), "not a hydroloom object")
})

test_that("hy_capabilities returns correct logical vector", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  # hy_topo capabilities
  h <- hy(x, add_topo = TRUE)
  caps <- hy_capabilities(h)
  expect_true(caps[["sort_network"]])
  expect_true(caps[["add_levelpaths"]])
  expect_false(caps[["add_toids"]])
  expect_false(caps[["add_divergence"]])

  # hy_node capabilities
  hn <- hy(x)
  caps_n <- hy_capabilities(hn)
  expect_true(caps_n[["add_toids"]])
  expect_true(caps_n[["add_divergence"]])
  expect_false(caps_n[["sort_network"]])

  # include_conversions for hy_node
  caps_nc <- hy_capabilities(hn, include_conversions = TRUE)
  expect_true(caps_nc[["sort_network"]])
  expect_true(caps_nc[["add_levelpaths"]])
})

test_that("print methods produce expected output", {
  x <- data.frame(id = 1:3, toid = c(2L, 3L, 0L))
  h <- hy(x)

  out <- capture_output(print(h))
  expect_true(grepl("hydroloom", out))
  expect_true(grepl("edge list", out))
  expect_true(grepl("3 features", out))

  x2 <- data.frame(id = 1:3, fromnode = c(1L, 2L, 3L), tonode = c(2L, 3L, 4L))
  h2 <- hy(x2)

  out2 <- capture_output(print(h2))
  expect_true(grepl("fromnode/tonode graph", out2))
  expect_true(grepl("3 features", out2))
})

test_that("new_hy_flownetwork validates correctly", {
  good <- data.frame(
    id = c(1, 1, 2), toid = c(2, 3, 0),
    upmain = c(TRUE, FALSE, TRUE),
    downmain = c(TRUE, FALSE, TRUE)
  )
  fn <- new_hy_flownetwork(good)
  expect_s3_class(fn, "hy_flownetwork")

  # duplicate downmain per id should fail
  bad_dm <- data.frame(
    id = c(1, 1), toid = c(2, 3),
    upmain = c(TRUE, FALSE),
    downmain = c(TRUE, TRUE)
  )
  expect_error(new_hy_flownetwork(bad_dm), "multiple downmain")
})

test_that("class hierarchy preserved through hy_reverse round-trip", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  h <- hy(x, add_topo = TRUE)
  expect_s3_class(h, "hy_topo")

  rev <- hy_reverse(h)
  expect_false(any(hy_classes %in% class(rev)))

  # re-converting should restore class
  h2 <- hy(rev, add_topo = TRUE)
  expect_s3_class(h2, "hy_topo")
})

test_that("add_topo = TRUE with NHD data produces hy_topo", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  h <- hy(x, add_topo = TRUE)

  expect_s3_class(h, "hy_topo")
  expect_true("toid" %in% names(h))
})

test_that("hy_node preserves fromnode/tonode form", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  h <- hy(x)

  expect_s3_class(h, "hy_node")
  expect_true("fromnode" %in% names(h))
  expect_true("tonode" %in% names(h))
})

test_that("new_hy_topo rejects duplicated id", {
  x <- hy(data.frame(id = c(1, 1, 2), toid = c(2, 2, 0)))
  expect_error(new_hy_topo(x), "unique id")
})

test_that("new_hy_topo rejects non-hy input", {
  x <- data.frame(id = 1:3, toid = c(2L, 3L, 0L))
  expect_error(new_hy_topo(x), "inherits")
})

test_that("new_hy_node rejects duplicated id", {
  x <- hy(data.frame(id = c(1, 1), fromnode = c(1L, 1L), tonode = c(2L, 2L)))
  expect_error(new_hy_node(x), "unique id")
})

test_that("new_hy_node rejects non-hy input", {
  x <- data.frame(id = 1:3, fromnode = 1:3, tonode = 2:4)
  expect_error(new_hy_node(x), "inherits")
})

test_that("new_hy_leveled rejects missing columns", {
  x <- hy(data.frame(id = 1:3, toid = c(2L, 3L, 0L)))
  expect_error(new_hy_leveled(x), "requires")
})

test_that("new_hy_flownetwork rejects multiple upmain per toid", {
  bad_um <- data.frame(
    id = c(1, 2), toid = c(3, 3),
    upmain = c(TRUE, TRUE),
    downmain = c(TRUE, TRUE)
  )
  expect_error(new_hy_flownetwork(bad_um), "multiple upmain")
})

test_that("is_dendritic detects duplicated id with toid", {
  x <- data.frame(id = c(1, 1, 2), toid = c(2, 3, 0))
  expect_false(is_dendritic(x))
})

test_that("existing hy() behavior preserved", {
  # basic data.frame test
  x <- data.frame(comid = 1, fromnode = 2, tonode = 3)

  y <- hy(x)

  expect_equal(names(y)[1:3], c("id", "fromnode", "tonode"))
  expect_s3_class(y, "hy")
  expect_true(is.hy(y))

  # sf class preserved
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
  expect_s3_class(hy(x), "sf")
  expect_false(inherits(hy(x, clean = TRUE), "sf"))
  expect_true(inherits(hy(x), "tbl"))

  # hy_reverse round-trip
  x <- sf::st_sf(dplyr::as_tibble(x))
  expect_equal(x, hy_reverse(hy(x)))
})

# ---- Phase 2: producer function class tests ----

test_that("producers stamp correct output class", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
  h <- hy(x)

  h_topo <- add_toids(h, return_dendritic = TRUE)
  expect_s3_class(h_topo, "hy_topo")

  expect_s3_class(sort_network(h_topo), "hy_topo")

  expect_s3_class(add_topo_sort(h_topo), "hy_topo")

  # add_levelpaths -> hy_leveled (inherits hy_topo)
  h_lev <- add_levelpaths(h_topo, "GNIS_ID", "arbolate_sum")
  expect_s3_class(h_lev, "hy_leveled")
  expect_s3_class(h_lev, "hy_topo")

  # make_node_topology dendritic -> hy_topo (has toid + nodes)
  h_nodes <- make_node_topology(dplyr::select(h_topo, -fromnode, -tonode))
  expect_s3_class(h_nodes, "hy_topo")

  # make_node_topology add_div = TRUE -> hy_node (toid dropped)
  h_nd <- make_node_topology(dplyr::select(h_topo, -fromnode, -tonode),
    add_div = TRUE)
  expect_s3_class(h_nd, "hy_node")

  expect_s3_class(to_flownetwork(x), "hy_flownetwork")
})

test_that("add_toids warns for return_dendritic = FALSE", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
  expect_warning(add_toids(x, return_dendritic = FALSE), "deprecated")
})

test_that("add_divergence.hy sets dendritic attr to FALSE", {
  h <- hy(data.frame(
    id = c(1, 2, 3, 4, 5, 6, 7),
    fromnode = c(1, 2, 3, 3, 4, 5, 6),
    tonode = c(3, 3, 4, 5, 6, 6, 7),
    name = c("", "", "", "", "", "", ""),
    type = c(1, 1, 1, 1, 1, 1, 1)))

  result <- add_divergence(h, 7, c(),
    name_attr = "name", type_attr = "type", major_types = 1)

  expect_false(attr(result, "dendritic"))
  expect_s3_class(result, "hy_node")
})

# ---- Phase 3: dispatch and guided error tests ----

test_that("hy_topo functions error on bare hy", {
  h_bare <- hy(data.frame(id = 1:3, x = letters[1:3]))

  expect_error(sort_network(h_bare), "requires hy_topo")
  expect_error(add_topo_sort(h_bare), "requires hy_topo")
  expect_error(add_pathlength(h_bare), "requires hy_topo")
  expect_error(make_node_topology(h_bare), "requires hy_topo")
  expect_error(make_index_ids(h_bare), "requires hy_topo")
  expect_error(get_bridge_flowlines(h_bare), "requires hy_topo")
  expect_error(accumulate_downstream(h_bare, "x"), "requires hy_topo")
  expect_error(add_streamorder(h_bare), "requires hy_topo")
})

test_that("hy_leveled functions error on bare hy and hy_topo", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
  h_topo <- add_toids(hy(x))

  h_topo_bare <- hy(data.frame(id = 1:3, toid = c(2, 3, 0)))

  expect_error(add_pfafstetter(h_topo_bare), "requires hy_leveled")
  expect_error(add_streamlevel(h_topo_bare), "requires hy_leveled")
  expect_error(navigate_hydro_network(h_topo_bare, 1, "UM"), "requires hy_leveled")
  expect_error(to_flownetwork(h_topo_bare), "requires hy_leveled")
})

test_that("hy_node functions error on bare hy", {
  h_bare <- hy(data.frame(id = 1:3, x = letters[1:3]))

  expect_error(subset_network(h_bare, 1), "requires hy_node")
  expect_error(add_return_divergence(h_bare), "requires hy_node")
})

test_that("add_toids.hy_topo guard fires", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
  h_topo <- add_toids(hy(x))

  expect_error(add_toids(h_topo), "already has toid")
})

test_that("hy_node auto-convert works with warning", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
  h_node <- hy(x)

  expect_message(sort_network(h_node), "converting hy_node")
  expect_message(result <- add_topo_sort(h_node), "converting hy_node")
  expect_s3_class(result, "hy_topo")
})

test_that("add_toids.hy auto-promotes from bare hy with nodes", {
  h <- hy(data.frame(
    id = 1:3,
    fromnode = c(1L, 2L, 3L),
    tonode = c(2L, 3L, 0L),
    divergence = c(0L, 0L, 0L)))

  result <- add_toids(h)
  expect_s3_class(result, "hy_topo")
})

test_that("to_flownetwork generic dispatch works", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  # data.frame path still works
  fn <- to_flownetwork(x)
  expect_s3_class(fn, "hy_flownetwork")
})
