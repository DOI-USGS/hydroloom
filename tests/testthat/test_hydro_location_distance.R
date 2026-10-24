test_that("get_confluences finds a tributary confluence as a hydrolocation on the receiver", {

  base <- dplyr::tibble(
    id   = c("a1", "a2", "a3", "b1", "b2"),
    toid = c("",   "a1", "a2", "a2", "b1"),
    aggregate_id = c("A", "A", "A", "B", "B"),
    length_km = c(1, 1, 1, 1, 1))

  conf <- get_confluences(hy(base))

  expect_equal(nrow(conf), 1)
  expect_equal(conf$aggregate_id, "A")
  expect_equal(conf$hydro_location_id, "B")
  expect_equal(conf$id, "a2")
  expect_equal(conf$id_measure, 100)
})

test_that("a divergence == 2 tributary warns and yields no confluence", {

  base <- dplyr::tibble(
    id   = c("a1", "a2", "a3", "b1", "b2"),
    toid = c("",   "a1", "a2", "a2", "b1"),
    aggregate_id = c("A", "A", "A", "B", "B"),
    length_km = c(1, 1, 1, 1, 1),
    divergence = c(0, 2, 0, 0, 0))

  expect_warning(conf <- get_confluences(hy(base)),
    "connect down a divergence")

  expect_equal(nrow(conf), 0)
})

test_that("a broken chain stops the frame naming the mainstem", {

  base <- dplyr::tibble(
    id   = c("a1", "a2", "a3", "b1", "b2"),
    toid = c("",   "a1", "a2", "a2", "b1"),
    aggregate_id = c("A", "A", "A", "B", "B"),
    length_km = c(1, 1, 1, 1, 1))

  # two runs given one mainstem id: b2 relabeled A but not linked into A
  two_runs <- base
  two_runs$aggregate_id[two_runs$id == "b2"] <- "A"
  expect_error(get_confluences(hy(two_runs)), "not continuous")

  # a toid set to the outlet value mid-chain breaks A above a2
  mid_outlet <- base
  mid_outlet$toid[mid_outlet$id == "a2"] <- ""
  expect_error(get_confluences(hy(mid_outlet)), "not continuous")
})

test_that("a bare hy gets a guided error, not a recursion", {

  bare <- data.frame(id = c("a1", "a2"), aggregate_id = c("A", "A"),
    length_km = c(1, 1))

  pos <- data.frame(id = "a1", id_measure = 50)
  hl <- data.frame(aggregate_id = "A", hydro_location_id = "B", id = "a1",
    id_measure = 100)
  rec <- data.frame(aggregate_id = "A", hydro_location_id = "B",
    distance_km = 0.5)

  expect_error(add_hydro_location_distance(pos, hl, bare),
    "add_hydro_location_distance\\(\\) requires hy_topo or hy_node")
  expect_error(index_hydro_location_distance(rec, hl, bare),
    "index_hydro_location_distance\\(\\) requires hy_topo or hy_node")
  expect_error(get_substring(pos, pos, bare),
    "get_substring\\(\\) requires hy_topo or hy_node")
})

test_that("round trip on V2 returns each flowline's own id and measure", {

  hv <- read_v2()
  conf <- get_confluences(hv)

  idx <- dplyr::tibble(id = hv$id[!is.na(hv$aggregate_id)], id_measure = 50)

  with_dist <- suppressMessages(add_hydro_location_distance(idx, conf, hv))

  on_ms <- with_dist[!is.na(with_dist$distance_km), ]

  back <- index_hydro_location_distance(
    dplyr::select(on_ms, aggregate_id, hydro_location_id, distance_km),
    conf, hv)

  expect_true(all(back$status == "placed"))
  expect_equal(back$id, on_ms$id)
  expect_equal(back$id_measure, rep(50, nrow(back)), tolerance = 1e-6)
})

test_that("a position equidistant between two referents takes the smaller hydro_location_id", {

  base <- dplyr::tibble(
    id   = c("a1", "a2", "a3", "a4", "a5"),
    toid = c("",   "a1", "a2", "a3", "a4"),
    aggregate_id = "A",
    length_km = 1)

  net <- hy(base)

  # a referent 1 km below and one 1 km above the position, on opposite sides
  hl <- dplyr::tibble(
    hydro_location_id = c("Z", "A"),
    id = c("a2", "a4"),
    id_measure = 50)

  pos <- dplyr::tibble(id = "a3", id_measure = 50)

  res <- add_hydro_location_distance(pos, hl, net)

  expect_equal(res$hydro_location_id, "A")
})

test_that("two coincident referents take the smaller hydro_location_id", {

  base <- dplyr::tibble(
    id   = c("a1", "a2", "a3"),
    toid = c("",   "a1", "a2"),
    aggregate_id = "A",
    length_km = 1)

  net <- hy(base)

  hl <- dplyr::tibble(
    hydro_location_id = c("Z", "A"),
    id = c("a2", "a2"),
    id_measure = 50)

  pos <- dplyr::tibble(id = "a2", id_measure = 50)

  res <- add_hydro_location_distance(pos, hl, net)

  expect_equal(res$hydro_location_id, "A")
  expect_equal(res$distance_km, 0)
})

test_that("the Northeast Creek mouth at distance 0 resolves on 3DHP", {

  tbl <- read_3dhp()
  if (is.null(tbl)) skip("3DHP sample not available")

  h3 <- hy(tbl)
  conf <- get_confluences(h3)

  ne_row <- conf[conf$hydro_location_id == ne, ]
  expect_equal(nrow(ne_row), 1)

  pos <- dplyr::tibble(aggregate_id = nh, hydro_location_id = ne,
    distance_km = 0)
  res <- index_hydro_location_distance(pos, conf, h3)

  expect_equal(res$status, "placed")
  expect_equal(res$id, ne_row$id)
  expect_equal(res$id_measure, 100, tolerance = 1e-6)
})

test_that("0.5 km above and below the Morgan Creek mouth resolve on 3DHP on the same sides", {

  tbl <- read_3dhp()
  if (is.null(tbl)) skip("3DHP sample not available")

  h3 <- hy(tbl)
  conf <- get_confluences(h3)

  mc_id <- conf$id[conf$hydro_location_id == mc]

  pos <- dplyr::tibble(aggregate_id = c(nh, nh),
    hydro_location_id = c(mc, mc), distance_km = c(0.5, -0.5))

  res <- index_hydro_location_distance(pos, conf, h3)

  expect_true(all(res$status == "placed"))
  # the downstream position (-0.5) sits on the confluence flowline;
  # the upstream position (+0.5) sits on a flowline above it
  expect_equal(res$id[2], mc_id)
  expect_false(res$id[1] == res$id[2])
})

test_that("a referent absent from x gives hydro_location_not_in_x", {

  tbl <- read_3dhp()
  if (is.null(tbl)) skip("3DHP sample not available")

  h3 <- hy(tbl)
  conf <- get_confluences(h3)

  pos <- dplyr::tibble(aggregate_id = nh,
    hydro_location_id = "no/such/uri", distance_km = 1)

  res <- index_hydro_location_distance(pos, conf, h3)

  expect_equal(res$status, "hydro_location_not_in_x")
  expect_true(is.na(res$id))
})

test_that("a truncated chain gives the same distance between two confluences", {

  tbl <- read_3dhp()
  if (is.null(tbl)) skip("3DHP sample not available")

  h3 <- hy(tbl)
  conf <- get_confluences(h3)

  fr_full <- hydroloom:::chain_frame(h3)

  nh_rows <- fr_full[fr_full$aggregate_id == nh, ]
  drop_ids <- nh_rows$id[order(nh_rows$from_km)][1:5]

  fr_trunc <- hydroloom:::chain_frame(hy(tbl[!tbl$id %in% drop_ids, ]))

  conf_nh <- conf[conf$aggregate_id == nh &
    conf$hydro_location_id %in% c(ne, mc), ]

  km_full <- hydroloom:::position_to_km(conf_nh$id, conf_nh$id_measure,
    fr_full)$km
  km_trunc <- hydroloom:::position_to_km(conf_nh$id, conf_nh$id_measure,
    fr_trunc)$km

  expect_equal(diff(km_full), diff(km_trunc), tolerance = 1e-6)
})

test_that("the hy_node form yields the same confluence, including z truncated above the node", {

  tbl <- read_3dhp()
  if (is.null(tbl)) skip("3DHP sample not available")

  h3 <- hy(tbl)
  conf_topo <- get_confluences(h3)
  ne_topo <- conf_topo[conf_topo$hydro_location_id == ne, ]

  # node form: add nodes, drop toid so it classifies hy_node
  nt <- sf::st_drop_geometry(suppressMessages(make_node_topology(h3)))
  nt$toid <- NULL
  hn <- hy(nt)
  expect_equal(hy_network_type(hn), "hy_node")

  ne_node <- get_confluences(hn)
  ne_node <- ne_node[ne_node$hydro_location_id == ne, ]
  expect_equal(ne_node$id, ne_topo$id)

  # truncate z (New Hope) above the node: remove all NH flowlines above t
  fr <- hydroloom:::chain_frame(h3)
  t_to_km <- fr$to_km[fr$id == ne_topo$id]
  nh_above <- fr$id[fr$aggregate_id == nh & fr$from_km >= t_to_km - 1e-9]

  hn2 <- hy(nt[!nt$id %in% nh_above, ])
  ne_node2 <- get_confluences(hn2)
  ne_node2 <- ne_node2[ne_node2$hydro_location_id == ne, ]

  expect_equal(ne_node2$id, ne_topo$id)
})
