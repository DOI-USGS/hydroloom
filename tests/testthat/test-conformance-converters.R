# Round trips for the four expectation-layer converters (SPEC S-7) through
# GeoPackage, and the comparison rules (D-13, D-14).

round_trip <- function(layer) {

  f <- tempfile(fileext = ".gpkg")
  on.exit(unlink(f))

  write_layer(layer, f, "case")
  read_layer(f, "case")
}

test_that("frame layer round trips with types", {
  skip_if_no_conformance()

  x <- data.frame(
    id = c(10, 20, 30),
    toid = c(20, 30, 0),
    length_km = c(1.5, 2.25, NA),
    stream_order = c(1, 1, 2),
    terminal_flag = c(FALSE, FALSE, TRUE),
    feature_type = c("a", NA, "c"),
    extra = c(0.1, 0.2, 0.3))

  layer <- frame_to_layer(x)

  expect_identical(layer$id, c("10", "20", "30"))
  expect_identical(layer$stream_order, c(1L, 1L, 2L))

  back <- round_trip(layer)

  expect_identical(vapply(back, typeof, ""), vapply(layer, typeof, ""))
  expect_identical(compare_layers(layer, back), character())
  expect_identical(compare_layers(frame_to_layer(x), back), character())

  y <- data.frame(id = c("a", "b"), toid = c("b", ""))
  expect_identical(round_trip(frame_to_layer(y))$toid, c("b", ""))

  z <- frame_to_layer(x[0, ])
  zb <- round_trip(z)
  expect_identical(nrow(zb), 0L)
  expect_identical(vapply(zb, typeof, ""), vapply(z, typeof, ""))
})

test_that("a count beyond the 32-bit range is kept as double", {
  skip_if_no_conformance()

  x <- data.frame(id = 1:2, topo_sort = c(550000100012345, 2), stream_order = c(1, 2))

  layer <- expect_silent(frame_to_layer(x))

  expect_identical(layer$topo_sort, c(550000100012345, 2))
  expect_identical(layer$stream_order, c(1L, 2L))
  expect_identical(compare_layers(layer, round_trip(layer)), character())

  y <- x
  y$topo_sort[1] <- 550000100012346
  expect_match(compare_layers(frame_to_layer(y), layer), "topo_sort")
})

test_that("frame layer round trips with geometry", {
  skip_if_no_conformance()

  x <- sf::st_sf(id = 1:2, toid = c(2L, 0L),
    geom = sf::st_sfc(sf::st_point(c(1000.5, 2000.25)), sf::st_point(c(3, 4)), crs = 5070))

  layer <- frame_to_layer(x)
  back <- round_trip(layer)

  expect_s3_class(back, "sf")
  expect_identical(compare_layers(layer, back), character())

  moved <- x
  sf::st_geometry(moved) <- sf::st_sfc(sf::st_point(c(1000.5, 2000.3)), sf::st_point(c(3, 4)), crs = 5070)
  expect_match(compare_layers(frame_to_layer(moved), back), "geometry")
})

test_that("graph layer round trips", {
  skip_if_no_conformance()

  x <- data.frame(id = c(1, 2, 3, 4, 5), toid = c(3, 3, 5, 5, 0))

  to <- graph_to_layer(make_index_ids(x, mode = "to"))

  expect_identical(names(to), c("id", "position", "neighbor"))
  expect_identical(nrow(to), 4L)

  g <- graph_from_layer(round_trip(to))
  expect_identical(g$id, c("1", "2", "3", "4"))
  expect_identical(unclass(g$neighbor), list("3", "3", "5", "5"))

  from <- graph_from_layer(round_trip(graph_to_layer(make_index_ids(x, mode = "from"))))
  expect_identical(from$id, c("3", "5"))
  expect_identical(lapply(from$neighbor, sort), list(c("1", "2"), c("3", "4")))

  both <- graph_to_layer(make_index_ids(x, mode = "both"))
  expect_identical(sort(unique(both$mode)), c("from", "to"))
  expect_identical(compare_layers(both, round_trip(both)), character())

  # a divergence with downmain carries per-edge main
  d <- data.frame(id = c(1, 1, 2, 3), toid = c(2, 3, 4, 4),
    downmain = c(TRUE, FALSE, TRUE, TRUE))

  dl <- graph_to_layer(make_index_ids(d))
  expect_identical(typeof(dl$main), "logical")

  db <- round_trip(dl)
  expect_identical(compare_layers(dl, db), character())

  dg <- graph_from_layer(db)
  expect_identical(dg$neighbor[[1]], c("2", "3"))
  expect_identical(dg$main[[1]], c(TRUE, FALSE))
})

test_that("traversal layer round trips with the path split", {
  skip_if_no_conformance()

  r <- list(list(`1` = c(1L, 2L, 4L), `2` = 3L), list())
  layer <- traversal_to_layer(r, c(1, 9))

  expect_identical(layer$start, rep("1", 4))
  expect_identical(layer$path, c(0L, 0L, 0L, 1L))
  expect_identical(layer$order, 0:3)
  expect_identical(layer$id, c("1", "2", "4", "3"))

  back <- round_trip(layer)
  expect_identical(compare_layers(layer, back), character())
  expect_identical(traversal_from_layer(back, c(1, 9)),
    list(list(c("1", "2", "4"), "3"), list()))

  x <- data.frame(id = c(1, 1, 2, 3, 4), toid = c(2, 3, 4, 4, 0))
  nav <- navigate_network_dfs(x, 1)
  nl <- traversal_to_layer(nav, 1)

  expect_identical(length(unique(nl$path)), length(nav[[1]]))
  expect_identical(traversal_from_layer(round_trip(nl), 1),
    lapply(nav, function(s) unname(lapply(s, id_to_text))))
})

test_that("scalar layer round trips", {
  skip_if_no_conformance()

  for (v in list(TRUE, "hy_topo", 3L, 2.5)) {
    back <- round_trip(scalar_to_layer(v))
    expect_identical(names(back), "value")
    expect_identical(scalar_from_layer(back), v)
  }
})

test_that("comparison follows tolerance, order, and column-name rules", {
  skip_if_no_conformance()

  e <- frame_to_layer(data.frame(id = c(1, 2), length_km = c(1, 2), total_da_sqkm = c(5, 6)))

  near <- frame_to_layer(data.frame(id = c(1, 2), length_km = c(1.0009, 2), total_da_sqkm = c(5, 6)))
  far <- frame_to_layer(data.frame(id = c(1, 2), length_km = c(1.002, 2), total_da_sqkm = c(5, 6)))

  expect_identical(compare_layers(near, e, compare = "tolerance"), character())
  expect_match(compare_layers(near, e), "length_km")
  expect_match(compare_layers(far, e, compare = "tolerance"), "length_km")
  expect_identical(compare_layers(far, e, compare = "tolerance", atol = 0.01), character())

  # D-14: columns by name, not position
  expect_identical(compare_layers(e[, c("total_da_sqkm", "id", "length_km")], e), character())

  # rows in another order pass only under order: ignore
  flipped <- e[c(2, 1), ]
  expect_match(compare_layers(flipped, e), "differ")
  expect_identical(compare_layers(flipped, e, order = "ignore", key = "id"), character())

  expect_match(compare_layers(e[, c("id", "length_km")], e), "missing column")
})

test_that("named vectors go through the frame converter (D3-13)", {
  skip_if_no_conformance()

  defs <- frame_to_layer(hydroloom_name_definitions)

  expect_identical(names(defs), c("name", "definition"))
  expect_identical(defs$name, names(hydroloom_name_definitions))
  expect_identical(defs$definition, unname(as.character(unclass(hydroloom_name_definitions))))

  back <- round_trip(defs)
  expect_identical(back$name, defs$name)
  expect_identical(compare_layers(defs, back), character())

  v <- c(b = 2L, a = 1L, c = NA)
  vl <- frame_to_layer(v)

  expect_identical(names(vl), c("name", "value"))
  expect_identical(vl$name, c("b", "a", "c"))
  expect_identical(vl$value, c(2L, 1L, NA))

  vb <- round_trip(vl)
  expect_identical(vapply(vb, typeof, ""), vapply(vl, typeof, ""))
  expect_identical(compare_layers(vl, vb), character())

  s <- frame_to_layer(c(comid = "id", tocomid = "toid"))
  expect_identical(s$value, c("id", "toid"))
})

test_that("a bare TRUE goes through the frame converter as the scalar layer (D3-53)", {
  skip_if_no_conformance()

  layer <- frame_to_layer(TRUE)
  expect_identical(layer, scalar_to_layer(TRUE))

  back <- round_trip(layer)
  expect_identical(scalar_from_layer(back), TRUE)
  expect_identical(compare_layers(layer, back), character())

  rows <- frame_to_layer(data.frame(id = 5:6, toid = 6:5, row = 5:6))
  expect_match(compare_layers(rows, back), "missing column: value", all = FALSE)
})

test_that("case args keep a null start and a large integer", {
  skip_if_no_conformance()
  skip_if_not_installed("yaml")

  f <- tempfile(fileext = ".yaml")
  on.exit(unlink(f))
  writeLines(c("a: {starts: [null]}", "b: {starts: []}", "c: {starts: [15000500004891, 2]}",
    "d: {starts: [1, null]}"), f)

  y <- read_conformance_yaml(f)

  expect_identical(case_args(y$a)$starts, NA)
  expect_null(case_args(y$b)$starts)
  expect_identical(case_args(y$c)$starts, c(15000500004891, 2))
  expect_identical(case_args(y$d)$starts, c(1L, NA))
})
