test_that("compatibalize", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
  one <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  attr(one, "sf_column") <- "geotest"
  names(one)[names(one) == "geom"] <- "geotest"

  two <- sf::st_transform(x, 5070)

  three <- st_compatibalize(one, two)

  expect_equal(sf::st_crs(two), sf::st_crs(three))

  expect_true(all(names(two) == names(three)))

})

test_that("drop_geometry", {

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  expect_true("sf" %in% class(x))

  x <- sf::st_drop_geometry(x)

  expect_true(!"sf" %in% class(x))

  x <- sf::st_drop_geometry(x)

  expect_true(!"sf" %in% class(x))
})

test_that("rname geometry", {
  g <- sf::st_sf(a=3, geo = sf::st_sfc(sf::st_point(1:2)))

  g <- rename_geometry(g, "geometry")

  expect_true("geometry" %in% names(g))

  expect_equal(attr(g, "sf_column"), "geometry")

})

test_that("get_node", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  start <- get_node(x, "start")
  end <- get_node(x, "end")

  # plot(sf::st_zm(x$geom[1]))
  # plot(sf::st_geometry(start)[1], add = TRUE)
  # plot(sf::st_geometry(end)[1], add = TRUE)
  # dput(sf::st_coordinates(sf::st_geometry(start)[1]))
  # dput(sf::st_coordinates(sf::st_geometry(end)[1]))

  expect_equal(sf::st_coordinates(sf::st_geometry(start)[1]),
               structure(c(1518702.12558262, 1557297.72465482), dim = 1:2, dimnames = list(
                 "1", c("X", "Y"))), ignore_attr = TRUE)

  expect_equal(sf::st_coordinates(sf::st_geometry(end)[1]),
               structure(c(1517348.69555168, 1556089.85144106), dim = 1:2, dimnames = list(
                 "1", c("X", "Y"))), ignore_attr = TRUE)

  x <- suppressWarnings(sf::st_cast(x, "LINESTRING"))

  start <- get_node(x, "start")
  end <- get_node(x, "end")

  expect_equal(sf::st_coordinates(sf::st_geometry(start)[1]),
               structure(c(1518702.12558262, 1557297.72465482), dim = 1:2, dimnames = list(
                 "1", c("X", "Y"))), ignore_attr = TRUE)

  expect_equal(sf::st_coordinates(sf::st_geometry(end)[1]),
               structure(c(1517348.69555168, 1556089.85144106), dim = 1:2, dimnames = list(
                 "1", c("X", "Y"))), ignore_attr = TRUE)
})

test_that("fix_flowdir", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- add_toids(hy(x))

  # Look at the end node of the 10th line.
  n1 <- get_node(x[10, ], position = "end")

  # Break the geometry by reversing it.
  sf::st_geometry(x)[10] <- sf::st_reverse(sf::st_geometry(x)[10])

  # Note that the end node is different now.
  n2 <- get_node(x[10, ], position = "end")

  # Pass the broken geometry to fix_flowdir with the network for toCOMID
  sf::st_geometry(x)[10] <- fix_flowdir(x$id[10], x)

  # Note that the geometry is now in the right order.
  n3 <- get_node(x[10, ], position = "end")

  expect_equal(n1, n3)

  n1 <- get_node(x[1, ], position = "end")
  sf::st_geometry(x)[1] <- sf::st_reverse(sf::st_geometry(x)[1])
  sf::st_geometry(x)[1] <- fix_flowdir(x$id[1], x)
  expect_equal(n1, get_node(x[1, ], position = "end"))

  x$toid[707] <- 0
  n1 <- get_node(x[707, ], position = "end")
  sf::st_geometry(x)[707] <- sf::st_reverse(sf::st_geometry(x)[707])
  sf::st_geometry(x)[707] <- fix_flowdir(x$id[707], x)
  expect_equal(n1, get_node(x[707, ], position = "end"))

  fn_list <- list(flowline = x[707, ],
                  network = x[x$toid == x$id[707],],
                  check_end = "start")

  sf::st_geometry(x)[707] <- sf::st_reverse(sf::st_geometry(x)[707])
  sf::st_geometry(x)[707] <- fix_flowdir(x$id[707], fn_list = fn_list)
  expect_equal(n1, get_node(x[707, ], position = "end"))
})

test_that("rescale", {
  expect_equal(rescale_measures(50, 50, 100), 0)
  expect_equal(rescale_measures(49.95, 50, 100), 0)
  expect_equal(rescale_measures(50, 0, 50), 100)
  expect_equal(rescale_measures(50.01, 0, 50), 100)
  expect_equal(rescale_measures(25, 0, 50), 50)
  expect_error(rescale_measures(75, 0, 50), "measure must be between from and to")
})

