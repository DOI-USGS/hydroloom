test_that("make toid", {

  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- hy(g)

  z <- add_toids(g, FALSE)

  expect_equal(names(z)[1], "COMID")

  y <- add_toids(x, TRUE)

  names(x)[names(x) == "divergence"] <- "div"

  expect_error(add_toids(x), "To remove non dendritic paths, a divergence attribute is required.")

  expect_error(add_toids(y), "already has toid")

  g$COMID <- as.character(g$COMID)

  z <- add_toids(g, TRUE)

  expect_true(inherits(z$toid, "character"))
})

test_that("tolerated divergence codings warn naming rows by id", {

  # id 1 splits at node 20 into id 2 and id 3, rejoining at node 50
  x <- data.frame(
    id = 1:5,
    fromnode = c(10L, 20L, 20L, 30L, 40L),
    tonode = c(20L, 30L, 40L, 50L, 50L),
    divergence = c(0L, 0L, 2L, 0L, 0L))

  expect_warning(z <- add_toids(hy(x)),
    "coded 0 and was read as the main path: 2\\.")

  expect_equal(z$toid, c(2L, 4L, 5L, 0L, 0L))

  # main path id 2 clipped away: id 3, coded 2, is the only flowline below id 1
  x <- data.frame(
    id = c(1L, 3L, 4L, 5L),
    fromnode = c(10L, 20L, 30L, 40L),
    tonode = c(20L, 40L, 50L, 50L),
    divergence = c(0L, 2L, 0L, 0L))

  expect_warning(z <- add_toids(hy(x)),
    "the only flowline downstream of the flowline above it: 3\\.")

  expect_equal(z$toid, c(3L, 5L, 0L, 0L))

  expect_equal(z$fromnode, x$fromnode)
})
