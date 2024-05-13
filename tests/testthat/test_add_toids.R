test_that("make toid", {

  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- hy(g)

  y <- add_toids(x, FALSE)

  z <- add_toids(g, FALSE)

  expect_equal(y$toid, z$toid)

  expect_equal(names(z)[1], "COMID")

  expect_true(nrow(y) > nrow(x))

  expect_true(!any(is.na(y$toid)))

  y <- add_toids(x, TRUE)

  expect_true(nrow(y) == nrow(x))

  expect_true(!any(is.na(y$toid)))

  names(x)[names(x) == "divergence"] <- "div"

  expect_error(add_toids(x), "To remove non dendritic paths, a divergence attribute is required.")

  expect_error(add_toids(y), "network already contains a toid attribute")

  g$COMID <- as.character(g$COMID)

  z <- add_toids(g, TRUE)

  expect_true(inherits(z$toid, "character"))

  expect_equal(which(y$toid == 0), which(z$toid == ""))
})

test_that("don't change fromnode", {
  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- add_toids(g, return_dendritic = TRUE)

  expect_equal(g$FromNode, x$FromNode)
})
