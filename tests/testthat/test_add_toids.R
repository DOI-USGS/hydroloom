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

  names(y)[names(y) == "divergence"] <- "div"

  expect_error(add_toids(y))
})
