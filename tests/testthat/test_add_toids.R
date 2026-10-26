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
