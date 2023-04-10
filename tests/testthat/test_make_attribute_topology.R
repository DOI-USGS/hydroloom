test_that("attribute topology basics", {

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  y <- dplyr::select(x, COMID)

  y <- sf::st_transform(y, 5070)

  z <- make_attribute_topology(y, 10)

  x <- add_toids(hy(x), return_dendritic = FALSE)

  expect_true(all(sapply(unique(x$id), function(i) {
    xtid <- order(x[x$id == i,]$toid)
    ztid <- order(z[z$id == i,]$toid)

    all(xtid == ztid)
  })))

})
