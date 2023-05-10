test_that("attribute topology basics", {

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  # select just the ID and the geometry
  y <- dplyr::select(x, COMID)

  # meters
  y <- sf::st_transform(y, 5070)

  # generate a network from the geometry
  z <- make_attribute_topology(hy(y), 10)

  expect_equal(sum(z$toid == 0), 1)

  # add toids from the source network fromnode tonode
  x <- add_toids(hy(x), return_dendritic = FALSE)

  # make sure that all are the same from scratch
  expect_true(all(sapply(unique(x$id), function(i) {
    xtid <- order(x[x$id == i,]$toid)
    ztid <- order(z[z$id == i,]$toid)

    all(xtid == ztid)
  })))

  expect_error(make_node_topology(z),
               "duplicate identifiers found and 'add_div' is not TRUE")

  # add nodes to network from geometry
  z <- make_node_topology(z, add_div = TRUE)

  # add toids based on those nodes.
  a <- add_toids(z, return_dendritic = FALSE)

  # make sure we get the same toids!
  all(z$toid == a$toid)
})
