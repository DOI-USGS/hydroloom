test_that("s3 class creation", {
  x <- data.frame(comid = 1, fromnode = 2, tonode = 3)

  y <- hy(x)

  expect_equal(names(y), c("id", "fromnode", "tonode"))

  expect_s3_class(y, "hy")

  x <- data.frame(comid = c(1,2), tocomid = c(2, NA), fromnode = c(1, 2), tonode = c(2, 3))

  y <- hy(x)

  expect_true("orig_names" %in% names(attributes(y)))

  expect_equal(y$toid, c(2,0))

  expect_true(is.hy(y))

  expect_false(is.hy(unclass(y)))

  y$toid[1] <- NA

  expect_false(is.hy(y))

  y <- hy(x)

  attr(y, "orig_names") <- NULL

  expect_false(is.hy(y))

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  expect_s3_class(hy(x), "sf")

  expect_false(inherits(hy(x, clean = TRUE), "sf"))
})

test_that("drop_geometry", {

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  expect_true("sf" %in% class(x))

  x <- hydroloom:::drop_geometry(x)

  expect_true(!"sf" %in% class(x))

  x <- hydroloom:::drop_geometry(x)

  expect_true(!"sf" %in% class(x))
  })

test_that("make toid", {
  x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))

  y <- add_toids(x, FALSE)

  expect_true(nrow(y) > nrow(x))

  expect_true(!any(is.na(y$toid)))

  y <- add_toids(x, TRUE)

  expect_true(nrow(y) == nrow(x))

  expect_true(!any(is.na(y$toid)))

  names(y)[names(y) == "divergence"] <- "div"

  expect_error(add_toids(y))
})

test_that("add indid", {
  x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))

  y <- add_toids(x)

  y <- make_index_ids(y)

  expect_equal(y$indid, 1:nrow(y))

  expect_true(all(y$toindid %in% c(y$indid, 0)))

  y <- add_toids(x, return_dendritic = FALSE)

  y <- make_index_ids(y)

  expect_true(all(y$indid %in% 1:nrow(y)))

  expect_true(all(y$toindid %in% c(y$indid, 0)))

})

test_that("make fromid", {
  x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))

  x <- add_toids(x)

  x <- make_index_ids(x)

  y <- hydroloom:::make_fromids(x)

  expect_equal(names(y), c("froms", "lengths"))
  expect_is(y$froms, "matrix")

  y <- hydroloom:::make_fromids(x, return_list = TRUE)

  expect_equal(names(y), c("froms", "lengths", "froms_list"))

  # manually verified
  expect_equal(y$froms[,10], c(12, 558, NA))
  expect_equal(as.numeric(y$lengths[10]), 2)
})


test_that("format toid", {
  x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))

  x <- add_toids(x, return_dendritic = FALSE)

  x <- make_index_ids(x)

  y <- hydroloom:::format_nonden_toid(x)

  expect_equal(names(y), c("to", "lengths"))
  expect_is(y$to, "matrix")

  y <- hydroloom:::format_nonden_toid(x, return_list = TRUE)

  expect_equal(names(y), c("to", "lengths", "to_list"))

  # manually verified
  expect_equal(y$to[,8], c(7, 575, NA))
  expect_equal(as.numeric(y$lengths[8]), 2)
})
