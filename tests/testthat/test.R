test_that("s3 class creation", {
  x <- data.frame(comid = 1, fromnode = 2, tonode = 3)

  y <- hy(x)

  expect_equal(names(y), c("id", "fromnode", "tonode"))

  expect_s3_class(y, "hy")

  x <- dplyr::tibble(comid = c(1,2), tocomid = c(2, NA), fromnode = c(1, 2), tonode = c(2, 3))

  y <- hy(x)

  expect_true("orig_names" %in% names(attributes(y)))

  expect_true(inherits(y, "tbl"))

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

  expect_true(inherits(hy(x), "tbl"))

  expect_error(x <- hy_reverse(x))

  x <- sf::st_sf(dplyr::as_tibble(x))

  expect_equal(x, hy_reverse(hy(x)))
})

test_that("make fromid", {
  x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))

  x <- add_toids(x)

  x <- make_index_ids(x)

  y <- hydroloom:::make_fromids(x)

  expect_equal(names(y), c("froms", "lengths"))
  expect_true(is.matrix(y$froms))

  y <- hydroloom:::make_fromids(x, return_list = TRUE)

  expect_equal(names(y), c("froms", "lengths", "froms_list"))

  # manually verified
  expect_equal(y$froms[,10], c(12, 558, NA))
  expect_equal(as.numeric(y$lengths[10]), 2)

  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
                          toid = c(2, 3, 4, 0, 7, 8, 9, 4))

  test_data <- make_index_ids(test_data)

  expect_equal(ncol(hydroloom:::make_fromids(test_data)$froms),
               nrow(test_data$to_list))

  test_data <- data.frame(id = c(1, 2, 3, 4),
                          toid = c(2, 3, 4, 0))

  test_data <- make_index_ids(test_data)

  expect_equal(ncol(hydroloom:::make_fromids(test_data)$froms),
               nrow(test_data$to_list))
})


test_that("format toid", {
  x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))

  x <- add_toids(x, return_dendritic = FALSE)

  x <- make_index_ids(x)

  expect_equal(names(x), c("to", "lengths", "to_list"))
  expect_true(is.matrix(x$to))

  # manually verified
  expect_equal(x$to[,8], c(7, 575, NA))
  expect_equal(as.numeric(x$lengths[8]), 2)
})
