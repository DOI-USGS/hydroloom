test_that("s3 class creation", {
  x <- data.frame(comid = 1, fromnode = 2, tonode = 3)

  y <- hy(x)

  expect_equal(names(y), c("id", "fromnode", "tonode"))

  expect_s3_class(y, "hy")

  x <- dplyr::tibble(comid = c(1, 2), tocomid = c(2, NA), fromnode = c(1, 2), tonode = c(2, 3))

  y <- hy(x)

  expect_true("orig_names" %in% names(attributes(y)))

  expect_true(inherits(y, "tbl"))

  expect_equal(y$toid, c(2, 0))

  expect_true(is.hy(y))

  expect_message(expect_false(is.hy(unclass(y))), "no hy class attribute")

  y$toid[1] <- NA

  expect_message(expect_false(is.hy(y)), "some na toids")

  y <- hy(x)

  attr(y, "orig_names") <- NULL

  expect_message(expect_false(is.hy(y)), "no original names attribute")

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  expect_s3_class(hy(x), "sf")

  expect_false(inherits(hy(x, clean = TRUE), "sf"))

  expect_true(inherits(hy(x), "tbl"))

  expect_message(expect_error(x <- hy_reverse(x)), "no hy class attribute")

  x <- sf::st_sf(dplyr::as_tibble(x))

  expect_equal(x, hy_reverse(hy(x)))
})

test_that("test print", {

  p <- capture_output(print(hydroloom_name_definitions))

  expect_true(grepl("topo_sort", p))

})
