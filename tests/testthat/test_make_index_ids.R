test_that("add indid", {

  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- hy(g)

  y <- add_toids(x)

  y <- make_index_ids(y)

  z <- add_toids(g)

  z <- make_index_ids(z)

  expect_equal(z$to_list, y$to_list)

  expect_equal(y$to_list$indid, 1:nrow(x))

  expect_true(all(unlist(y$to_list$toindid) %in% c(y$to_list$indid, 0)))

  y <- add_toids(x, return_dendritic = FALSE)

  y <- make_index_ids(y)

  expect_true(all(y$to_list$indid %in% 1:nrow(x)))

  expect_true(all(unlist(y$to_list$toindid) %in% c(y$to_list$indid, 0)))

  y <- add_toids(x)

  y <- make_index_ids(y)

  expect_equal(names(y), c("to", "lengths", "to_list"))

  expect_equal(length(y$to_list$indid), length(unique(x$id)))

  z <- add_toids(x)

  z <- make_index_ids(z, long_form = TRUE)

  expect_equal(names(z), c("id", "indid", "toindid"))

  zz <- format_index_ids(z)

  expect_equal(names(zz), c("to", "lengths"))

  z <- format_index_ids(z, return_list = TRUE)

  expect_equal(z, y)

  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
                          toid = c(2, 3, 4, 9, 7, 8, 9, 4))

  expect_error(hydroloom:::make_index_ids(test_data),
               "found one or more pairs of features that reference eachother.")

  x$id <- as.character(x$id)

  y <- add_toids(x)

  y <- make_index_ids(y)

  expect_equal(class(y$to_list$toindid[[1]]), "integer")
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
