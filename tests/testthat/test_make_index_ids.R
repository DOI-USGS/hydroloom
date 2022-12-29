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

  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
                          toid = c(2, 3, 4, 9, 7, 8, 9, 4))

  expect_error(hydroloom:::make_index_ids(test_data),
               "found one or more pairs of features that reference eachother.")
})
