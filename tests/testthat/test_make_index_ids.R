test_that("add indid", {

  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- hy(g)

  y <- add_toids(x)

  y <- make_index_ids(y)

  z <- add_toids(g)

  z <- make_index_ids(z)

  expect_equal(z$toindid, y$toindid)

  expect_equal(y$indid, 1:nrow(y))

  expect_true(all(y$toindid %in% c(y$indid, 0)))

  y <- add_toids(x, return_dendritic = FALSE)

  y <- make_index_ids(y)

  expect_true(all(y$indid %in% 1:nrow(y)))

  expect_true(all(y$toindid %in% c(y$indid, 0)))

  y <- add_toids(x)

  y <- make_index_ids(y, format = TRUE, complete = TRUE)

  expect_equal(names(y), c("to", "lengths", "to_list"))

  expect_equal(length(y$to_list$indid), length(unique(x$id)))
})
