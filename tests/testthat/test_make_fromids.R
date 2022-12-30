test_that("make fromid", {
  x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))

  y <- add_toids(x)

  y <- make_index_ids(y)

  z <- make_fromids(y)

  expect_equal(names(z), c("froms", "lengths"))
  expect_true(is.matrix(z$froms))

  z <- make_fromids(y, return_list = TRUE)

  expect_equal(names(z), c("froms", "lengths", "froms_list"))

  # manually verified
  expect_equal(z$froms[,10], c(12, 558, NA))
  expect_equal(as.numeric(z$lengths[10]), 2)

  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
                          toid = c(2, 3, 4, 0, 7, 8, 9, 4))

  test_data <- make_index_ids(test_data)

  expect_equal(ncol(make_fromids(test_data)$froms),
               nrow(test_data$to_list))

  test_data <- data.frame(id = c(1, 2, 3, 4),
                          toid = c(2, 3, 4, 0))

  test_data <- make_index_ids(test_data)

  expect_equal(ncol(make_fromids(test_data)$froms),
               nrow(test_data$to_list))

  y <- add_toids(x, return_dendritic = FALSE)

  y <- make_index_ids(y)

  z <- make_fromids(y)

  expect_equal(z$froms[, which(y$to_list$id == 8893420)], c(25, 534, NA))
})
