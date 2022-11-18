test_that("s3 class creation", {
  x <- data.frame(comid = 1, fromnode = 2, tonode = 3)

  y <- hy(x)

  expect_equal(names(y), c("id", "fromnode", "tonode"))

  expect_s3_class(y, "hy")

  x <- data.frame(comid = c(1,2), tocomid = c(2, NA), fromnode = c(1, 2), tonode = c(2, 3))

  y <- hy(x)

  expect_equal(y$toid, c(2,0))
})


test_that("make toid", {
  x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))

  y <- add_toids(x, FALSE)

  expect_true(nrow(y) > nrow(x))

  expect_true(!any(is.na(y$toid)))

  y <- add_toids(x, TRUE)

  expect_true(nrow(y) == nrow(x))

  expect_true(!any(is.na(y$toid)))
})

test_that("add indid", {
  x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))

  y <- add_toids(x)

  y <- make_index_ids(y)

  expect_equal(y$indid, 1:nrow(y))

  expect_true(all(y$toindid %in% c(y$indid, 0)))

  y <- add_toids(x, return_dendritic = FALSE)

  y <- make_index_ids(y)

  expect_equal(y$indid, 1:nrow(y))

  expect_true(all(y$toindid %in% c(y$indid, 0)))

})
