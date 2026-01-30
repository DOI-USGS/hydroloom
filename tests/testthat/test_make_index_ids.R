test_that("add indid", {

  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- hy(g)

  y <- add_toids(x)

  y <- make_index_ids(y)

  expect_equal(y$to[1, which(!y$to_list$toindid %in% unlist(y$to_list$indid))], 0)

  z <- add_toids(g)

  z <- make_index_ids(z)

  expect_equal(z$to_list, y$to_list)

  expect_equal(y$to_list$indid, seq_len(nrow(x)))

  expect_true(all(unlist(y$to_list$toindid) %in% c(y$to_list$indid, 0)))

  y <- add_toids(x, return_dendritic = FALSE)

  y <- make_index_ids(y)

  expect_true(all(y$to_list$indid %in% seq_len(nrow(x))))

  expect_true(all(unlist(y$to_list$toindid) %in% c(y$to_list$indid, 0)))

  y <- add_toids(x)

  y <- make_index_ids(y)

  expect_equal(names(y), c("to", "lengths", "to_list"))

  expect_equal(length(y$to_list$indid), length(unique(x$id)))

  z <- add_toids(x)

  expect_warning({
    z <- make_index_ids(z, long_form = TRUE)
  }, "long_form is deprecated and will be removed in a future release.")

  expect_equal(names(z), c("id", "indid", "toindid"))

  expect_warning(zz <- format_index_ids(z), "deprecated")

  expect_equal(names(zz), c("to", "lengths"))

  suppressWarnings(z <- format_index_ids(z, return_list = TRUE))

  expect_equal(z, y)

  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
    toid = c(2, 3, 4, 9, 7, 8, 9, 4))

  expect_error(hydroloom:::make_index_ids(test_data),
    "found one or more pairs of features that reference eachother.")

  x$id <- as.character(x$id)

  y <- add_toids(x)

  z <- make_index_ids(y)

  expect_equal(class(z$to_list$toindid[[1]]), "integer")

  expect_error(make_index_ids(y, mode = "borked"), "mode must be one of 'to', 'from', 'both', or 'none'.")

})

test_that("both option", {

  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- hy(g)

  y <- add_toids(x)

  y <- make_index_ids(y, mode = "both")

  expect_equal(names(y), c("to", "from"))

  expect_equal(ncol(y$to$to), ncol(y$from$froms))

  expect_equal(y$to$to_list$id, y$from$froms_list$id)

  expect_equal(dim(y$to$to), c(1, 746))

  expect_equal(dim(y$from$froms), c(3, 746))

})

test_that("none option", {

  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- hy(g)

  y <- add_toids(x)

  y <- make_index_ids(y, mode = "none")

  expect_equal(names(y), c("link", "lengths", "link_list"))

  expect_true(is.matrix(y$link))

  expect_equal(dim(y$link), c(4, 746))

})

test_that("with downmain", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x_dend <- hy(x) |>
    add_toids(return_dendritic = TRUE) |>
    to_flownetwork(warn_dendritic = FALSE)

  i <- make_index_ids(x_dend)

  x <- to_flownetwork(x)

  i <- make_index_ids(x)

  expect_contains(names(i), "main")

  expect_contains(names(i$to_list), "main")

  suppressWarnings(j <- make_fromids(i, upmain = distinct(select(x, id, upmain)), return_list = TRUE))

  expect_contains(names(j), "main")

  expect_contains(names(j$froms_list), "main")

  k <- make_index_ids(x, mode = "from")

  expect_equal(names(j), names(k))
  expect_equal(names(j$froms_list), names(k$froms_list))

  expect_equal(j$froms_list$id, k$froms_list$id)
  expect_equal(j$froms_list$indid, k$froms_list$indid)
  expect_equal(j$froms_list$toindid, k$froms_list$toindid)

  expect_equal(j$froms_list$main, k$froms_list$main)

  expect_equal(j$froms, k$froms)
  expect_equal(j$lengths, k$lengths)
})

test_that("with upmain", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
  
  x <- to_flownetwork(x)
  
  i <- make_index_ids(x)
  
  expect_equal(names(i), c("to", "lengths", "main", "to_list"))
  
  expect_equal(class(i$main[1,1]), "logical")
  
  expect_equal(dim(i$main), dim(i$to))
  
})

test_that("format toid", {
  x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))

  x <- add_toids(x, return_dendritic = FALSE)

  x <- make_index_ids(x)

  expect_equal(names(x), c("to", "lengths", "to_list"))
  expect_true(is.matrix(x$to))

  # manually verified
  expect_equal(x$to[, 8], c(7, 575, NA))
  expect_equal(as.numeric(x$lengths[8]), 2)
})

test_that("make fromid", {
  x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))

  y <- add_toids(x)

  z <- make_index_ids(y)

  expect_warning(z <- make_fromids(z))

  expect_equal(names(z), c("froms", "lengths"))
  expect_true(is.matrix(z$froms))

  z <- make_index_ids(y, mode = "from")

  expect_equal(names(z), c("froms", "lengths", "froms_list"))

  # manually verified
  expect_equal(z$froms[, 10], c(12, 558, NA))
  expect_equal(as.numeric(z$lengths[10]), 2)

  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
    toid = c(2, 3, 4, 0, 7, 8, 9, 4))

  test_data <- make_index_ids(test_data)

  expect_equal(suppressWarnings(ncol(make_fromids(test_data)$froms)),
    nrow(test_data$to_list))

  test_data <- data.frame(id = c(1, 2, 3, 4),
    toid = c(2, 3, 4, 0))

  test_data <- make_index_ids(test_data)

  expect_equal(suppressWarnings(ncol(make_fromids(test_data)$froms)),
    nrow(test_data$to_list))

  y <- add_toids(x, return_dendritic = FALSE)

  y <- make_index_ids(y, mode = "both")

  expect_equal(y$from$froms[, which(y$to$to_list$id == 8893420)], c(25, 534, NA))
})
