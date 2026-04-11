test_that("add streamorder", {

  x <- sf::read_sf(system.file("extdata", "walker.gpkg", package = "hydroloom")) |>
    add_toids(return_dendritic = FALSE)

  expect_error(y <- add_streamorder(x))

  y <- dplyr::select(x, -StreamOrde, -StreamCalc)

  y <- add_streamorder(y)

  expect_equal(y$stream_order, x$StreamOrde)

  # also works with fronnode and tonode
  x <- sf::read_sf(system.file("extdata", "walker.gpkg", package = "hydroloom"))

  y <- x |>
    dplyr::select(-StreamOrde, -StreamCalc) |>
    add_streamorder()

  expect_equal(y$stream_order, x$StreamOrde)
  expect_equal(y$stream_calculator, x$StreamCalc)

  x <- readRDS(list.files(pattern = "network.rds", recursive = TRUE, full.names = TRUE)) |>
    add_toids(return_dendritic = FALSE)

  y <- select(x, -StreamOrde, -StreamCalc)

  y <- add_streamorder(y)

  expect_equal(y$stream_order,
    x$StreamOrde)

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")) |>
    add_toids(return_dendritic = FALSE)

  y <- select(x, -StreamOrde, -StreamCalc)

  y <- add_streamorder(y)

  expect_equal(y$stream_order,
    x$StreamOrde)

  # this catchment is downstream of a fourth order secondary path.
  # the first order primary path should dominate according to the
  # nhdplus algorithm.
  expect_equal(y$stream_order[y$COMID == 8893794], 1)

})

test_that("get_streamlevel", {

  x <- sf::read_sf(system.file("extdata", "walker.gpkg", package = "hydroloom")) |>
    add_toids()

  x$DnLevelPat[1] <- 0

  x <- dplyr::rename(x, orig_streamlevel = StreamLeve)

  expect_equal(x$orig_streamlevel, add_streamlevel(x)$stream_level)

  x$coastal <- rep(FALSE, nrow(x))
  expect_equal(x$orig_streamlevel + 3, add_streamlevel(x, coastal = "coastal")$stream_level)

  x$coastal[!x$DnLevelPat %in% x$LevelPathI] <- TRUE
  expect_equal(x$orig_streamlevel, add_streamlevel(x, coastal = "coastal")$stream_level)

})

test_that("exists bug #35", {

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")) |>
    hydroloom::add_toids() |>
    dplyr::select(-StreamOrde)

  net = data.frame()

  expect_true(is.data.frame(hydroloom::add_streamorder(x)))

})
