test_that("accumulate downstream", {
  test_data <- data.frame(id = c(1, 2, 3, 4, 6, 7, 8, 9),
                          toid = c(2, 3, 4, 0, 7, 8, 9, 4),
                          a = c(1, 2, 3, 4, 1, 2, 3, 4))

  expect_equal(accumulate_downstream(test_data, "a"),
               c(1, 3, 6, 20, 1, 3, 6, 10))


  expect_error(accumulate_downstream(test_data, "b"), "b must be in x")

  expect_error(accumulate_downstream(dplyr::rename(test_data, borked = id), "a"),
               "accumulation requires id, toid hydroloom attributes.")

  x <- sf::read_sf(system.file("extdata", "walker.gpkg", package = "hydroloom"))

  x <- add_toids(x, return_dendritic = TRUE)

  y <- accumulate_downstream(x, "AreaSqKM")

  expect(mean(abs(y - x$TotDASqKM)) < 1e-3, "drainage area not close enough")
  expect(max(abs(y - x$TotDASqKM)) < 1e-2, "drainage area not close enough")

  x$AreaSqKM[1] <- NA

  expect_warning(accumulate_downstream(x, "AreaSqKM"), "NA values found, accumulation math may fail.")
})
