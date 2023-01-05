test_that("align_names", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x <- align_names(x)

  expect_true(all(c("id", "length_km", "aggregate_id") %in% names(x)))

  expect_true(is.character(hydroloom_name_definitions))
})

test_that("hydroloom_names", {
  hln <- hydroloom_names()

  expect_equal(hln, hydroloom:::hydroloom_env$hydroloom_name_map)

  hln_add <- hydroloom_names(c(foo = "bar"))

  expect_error(hydroloom_names(c("foobar")), "input must be named")

  expect_true(hln_add[["foo"]] == "bar")
  expect_true(hydroloom:::hydroloom_env$hydroloom_name_map[["foo"]] == "bar")

  hln_clear <- hydroloom_names(clear = TRUE)

  expect_equal(hln_clear, c())

  hln_reset <- hydroloom_names(hln)

  expect_equal(hln, hln_reset)
  expect_equal(hydroloom:::hydroloom_env$hydroloom_name_map, hln_reset)
})
