test_that("align_names", {
  x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

  x$TotDA <- x$TotDASqKM

  expect_warning(x <- align_names(x), "Duplicate names found when aligning with hydroloom")

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

  expect_equal(hln_clear, hydroloom:::vocab_alias_map())
  expect_equal(hydroloom:::hydroloom_env$hydroloom_name_map, hln_clear)
})

test_that("hydroloom_names clear = TRUE resets to the vocabulary aliases plus x (H-03, D3-20)", {
  on.exit(hydroloom_names(clear = TRUE))

  hydroloom_names(c(foo = "bar"))

  out <- hydroloom_names(c(FlowlineID = "id"), clear = TRUE)

  expect_equal(out, c(hydroloom:::vocab_alias_map(), c(flowlineid = "id")))
  expect_false("foo" %in% names(out))
  expect_equal(hydroloom:::hydroloom_env$hydroloom_name_map, out)

  x <- data.frame(FlowlineID = c(1, 2), TOCOMID = c(2, 0))

  expect_equal(names(hy(x))[1:2], c("id", "toid"))
})

test_that("hydroloom_names lowercases a registered alias (D3-65)", {
  on.exit(hydroloom_names(clear = TRUE))

  out <- hydroloom_names(c(FlowlineID = "id"))

  expect_equal(out[[length(out)]], "id")
  expect_equal(names(out)[length(out)], "flowlineid")

  x <- data.frame(FlowlineID = c(1, 2), toid = c(2, 0))

  expect_equal(names(hy(x))[1], "id")
})
