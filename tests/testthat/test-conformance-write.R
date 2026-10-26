# Write mode leaves an expectation file alone when its layers already match
# (D2-20) and rewrites a layer that is missing or changed.

test_that("write mode rewrites an expectation file only on a missing or changed layer", {
  skip_if_no_conformance()

  dir <- tempfile("conformance")
  dir.create(file.path(dir, "fixtures"), recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE))

  fixture <- file.path(dir, "fixtures", "net.gpkg")
  sf::st_write(data.frame(id = 1:3, toid = c(2L, 3L, 0L)), fixture, layer = "net", quiet = TRUE)

  case <- list(id = "net_case", `function` = "hy", fixture = "net.gpkg",
    compare = "exact", order = "exact", supported_by = list("r"), generator = list("r"))
  fn_entry <- list(returns = "frame")
  path <- file.path(dir, "expected", "hy.gpkg")

  expect_message(
    expect_identical(run_conformance_case(case, fn_entry, dir = dir, write = TRUE), character()),
    "new expectation layer")

  before <- unname(tools::md5sum(path))
  mtime <- file.mtime(path)

  expect_silent(
    expect_identical(run_conformance_case(case, fn_entry, dir = dir, write = TRUE), character()))
  expect_identical(unname(tools::md5sum(path)), before)
  expect_identical(file.mtime(path), mtime)

  unlink(fixture)
  sf::st_write(data.frame(id = 1:3, toid = c(3L, 3L, 0L)), fixture, layer = "net", quiet = TRUE)

  expect_message(
    expect_identical(run_conformance_case(case, fn_entry, dir = dir, write = TRUE), character()),
    "net_case changed")
  expect_false(identical(unname(tools::md5sum(path)), before))
  expect_identical(read_layer(path, "net_case")$toid, c("3", "3", "0"))
})
