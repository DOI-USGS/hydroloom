# Every fixture the index lists exists, holds one layer, and reads with sf
# (SPEC S-5, S-10; VERIFICATION 2.1).

test_that("every indexed fixture exists and reads", {
  skip_if_no_conformance()
  skip_if_not_installed("yaml")

  dir <- file.path(conformance_dir(), "fixtures")
  index <- yaml::read_yaml(file.path(dir, "index.yaml"))$fixtures

  expect_true(length(index) > 0)

  files <- vapply(index, function(x) x$file, "")
  expect_false(any(duplicated(files)))
  expect_setequal(files, list.files(dir, pattern = "\\.gpkg$"))

  for (entry in index) {
    f <- file.path(dir, entry$file)
    expect_true(file.exists(f), info = entry$file)

    layers <- sf::st_layers(f)$name
    expect_identical(layers, sub("\\.gpkg$", "", entry$file), info = entry$file)

    x <- sf::read_sf(f)
    expect_identical(nrow(x), as.integer(entry$rows), info = entry$file)
    expect_identical(inherits(x, "sf"), entry$geometry != "none", info = entry$file)
    expect_true(file.size(f) < 2^20, info = entry$file)
  }
})
