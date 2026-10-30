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

# Every D rule a description cites has a fixture tagged with it (RULES P-3).
# D-15 is held by the Python D-7 permutation property (VERIFICATION 4).
test_that("every D rule a description cites is tagged on a fixture", {
  skip_if_no_conformance()
  skip_if_not_installed("yaml")

  index <- yaml::read_yaml(file.path(conformance_dir(), "fixtures", "index.yaml"))$fixtures
  tagged <- unique(unlist(lapply(index, function(x) x$rules)))

  descriptions <- list.files(file.path(conformance_dir(), "algorithms"),
    pattern = "\\.md$", full.names = TRUE)
  cited <- unique(unlist(lapply(descriptions, function(f) {
    regmatches(readLines(f, warn = FALSE), gregexpr("D-[0-9]+", readLines(f, warn = FALSE)))
  })))

  expect_identical(setdiff(setdiff(cited, "D-15"), tagged), character())
})

# Expectation files stay under 1 MiB (D3-185; RULES P-3).
test_that("every expectation file is under 1 MiB", {
  skip_if_no_conformance()

  files <- list.files(file.path(conformance_dir(), "expected"),
    pattern = "\\.gpkg$", full.names = TRUE)
  expect_true(length(files) > 0)
  for (f in files) expect_true(file.size(f) < 2^20, info = basename(f))
})
