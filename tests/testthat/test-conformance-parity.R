test_that("the committed parity table is current", {
  skip_if_no_conformance()
  skip_if_not_installed("yaml")

  manifest <- read_conformance_yaml(file.path(conformance_dir(), "manifest.yaml"))
  lines <- parity_lines(manifest)

  if (write_expected_mode()) write_parity(lines)

  expect_true(file.exists(parity_file()))
  expect_identical(readLines(parity_file(), warn = FALSE, encoding = "UTF-8"), lines)
})

test_that("every manifest case lists r and every function has cases", {
  skip_if_no_conformance()
  skip_if_not_installed("yaml")

  # M2 asserted every function `r` only; ROADMAP M5 Done-when moves the
  # phase-1 functions to `r, python`. R stays the generator, so every case
  # still lists r (S-7).
  manifest <- read_conformance_yaml(file.path(conformance_dir(), "manifest.yaml"))
  rows <- parity_rows(manifest)

  expect_true(all(vapply(manifest$cases, function(x) "r" %in% unlist(x$supported_by), logical(1))))
  expect_true(all(rows$cases > 0))
})

test_that("parity rows take the languages common to every case", {
  m <- list(suite_version = "0.0.0",
    functions = list(a = list(), hy = list(), b = list()),
    cases = list(
      list(id = "a1", `function` = "a", supported_by = list("r", "python")),
      list(id = "a2", `function` = "a", supported_by = list("r")),
      list(id = "h1", `function` = "hy", supported_by = list("python", "r"))))

  rows <- parity_rows(m)

  expect_identical(rows$supported_by, c("r", "r, python", "none"))
  expect_identical(rows$python, c("a", "as_hy", "b"))
  expect_identical(rows$cases, c(2L, 1L, 0L))
})
