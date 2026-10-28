# Cases that raise or warn (SPEC S-6): the manifest fields and the harness.

raises_suite <- function() {

  dir <- tempfile("conformance")
  dir.create(file.path(dir, "fixtures"), recursive = TRUE)

  sf::st_write(data.frame(id = 1:3, fromnode = 1:3, tonode = c(2L, 3L, 4L), divergence = 0L),
    file.path(dir, "fixtures", "nodes.gpkg"), layer = "nodes", quiet = TRUE)

  sf::st_write(data.frame(a = 1:2),
    file.path(dir, "fixtures", "no_id.gpkg"), layer = "no_id", quiet = TRUE)

  dir
}

raises_case <- function(id, fixture, raises = NULL, args = NULL) {

  case <- list(id = id, `function` = "add_toids", fixture = fixture,
    compare = "exact", order = "exact", supported_by = list("r"), generator = list("r"))

  if (!is.null(args)) case$args <- args
  if (!is.null(raises)) case$raises <- raises

  case
}

test_that("an error case asserts the error and writes no layer", {
  skip_if_no_conformance()

  dir <- raises_suite()
  on.exit(unlink(dir, recursive = TRUE))

  case <- raises_case("no_id_raises", "no_id.gpkg", "error")

  expect_identical(run_conformance_case(case, list(returns = "frame"), dir = dir, write = TRUE),
    character())
  expect_false(file.exists(file.path(dir, "expected", "add_toids.gpkg")))

  expect_identical(run_conformance_case(case, list(returns = "frame"), dir = dir, write = FALSE),
    character())

  quiet <- raises_case("nodes_no_error", "nodes.gpkg", "error")
  expect_failure(run_conformance_case(quiet, list(returns = "frame"), dir = dir, write = FALSE))
})

test_that("a warning case asserts the warning and writes no layer", {
  skip_if_no_conformance()

  dir <- raises_suite()
  on.exit(unlink(dir, recursive = TRUE))

  case <- raises_case("nodes_warns", "nodes.gpkg", "warning",
    list(return_dendritic = FALSE))

  expect_identical(run_conformance_case(case, list(returns = "frame"), dir = dir, write = TRUE),
    character())
  expect_false(file.exists(file.path(dir, "expected", "add_toids.gpkg")))

  keyed <- c(case, list(raises_key = "return_dendritic_deprecated"))
  expect_identical(run_conformance_case(keyed, list(returns = "frame"), dir = dir, write = FALSE),
    character())

  other <- c(case, list(raises_key = "divergence_zero_main_path"))
  expect_failure(run_conformance_case(other, list(returns = "frame"), dir = dir, write = FALSE))

  silent <- raises_case("nodes_silent", "nodes.gpkg", "warning")
  expect_failure(run_conformance_case(silent, list(returns = "frame"), dir = dir, write = TRUE))
})

test_that("a warning key matches its r_text with placeholders as wildcards", {
  skip_if_no_conformance()

  expect_true(warning_matches_key(
    "Flag column TerminalFl holds values other than 0, 1, or missing on these rows: 20, 30. The column is left as it arrives, not read as true and false. Recode those values to 0 or 1 and call again.",
    "flag_value_not_binary"))
  expect_true(warning_matches_key(
    "Duplicate names found when aligning with hydroloom conventions \n using featureid from nhdplusid, featureid",
    "name_alias_duplicate"))
  expect_false(warning_matches_key("Flag column TerminalFl holds values", "flag_value_not_binary"))
  expect_false(warning_matches_key("anything", "not_a_key"))
})

test_that("write mode deletes a layer no case stores and VACUUMs the file", {
  skip_if_no_conformance()

  dir <- raises_suite()
  on.exit(unlink(dir, recursive = TRUE))

  stored <- raises_case("nodes_stored", "nodes.gpkg", args = list(return_dendritic = FALSE))
  expect_message(run_conformance_case(stored, list(returns = "frame"), dir = dir, write = TRUE),
    "new expectation layer")

  path <- file.path(dir, "expected", "add_toids.gpkg")
  write_layer(data.frame(a = seq_len(5000)), path, "nodes_warns")
  size <- file.size(path)

  manifest <- list(cases = list(stored,
    raises_case("nodes_warns", "nodes.gpkg", "warning", list(return_dendritic = FALSE))))

  expect_message(tidy_expected(manifest, dir), "nodes_warns: stale layer deleted")
  expect_identical(sf::st_layers(path)$name, "nodes_stored")
  expect_lt(file.size(path), size)
})

test_that("a case listing columns keeps only those in its layer", {
  skip_if_no_conformance()

  dir <- raises_suite()
  on.exit(unlink(dir, recursive = TRUE))

  case <- c(raises_case("nodes_columns", "nodes.gpkg"), list(columns = list("id", "toid")))
  expect_message(run_conformance_case(case, list(returns = "frame"), dir = dir, write = TRUE),
    "new expectation layer")
  expect_identical(names(read_layer(file.path(dir, "expected", "add_toids.gpkg"),
    "nodes_columns")), c("id", "toid"))

  absent <- c(raises_case("nodes_absent", "nodes.gpkg"), list(columns = list("id", "nope")))
  expect_match(run_conformance_case(absent, list(returns = "frame"), dir = dir, write = FALSE),
    "listed column not in the result: nope")
})

test_that("the manifest schema accepts raises and raises_key", {
  skip_if_no_conformance()
  skip_if_not_installed("jsonvalidate")

  schema <- file.path(conformance_dir(), "manifest.schema.json")

  valid <- function(case) {
    m <- list(suite_version = "0.1.0",
      functions = list(add_toids = list(requires = "algorithms/add_toids.md#requires",
        returns = "frame")),
      cases = list(case))

    jsonvalidate::json_validate(yaml_to_json(m), schema, engine = "ajv")
  }

  base <- raises_case("a_case", "nodes.gpkg")

  expect_true(valid(base))
  expect_true(valid(c(base, list(raises = "error"))))
  expect_true(valid(c(base, list(raises = "warning", raises_key = "weight_missing"))))
  expect_false(valid(c(base, list(raises = "stop"))))
  expect_false(valid(c(base, list(raises_key = "weight_missing"))))

  expect_true(valid(c(base, list(columns = list("COMID", "toid")))))
  expect_false(valid(c(base, list(columns = list()))))
  expect_false(valid(c(base, list(raises = "warning", columns = list("id")))))
})

test_that("an error case with a representation key asserts the condition class", {
  skip_if_no_conformance()

  classed <- function() hydroloom:::hy_representation_error("not this pattern")
  plain <- function() stop("not this pattern")

  expect_true(call_case_function(classed, list(), "error", "classed", "needs_topo")$raised)

  failed <- tryCatch(call_case_function(plain, list(), "error", "plain", "needs_topo"),
    expectation_failure = function(e) e)
  expect_s3_class(failed, "expectation_failure")
  expect_match(conditionMessage(failed), "hydroloom_representation_error")

  expect_true(call_case_function(plain, list(), "error", "plain_other", "network_cycle")$raised)
})
