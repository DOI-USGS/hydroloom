test_that("manifest validates against its schema", {
  skip_if_no_conformance()
  skip_if_not_installed("yaml")
  skip_if_not_installed("jsonvalidate")

  dir <- conformance_dir()
  manifest <- read_conformance_yaml(file.path(dir, "manifest.yaml"))

  expect_true(jsonvalidate::json_validate(
    yaml_to_json(manifest), file.path(dir, "manifest.schema.json"),
    engine = "ajv", error = TRUE))

  expect_identical(manifest_problems(manifest), character())
})

test_that("algorithm descriptions follow the template", {
  skip_if_no_conformance()
  skip_if_not_installed("yaml")

  files <- list.files(file.path(conformance_dir(), "algorithms"),
    pattern = "\\.md$", full.names = TRUE)
  skip_if(length(files) == 0, "no algorithm descriptions yet")

  for (f in files) expect_identical(description_problems(f), character(), info = basename(f))
})

plain_case <- function(id, fn, fixture = NULL, ...) {

  case <- list(id = id, `function` = fn, compare = "exact", order = "exact",
    supported_by = list("r"), generator = list("r"))

  if (!is.null(fixture)) case$fixture <- fixture

  c(case, list(...))
}

test_that("the manifest schema accepts prepare and an absent fixture", {
  skip_if_no_conformance()
  skip_if_not_installed("jsonvalidate")

  schema <- file.path(conformance_dir(), "manifest.schema.json")

  valid <- function(case) {
    m <- list(suite_version = "0.1.0",
      functions = list(hy_reverse = list(returns = "frame")),
      cases = list(case))

    jsonvalidate::json_validate(yaml_to_json(m), schema, engine = "ajv")
  }

  prep <- list(`function` = "hy", args = list(add_topo = TRUE))

  expect_true(valid(plain_case("no_fixture", "hy_reverse")))
  expect_true(valid(plain_case("prepared", "hy_reverse", "nodes.gpkg", prepare = prep)))
  expect_true(valid(plain_case("prepared_no_args", "hy_reverse", "nodes.gpkg",
    prepare = list(`function` = "hy"))))

  expect_false(valid(plain_case("prep_no_fixture", "hy_reverse", prepare = prep)))
  expect_false(valid(plain_case("prep_no_fn", "hy_reverse", "nodes.gpkg",
    prepare = list(args = list(add_topo = TRUE)))))
  expect_false(valid(plain_case("prep_nested", "hy_reverse", "nodes.gpkg",
    prepare = c(prep, list(prepare = list(`function` = "hy"))))))
  expect_false(valid(plain_case("prep_code", "hy_reverse", "nodes.gpkg",
    prepare = list(`function` = "hy", args = list(add_topo = list(a = 1))))))
})

test_that("the harness applies prepare and runs cases without a fixture", {
  skip_if_no_conformance()

  dir <- tempfile("conformance")
  dir.create(file.path(dir, "fixtures"), recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE))

  nodes <- data.frame(comid = 1:3, fromnode = 1:3, tonode = c(2L, 3L, 4L))
  sf::st_write(nodes, file.path(dir, "fixtures", "nodes.gpkg"), layer = "nodes", quiet = TRUE)

  frame <- list(returns = "frame")

  prepared <- plain_case("reverse_prepared", "hy_reverse", "nodes.gpkg",
    prepare = list(`function` = "hy", args = list(add_topo = TRUE)))

  expect_message(
    expect_identical(run_conformance_case(prepared, frame, dir = dir, write = TRUE), character()),
    "new expectation layer")

  got <- read_layer(file.path(dir, "expected", "hy_reverse.gpkg"), "reverse_prepared")

  expect_true("toid" %in% names(got))
  expect_true("comid" %in% names(got))
  expect_identical(
    compare_layers(frame_to_layer(suppressMessages(hy_reverse(case_input(prepared, dir)))), got),
    character())
  expect_identical(run_conformance_case(prepared, frame, dir = dir, write = FALSE), character())

  defs <- plain_case("definitions", "hydroloom_name_definitions")
  expect_message(run_conformance_case(defs, frame, dir = dir, write = TRUE), "new expectation layer")
  expect_identical(
    names(read_layer(file.path(dir, "expected", "hydroloom_name_definitions.gpkg"), "definitions")),
    c("name", "definition"))

  nm <- plain_case("names", "hydroloom_names")
  expect_message(run_conformance_case(nm, frame, dir = dir, write = TRUE), "new expectation layer")
  expect_identical(run_conformance_case(nm, frame, dir = dir, write = FALSE), character())

  expect_error(run_conformance_case(c(defs, list(args = list(x = "a"))), frame, dir = dir,
    write = FALSE), "is a value")
})
