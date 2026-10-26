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
