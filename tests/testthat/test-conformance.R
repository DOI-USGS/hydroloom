# Manifest-driven conformance cases (SPEC S-4, S-7; VERIFICATION 2). With
# HYDROLOOM_WRITE_EXPECTED=1 each R-generated case writes its expectation
# layer, then every case compares against expected/<function>.gpkg.

cases <- r_conformance_cases()

test_that("manifest has R-generated conformance cases", {
  skip_if_no_conformance()
  skip_if_not_installed("yaml")
  skip_if(length(cases) == 0, "no R-generated cases in the manifest")

  expect_true(length(cases) > 0)
})

for (cc in cases) {
  test_that(paste("conformance:", cc$case$id), {
    skip_if_no_conformance()

    expect_identical(run_conformance_case(cc$case, cc$fn_entry), character())
  })
}

# Write mode: delete layers no case stores and VACUUM what changed (D3-163,
# D3-182).
if (write_expected_mode() && length(cases)) {
  test_that("write mode leaves only stored layers in the expectation files", {
    manifest <- read_conformance_yaml(file.path(conformance_dir(), "manifest.yaml"))

    tidy_expected(manifest)

    stored <- vapply(Filter(function(x) is.null(x$raises), manifest$cases), `[[`, "", "id")
    layers <- unlist(lapply(list.files(file.path(conformance_dir(), "expected"),
      pattern = "\\.gpkg$", full.names = TRUE), function(p) sf::st_layers(p)$name))

    expect_identical(setdiff(layers, stored), character())
  })
}
