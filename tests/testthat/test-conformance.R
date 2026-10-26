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
