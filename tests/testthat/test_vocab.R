test_that("vocabulary files validate against their schemas", {
  skip_if_not_installed("jsonvalidate")

  for (f in c("hy_vocab", "abbreviations")) {
    json <- system.file("extdata", paste0(f, ".json"), package = "hydroloom")
    schema <- system.file("extdata", paste0(f, ".schema.json"), package = "hydroloom")

    expect_true(jsonvalidate::json_validate(json, schema, engine = "ajv", error = TRUE))
  }
})

test_that("vocabulary covers good_names and aliases", {
  terms <- hydroloom:::hy_vocab()$terms
  good_names <- hydroloom:::vocab_good_names()

  expect_length(good_names, 46)
  expect_true(all(good_names %in% names(terms)))

  for (n in good_names) expect_true(!is.null(terms[[n]]$type), info = n)

  expect_equal(hydroloom_names()[["outletid"]], "levelpath_outlet_id")
})

test_that("3DHP hydrosequence is a topo_sort alias", {
  expect_equal(hydroloom_names()[["hydrosequence"]], "topo_sort")

  x <- data.frame(id3dhp = c("a", "b"), hydrosequence = c(2, 1))

  expect_true("topo_sort" %in% names(align_names(x)))
})

test_that("3DHP dnhydrosequence and uphydrosequence are dn_topo_sort and up_topo_sort aliases (D3-184)", {
  hln <- hydroloom_names()

  expect_equal(hln[["dnhydrosequence"]], "dn_topo_sort")
  expect_equal(hln[["uphydrosequence"]], "up_topo_sort")

  # the NHDPlus alias comes first, so it keeps priority
  expect_lt(which(names(hln) == "dnhydroseq"), which(names(hln) == "dnhydrosequence"))
  expect_lt(which(names(hln) == "uphydroseq"), which(names(hln) == "uphydrosequence"))

  x <- data.frame(id3dhp = c("a", "b"), dnhydrosequence = c(1, 0),
    uphydrosequence = c(0, 2))

  expect_true(all(c("dn_topo_sort", "up_topo_sort") %in% names(align_names(x))))
})
