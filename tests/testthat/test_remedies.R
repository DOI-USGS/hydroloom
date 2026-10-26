# remedies.json ships with the package (SPEC S-1, A-10), so these tests run on
# an installed package; the conformance cross-check is in test-conformance-remedies.R.

remedies_path <- function() system.file("extdata", "remedies.json", package = "hydroloom")

message_placeholders <- function(msg) {
  m <- regmatches(msg, gregexpr("\\{[a-z][a-z0-9_]*\\}", msg))[[1]]

  unique(gsub("[{}]", "", m))
}

test_that("remedies.json validates against its schema", {
  skip_if_not_installed("jsonvalidate")

  schema <- system.file("extdata", "remedies.schema.json", package = "hydroloom")

  expect_true(jsonvalidate::json_validate(remedies_path(), schema, engine = "ajv", error = TRUE))
})

test_that("each remedy's placeholders, languages, and R text agree", {

  remedies <- jsonlite::read_json(remedies_path())$remedies

  for (key in names(remedies)) {
    r <- remedies[[key]]

    # str.format: every brace is a placeholder listed in `placeholders`
    expect_identical(sort(message_placeholders(r$message)), sort(names(r$placeholders)),
      info = key)
    expect_false(grepl("[{}]", gsub("\\{[a-z][a-z0-9_]*\\}", "", r$message)), info = key)

    expect_identical("r" %in% r$languages, length(r$r_text) > 0, info = key)
    expect_identical("python" %in% r$languages, !is.null(r$python_class), info = key)

    if (identical(r$python_class, "RepresentationError"))
      expect_identical(r$r_class, "hydroloom_representation_error", info = key)

    # where hydroloom raises it today, the message is R's text with named
    # placeholders, so R output is unchanged
    if (length(r$r_text) && is.null(r$r_text_from)) {
      parts <- strsplit(r$message, "\\{[a-z][a-z0-9_]*\\}")[[1]]
      if (grepl("\\}$", r$message)) parts <- c(parts, "")
      pattern <- paste0("^", paste(gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", parts),
        collapse = ".+"), "$")

      expect_true(grepl(pattern, r$r_text[[1]]), info = key)
    }
  }
})
