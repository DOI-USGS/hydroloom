# Every remedies key a description's Remedies table or a manifest case cites
# exists in remedies.json (SPEC A-10, S-8, D3-81).

remedies_table_keys <- function(path) {

  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)

  start <- grep("^## Remedies", lines)
  if (length(start) == 0) return(character())

  end <- grep("^## ", lines)
  end <- c(end[end > start[1]], length(lines) + 1)[1]

  rows <- grep("^\\|", lines[seq(start[1] + 1, end - 1)], value = TRUE)
  rows <- rows[-(1:2)]

  last <- vapply(strsplit(rows, "|", fixed = TRUE), function(cells) {
    cells <- trimws(cells)
    cells <- cells[nzchar(cells)]
    cells[length(cells)]
  }, "")

  keys <- regmatches(last, gregexpr("`[^`]+`", last))

  unique(gsub("`", "", unlist(keys)))
}

test_that("cited remedies keys exist in remedies.json", {
  skip_if_no_conformance()
  skip_if_not_installed("yaml")

  remedies <- jsonlite::read_json(
    system.file("extdata", "remedies.json", package = "hydroloom"))$remedies

  files <- list.files(file.path(conformance_dir(), "algorithms"),
    pattern = "\\.md$", full.names = TRUE)
  files <- files[basename(files) != "TEMPLATE.md"]

  for (f in files) {
    missing <- setdiff(remedies_table_keys(f), names(remedies))
    expect_identical(missing, character(), info = basename(f))
  }

  manifest <- read_conformance_yaml(file.path(conformance_dir(), "manifest.yaml"))

  for (case in manifest$cases) {
    if (is.null(case$raises_key)) next

    expect_true(case$raises_key %in% names(remedies), info = case$id)
    expect_identical(remedies[[case$raises_key]]$kind, case$raises, info = case$id)
  }
})

test_that("the Remedies table reader returns the key column", {

  path <- tempfile(fileext = ".md")
  on.exit(unlink(path))

  writeLines(c("## Remedies", "",
    "| condition | what is wrong | which rows | the one call that fixes it | `remedies.json` key |",
    "| --- | --- | --- | --- | --- |",
    "| a | b | c | `fix()` | `needs_topo` |", "", "## References"), path)

  expect_identical(remedies_table_keys(path), "needs_topo")
})
