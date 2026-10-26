# The hy_capabilities() code table against the descriptions (SPEC A-4, S-4,
# S-8). Each code-table entry whose function is listed in the manifest must
# equal the `requires` front matter of that function's description; entries
# for functions with no description yet are checked as they land (D3-33).

front_matter_requires <- function(path) {

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  fences <- which(trimws(lines) == "---")

  yaml::yaml.load(paste(lines[2:(fences[2] - 1)], collapse = "\n"))$requires
}

# Pattern and columns as character vectors, via as a named character vector,
# so a YAML `[]` or `{}` compares equal to the code table's empty vectors.
normalize_requires <- function(r) {

  via <- unlist(r$via)
  if (is.null(via)) via <- stats::setNames(character(), character())

  list(
    pattern = as.character(unlist(r$pattern)),
    columns = as.character(unlist(r$columns)),
    via = via[order(names(via))]
  )
}

test_that("hy_capabilities code table equals the manifest requires", {
  skip_if_no_conformance()
  skip_if_not_installed("yaml")

  dir <- conformance_dir()
  manifest <- read_conformance_yaml(file.path(dir, "manifest.yaml"))

  table <- hydroloom:::hy_requires
  checked <- intersect(names(table), names(manifest$functions))

  expect_true(length(checked) > 0)

  for (fn in checked) {
    ref <- sub("#requires$", "", manifest$functions[[fn]]$requires)
    want <- normalize_requires(front_matter_requires(file.path(dir, ref)))

    expect_identical(normalize_requires(table[[fn]]), want, info = fn)
  }
})
