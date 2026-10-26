# Parity table generated from the manifest's `supported_by` (SPEC A-15, S-4;
# ROADMAP M2). One row per manifest function, in manifest order. A function's
# `supported_by` is the set of languages that support every one of its cases.

parity_file <- function() file.path(conformance_dir(), "parity.md")

# Python names that differ from the R export (ROADMAP section 2). Any manifest
# function not listed here keeps its R name.
parity_python_names <- c(
  hy = "as_hy",
  is.hy = "is_hy",
  hydroloom_names = "register_names",
  hydroloom_name_definitions = "name_definitions",
  hy_network_type = "representation",
  hy_capabilities = "capabilities"
)

# Returns a data.frame with columns function, python, supported_by, cases.
parity_rows <- function(manifest) {

  fns <- names(manifest$functions)
  case_fn <- vapply(manifest$cases, function(x) x[["function"]], "")

  supported <- vapply(fns, function(fn) {
    by <- lapply(manifest$cases[case_fn == fn], function(x) unlist(x$supported_by))
    if (length(by) == 0) return("none")
    langs <- intersect(c("r", "python"), Reduce(intersect, by))
    if (length(langs) == 0) "none" else paste(langs, collapse = ", ")
  }, "")

  python <- ifelse(fns %in% names(parity_python_names),
    parity_python_names[fns], fns)

  data.frame(`function` = fns, python = unname(python),
    supported_by = unname(supported),
    cases = vapply(fns, function(fn) sum(case_fn == fn), 0L, USE.NAMES = FALSE),
    check.names = FALSE, stringsAsFactors = FALSE)
}

# Returns the lines of parity.md.
parity_lines <- function(manifest) {

  rows <- parity_rows(manifest)

  c("<!-- Generated from manifest.yaml `supported_by` by",
    "     tests/testthat/helper-conformance-parity.R. Do not edit by hand.",
    "     Regenerate: HYDROLOOM_WRITE_EXPECTED=1, then",
    "     devtools::test(filter = \"conformance-parity\"). -->",
    "",
    "# Parity table",
    "",
    paste0("Suite version ", manifest$suite_version, ". A function's `supported_by` ",
      "lists the languages that support every one of its cases."),
    "",
    "| function | Python | supported_by | cases |",
    "| --- | --- | --- | --- |",
    sprintf("| `%s` | `%s` | %s | %d |",
      rows[["function"]], rows$python, rows$supported_by, rows$cases))
}

write_parity <- function(lines, path = parity_file()) {

  con <- file(path, "wb")
  on.exit(close(con))
  writeLines(lines, con, sep = "\n", useBytes = TRUE)
}
