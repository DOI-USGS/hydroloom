# Parity table generated from the manifest's `supported_by` (SPEC A-15, S-4;
# ROADMAP M2). One row per manifest function, in manifest order. A function's
# `supported_by` is the set of languages that support every one of its cases
# that carries no `r_only` and is not Python-only; a case with `r_only` is
# R-only by design and is counted apart (SPEC S-4, RULES P-5), and a case
# with `supported_by: [python]` is Python-only by design and is counted apart
# in the `python_only` column (SPEC S-4, RULES B-5). A function whose every
# case is counted apart reads `r, python` when its Python name is in the
# `__all__` list of src/hydroloom/__init__.py, and `r` when it is not or when
# that file is absent, as in the CRAN tarball (SPEC S-4, RULES P-3).

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

# Returns the quoted names between `__all__ = [` and `]` in the Python
# package's __init__.py, or character() when the file is absent.
parity_python_all <- function(
  path = testthat::test_path("..", "..", "src", "hydroloom", "__init__.py")) {

  if (!file.exists(path)) return(character())

  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  block <- regmatches(text, regexpr("__all__\\s*=\\s*\\[[^]]*\\]", text))
  if (length(block) == 0) return(character())

  quoted <- regmatches(block, gregexpr("[\"'][^\"']+[\"']", block))[[1]]

  gsub("[\"']", "", quoted)
}

# Returns a data.frame with columns function, python, supported_by, cases,
# r_only, python_only.
parity_rows <- function(manifest, python_all = parity_python_all()) {

  fns <- names(manifest$functions)
  case_fn <- vapply(manifest$cases, function(x) x[["function"]], "")
  r_only <- vapply(manifest$cases, function(x) !is.null(x$r_only), TRUE)
  python_only <- vapply(manifest$cases,
    function(x) identical(unlist(x$supported_by), "python"), TRUE)

  python <- ifelse(fns %in% names(parity_python_names),
    parity_python_names[fns], fns)
  names(python) <- fns

  supported <- vapply(fns, function(fn) {
    cases <- manifest$cases[case_fn == fn & !r_only & !python_only]
    if (length(cases) == 0 && any(case_fn == fn)) {
      return(if (python[[fn]] %in% python_all) "r, python" else "r")
    }
    by <- lapply(cases, function(x) unlist(x$supported_by))
    if (length(by) == 0) return("none")
    langs <- intersect(c("r", "python"), Reduce(intersect, by))
    if (length(langs) == 0) "none" else paste(langs, collapse = ", ")
  }, "")

  data.frame(`function` = fns, python = unname(python),
    supported_by = unname(supported),
    cases = vapply(fns, function(fn) sum(case_fn == fn), 0L, USE.NAMES = FALSE),
    r_only = vapply(fns, function(fn) sum(case_fn == fn & r_only), 0L, USE.NAMES = FALSE),
    python_only = vapply(fns, function(fn) sum(case_fn == fn & python_only), 0L,
      USE.NAMES = FALSE),
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
      "lists the languages that support every one of its cases that carries no ",
      "`r_only` and is not Python-only. A case with `r_only` is R-only by design and ",
      "counts apart, in the `r_only` column (SPEC S-4, RULES P-5); a case with ",
      "`supported_by: [python]` is Python-only by design and counts apart, in the ",
      "`python_only` column (SPEC S-4, RULES B-5). A function whose every case counts ",
      "apart reads `r, python` when its Python name is in `hydroloom.__all__`, and `r` ",
      "otherwise (SPEC S-4, RULES P-3)."),
    "",
    "| function | Python | supported_by | cases | r_only | python_only |",
    "| --- | --- | --- | --- | --- | --- |",
    sprintf("| `%s` | `%s` | %s | %d | %d | %d |",
      rows[["function"]], rows$python, rows$supported_by, rows$cases, rows$r_only,
      rows$python_only))
}

write_parity <- function(lines, path = parity_file()) {

  con <- file(path, "wb")
  on.exit(close(con))
  writeLines(lines, con, sep = "\n", useBytes = TRUE)
}
