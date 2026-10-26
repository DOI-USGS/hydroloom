# Conformance suite location and validators (SPEC S-4, S-8, S-10).
# The suite is Rbuildignored, so these resolve only from a source checkout.

conformance_dir <- function() {
  testthat::test_path("..", "..", "inst", "extdata", "conformance")
}

skip_if_no_conformance <- function() {
  testthat::skip_if_not(dir.exists(conformance_dir()),
    "conformance directory absent (installed package; SPEC S-10)")
}

# Read YAML so that every sequence stays a JSON array (even of length one)
# and every empty mapping stays a JSON object.
read_conformance_yaml <- function(path) {

  yaml::read_yaml(path, handlers = list(
    seq = function(x) as.list(x),
    map = function(x) if (length(x) == 0) stats::setNames(list(), character()) else x
  ))
}

yaml_to_json <- function(x) {

  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)
}

# Cross-reference checks the JSON Schema cannot express. Returns a character
# vector of problems, empty when the manifest is consistent.
manifest_problems <- function(manifest) {

  problems <- character()
  fns <- names(manifest$functions)
  ids <- vapply(manifest$cases, function(x) x$id, "")

  dup <- unique(ids[duplicated(ids)])
  if (length(dup)) problems <- c(problems, paste("duplicated case id:", dup))

  for (case in manifest$cases) {
    if (!case[["function"]] %in% fns)
      problems <- c(problems, paste0("case ", case$id, " names unlisted function ",
        case[["function"]]))

    extra <- setdiff(unlist(case$generator), unlist(case$supported_by))
    if (length(extra))
      problems <- c(problems, paste0("case ", case$id, " generator not in supported_by: ",
        paste(extra, collapse = ", ")))
  }

  problems
}

# Every section of the algorithm description template (SPEC S-8).
description_headings <- c(
  "Signature", "Preconditions", "Definition", "Algorithm", "Postconditions",
  "Complexity", "Edge cases", "Remedies", "References", "Worked example",
  "Changes"
)

# Checks one description file: YAML front matter that holds `requires`, and
# a level-two heading for every template section. Returns a character vector
# of problems, empty when the file conforms.
description_problems <- function(path) {

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  fences <- which(trimws(lines) == "---")

  if (length(lines) == 0 || trimws(lines[1]) != "---" || length(fences) < 2)
    return("missing YAML front matter delimited by '---' lines")

  front <- tryCatch(
    yaml::yaml.load(paste(lines[2:(fences[2] - 1)], collapse = "\n")),
    error = function(e) e)

  if (inherits(front, "error"))
    return(paste("front matter is not valid YAML:", conditionMessage(front)))

  problems <- character()
  if (!is.list(front) || !"requires" %in% names(front) || is.null(front$requires))
    problems <- c(problems, "front matter lacks `requires`")

  body <- lines[-seq_len(fences[2])]
  found <- sub("\\s+$", "", sub("^##\\s+", "", grep("^##\\s+\\S", body, value = TRUE)))

  missing <- setdiff(description_headings, found)
  if (length(missing))
    problems <- c(problems, paste("missing heading: ##", missing))

  problems
}
