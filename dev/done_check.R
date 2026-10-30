# Milestone done-check (dev/done_check_plan.md; .kiro/context/orchestrator.md).
# Usage from the checkout root: Rscript dev/done_check.R M3
# Prints one line per mechanical Done-when item; exits 1 on any fail. The
# non-mechanical items (reader rounds dispositioned, Summary written, R CMD
# check) stay with the orchestrator.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1 || !grepl("^M[0-9]+[a-z]?$", args[1])) {
  stop("usage: Rscript dev/done_check.R M<n>", call. = FALSE)
}
milestone <- args[1]

results <- list()
note <- function(item, ok, detail = "") {
  results[[length(results) + 1]] <<- list(item = item, ok = ok, detail = detail)
}

# 1. Conformance and vocabulary tests, which carry the validator, the manifest
#    schema, D-rule coverage, file sizes, the parity table, and every case.
res <- as.data.frame(devtools::test(filter = "conformance|vocab", reporter = "silent"))
bad <- res[res$failed > 0 | res$error, ]
note("conformance and vocabulary tests green",
  nrow(bad) == 0,
  if (nrow(bad)) paste(unique(bad$file), collapse = ", ") else
    sprintf("%d tests, %d skipped", nrow(res), sum(res$skipped)))

# 2. No disposition cell left empty in the milestone's Log.
record <- file.path("dev", "milestones", paste0(milestone, ".md"))
if (file.exists(record)) {
  lines <- readLines(record, warn = FALSE)
  log_start <- grep("^## Log", lines)
  body <- if (length(log_start)) lines[log_start[1]:length(lines)] else character()
  empty <- grep("\\|\\s*—\\s*\\|\\s*$", body)
  note("no empty disposition cell in the Log", length(empty) == 0,
    if (length(empty)) sprintf("%d cells at lines %s", length(empty),
      paste(log_start[1] - 1 + head(empty, 10), collapse = " ")) else "")
} else {
  note("milestone record exists", FALSE, record)
}

# 3. No BACKLOG row under the milestone being closed.
backlog <- readLines(file.path("dev", "plan", "BACKLOG.md"), warn = FALSE)
heads <- grep("^## ", backlog)
this <- which(backlog[heads] == paste("##", milestone))
if (length(this)) {
  from <- heads[this] + 1
  to <- if (this < length(heads)) heads[this + 1] - 1 else length(backlog)
  rows <- backlog[from:to]
  rows <- rows[grepl("^\\|", rows) & !grepl("^\\| Item \\||^\\| ---", rows) &
    !grepl("^\\| none \\|", rows)]
  note(sprintf("no BACKLOG row under %s", milestone), length(rows) == 0,
    if (length(rows)) sprintf("%d rows", length(rows)) else "")
} else {
  note(sprintf("no BACKLOG row under %s", milestone), TRUE, "no section")
}

for (r in results) {
  cat(sprintf("%-5s %s%s\n", if (r$ok) "pass" else "FAIL", r$item,
    if (nzchar(r$detail)) paste0(" (", r$detail, ")") else ""))
}
quit(status = if (all(vapply(results, function(r) r$ok, TRUE))) 0 else 1)
