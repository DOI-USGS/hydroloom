# pixi run document-check: regenerates man/ and NAMESPACE from roxygen and
# exits nonzero if either differs from the commit, including new untracked
# files under man/ (SPEC A-13 makes roxygen the authored source). The
# docstrings-diff CI job runs this before docstrings-check, since a roxygen
# edit without devtools::document() would otherwise pass with a stale
# docstrings.json.

devtools::document()

diff_status <- system2("git", c("diff", "--exit-code", "--", "man/", "NAMESPACE"))

untracked <- system2(
  "git",
  c("ls-files", "--others", "--exclude-standard", "--", "man/"),
  stdout = TRUE
)

if (length(untracked) > 0) {
  message("document-check: untracked files under man/:\n", paste(untracked, collapse = "\n"))
}

ok <- diff_status == 0 && length(untracked) == 0

message(
  "document-check: man/ and NAMESPACE ",
  if (ok) "match roxygen" else "differ from roxygen; run devtools::document() and commit"
)

quit(save = "no", status = if (ok) 0L else 1L)
