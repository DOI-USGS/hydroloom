# pixi run r-check: R CMD check --as-cran on a tarball built from the checkout,
# then devtools::test() from the checkout, which is where R conformance runs
# (VERIFICATION 1.5). The test step runs whatever the check returned; the
# script exits nonzero if either step failed.
#
# PKG_BUILD_COPY_METHOD=copy (set by the task) makes pkgbuild copy the package
# tree with .Rbuildignore applied before it calls R CMD build, which otherwise
# copies everything first, .pixi/ (about 9 GB) included. src/Makefile and
# src/install.libs.R let R install the package with src/ present. The copy
# drops src/ (^src$), so nothing compiles; needs_compilation = FALSE stops
# pkgbuild from requiring Rtools because the checkout has src/.

root <- getwd()

d <- tempfile("hl-r-check")
dir.create(d)

tgz <- devtools::build(path = d, quiet = TRUE, needs_compilation = FALSE)

setwd(d)
check_status <- system2(
  file.path(R.home("bin"), "R"),
  c("CMD", "check", "--as-cran", basename(tgz))
)
setwd(root)

test_ok <- tryCatch(
  {
    devtools::test(stop_on_failure = TRUE)
    TRUE
  },
  error = function(e) {
    message(conditionMessage(e))
    FALSE
  }
)

message(
  "r-check: R CMD check exit status ", check_status,
  "; devtools::test() ", if (test_ok) "passed" else "failed"
)

quit(save = "no", status = if (check_status == 0 && test_ok) 0L else 1L)
