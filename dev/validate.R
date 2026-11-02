# pixi run validate: the conformance-schema tests.
#
# The tests run in a callr child that writes its failure count to a file. The
# conda-forge r-v8 8.2.0 used by jsonvalidate can crash at process exit
# (win-64), so the exit status comes from that file, not from the child; a
# missing file (an error or crash before the write) exits 1.

s <- tempfile()

try(
  callr::r(
    function(s) {
      r <- as.data.frame(devtools::test(filter = "conformance-schema"))
      writeLines(as.character(sum(r[["failed"]]) + sum(r[["error"]])), s)
    },
    list(s),
    show = TRUE
  ),
  silent = TRUE
)

ok <- file.exists(s) && identical(readLines(s), "0")

quit(save = "no", status = if (ok) 0L else 1L)
