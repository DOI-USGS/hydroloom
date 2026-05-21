.onLoad <- function(libname, pkgname) {
  chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))

  if (nzchar(chk) && !chk %in% c("false", "0")) {
    data.table::setDTthreads(2L)
  }
}
