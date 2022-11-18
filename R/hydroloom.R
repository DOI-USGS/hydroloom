#' @title create an hy fabric object
#' @description converts a compatible dataset into a fabric s3 class
#' @param x data.frame with compatible attribute names from nhdplus
#' @importFrom dplyr select
#' @importFrom tidyselect any_of
#' @importFrom nhdplusTools align_nhdplus_names
#' @export
#' @examples
#' library(nhdplusTools)
#' source(system.file("extdata/nhdplushr_data.R", package = "nhdplusTools"))
#' nhd <- hr_data$NHDFlowline
#' hy(nhd)

hy <- function(x) {
  x <- align_nhdplus_names(x)

  names(x) <- tolower(names(x))

  x <- select(x, any_of(c(id = "comid", toid = "tocomid", "fromnode", "tonode", "divergence")))

  if("toid" %in% names(x)) {
    x$toid <- tidyr::replace_na(x$toid, 0)
  }

  class(x) <- c("hy", class(x))

  x
}

is.hy <- function(x) {

  if(!inherits("hy")) {
    message("no hy class attribute")
    return(FALSE)
  }

  if("toid" %in% names(x) & any(is.na(x$toid))) {
    message("some na toids")
    return(FALSE)
  }

  TRUE
}

drop_geometry <- function(x) {
  if("sf" %in% class(x)) {
    sf::st_drop_geometry(x)
  } else {
    x
  }
}
