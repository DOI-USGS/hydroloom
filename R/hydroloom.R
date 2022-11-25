# NHDPlus Attributes
id <- "id"
toid <- "toid"
tonode <- "tonode"
fromnode <- "fromnode"
divergence <- "divergence"

good_names <- c(id, toid, fromnode, tonode, divergence)

# input names that should be changed to replacement names
attributes <- c(
  comid = id,
  nhdplusid = id,
  toid = toid,
  tocomid = toid,
  featureid = id,
  tonode = tonode,
  fromnode = fromnode)

hydroloom_env <- new.env()

assign("attributes", attributes, envir = hydroloom_env)

assign("good_names", good_names, envir = hydroloom_env)

#' @title create an hy fabric object
#' @description converts a compatible dataset into a fabric s3 class
#' @param x data.frame with compatible attribute names from nhdplus
#' @return hy object with attributes compatible with the hydroloom package.
#' @importFrom dplyr select rename
#' @importFrom tidyselect any_of all_of
#' @importFrom nhdplusTools align_nhdplus_names
#' @export
#' @examples
#' library(nhdplusTools)
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#' hy(x)

hy <- function(x, clean = FALSE) {
  orig_names <- names(x)

  x <- align_nhdplus_names(x)

  names(x) <- tolower(names(x))

  replace_names <- get("attributes", envir = hydroloom_env)
  good_names <- get("good_names", envir = hydroloom_env)

  replace_names <- replace_names[names(replace_names) %in% names(x)]

  x <- rename(x, any_of(stats::setNames(names(replace_names), unname(replace_names))))

  attr(x, "orig_names") <- stats::setNames(names(x), orig_names)

  if(clean) {
    x <- drop_geometry(x)

    x <- select(x, all_of(names(x)[names(x) %in% good_names]))

  }

  if("toid" %in% names(x)) {
    x$toid <- tidyr::replace_na(x$toid, 0)
  }

  if(inherits(x, "sf")) {
    x <- sf::st_sf(as.data.frame(x))
  } else {
    x <- as.data.frame(x)
  }

  class(x) <- c("hy", class(x))

  x
}

#' is hy?
#' @param x object to test
#' @export
#'
is.hy <- function(x) {

  if(!inherits(x, "hy")) {
    message("no hy class attribute")
    return(FALSE)
  }

  if("toid" %in% names(x) & any(is.na(x$toid))) {
    message("some na toids")
    return(FALSE)
  }

  if(!"orig_names" %in% names(attributes(x))) {
    message("no original names attribute")
    return(FALSE)
  }

  TRUE
}
