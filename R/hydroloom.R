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
#' @importFrom sf st_geometry st_sf
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#' x <- hy(x)
#'
hy <- function(x, clean = FALSE) {

  orig_names <- names(x)

  g <- NULL
  geom_name <- NULL
  if(inherits(x, "sf")) {
    geom_name <- attr(x, "sf_column")
    g <- st_geometry(x)
    x <- drop_geometry(x)
  }

  x <- align_nhdplus_names(x)

  names(x) <- tolower(names(x))

  replace_names <- get("attributes", envir = hydroloom_env)
  good_names <- get("good_names", envir = hydroloom_env)

  replace_names <- replace_names[names(replace_names) %in% names(x)]

  x <- rename(x, any_of(stats::setNames(names(replace_names), unname(replace_names))))

  keep_names <- orig_names

  if(clean) {

    keep_names <- orig_names[which(names(x) %in% good_names)]

    x <- select(x, all_of(names(x)[names(x) %in% good_names]))

    if(!is.null(geom_name))
      orig_names <- orig_names[!orig_names %in% geom_name]

  } else if(!is.null(g)) {

    x <- st_sf(x, geom = g)

  }

  if("toid" %in% names(x)) {
    x$toid <- tidyr::replace_na(x$toid, 0)
  }

  # strip tbl
  if(inherits(x, "sf")) {
    x <- st_sf(as.data.frame(x))
  } else {
    x <- as.data.frame(x)
  }

  attr(x, "orig_names") <- stats::setNames(names(x), keep_names)

  class(x) <- c("hy", class(x))

  x
}

#' is hy?
#' @description test if object is a valid according to the hy s3 class
#' @param x object to test
#' @return logical TRUE if valid
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

#' reverse hy to original names
#' @description renames hy object to original names and removes hy object
#' attributes.
#' @param x hy data.frame to be returned to original state
#' @importFrom sf st_sf
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#' x <- hy(x)
#'
#' hy_reverse(x)
#'
hy_reverse <- function(x) {

  if(!is.hy(x)) stop("must be an hy object")

  orig_names <- attr(x, "orig_names")

  attr(x, "orig_names") <- NULL

  rep_names <- names(orig_names)[match(names(x), orig_names)]

  names(x)[which(names(x) %in% orig_names)] <- rep_names[!is.na(rep_names)]

  class(x) <- class(x)[!class(x) == "hy"]

  if(inherits(x, "sf"))
    x <- st_sf(x)

  x

}
