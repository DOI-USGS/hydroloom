#' @title Create a hy Fabric S3 Object
#' @description converts a compatible dataset into a fabric s3 class
#' @inheritParams add_levelpaths
#' @param clean logical if TRUE, geometry and non-hydroloom compatible attributes
#' will be removed.
#' @return hy object with attributes compatible with the hydroloom package.
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' hy(x)
#'
#' hy(x, clean = TRUE)[1:10,]
#'
#' attr(hy(x), "orig_names")
#'
hy <- function(x, clean = FALSE) {

  orig_names <- names(x)

  g <- NULL
  geom_name <- NULL
  if(inherits(x, "sf")) {
    geom_name <- attr(x, "sf_column")
    g <- st_geometry(x)
    x <- st_drop_geometry(x)
  }

  x <- align_names(x)

  keep_names <- orig_names

  if(clean) {

    keep_names <- orig_names[which(names(x) %in% good_names)]

    x <- select(x, all_of(names(x)[names(x) %in% good_names]))

    if(!is.null(geom_name))
      orig_names <- orig_names[!orig_names %in% geom_name]

  } else if(!is.null(g)) {

    keep_names <- keep_names[c(which(keep_names != geom_name), which(keep_names == geom_name))]

    x <- st_sf(x, geom = g)

  }

  if("toid" %in% names(x)) {
    out_val <- get_outlet_value(x)

    x$toid <- replace_na(x$toid, out_val)
  }

  # strip tbl
  if(inherits(x, "sf")) {
    x <- st_sf(as_tibble(x))
  } else {
    x <- as_tibble(x)
  }

  attr(x, "orig_names") <- setNames(names(x), keep_names)

  class(x) <- c("hy", class(x))

  x
}

#' Is Valid `hy` Class?
#' @description test if object is a valid according to the hy s3 class
#' @param x object to test
#' @param silent logical should messages be emitted?
#' @return logical TRUE if valid
#' @export
#'
is.hy <- function(x, silent = FALSE) {

  if(!inherits(x, "hy")) {
    if(!silent)
      message("no hy class attribute")
    return(FALSE)
  }

  if("toid" %in% names(x) && any(is.na(x$toid))) {
    if(!silent)
      message("some na toids")
    return(FALSE)
  }

  if(!"orig_names" %in% names(attributes(x))) {
    if(!silent)
      message("no original names attribute")
    return(FALSE)
  }

  TRUE
}

#' Reverse `hy` to Original Names
#' @description renames hy object to original names and removes hy object
#' attributes.
#' @inheritParams add_levelpaths
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

  if(inherits(x, "sf")) {
    attr(x, "sf_column") <- names(orig_names)[orig_names == attr(x, "sf_column")]
    x <- st_sf(x)
  }

  x

}
