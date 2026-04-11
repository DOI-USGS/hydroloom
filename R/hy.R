#' @title Create a hy Fabric S3 Object
#' @description converts a compatible dataset into a fabric s3 class.
#' Automatically detects the network representation and classifies the
#' result as the most specific hydroloom subclass (e.g. \code{hy_topo},
#' \code{hy_node}). With \code{add_topo = TRUE}, also auto-builds
#' topology (e.g. toid from fromnode/tonode).
#' @param x data.frame network compatible with \link{hydroloom_names}.
#' @param clean logical if TRUE, geometry and non-hydroloom compatible attributes
#' will be removed.
#' @param add_topo logical. If TRUE, auto-build topology (e.g. toid
#' from fromnode/tonode) and classify into subclass hierarchy. If FALSE
#' (default), classify based on existing columns only without constructing
#' new attributes.
#' @returns hy object with attributes compatible with the hydroloom package.
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' hy(x)
#'
#' hy(x, clean = TRUE)[1:10, ]
#'
#' attr(hy(x), "orig_names")
#'
#' # auto-build toid from fromnode/tonode
#' hy(x, add_topo = TRUE)
#'
hy <- function(x, clean = FALSE, add_topo = FALSE) {

  orig_names <- names(x)

  g <- NULL
  geom_name <- NULL
  if (inherits(x, "sf")) {
    geom_name <- attr(x, "sf_column")
    g <- st_geometry(x)
    x <- st_drop_geometry(x)
  }

  x <- align_names(x)

  keep_names <- orig_names

  if (clean) {

    keep_names <- orig_names[which(names(x) %in% good_names)]

    x <- select(x, all_of(names(x)[names(x) %in% good_names]))

    if (!is.null(geom_name))
      orig_names <- orig_names[!orig_names %in% geom_name]

  } else if (!is.null(g)) {

    keep_names <- keep_names[c(which(keep_names != geom_name), which(keep_names == geom_name))]

    x <- st_sf(x, geom = g)

  }

  if ("toid" %in% names(x)) {
    out_val <- get_outlet_value(x)

    x$toid <- replace_na(x$toid, out_val)
  }

  # strip tbl
  if (inherits(x, "sf")) {
    x <- st_sf(as_tibble(x))
  } else {
    x <- as_tibble(x)
  }

  attr(x, "orig_names") <- setNames(names(x), keep_names)

  class(x) <- c("hy", class(x))

  # set dendritic property
  if (divergence %in% names(x) && any(x[[divergence]] == 2, na.rm = TRUE)) {
    attr(x, "dendritic") <- FALSE
  } else {
    attr(x, "dendritic") <- TRUE
  }

  # classify into subclass hierarchy
  unique_id <- length(unique(x$id)) == nrow(x)
  has_toid  <- toid %in% names(x)
  has_nodes <- all(c(fromnode, tonode) %in% names(x))

  if (add_topo) {

    # auto-build toid from nodes if needed
    if (has_nodes && !has_toid && unique_id) {

      has_div <- divergence %in% names(x)
      orig_names_save <- attr(x, "orig_names")
      dendritic_save <- attr(x, "dendritic")

      x <- add_toids(x, return_dendritic = has_div)

      # add_toids strips hy class through internal dplyr/data.frame ops
      if (!inherits(x, "hy")) class(x) <- c("hy", class(x))

      attr(x, "orig_names") <- orig_names_save
      attr(x, "dendritic") <- dendritic_save
    }
  }

  if (add_topo && toid %in% names(x) && any(duplicated(x$id))) {
    warning("Non-unique id values detected. This self-referencing table has ",
            "duplicated rows (likely from divergences). Use to_flownetwork() ",
            "for a junction-table representation, or make_node_topology() for ",
            "a fromnode/tonode representation. hy_topo requires unique id.",
            call. = FALSE)
  }

  x <- classify_hy(x)

  x
}

#' Is Valid `hy` Class?
#' @description test if object is a valid according to the hy s3 class
#' @param x object to test
#' @param silent logical should messages be emitted?
#' @returns logical TRUE if valid
#' @export
#'
is.hy <- function(x, silent = FALSE) {

  if (!inherits(x, "hy")) {
    if (!silent)
      message("no hy class attribute")
    return(FALSE)
  }

  if ("toid" %in% names(x) && any(is.na(x$toid))) {
    if (!silent)
      message("some na toids")
    return(FALSE)
  }

  if (!"orig_names" %in% names(attributes(x))) {
    if (!silent)
      message("no original names attribute")
    return(FALSE)
  }

  TRUE
}

#' Reverse `hy` to Original Names
#' @description renames hy object to original names and removes hy object
#' attributes.
#' @param x data.frame network compatible with \link{hydroloom_names}.
#' @returns returns x with attribute names converted to original names provided to \link{hy}
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#' x <- hy(x)
#'
#' hy_reverse(x)
#'
hy_reverse <- function(x) {

  if (!is.hy(x)) stop("must be an hy object")

  orig_names <- attr(x, "orig_names")

  attr(x, "orig_names") <- NULL

  rep_names <- names(orig_names)[match(names(x), orig_names)]

  names(x)[which(names(x) %in% orig_names)] <- rep_names[!is.na(rep_names)]

  class(x) <- class(x)[!class(x) %in% hy_classes]
  attr(x, "dendritic") <- NULL

  if (inherits(x, "sf")) {
    attr(x, "sf_column") <- names(orig_names)[orig_names == attr(x, "sf_column")]
    x <- st_sf(x)
  }

  x

}
