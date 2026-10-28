#' @title Add Downstream IDs
#' @description Generates a toid attribute from node topology by joining
#' tonode and fromnode attributes.
#' @param x data.frame network compatible with \link{hydroloom_names}.
#' @param return_dendritic logical remove non dendritic paths if TRUE. Requires
#' a "divergence" flag where 1 is main and 2 is secondary.
#' @details
#'
#' Required attributes: `fromnode`, `tonode`
#'
#' Conditionally: `divergence` (if `return_dendritic = TRUE`)
#'
#' @returns hy object with toid attribute
#' @seealso [hy_node], [hy_topo], [make_node_topology()]
#' @name add_toids
#' @export
#' @examples
#' g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' x <- add_toids(hy(g))
#'
#' y <- add_toids(g)
#'
#' names(g)[1:4]
#'
#' names(x)[1:4]
#'
#' names(y)[1:4]
#'
add_toids <- function(x, return_dendritic = TRUE) {
  UseMethod("add_toids")
}

#' @name add_toids
#' @export
add_toids.data.frame <- function(x, return_dendritic = TRUE) {

  if (!return_dendritic)
    warning("return_dendritic = FALSE is deprecated. ",
      "Use to_flownetwork() for non-dendritic edge lists.",
      call. = FALSE)

  x <- hy(x)

  orig_names <- attr(x, "orig_names")

  x <- add_toids(x, return_dendritic)

  attr(x, "orig_names") <- orig_names
  class(x) <- c("hy", class(x))

  hy_reverse(x)
}

#' @name add_toids
#' @export
add_toids.hy <- function(x, return_dendritic = TRUE) {
  hy_classify_and_redispatch(x, "add_toids", "hy_node", hy_guidance_node,
    return_dendritic = return_dendritic)
}

#' @name add_toids
#' @export
add_toids.hy_topo <- function(x, return_dendritic = TRUE) {
  hy_representation_error("This network already has toid (class: ", hy_network_type(x),
    "). add_toids() converts fromnode/tonode to toid.",
    call. = FALSE)
}

#' @name add_toids
#' @export
add_toids.hy_flownetwork <- function(x, return_dendritic = TRUE) {
  hy_representation_error("This network already has toid (class: ", hy_network_type(x),
    "). add_toids() converts fromnode/tonode to toid.",
    call. = FALSE)
}

#' @name add_toids
#' @export
add_toids.hy_node <- function(x, return_dendritic = TRUE) {

  if ("toid" %in% names(x)) hy_representation_error("network already contains a toid attribute")

  # nolint start
  # joiner_fun <- function(x) {
  #   select(
  #     left_join(select(st_drop_geometry(x), "id", "tonode"),
  #               select(st_drop_geometry(x), toid = "id", "fromnode"),
  #               by = c("tonode" = "fromnode")), -"tonode")
  # }
  # nolint end

  # slightly faster data.table
  # rows with a missing fromnode are never targets; a missing tonode matches
  # nothing, so its row keeps its position and takes the outlet value
  joiner_fun <- function(x) {
    keep <- !is.na(x$fromnode)
    as.data.frame(
      data.table(toid = x$id[keep],
        node = x$fromnode[keep])[data.table(id = x$id,
        node = x$tonode),
      on = "node"]
    )[, c("id", "toid")]
  }

  if (return_dendritic) {
    if (!"divergence" %in% names(x)) {
      stop("To remove non dendritic paths, a divergence attribute is required.")
    }

    miss <- is.na(x$divergence)

    if (any(miss))
      stop("Column divergence is missing on these rows: ",
        paste(x$id[miss], collapse = ", "),
        ". Keeping only the main path where a flowline splits needs divergence ",
        "on every flowline: 0 not below a divergence, 1 the main path, 2 a minor ",
        "path. Compute it with add_divergence() and call again.", call. = FALSE)

    # a split is a node that is some flowline's tonode and the fromnode of two
    # or more flowlines; exactly one of those must not be coded 2
    fn <- x$fromnode
    at_split <- !is.na(fn) & fn %in% x$tonode &
      (duplicated(fn) | duplicated(fn, fromLast = TRUE))

    n_main <- tapply(x$divergence[at_split] != 2, fn[at_split], sum)
    bad <- at_split & as.character(fn) %in% names(n_main)[n_main != 1]

    if (any(bad))
      stop("Where a flowline splits, exactly one downstream flowline is the main ",
        "path (divergence other than 2) and the rest are minor paths (divergence ",
        "2). On these rows the split has no main path or more than one: ",
        paste(x$id[bad], collapse = ", "),
        ". Recompute divergence with add_divergence() and call again.",
        call. = FALSE)

    x <- mutate(x,
      orig_fromnode = fromnode,
      fromnode = ifelse(.data$divergence == 2, NA, fromnode))

  }

  out_val <- get_outlet_value(x)

  x <- left_join(x, joiner_fun(x), by = c("id")) |>
    mutate(toid = replace_na(toid, out_val))

  sf_t <- inherits(x, "sf")

  as.data.frame(
    x <- x[, c("id", "toid",
      names(x)[!names(x) %in% c("id", "toid")])]
  )

  if (sf_t)
    x <- st_sf(x)

  if (return_dendritic) {
    x <- select(x, -fromnode)
    x <- rename(x, fromnode = "orig_fromnode")
  }

  classify_hy(x)

}

#' add toids
#' given an hy object, adds toids. If flownetwork is TRUE,
#' a flownetwork with id, toid, upmain and downmain is returned.
#' @param x data.frame network compatible with hydroloom_names.
#' @return data.frame containing toid
#' @noRd
try_add_toids <- function(x, flownetwork = FALSE) {
  if (!toid %in% names(x) && # if we can create a flow network
    fromnode %in% names(x) &&
    flownetwork) { # and main is the goal
    x <- to_flownetwork(x)
  } else if (!toid %in% names(x) && fromnode %in% names(x)) {
    # otherwise make sure we have toids
    x <- add_toids(x, return_dendritic = FALSE)
  }
  x
}
