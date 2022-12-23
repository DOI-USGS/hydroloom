required_atts_accumulate <- c(id, toid)

#' Accumulate Variable Downstream
#' @description given a variable, accumulate according to network topology.
#' @inheritParams navigate_hydro_network
#' @param var variable to accumulate.
#' @name accumulate_downstream
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' x$totDASqKM <- accumulate_downstream(add_toids(x), "AreaSqKM")
#'
#' plot(x['totDASqKM'], lwd = x$totDASqKM / 50)
accumulate_downstream <- function(x, var) {
  if(!var %in% names(x)) stop(paste(var, "must be in x"))

  UseMethod("accumulate_downstream")
}

#' @name accumulate_downstream
#' @export
accumulate_downstream.data.frame <- function(x, var) {
  x <- hy(x)

  accumulate_downstream(x, attr(x, "orig_names")[var])
}

accumulate_downstream.hy <- function(x, var) {

  check_names(x, required_atts_accumulate, "accumulation")

  x <- drop_geometry(x)

  cat_order <- select(x, "id")

  x[["toid"]] <- replace_na(x[["toid"]], 0)

  x <- sort_network(x)

  x[["toid_row"]] <- match(x[["toid"]], x[["id"]])

  var_out <- x[[var]]

  if(any(is.na(x[[var]]))) {
    warning("NA values found, accumulation math may fail.")
  }

  toid_row <- x[["toid_row"]]

  for(cat in 1:length(var_out)) {
    var_out[toid_row[cat]] <- var_out[toid_row[cat]] + var_out[cat]
  }

  x[[var]] <- var_out

  x <- distinct(left_join(cat_order, x, by = "id"))

  return(x[[var]])
}
