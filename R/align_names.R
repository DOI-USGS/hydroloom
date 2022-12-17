# TODO fix up ocumentation to describe how to custmize renaming.
#' @title Align names to hydroloom convention
#' @description this function aligns the attribute names
#' with those used in hydroloom.
#' @param x data.frame
#' @return data.frame renamed to match hydroloom as possible.
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' names(x)
#'
#' x <- align_names(x)
#'
#' names(x)
#'
align_names <- function(x){

  orig_names <- names(x)

  names(x) <- tolower(names(x))

  replace_names <- get("attributes", envir = hydroloom_env)
  good_names <- get("good_names", envir = hydroloom_env)

  replace_names <- replace_names[names(replace_names) %in% names(x)]

  x <- rename(x, any_of(stats::setNames(names(replace_names), unname(replace_names))))

  switch_back <- !names(x) %in% good_names

  names(x)[switch_back] <- orig_names[switch_back]

  return(x)

}
