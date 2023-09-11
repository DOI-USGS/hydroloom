align_name_char <- function(x) {

  if(tolower(x) %in% names(hydroloom_name_map))
    return(hydroloom_name_map[[tolower(x)]])

  x
}

#' @title Align Names to Hydroloom Convention
#' @description this function aligns the attribute names in x
#' with those used in hydroloom. See \link{hydroloom_names} for how
#' to add more attribute name mappings if the attributes in your data
#' are not supported.
#'
#' See \link{hydroloom_name_definitions} for definitions of the names
#' used in hydroloom.
#'
#' @inheritParams add_levelpaths
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
align_names <- function(x) {

  orig_names <- names(x)

  names(x) <- tolower(names(x))

  replace_names <- get("hydroloom_name_map", envir = hydroloom_env)
  good_names <- get("good_names", envir = hydroloom_env)

  replace_names <- replace_names[names(replace_names) %in% names(x)]

  change_names <- replace_names[!names(replace_names) == unname(replace_names)]

  change_names <- change_names[which(change_names %in% names(x))]

  if(length(change_names) > 0) {
    names(change_names) <- orig_names[which(tolower(orig_names) == names(change_names))]

    stop(paste("Problem aligning names.", paste(names(change_names), collapse = ", "),
               "conflicts with hydroloom name", paste(unname(change_names), collapse = ", "),
               "can't proceed converting to hy object."))
  }

  if(any(duplicated(replace_names))) {

    if("permanent_identifier" %in% names(replace_names)) {

      replace_names <- replace_names[!names(replace_names) == "permanent_identifier"]

      message("defaulting to comid rather than permanent_identifier")

    }

  }

  if(any(duplicated(replace_names))) {

    doop <- replace_names[duplicated(replace_names)]
    all_doop <- replace_names[replace_names %in% doop]
    warning(paste0("Duplicate names found when aligning with hydroloom conventions \n using ",
                   names(doop), " from ", paste(names(all_doop), collapse = ", ")))

    remove <- all_doop[!names(all_doop) %in% names(doop)]
    replace_names <- replace_names[!names(replace_names) %in% names(remove)]

  }

  x <- rename(x, any_of(setNames(names(replace_names), unname(replace_names))))

  switch_back <- !names(x) %in% good_names

  names(x)[switch_back] <- orig_names[switch_back]

  x

}

#' Hydroloom Name Definitions
#' @description A names character vector containing definitions of all
#' attributes used in the hydroloom package.
#' @name hydroloom_name_definitions
#' @export
#' @examples
#' cat(paste0(names(hydroloom_name_definitions), ", ",
#'            hydroloom_name_definitions), sep = "\n")
hydroloom_name_definitions

#' @title Get or Set Hydroloom Names
#' @description Retrieve hydroloom name mapping from hydroloom
#' environment. Hydroloom uses a specific set of attribute names within
#' the package and includes mappings from names used in some data sources.
#' This function will return those names and can be used to set additional
#' name mappings.
#'
#' NOTE: these values will reset when R is restarted. Add desired settings
#' to a project or user .Rprofile to make long term additions.
#'
#' @param x named character vector of additional names to add to the
#' hydroloom environment. If not specified, no names will be added and
#' the current value stored in the hydroloom environment will be returned.
#' @param clear logical if TRUE, all names will be removed and replaced with
#' x.
#' @export
hydroloom_names <- function(x = NULL, clear = FALSE) {

  hl <- get("hydroloom_name_map", envir = hydroloom_env)

  if(!is.null(x) & is.null(names(x))) stop("input must be named")

  if(clear) {
    hl <- c()
    assign("hydroloom_name_map", hl, envir = hydroloom_env)
  }

  if(is.null(x)) {
    return(hl)
  }

  hl <- c(hl, x)

  assign("hydroloom_name_map", hl, envir = hydroloom_env)

  hl

}
