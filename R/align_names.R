align_name_char <- function(x) {

  name_map <- vocab_alias_map()

  if (tolower(x) %in% names(name_map))
    return(name_map[[tolower(x)]])

  x
}

#' @title Align Names to Hydroloom Convention
#' @description this function aligns the attribute names in the table
#' with those used in hydroloom. In R, see \link{hydroloom_names} for how
#' to add more attribute name mappings if the attributes in your data
#' are not supported.
#'
#' See \link{hydroloom_name_definitions} for definitions of the names
#' used in hydroloom.
#'
#' @param x a table of flowlines or catchments; columns hydroloom knows by
#' another name (such as NHDPlus `COMID`, `ToCOMID`) are renamed to hydroloom
#' names.
#' @returns data.frame renamed to match hydroloom as possible.
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
  good_names <- vocab_good_names()

  replace_names <- replace_names[names(replace_names) %in% names(x)]

  # an alias mapped to two or more hydroloom names resolves to its latest
  # mapping, so a registered alias wins over the vocabulary alias of another
  # term (align_names.md step 2, G-11)
  if (any(duplicated(names(replace_names)))) {
    n_values <- tapply(unname(replace_names), names(replace_names),
      function(v) length(unique(v)))
    mixed <- names(replace_names) %in% names(n_values)[n_values > 1]
    replace_names <- replace_names[!mixed | !duplicated(names(replace_names), fromLast = TRUE)]
  }

  # the permanent_identifier rule runs before the conflict check; a column
  # already holding a hydroloom name counts once among its candidates
  # (align_names.md step 3, B-1)
  held <- names(x)[names(x) %in% good_names & !names(x) %in% names(replace_names)]

  if (any(duplicated(c(unname(replace_names), held)))) {

    if ("permanent_identifier" %in% names(replace_names)) {

      replace_names <- replace_names[!names(replace_names) == "permanent_identifier"]

      message("defaulting to comid rather than permanent_identifier")

    }

  }

  change_names <- replace_names[!names(replace_names) == unname(replace_names)]

  change_names <- change_names[which(change_names %in% names(x))]

  if (length(change_names) > 0) {
    names(change_names) <- orig_names[which(tolower(orig_names) == names(change_names))]

    stop(paste("Problem aligning names.", paste(names(change_names), collapse = ", "),
      "conflicts with hydroloom name", paste(unname(change_names), collapse = ", "),
      "can't proceed converting to hy object."))
  }

  if (any(duplicated(replace_names))) {

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
#' @description Definitions of all attributes used in the hydroloom package.
#' In R, these are a named character vector, read by name rather than called.
#' @name hydroloom_name_definitions
#' @returns each hydroloom name with its definition, one entry per name, in
#' the order of the vocabulary. In R, the value is a named character vector
#' whose names are the hydroloom names, with class `hydroloom_names` for its
#' custom print method.
#' @export
#' @examples
#' hydroloom_name_definitions
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
#' @param clear logical if TRUE, the names revert to those set when the
#' package loads (the vocabulary aliases), and x, if given, is added to them.
#' @returns named character vector containing hydroloom names with registered attribute name mappings in `names`.
#' @export
#' @examples
#'
#' hydroloom_names()
#'
hydroloom_names <- function(x = NULL, clear = FALSE) {

  hl <- get("hydroloom_name_map", envir = hydroloom_env)

  if (!is.null(x) && is.null(names(x))) stop("input must be named")

  # both checks run before clear, so the registry is unchanged on a stop
  # (hydroloom_names.md step 3)
  if (!is.null(x)) {
    good_names <- vocab_good_names()

    unknown <- !x %in% good_names
    if (any(unknown))
      stop("These aliases map to names that are not hydroloom names: ",
        paste(names(x)[unknown], "=", x[unknown], collapse = ", "),
        ". A column renamed to a name hydroloom does not use gets its input ",
        "name back, so these entries would rename nothing. Map each alias to ",
        "a hydroloom name and register again; nothing was registered.")

    is_name <- tolower(names(x)) %in% good_names & tolower(names(x)) != x
    if (any(is_name))
      stop("These aliases are themselves hydroloom names, mapped to a different ",
        "hydroloom name: ", paste(names(x)[is_name], "=", x[is_name], collapse = ", "),
        ". Each entry would rename a column that already carries a hydroloom ",
        "name to another one, for example a toid column to id. Use the input ",
        "column's own name as the alias, or drop the entry, and register again; ",
        "nothing was registered.")
  }

  # clear = TRUE resets to the registry seeded at load: the vocabulary
  # aliases (hydroloom_names H-03, D3-20)
  if (clear) {
    hl <- vocab_alias_map()
    assign("hydroloom_name_map", hl, envir = hydroloom_env)
  }

  if (is.null(x)) {
    return(hl)
  }

  # align_names() lowercases column names before lookup, so aliases are
  # stored lowercase (D3-65)
  names(x) <- tolower(names(x))

  hl <- c(hl, x)

  assign("hydroloom_name_map", hl, envir = hydroloom_env)

  hl

}
