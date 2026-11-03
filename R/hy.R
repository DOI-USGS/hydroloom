#' @title Align a table with hydroloom names and types
#' @description Renames each column hydroloom knows by another name to its
#' standard hydroloom name, as \code{COMID} becomes \code{id} and
#' \code{ToCOMID} becomes \code{toid}. The vocabulary, the list of standard
#' names and the source names known for each, ships with the package as
#' \code{hy_vocab.json}. When two columns could take one name, the column
#' whose source name comes later in the vocabulary takes it, with a warning,
#' and the other keeps its input name. A \code{permanent_identifier} column
#' keeps its name whenever another column could take the same name. Other
#' columns keep their input names and values.
#'
#' Each identifier column takes one identifier type, and each flag column
#' coded 1/0 becomes \code{TRUE}/\code{FALSE}. In R, that type is integer when
#' the column, with its partner if it has one (\code{toid} for \code{id},
#' \code{tonode} for \code{fromnode}), holds only whole numbers within the
#' 32-bit integer range, and text when either holds larger whole numbers or
#' text. A missing \code{toid} becomes the value that marks an outlet (a
#' flowline that flows to nothing else in the table), \code{""} when
#' identifiers are text. In R,
#' that value is \code{0} when identifiers are integer. A table with geometry
#' keeps its geometry column, moved to the last position. In R, that column
#' is renamed \code{geom}.
#' \code{hy_network_type()} reports which network layout the result follows,
#' such as an id/toid list or a fromnode/tonode list.
#' @param x data.frame network compatible with \link{hydroloom_names}.
#' @param clean logical if TRUE, geometry and non-hydroloom compatible attributes
#' will be removed.
#' @param add_topo logical. If TRUE, auto-build topology (e.g. toid
#' from fromnode/tonode) and classify into subclass hierarchy. If FALSE
#' (default), classify based on existing columns only without constructing
#' new attributes.
#' @returns the input table with hydroloom names and types, keeping its
#' geometry when it has one. In R, the result is a \code{hy} object whose S3
#' class is the most specific hydroloom subclass its columns fit, such as
#' \code{hy_topo} or \code{hy_node}.
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

  in_names <- names(x)

  x <- align_names(x)

  x <- ingest_ids(x, in_names)

  x <- ingest_flags(x, in_names)

  keep_names <- orig_names

  if (clean) {

    good_names <- vocab_good_names()

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

# Identifier ingest (SPEC I-2). Each identifier column that is integer,
# integer64, or whole-valued double becomes integer when every value fits in
# 32 bits, else its exact decimal string. id/toid and fromnode/tonode are
# typed as pairs: if either is character, both are (D3-06). A double column
# with a fractional value stops (identifier_not_whole, D3-07), as does one
# with an infinite value (identifier_not_finite, I2-3); character,
# factor, and logical columns are left as they arrive. in_names are the input
# column names, position for position with names(x), for the error message.
#' @noRd
ingest_ids <- function(x, in_names) {

  cols <- intersect(vocab_id_names(), names(x))

  for (n in cols) {
    v <- x[[n]]

    if (is.double(v) && !inherits(v, "integer64")) {
      bad <- which(is.finite(v) & v != round(v))

      if (length(bad)) {
        stop("Column ", in_names[match(n, names(x))],
          " holds numbers with a fractional part on these rows: ",
          paste(bad, collapse = ", "),
          ". An identifier is a whole number or text; correct those values, ",
          "or cast the column to text, and call again.", call. = FALSE)
      }

      inf <- which(is.infinite(v))

      if (length(inf)) {
        stop("Column ", in_names[match(n, names(x))],
          " holds an infinite number (Inf or -Inf) on these rows: ",
          paste(inf, collapse = ", "),
          ". An identifier is a whole number or text, and an infinite value ",
          "is neither; correct those values and call again.", call. = FALSE)
      }
    }

    x[[n]] <- ingest_id(v)
  }

  for (p in list(c(id, toid), c(fromnode, tonode))) {
    if (all(p %in% names(x)) && any(vapply(p, function(n) is.character(x[[n]]), logical(1)))) {
      for (n in p) x[[n]] <- as.character(x[[n]])
    }
  }

  x
}

# Flag ingest (S-2, M1 M-18). Each flag column that is integer or double with
# every non-missing value 0 or 1 becomes logical; one holding any other value
# is left as it arrives and warns, naming the rows by id
# (flag_value_not_binary, D3-152). Other column types are left as they arrive.
# in_names are the input column names, position for position with names(x).
#' @noRd
ingest_flags <- function(x, in_names) {

  for (n in intersect(vocab_flag_names(), names(x))) {
    v <- x[[n]]

    if (!(is.integer(v) || is.double(v)) || inherits(v, "integer64")) next

    bad <- which(!is.na(v) & !v %in% c(0, 1))

    if (length(bad)) {
      rows <- if ("id" %in% names(x)) x$id[bad] else bad

      warning("Flag column ", in_names[match(n, names(x))],
        " holds values other than 0, 1, or missing on these rows: ",
        paste(rows, collapse = ", "),
        ". The column is left as it arrives, not read as true and false. ",
        "Recode those values to 0 or 1 and call again.", call. = FALSE)

      next
    }

    x[[n]] <- v == 1
  }

  x
}

#' @noRd
ingest_id <- function(v) {

  max_id <- .Machine$integer.max

  if (inherits(v, "integer64")) {
    s <- as.character(v)
    d <- as.numeric(s)

    if (all(is.na(d) | abs(d) <= max_id)) return(as.integer(d))

    return(s)
  }

  if (!is.double(v)) return(v)

  if (any(!is.na(v) & (!is.finite(v) | v != round(v)))) return(v)

  if (all(is.na(v) | abs(v) <= max_id)) return(as.integer(v))

  s <- sprintf("%.0f", v)
  s[is.na(v)] <- NA_character_

  s
}

#' Test whether a table is in the form `hy()` returns
#' @description tests whether a table is in the form \code{hy()} returns:
#' hydroloom column names, and identifier columns in the type \code{hy()}
#' gives them. In R, the test reads the object's S3 class, \code{hy} or
#' \code{hy_flownetwork}, and the original names attribute \code{hy()}
#' attaches, so a table in hydroloom names that \code{hy()} did not produce
#' is \code{FALSE}.
#' @param x object to test
#' @param silent logical should messages be emitted?
#' @returns logical TRUE if valid
#' @export
#'
is.hy <- function(x, silent = FALSE) {

  # hy_flownetwork does not inherit hy, but hy() output routed there carries
  # orig_names and hy_reverse() restores its names (D3-64, D3-16).
  if (!inherits(x, c("hy", "hy_flownetwork"))) {
    if (!silent)
      message("no hy class attribute")
    return(FALSE)
  }

  # NA in toid is allowed -- it identifies the row as an outlet under the
  # is_outlet() rule (toid not present in id).

  if (!"orig_names" %in% names(attributes(x))) {
    if (!silent)
      message("no original names attribute")
    return(FALSE)
  }

  TRUE
}

#' Restore source column names
#' @description renames columns from their hydroloom names back to source
#' names. A column with no source name keeps its name. In R, the source names
#' are the names the table had when it was passed to \code{hy()}, and the hy
#' classes and attributes are removed.
#' @param x data.frame network compatible with \link{hydroloom_names}.
#' @returns the input table with its columns renamed from hydroloom names to
#' source names.
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#' x <- hy(x)
#'
#' hy_reverse(x)
#'
hy_reverse <- function(x) {

  if (!is.hy(x)) hy_representation_error("must be an hy object")

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
