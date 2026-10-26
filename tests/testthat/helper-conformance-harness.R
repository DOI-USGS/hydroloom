# Conformance harness (SPEC S-4, S-7, D-13, D-14, I-1; VERIFICATION 2).
#
# One converter per manifest `returns` kind turns an R return value into an
# expectation layer: frame, graph, traversal, scalar. Layers are written to and
# read from GeoPackage with sf, which handles aspatial layers and keeps integer,
# double, character, and logical columns. Identifier columns are stored as
# their decimal string and compared as text (S-7, I-1). Every other column is
# coerced to the R type its vocabulary type declares before comparison.

# Vocabulary term for a column, by hydroloom name and then by alias. NULL for a
# column the vocabulary does not name.
vocab_term <- function(name) {

  terms <- hydroloom:::vocab_columns()
  if (name %in% names(terms)) return(c(terms[[name]], list(hy_name = name)))

  alias <- unname(hydroloom:::vocab_alias_map()[tolower(name)])
  if (is.na(alias)) return(NULL)

  c(terms[[alias]], list(hy_name = alias))
}

# Exact decimal string of an identifier column (S-7).
id_to_text <- function(x) {

  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) return(x)
  if (inherits(x, "integer64")) return(as.character(x))

  if (is.double(x)) {
    if (any(!is.na(x) & x != round(x)))
      stop("identifier column holds non-integer values")

    out <- sprintf("%.0f", x)
    out[is.na(x)] <- NA_character_

    return(out)
  }

  as.character(x)
}

# Coerce one column to the R type of its vocabulary type (I-15 table).
# id_cols names extra identifier columns that are not vocabulary terms.
coerce_column <- function(x, name, id_cols = character()) {

  if (name %in% id_cols) return(id_to_text(x))

  type <- vocab_term(name)$type
  if (is.null(type)) return(x)

  switch(type,
    id = id_to_text(x),
    flag = as.logical(x),
    count = ,
    code = as.integer(x),
    text = as.character(x),
    as.double(x))
}

# Plain column list of a table with every column coerced; geometry dropped.
layer_columns <- function(x, id_cols = character()) {

  if (inherits(x, "sf")) x <- sf::st_drop_geometry(x)

  cols <- lapply(names(x), function(n) {
    col <- x[[n]]
    if (is.list(col)) stop("column ", n, " is a list column; a layer holds atomic columns")
    coerce_column(as.vector(col), n, id_cols)
  })

  list2DF(stats::setNames(cols, names(x)))
}

frame_to_layer <- function(x) {

  out <- layer_columns(x)

  if (inherits(x, "sf")) out <- sf::st_sf(out, geom = sf::st_geometry(x))

  out
}

# Graph layer (S-7): id, position (0-based rank in the neighbor list as R
# returns it), neighbor, and main or edge_id when present. Terminals have no
# rows. mode = "both" is one layer with a mode column.
graph_to_layer <- function(g) {

  if (all(c("to", "from") %in% names(g)) && !is.matrix(g$to)) {
    to <- graph_to_layer(g$to)
    from <- graph_to_layer(g$from)

    return(rbind(
      cbind(list2DF(list(mode = rep("to", nrow(to)))), to),
      cbind(list2DF(list(mode = rep("from", nrow(from)))), from)))
  }

  lst <- if ("to_list" %in% names(g)) g$to_list else g$froms_list
  nb_col <- intersect(c("toindid", "fromindid"), names(lst))

  nb <- lst[[nb_col]]
  row <- rep(seq_len(nrow(lst)), lengths(nb))
  flat <- unlist(nb, use.names = FALSE)
  keep <- !is.na(flat) & flat != 0

  row <- row[keep]
  ids <- lst[["id"]]

  out <- list(
    id = id_to_text(ids[row]),
    position = sequence(rle(row)$lengths) - 1L,
    neighbor = id_to_text(ids[match(flat[keep], lst[["indid"]])]))

  if ("main" %in% names(lst))
    out$main <- as.logical(unlist(lst$main, use.names = FALSE)[keep])

  if ("edge_id" %in% names(lst))
    out$edge_id <- as.integer(unlist(lst$edge_id, use.names = FALSE)[keep])

  list2DF(out)
}

# Inverse view of a graph layer: one row per node with neighbors, a list
# column ordered by position. A list of two such tables when a mode column is
# present.
graph_from_layer <- function(layer) {

  if ("mode" %in% names(layer)) {
    return(lapply(c(to = "to", from = "from"), function(m)
      graph_from_layer(layer[layer$mode == m, setdiff(names(layer), "mode"), drop = FALSE])))
  }

  layer <- layer[order(layer$id, layer$position, method = "radix"), , drop = FALSE]
  ids <- unique(layer$id)

  out <- data.frame(id = ids)
  out$neighbor <- I(unname(split(layer$neighbor, factor(layer$id, levels = ids))))

  if ("main" %in% names(layer))
    out$main <- I(unname(split(layer$main, factor(layer$id, levels = ids))))

  out
}

# Traversal layer (S-7): start, path (0-based index of the path within that
# start's result, as navigate_network_dfs splits paths at divergences), order
# (0-based visit position across the start's paths), id. navigate_network_dfs
# returns one element per unique start; a start with nothing left to visit
# has no rows.
traversal_to_layer <- function(r, starts) {

  starts <- unique(starts)
  if (length(r) != length(starts))
    stop("traversal result has ", length(r), " elements for ", length(starts), " unique starts")

  per_start <- lapply(seq_along(r), function(k) {
    len <- lengths(r[[k]])
    n <- sum(len)

    list(
      start = id_to_text(rep(starts[k], n)),
      path = rep(seq_along(len) - 1L, len),
      order = seq_len(n) - 1L,
      id = id_to_text(unlist(r[[k]], use.names = FALSE)))
  })

  cols <- c("start", "path", "order", "id")
  list2DF(stats::setNames(lapply(cols, function(n) {
    v <- lapply(per_start, `[[`, n)
    if (length(v)) do.call(c, v) else if (n %in% c("path", "order")) integer() else character()
  }), cols))
}

# Inverse of traversal_to_layer: one element per unique start, each a list of
# paths of identifier strings. Path names are not reconstructed; R names them
# with a counter that runs across starts and is not contract.
traversal_from_layer <- function(layer, starts) {

  lapply(id_to_text(unique(starts)), function(s) {
    rows <- layer[layer$start == s, , drop = FALSE]
    rows <- rows[order(rows$order), , drop = FALSE]

    unname(split(rows$id, factor(rows$path, levels = sort(unique(rows$path)))))
  })
}

# Scalar layer (S-7): one row, one column named value, in the R type returned.
scalar_to_layer <- function(v) {

  if (!is.atomic(v) || length(v) != 1)
    stop("a scalar return must be an atomic vector of length one")

  list2DF(list(value = unname(as.vector(v))))
}

scalar_from_layer <- function(layer) layer$value[[1]]

layer_id_cols <- c("start", "neighbor")

to_layer <- function(result, returns, args = list()) {

  switch(returns,
    frame = frame_to_layer(result),
    graph = graph_to_layer(result),
    traversal = traversal_to_layer(result, args$starts),
    scalar = scalar_to_layer(result),
    stop("unknown returns kind: ", returns))
}

write_layer <- function(layer, path, name) {

  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

  exists <- file.exists(path) && name %in% sf::st_layers(path)$name

  sf::st_write(layer, path, layer = name, delete_layer = exists, quiet = TRUE)

  invisible(path)
}

read_layer <- function(path, name) {

  x <- sf::st_read(path, layer = name, quiet = TRUE, stringsAsFactors = FALSE,
    int64_as_string = TRUE)

  rownames(x) <- NULL

  x
}

has_layer <- function(path, name) {
  file.exists(path) && name %in% sf::st_layers(path)$name
}

sort_layer_rows <- function(df, cols) {

  if (nrow(df) < 2 || length(cols) == 0) return(seq_len(nrow(df)))

  do.call(order, c(unname(as.list(df[cols])), list(method = "radix")))
}

# Per-row absolute tolerance for a double column (D-13). A case atol or rtol
# overrides the vocabulary default; a column the vocabulary does not type
# defaults to exact.
column_tolerance <- function(name, expected, layer, atol = NULL, rtol = NULL) {

  if (!is.null(atol) || !is.null(rtol)) {
    a <- if (is.null(atol)) 0 else atol
    r <- if (is.null(rtol)) 0 else rtol

    return(a + r * abs(expected))
  }

  term <- vocab_term(name)
  if (is.null(term)) return(0)

  switch(term$type,
    length = if (identical(term$unit, "m")) 1 else 1e-3,
    area = 1e-6,
    measure = {
      len <- Filter(function(n) identical(vocab_term(n)$hy_name, "length_km"), names(layer))
      if (length(len)) 100 / (as.double(layer[[len[1]]]) * 1000) else 0
    },
    0)
}

column_problem <- function(name, a, e, tol = NULL) {

  if (!identical(class(a), class(e)))
    return(sprintf("column %s: type %s, expected %s", name,
      paste(class(a), collapse = "/"), paste(class(e), collapse = "/")))

  same_na <- is.na(a) == is.na(e)
  both <- !is.na(a) & !is.na(e)

  ok <- if (is.double(e) && !is.null(tol)) {
    same_na & (!both | abs(a - e) <= tol)
  } else {
    same_na & (!both | a == e)
  }

  bad <- which(!ok)
  if (length(bad) == 0) return(character())

  sprintf("column %s: %d of %d rows differ (first at row %d: %s, expected %s)",
    name, length(bad), length(e), bad[1], format(a[bad[1]]), format(e[bad[1]]))
}

# Compare an actual layer to an expected layer (S-7, D-13, D-14). Columns
# compare by name, not position; rows compare in order unless order is
# "ignore", when both are sorted on key (all columns by name when key is
# NULL), byte order. Returns a character vector of problems.
compare_layers <- function(actual, expected, compare = "exact", order = "exact",
  key = NULL, atol = NULL, rtol = NULL, id_cols = layer_id_cols) {

  a_geom <- if (inherits(actual, "sf")) sf::st_geometry(actual)
  e_geom <- if (inherits(expected, "sf")) sf::st_geometry(expected)

  a <- layer_columns(actual, id_cols)
  e <- layer_columns(expected, id_cols)

  e <- e[, setdiff(names(e), "fid"), drop = FALSE]

  problems <- character()

  extra <- setdiff(names(a), names(e))
  missing <- setdiff(names(e), names(a))
  if (length(extra)) problems <- c(problems, paste("unexpected column:", extra))
  if (length(missing)) problems <- c(problems, paste("missing column:", missing))
  if (is.null(a_geom) != is.null(e_geom)) problems <- c(problems, "geometry presence differs")
  if (nrow(a) != nrow(e))
    problems <- c(problems, sprintf("%d rows, expected %d", nrow(a), nrow(e)))

  if (length(problems)) return(problems)

  ia <- ie <- seq_len(nrow(e))
  if (order == "ignore") {
    cols <- if (is.null(key)) sort(names(e), method = "radix") else unlist(key)
    ia <- sort_layer_rows(a, cols)
    ie <- sort_layer_rows(e, cols)
  }

  a <- a[ia, , drop = FALSE]
  e <- e[ie, , drop = FALSE]

  for (n in names(e)) {
    av <- a[[n]]
    ev <- e[[n]]
    tol <- NULL

    if (compare == "tolerance" && is.double(ev)) {
      if (identical(vocab_term(n)$hy_name, "aggregate_id_measure") && is.null(atol) && is.null(rtol)) {
        av <- round(av, 4)
        ev <- round(ev, 4)
      } else {
        tol <- column_tolerance(n, ev, e, atol, rtol)
      }
    }

    problems <- c(problems, column_problem(n, av, ev, tol))
  }

  if (!is.null(e_geom)) {
    if (sf::st_crs(a_geom) != sf::st_crs(e_geom)) problems <- c(problems, "geometry crs differs")

    wa <- sf::st_as_binary(sf::st_geometry(a_geom)[ia])
    we <- sf::st_as_binary(sf::st_geometry(e_geom)[ie])
    bad <- which(!vapply(seq_along(we), function(i) identical(wa[[i]], we[[i]]), TRUE))

    if (length(bad))
      problems <- c(problems, sprintf("geometry: %d rows differ (first at row %d)", length(bad), bad[1]))
  }

  problems
}

# Read a fixture: one layer per file (S-5).
read_fixture <- function(path) {

  layers <- sf::st_layers(path)$name
  if (length(layers) != 1) stop("fixture ", basename(path), " must hold exactly one layer")

  sf::read_sf(path, layer = layers)
}

# Case args (S-4): literal values, a sequence becomes an atomic vector, and a
# string naming a file under fixtures/ is read as a second table.
case_args <- function(args, dir = conformance_dir()) {

  if (is.null(args)) return(list())

  lapply(args, function(v) {
    if (is.list(v)) v <- unlist(v)

    if (is.character(v) && length(v) == 1 && grepl("\\.gpkg$", v)) {
      f <- file.path(dir, "fixtures", v)
      if (file.exists(f)) return(read_fixture(f))
    }

    v
  })
}

write_expected_mode <- function() identical(Sys.getenv("HYDROLOOM_WRITE_EXPECTED"), "1")

# Runs one manifest case. In write mode the expectation layer is (re)written
# first and any value change from the previous layer is reported with
# message(). Returns the comparison problems; empty when the case passes.
run_conformance_case <- function(case, fn_entry, dir = conformance_dir(),
  write = write_expected_mode()) {

  fn <- case[["function"]]
  x <- read_fixture(file.path(dir, "fixtures", case$fixture))
  args <- case_args(case$args, dir)

  fun <- get(fn, envir = asNamespace("hydroloom"), mode = "function")

  # Warnings and messages are not part of the expectation layer.
  result <- suppressMessages(suppressWarnings(do.call(fun, c(list(x), args))))

  layer <- to_layer(result, fn_entry$returns, args)

  expected <- if (is.null(case$expected)) paste0(fn, ".gpkg") else case$expected
  path <- file.path(dir, "expected", expected)

  cmp <- function(old) compare_layers(layer, old, compare = case$compare,
    order = case$order, key = fn_entry$key, atol = case$atol, rtol = case$rtol)

  if (write) {
    if (has_layer(path, case$id)) {
      diff <- cmp(read_layer(path, case$id))
      if (length(diff)) message("conformance ", case$id, " changed:\n  ", paste(diff, collapse = "\n  "))
    } else {
      message("conformance ", case$id, ": new expectation layer in ", expected)
    }

    write_layer(layer, path, case$id)
  }

  if (!has_layer(path, case$id))
    return(paste0("no expectation layer ", case$id, " in expected/", expected,
      "; run with HYDROLOOM_WRITE_EXPECTED=1"))

  cmp(read_layer(path, case$id))
}

# Manifest cases R generates; empty when the suite or a reader is absent.
r_conformance_cases <- function() {

  dir <- conformance_dir()
  if (!dir.exists(dir) || !requireNamespace("yaml", quietly = TRUE)) return(list())

  manifest <- read_conformance_yaml(file.path(dir, "manifest.yaml"))

  cases <- Filter(function(x) "r" %in% unlist(x$generator), manifest$cases)

  lapply(cases, function(x) list(case = x, fn_entry = manifest$functions[[x[["function"]]]]))
}
