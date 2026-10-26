##### vocab.R reads the hydroloom vocabulary from inst/extdata/hy_vocab.json. #####

#' @importFrom jsonlite read_json
#' @noRd
hy_vocab <- function() {

  vocab <- get0("hy_vocab", envir = hydroloom_env, inherits = FALSE)

  if (is.null(vocab)) {
    f <- system.file("extdata", "hy_vocab.json", package = "hydroloom")

    if (!nzchar(f)) stop("hydroloom vocabulary file hy_vocab.json not found")

    vocab <- read_json(f, simplifyVector = FALSE)

    assign("hy_vocab", vocab, envir = hydroloom_env)
  }

  vocab
}

# vocabulary terms that are columns (concept entries excluded)
vocab_columns <- function() {

  terms <- hy_vocab()$terms

  terms[!vapply(terms, function(t) isTRUE(t$concept), logical(1))]
}

# good_names: every hydroloom attribute name, in vocabulary order
vocab_good_names <- function() {
  names(vocab_columns())
}

# identifier names: hydroloom names whose vocabulary type is id
vocab_id_names <- function() {

  terms <- vocab_columns()

  names(terms)[vapply(terms, function(t) identical(t$type, "id"), logical(1))]
}

# alias map: named character vector, alias -> hydroloom name
vocab_alias_map <- function() {

  terms <- vocab_columns()

  map <- lapply(names(terms), function(n) {
    a <- vapply(terms[[n]]$aliases, function(x) x$name, character(1))
    setNames(rep(n, length(a)), a)
  })

  unlist(map)
}

# definitions: named character vector with the hydroloom_names print class
vocab_name_definitions <- function() {

  terms <- vocab_columns()

  defs <- setNames(vapply(terms, function(t) t$definition, character(1)), names(terms))

  class(defs) <- c("hydroloom_names", class(defs))

  defs
}
