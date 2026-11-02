# Generate inst/extdata/docstrings.json from man/*.Rd (SPEC A-13).
#
# Run from the repository root:
#   Rscript dev/docstrings.R          writes inst/extdata/docstrings.json
#   Rscript dev/docstrings.R --check  regenerates to a temp file and exits 1
#                                     if it differs from the committed file
#
# Each Rd file is indexed by every \alias it declares. Per alias the JSON holds
# title, description, details, arguments (one key per argument name, in Rd
# order), value, and sections (one key per \section title, in Rd order;
# @inheritSection content arrives here already resolved into man/). A section
# titled `Required columns` (the hy class pages) is left out, as `Required
# attributes:` lines are. \seealso, \note, \usage, and \examples are not
# transferred. A macro the converter does not know stops the script with the
# file and macro named.

out_path <- "inst/extdata/docstrings.json"

# Rd sections read into the JSON, and sections read past without transfer.
transferred <- c("\\title", "\\description", "\\details", "\\arguments", "\\value", "\\section")
skipped <- c(
  "\\name", "\\alias", "\\usage", "\\examples", "\\seealso",
  "\\note", "\\docType", "\\keyword", "\\concept", "\\encoding", "COMMENT"
)

# \section titles read past without transfer.
skipped_sections <- "Required columns"

# Hard line break and list markers survive whitespace collapsing as these.
br <- "\001"

macros_seen <- character()

fail <- function(file, ...) {

  stop(basename(file), ": ", ..., call. = FALSE)
}

unescape <- function(x) {

  gsub("\\\\([%{}\\\\])", "\\1", x)
}

# Concatenate the text of a node's children; used for \code and \verb, whose
# content is literal.
literal <- function(x, file) {

  paste(vapply(x, function(e) {
    tag <- attr(e, "Rd_tag")
    if (tag %in% c("TEXT", "RCODE", "VERB")) return(unescape(e))
    if (tag %in% c("\\code", "\\link", "\\verb")) {
      macros_seen <<- c(macros_seen, tag)
      return(literal(e, file))
    }
    fail(file, "unsupported macro ", tag, " inside \\code or \\verb")
  }, ""), collapse = "")
}

render_list <- function(x, tag, file) {

  items <- list()
  for (e in x) {
    etag <- attr(e, "Rd_tag")
    if (etag == "\\item") {
      macros_seen <<- c(macros_seen, etag)
      if (tag == "\\describe") {
        items[[length(items) + 1]] <- paste0(
          render(e[[1]], file), ": ", render(e[[2]], file)
        )
      } else {
        items[[length(items) + 1]] <- ""
      }
    } else if (tag == "\\describe" || length(items) == 0) {
      if (etag != "TEXT" || grepl("\\S", e)) {
        fail(file, "content outside \\item in ", tag)
      }
    } else {
      if (etag %in% c("\\itemize", "\\enumerate", "\\describe")) {
        fail(file, "nested list ", etag, " inside ", tag)
      }
      items[[length(items)]] <- paste0(items[[length(items)]], render(list(e), file))
    }
  }

  marker <- if (tag == "\\enumerate") paste0(seq_along(items), ". ") else rep("- ", length(items))
  items <- gsub("\\s+", " ", trimws(unlist(items)))

  paste0("\n\n", paste0(br, marker, items, collapse = ""), "\n\n")
}

render <- function(x, file) {

  paste(vapply(x, function(e) {
    tag <- attr(e, "Rd_tag")
    if (is.null(tag)) return(render(e, file))
    if (tag %in% c("TEXT", "RCODE", "VERB")) return(unescape(e))
    if (tag == "COMMENT") return("")

    macros_seen <<- c(macros_seen, tag)

    switch(tag,
      "\\code" = ,
      "\\verb" = paste0("`", literal(e, file), "`"),
      "\\link" = ,
      "\\pkg" = ,
      "\\url" = render(e, file),
      "\\href" = paste0(render(e[[2]], file), " (", render(e[[1]], file), ")"),
      "\\emph" = paste0("*", render(e, file), "*"),
      "\\strong" = paste0("**", render(e, file), "**"),
      "\\sQuote" = paste0("'", render(e, file), "'"),
      "\\dQuote" = paste0("\"", render(e, file), "\""),
      "\\eqn" = paste0("`", literal(e[[length(e)]], file), "`"),
      "\\R" = "R",
      "\\dots" = ,
      "\\ldots" = "...",
      "\\cr" = br,
      "\\itemize" = ,
      "\\enumerate" = ,
      "\\describe" = render_list(e, tag, file),
      "\\if" = {
        # roxygen writes a markdown hard break as \if{html}{\out{<br>}}.
        body <- e[[2]]
        if (length(body) == 1 && identical(attr(body[[1]], "Rd_tag"), "\\out") &&
          identical(trimws(paste(unlist(body[[1]]), collapse = "")), "<br>")) {
          macros_seen <<- c(macros_seen, "\\out")
          br
        } else {
          fail(file, "unsupported \\if content")
        }
      },
      fail(file, "unsupported macro ", tag)
    )
  }, ""), collapse = "")
}

# Paragraphs split on blank lines, `Required attributes:` lines dropped,
# whitespace inside a paragraph collapsed, hard breaks restored.
tidy <- function(text) {

  paras <- strsplit(text, "\n[ \t]*\n")[[1]]

  paras <- vapply(paras, function(p) {
    lines <- strsplit(p, "\n", fixed = TRUE)[[1]]
    lines <- lines[!grepl("^\\s*Required attributes:", lines)]
    p <- gsub("\\s+", " ", paste(lines, collapse = " "))
    p <- gsub(paste0(" *", br, " *"), br, p)
    p <- gsub(paste0("^", br, "+|", br, "+$"), "", p)
    gsub(br, "\n", p, fixed = TRUE)
  }, "", USE.NAMES = FALSE)

  paras <- trimws(paras)
  paras <- paras[nzchar(paras)]

  if (length(paras) == 0) return(NA_character_)

  paste(paras, collapse = "\n\n")
}

read_arguments <- function(x, file) {

  args <- list()
  for (e in x) {
    tag <- attr(e, "Rd_tag")
    if (tag == "\\item") {
      macros_seen <<- c(macros_seen, tag)
      names <- trimws(strsplit(render(e[[1]], file), ",")[[1]])
      text <- tidy(render(e[[2]], file))
      for (n in names) {
        if (!is.null(args[[n]])) fail(file, "argument ", n, " documented twice")
        args[[n]] <- text
      }
    } else if (tag != "COMMENT" && (tag != "TEXT" || grepl("\\S", e))) {
      fail(file, "content outside \\item in \\arguments")
    }
  }

  args
}

read_sections <- function(rd, tags, file) {

  out <- list()
  for (s in rd[tags == "\\section"]) {
    macros_seen <<- c(macros_seen, "\\section")
    title <- tidy(render(s[[1]], file))
    if (is.na(title)) fail(file, "\\section with no title")
    if (title %in% skipped_sections) next
    if (!is.null(out[[title]])) fail(file, "section ", title, " documented twice")
    out[[title]] <- tidy(render(s[[2]], file))
  }

  # an empty named list writes as {}
  if (length(out) == 0) out <- setNames(list(), character())

  out
}

read_rd <- function(file) {

  rd <- tools::parse_Rd(file, encoding = "UTF-8")
  tags <- vapply(rd, attr, "", "Rd_tag")

  unknown <- setdiff(tags, c(transferred, skipped, "TEXT"))
  if (length(unknown) > 0) fail(file, "unsupported Rd section ", unknown[1])

  section <- function(tag) {

    i <- which(tags == tag)
    if (length(i) > 1) fail(file, "more than one ", tag)
    if (length(i) == 0) return(NULL)
    rd[[i]]
  }

  text_of <- function(tag) {

    s <- section(tag)
    if (is.null(s)) return(NA_character_)
    tidy(render(s, file))
  }

  args <- section("\\arguments")
  args <- if (is.null(args)) list() else read_arguments(args, file)
  # an empty named list writes as {}
  if (length(args) == 0) args <- setNames(list(), character())

  entry <- list(
    arguments = args,
    description = text_of("\\description"),
    details = text_of("\\details"),
    sections = read_sections(rd, tags, file),
    title = text_of("\\title"),
    value = text_of("\\value")
  )

  aliases <- vapply(rd[tags == "\\alias"], function(a) trimws(unescape(paste(unlist(a), collapse = ""))), "")

  setNames(rep(list(entry), length(aliases)), aliases)
}

build <- function() {

  if (!file.exists("DESCRIPTION") || !dir.exists("man")) {
    stop("run dev/docstrings.R from the repository root", call. = FALSE)
  }

  files <- sort(list.files("man", pattern = "\\.Rd$", full.names = TRUE), method = "radix")

  out <- list()
  for (f in files) {
    entries <- read_rd(f)
    dup <- intersect(names(entries), names(out))
    if (length(dup) > 0) fail(f, "alias ", dup[1], " already defined in another Rd file")
    out <- c(out, entries)
  }

  out <- out[sort(names(out), method = "radix")]

  json <- jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null")

  counts <- table(macros_seen)
  message(
    "read ", length(files), " Rd files, wrote ", length(out), " aliases; macros converted: ",
    paste0(names(counts), " (", counts, ")", collapse = ", ")
  )

  charToRaw(enc2utf8(paste0(gsub("\r\n", "\n", json, fixed = TRUE), "\n")))
}

write_raw <- function(bytes, path) {

  con <- file(path, "wb")
  on.exit(close(con))
  writeBin(bytes, con)
}

args <- commandArgs(trailingOnly = TRUE)
bytes <- build()

if ("--check" %in% args) {
  tmp <- tempfile(fileext = ".json")
  write_raw(bytes, tmp)

  committed <- if (file.exists(out_path)) readBin(out_path, "raw", file.size(out_path)) else raw()

  if (!identical(committed, bytes)) {
    message(
      out_path, " differs from a fresh run of dev/docstrings.R; ",
      "run `pixi run docstrings` and commit the result (fresh copy: ", tmp, ")"
    )
    quit(status = 1)
  }

  message(out_path, " matches a fresh run")
} else {
  write_raw(bytes, out_path)
  message("wrote ", out_path)
}
