required_atts_add_levelpaths <- c("id", "toid")

#' Add Level Paths
#' @description Assigns level paths using the stream-leveling approach of
#' NHD and NHDPlus. If arbolate sum is provided in the weight column, this
#' will match the behavior of NHDPlus. Any numeric value can be
#' included in this column and the largest value will be followed when
#' no nameid is available.
#' @param x data.frame network compatible with \link{hydroloom_names}.
#' @param name_attribute character attribute to be used as name identifiers.
#' @param weight_attribute character attribute to be used as weight.
#' @param override_factor numeric multiplier to use to override `name_attribute`.
#' See details.
#' @param status boolean if status updates should be printed.
#' @returns data.frame with id, levelpath_outlet_id, topo_sort, and levelpath columns.
#' See details for more info.
#' @details
#' The levelpath algorithm defines upstream mainstem paths through a network.
#' At a given junction with two or more upstream flowpaths, the main path is
#' either 1) the path with the same name, 2) the path with any name, 3) or the
#' path with the larger weight. If the `weight_attribute` is `override_factor`
#' times larger on a path, it will be followed regardless of the name_attribute
#' indication.
#' @name add_levelpaths
#' @export
#' @examples
#' g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' test_flowline <- add_toids(g)
#'
#' # use NHDPlus attributes directly
#' add_levelpaths(test_flowline,
#'                name_attribute = "GNIS_ID",
#'                weight_attribute = "ArbolateSu")
#'
#' # use hy attributes where they can be mapped
#' add_levelpaths(hy(test_flowline),
#'                name_attribute = "GNIS_ID",
#'                weight_attribute = "arbolate_sum")
#'
add_levelpaths <- function(x, name_attribute, weight_attribute,
                              override_factor = NULL, status = FALSE) {

  if(any(!c(name_attribute, weight_attribute) %in% names(x))) {
    stop("name and weight attribute must be in x")
  }

  UseMethod("add_levelpaths")
}

#' @name add_levelpaths
#' @export
#'
add_levelpaths.data.frame <- function(x, name_attribute, weight_attribute,
                                         override_factor = NULL, status = FALSE) {
  x <- hy(x)

  name_attribute <- align_name_char(name_attribute)
  weight_attribute <- align_name_char(weight_attribute)

  x <- add_levelpaths(x, name_attribute, weight_attribute, override_factor,
                         status)

  hy_reverse(x)

}

#' @name add_levelpaths
#' @export
#'
add_levelpaths.hy <- function(x, name_attribute, weight_attribute,
                                 override_factor = NULL, status = FALSE) {

  if(!status) {
    pbopts <- pboptions(type = "none")
    on.exit(pboptions(pbopts), add = TRUE)
  }

  check_names(x, required_atts_add_levelpaths, "add_levelpaths")

  hy_g <- get_hyg(x, add = TRUE, id = id)

  orig_names <- attr(x, "orig_names")

  x <- st_drop_geometry(x)

  extra <- select(x, all_of(c(id, names(x)[!names(x) %in% required_atts_add_levelpaths])))

  x <- select(x, all_of(c(required_atts_add_levelpaths,
                          "lp_name_attribute" = name_attribute,
                          "lp_weight_attribute" = weight_attribute))) |>
    distinct()

  out_val <- get_outlet_value(x)

  x$toid <- replace_na(x$toid, out_val)

  x[["lp_name_attribute"]] <- replace_na(x[["lp_name_attribute"]], " ") # NHDPlusHR uses NA for empty names.
  x[["lp_name_attribute"]][x[["lp_name_attribute"]] == "-1"] <- " "

  x <- sort_network(x)

  x <- add_topo_sort(x)
  x$levelpath <- rep(0, nrow(x))

  x <- x |> # get downstream name id added
    left_join(st_drop_geometry(select(x, all_of(c("id", ds_nameid = "lp_name_attribute")))),
              by = c("toid" = "id")) |>
    # if it's na, we need it to be an empty string
    mutate(ds_nameid = ifelse(is.na(.data$ds_nameid),
                              " ", .data$ds_nameid)) |>
    # group on toid so we can operate on upstream choices
    group_by(.data$toid) |>
    group_split()

  # reweight sets up ranked upstream paths
  x <- pblapply(x, reweight, override_factor = override_factor,
                     nat = "lp_name_attribute", wat = "lp_weight_attribute", cl = "future")

  x <- x |>
    bind_rows() |>
    select(all_of(c("id", "toid", "topo_sort",
                  "levelpath", "lp_weight_attribute", "lp_name_attribute")))

  attr(x, "orig_names") <- orig_names
  class(x) <- c("hy", class(x))

  diff = 1
  checker <- 0
  done <- 0

  x <- arrange(x, .data$topo_sort)

  to_ind <- make_index_ids(x)

  from_ind <- make_fromids(to_ind, return_list = TRUE)

  x$done <- rep(FALSE, nrow(x))

  x$ind <- seq_len(nrow(x))
  x$toind <- to_ind$to[1,]

  outlet_ind <- x[which(x$toid == get_outlet_value(x)),]

  while(done < nrow(x) & checker < 10000000) {

    pathids <- if(nrow(outlet_ind) == 1) {
      list(par_get_path(as.list(outlet_ind), x, from_ind, status, "lp_weight_attribute"))
    } else {
      lapply(split(outlet_ind, seq_len(nrow(outlet_ind))),
             par_get_path,
             x_in = x, from_ind = from_ind,
             status = status, wat = "lp_weight_attribute")
    }

    pathids <- bind_rows(pathids)

    x$levelpath[pathids$ind] <- pathids$levelpath

    done <- done + nrow(pathids)

    x$done[pathids$ind] <- rep(TRUE, nrow(pathids))

    # grab everything that goes to the path we just followed
    outlet_ind <- na.omit(as.numeric(from_ind$froms[, pathids$ind]))
    # remove the path we just follwed
    outlet_ind <- x[outlet_ind[!outlet_ind %in% pathids$ind],]

    checker <- checker + 1

    if(status && checker %% 1000 == 0) {
      message(paste(done, "of", nrow(x), "remaining."))
    }
  }

  x <- add_levelpath_outlet_ids(x)

  x <- put_hyg(x, hy_g)

  x <- select(x, all_of(c(id, toid, levelpath_outlet_id, topo_sort, levelpath)),
              !all_of(c("done", "lp_name_attribute", "lp_weight_attribute", "ind", "toind")))

  left_join(x, select(extra, -any_of(c("levelpath_outlet_id", "topo_sort", "levelpath"))),
            by = id)

}

par_get_path <- function(outlet, x_in, from_ind, status, wat) {
  out <- get_path(x = x_in,
                  tailid = outlet[names(outlet) == "ind"][[1]],
                  from_ind = from_ind,
                  status = status, wat = wat)
  tibble(ind = out,
         levelpath = rep(outlet[names(outlet) == "topo_sort"][[1]],
                         length(out)))
}

add_levelpath_outlet_ids <-  function(x) {
  left_join(x, st_drop_geometry(x) |>
              group_by(.data$levelpath) |>
              filter(.data$topo_sort == min(.data$topo_sort)) |>
              ungroup() |>
              select(levelpath_outlet_id = "id", "levelpath"),
            by = "levelpath")
}

#' get level path
#' @noRd
#' @description Recursively walks up a network following the nameid
#' or, if no nameid exists, maximum weight column.
#' @param x data.frame with id, toid, nameid and weight columns.
#' @param tailid integer or numeric id of outlet catchment.
#' @param override_factor numeric follow weight if this many times larger
#' @param status print status?
#'
get_path <- function(x, tailid, from_ind, status, wat) {

  keep_going <- TRUE
  tracker <- rep(NA, nrow(x))
  counter <- 1

  toid <- NULL

  while(keep_going) {
    tryCatch({
      next_tails <- x[na.omit(from_ind$froms[,tailid]), ]

      if(nrow(next_tails) > 1) {

        next_tails <- next_tails[next_tails[[wat]] == max(next_tails[[wat]]), ]

      }

      if(nrow(next_tails) == 0) {

        keep_going <- FALSE

      }

      # if(tailid %in% tracker) stop(paste0("loop at", tailid))

      tracker[counter] <- tailid

      counter <- counter + 1

      tailid <- next_tails$ind

      if(status && counter %% 1000 == 0) message(paste("long mainstem", counter))
    }, error = function(e) {
      stop(paste0("Error with outlet tailid ", tailid, "\n",
                  "Original error was \n", e))
    })

  }

  tracker[!is.na(tracker)]
}

reweight <- function(x, ..., override_factor, nat, wat) {

  if(nrow(x) > 1) {

    cur_name <- x$ds_nameid[1]

    max_weight <- max(x[[wat]])

    rank <- 1

    total <- nrow(x)

    out <- x

    if(any(x[[nat]] != " ")) { # If any of the candidates are named.
      if(cur_name != " " & cur_name %in% x[[nat]]) {
        sub <- arrange(x[x[[nat]] == cur_name, ], desc(.data[[wat]]))

        out[1:nrow(sub), ] <- sub

        rank <- rank + nrow(sub)

        x <- x[!x$id %in% sub$id, ]
      }

      if(rank <= total) {
        if(any(x[[nat]] != " ")) {
          sub <-
            arrange(x[x[[nat]] != " ", ], desc(.data[[wat]]))

          out[rank:(rank + nrow(sub) - 1), ] <- sub

          rank <- rank + nrow(sub)

          x <- x[!x$id %in% sub$id, ]

        }

        if(rank <= total) {
          out[rank:total, ] <- x
        }

      }
    }

    if(!is.null(override_factor)) {
      out <- mutate(out, "{wat}" := ifelse(.data[[nat]] == .data$ds_nameid,
                                           .data[[wat]] * override_factor,
                                           .data[[wat]]))
    }

    if(rank < nrow(out)) {
      out[rank:nrow(out), ] <- arrange(x, desc(.data[[wat]]))
    }

    if(!is.null(override_factor)) {
      out <- arrange(out, desc(.data[[wat]]))
    }

    x <- out

  }

  x[[wat]] <- seq(nrow(x), 1)

  x
}
