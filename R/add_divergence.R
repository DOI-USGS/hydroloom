required_atts_add_divergence <- c("id", "fromnode", "tonode")

#' Add Divergence Attribute
#' @description Given a non-dendritic flow network and required attributes,
#' adds a divergence attribute according to NHDPlus data model methods.
#' @inheritParams add_levelpaths
#' @param coastal_outlet_ids vector of identifiers for network outlets that
#' terminate at the coast.
#' @param inland_outlet_ids vector of identifiers for network outlets that
#' terminate inland.
#' @param name_attr character attribute name of attribute containing a feature
#' name or name identifier.
#' @param type_attr character attribute name of attribute containing a feature
#' type indicator.
#' @param major_types vector of values of `type_attr` that should be interpreted
#' as being "major". e.g. river might be major and canal might be minor.
#' @details
#'
#' When considering downstream connections with diversions, there are three
#' factors considered to determine which is primary.<br>
#' 1a) same name<br>
#' 1b) is named<br>
#' 2) feature type (type_attr controls this)<br>
#' 3) flows to coast (has a coastal connection is preferred)<br>
#'
#'  The following list describes the order of precedence for tests<br>
#' 1: 1a, 2, 3<br>
#' 2: 1a, 2<br>
#' 3: The NHDPlus uses diverted fraction this is not used currently.<br>
#' 4: 1b, 2, 3<br>
#' 5: 2, 3<br>
#' 6: 1b, 3<br>
#' 7: 3,<br>
#' 8: 1b, 2<br>
#' 9: 2<br>
#' 10: 1b<br>
#'
#' If all checks return and no primary connection has been identified, the
#' connection with a smaller id is chosen.
#'
#' In the case that there are two or more upstream connections, the upstream
#' name to use is chosen 1) if there is only one upstream flowline with a name
#' 2) if one of the upstream flowlines with a name matches the downstream line,
#' 3) if one of the upstream flowlines is of a "major" type and others are not,
#' and, 4) if no criteria exist to select one, the smallest id value otherwise.
#'
#' @name add_divergence
#' @export
#' @examples
#'
#' f <- system.file("extdata/coastal_example.gpkg", package = "hydroloom")
#'
#' g <- sf::read_sf(f)
#' g <- g[g$FTYPE != "Coastline", ]
#'
#' outlets <- g$COMID[!g$ToNode %in% g$FromNode]
#'
#' g <- dplyr::select(g, COMID, gnis_id, FTYPE,
#'                    FromNode, ToNode)
#'
#' add_divergence(g,
#'                coastal_outlet_ids = outlets,
#'                inland_outlet_ids = c(),
#'                name_attr = "gnis_id",
#'                type_attr = "FTYPE",
#'                major_types = c("StreamRiver", "ArtificialPath", "Connector"))
#'
#'
add_divergence <- function(x, coastal_outlet_ids, inland_outlet_ids,
                           name_attr, type_attr, major_types) {
  UseMethod("add_divergence")
}

#' @name add_divergence
#' @export
add_divergence.data.frame <- function(x, coastal_outlet_ids, inland_outlet_ids,
                                      name_attr, type_attr, major_types) {

  x <- hy(x)

  x <- add_divergence(x,
                      coastal_outlet_ids,
                      inland_outlet_ids,
                      name_attr,
                      type_attr ,
                      major_types)

  hy_reverse(x)
}

#' @name add_divergence
#' @export
add_divergence.hy <- function(x, coastal_outlet_ids, inland_outlet_ids,
                              name_attr, type_attr, major_types) {

  x <- select(x, -any_of("toid"))

  name_attr <- align_name_char(name_attr)
  type_attr <- align_name_char(type_attr)

  x[[name_attr]] <- gsub("^\\s*$", "", x[[name_attr]])
  x[[name_attr]][x[[name_attr]] == ""] <- NA

  check_names(x, required_atts_add_divergence, "add_divergence")

  all_term <- x[!x$tonode %in% x$fromnode,]

  if(!all(all_term$id %in% c(coastal_outlet_ids, inland_outlet_ids)))
    stop("All outlets must be included in coastal and inland outlet id parameters")

  term <- split(all_term$id, cut(seq_along(all_term$id), 64, labels = FALSE))

  x_save <- x

  x <- drop_geometry(x) |>
    add_toids(return_dendritic = FALSE) |>
    select(-all_of(c(fromnode, tonode))) |>
    sort_network()

  x_atts <- select(x, all_of(c("id", name_attr, type_attr)))

  x_save <- x_save[x_save$id %in% x$id, ]

  x <- make_fromids(make_index_ids(x), return_list = TRUE)

  paths <- pblapply(term, function(i, net) {
    try(navigate_network_dfs(x = net, starts = i,
                             direction = "up",
                             reset = FALSE))
  }, net = x, cl = "future")

  paths_df <- data.frame(id = unlist(term),
                         paths = I(unlist(paths,
                                          recursive = FALSE))) |>
    mutate(coastal = id %in% coastal_outlet_ids) |>
    unnest(cols = c(paths)) |>
    unnest(cols = c(paths)) |>
    select(all_of(c(id = "paths", "coastal"))) |>
    distinct() |>
    group_by(id) |>
    filter(!(max(n()) > 1 & !.data$coastal)) |> # remove the non coastal outlet divergences
    ungroup() |>
    distinct()

  x <- rename(x_save, all_of(c(name_att = name_attr, type_att = type_attr)))

  name_count <- table(x$name_att)

  junctions <- x |>
    group_by(fromnode) |>
    filter(max(n()) > 1) |>
    pull(fromnode) |>
    unique() |>
    lapply(function(n, x_orig, major_types, name_count) winnow_upstream(n, x_orig, major_types, name_count),
      x_orig = x, major_types = major_types, name_count = name_count)

  junctions <- bind_rows(junctions)

  all_div <- unique(junctions$toid)

  junctions <- junctions |>
    group_by(id) |>
    left_join(paths_df, by = c("toid" = "id")) |>
    mutate(major_type = .data$dn_type_att %in% major_types) |>
    group_split()

  div <- lapply(junctions, down_level)

  x_save |>
    mutate(divergence = case_when(id %in% div ~ 1,
                                  id %in% all_div ~ 2,
                                  TRUE ~ 0))

}

# takes a group of upstream lines and figures out which one should be used
# as the primary for downstream divergence checks.
winnow_upstream <- function(n, x_orig, major_types, name_count) {

  ups <- filter(x_orig, tonode == n)
  dns <- filter(x_orig, fromnode == n)

  if(nrow(ups) > 1 &
     any(sum(!is.na(ups$name_att)) == 1)) {
    # use the one that is named.
    ups <- filter(ups, !is.na(.data$name_att))
  }

  # if that didn't get us there,
  if(nrow(ups) > 1 &
     # if one name matches
     sum(ups$name_att %in% dns$name_att) == 1) {
    ups <- filter(ups, .data$name_att %in% dns$name_att)
  }

  # if one major type and one not
  if(nrow(ups) > 1 &
     sum(ups$type_att %in% major_types) == 1) {
    ups <- filter(ups, .data$type_att %in% major_types)
  }

  # just pick the one with the smaller name id and log
  if(nrow(ups) > 1) {
    if(sum(!is.na(ups$name_att)) > 1) {

      counts <- name_count[names(name_count) %in% ups$name_att]

      pick <- names(counts)[which(counts == max(counts))]

      if(length(pick) == 1) {
        ups <- filter(ups, .data$name_att == pick)

        cat(paste0("picking ", ups$id, " as main.\n"), file = "divergence_checks.txt", append = TRUE)
      }

    }

    if(nrow(ups) > 1) {

      ups <- filter(ups, .data$id == min(.data$id))

    }

  }

  data.frame(id = rep(ups$id, nrow(dns)),
             name_att = rep(ups$name_att, nrow(dns)),
             type_att = rep(ups$type_att, nrow(dns)),
             toid = dns$id,
             dn_name_att = dns$name_att,
             dn_type_att = dns$type_att)

}

# there are three factors considered
# 1a) same name -- 1b) is named
# 2) feature type (canal/ditch and underground conduit are less preferred)
# 3) flows to coast (has a coastal connection is preferred)
#  The following are the order of precedence for tests
# 1: 1a, 2, 3
# 2: 1a, 2
# 3: uses divfrac -- skip
# 4: 1b, 2, 3
# 5: 2, 3 NOTE: there seems to be a type in the manual on this one?
# 6: 1b, 3
# 7: 3,
# 8: 1b, 2
# 9: 2
# 10: 1b
# 11:
down_level <- function(x) {

  # 460 Stream River
  # 558 Artificial Path
  # 334 Connector
  # 336 Canal Ditch
  # 420 Underground Conduit

  # if we have names on the upstream line we can check 1 and 2
  if(!is.na(x$name_att[1])) {
    pick <- which(x$dn_name_att == x$name_att[1] &
                    x$major_type & x$coastal)

    if(length(pick) == 1) return(x$toid[pick])

    pick <- which(x$dn_name_att == x$name_att[1] &
                    x$major_type)

    if(length(pick) == 1) return(x$toid[pick])

    pick <- which(x$dn_name_att == x$name_att[1])

    if(length(pick) == 1) return(x$toid[pick])
  }

  # if any of the downs are named and a major type is in the mix we can check 4
  if(any(!is.na(x$dn_name_att)) & any(x$major_type) & any(x$coastal)) {

    pick <- which(!is.na(x$dn_name_att) & x$major_type & x$coastal)

    if(length(pick) == 1) return(x$toid[pick])

  }

  # if all downs are unnamed and a major type and coastal are in the mix we can check 5
  if(all(is.na(x$dn_name_att)) & any(x$major_type) & any(x$coastal)) {

    pick <- which(x$major_type & x$coastal)

    if(length(pick) == 1) return(x$toid[pick])
  }

  # if any of the downs are named and one goes coastal we can check 6
  if(any(!is.na(x$dn_name_att)) & any(x$coastal)) {

    pick <- which(!is.na(x$dn_name_att) & x$coastal)

    if(length(pick) == 1) return(x$toid[pick])
  }

  # if any goes coastal we can check 7
  if(any(x$coastal)) {

    pick <- which(x$coastal)

    if(length(pick)== 1) return(x$toid[pick])

  }

  # if any of the downs are named and none are coastal we can check 8
  if(any(!is.na(x$dn_name_att)) & !any(x$coastal) & any(x$major_type)) {

    pick <- which(!is.na(x$dn_name_att))

    if(length(pick)== 1) return(x$toid[pick])

  }

  # if any of the downs are major type we can check 9
  if(any(x$major_type)) {

    pick <- which(x$major_type)

    if(length(pick) == 1) return(x$toid[pick])

  }

  # if any of the downs are named we can check 10
  if(any(!is.na(x$dn_name_att)) & !any(x$coastal)) {

    pick <- which(!is.na(x$dn_name_att))

    if(length(pick) == 1) return(x$toid[pick])

  }

  x$toid[which(x$toid == min(x$toid))]

}

#' Add Return Divergence
#' @description Adds a return divergence attribute to the provided network.
#' The method implemented matches that of the NHDPlus except
#' in the rare case that a diversion includes more than one secondary path.
#'
#' Requires and `id`, `fromnode`, `tonode` and `divergence` attribute.
#' See \link{add_divergence} and \link{make_node_topology}.
#'
#' Algorithm:
#'
#' All network connections with more than one downstream feature
#' are considered.
#'
#' \link{navigate_network_dfs} is used to find all downstream
#' features emanating from the primary (`divergence == 1`) outlet of the
#' diversion in question and secondary (`divergence == 2`) outlet(s) starting
#' with the primary outlet.
#'
#' \link{navigate_network_dfs} is called with `reset = FALSE` such that the
#' secondary diversion paths terminate where they combine with a previously
#' visited feature.
#'
#' If the diverted paths result in only one outlet, the feature it flows to
#' is marked as a return divergence.
#'
#' If the diverted paths result in more than one outlet, the one that flows to
#' the most upstream feature in the set of features downstream of the primary
#' outlet of the diversion is marked as the return divergence.
#'
#' @inheritParams add_levelpaths
#' @return data.frame containing `return_divergence` attribute
#' @export
#' @name add_return_divergence
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' x <- hy(x)
#'
#' x <- add_return_divergence(x)
#'
#' sum(x$return_divergence == x$RtnDiv)
#'
#' # see description for documentation of one that does not match
#'
add_return_divergence <- function(x, status = TRUE) {
  UseMethod("add_return_divergence")
}

#' @name add_return_divergence
#' @export
add_return_divergence.data.frame <- function(x, status = TRUE) {
  x <- hy(x)

  x <- add_return_divergence(x, status)

  hy_reverse(x)
}

add_return_divergence.hy <- function(x, status = TRUE) {

  required_atts <- c(id, fromnode, tonode, divergence)

  check_names(x, required_atts, "add_return_divergence")

  net <- select(drop_geometry(x), all_of(required_atts))

  # get all the divergence groups
  all_div <- net |>
    distinct() |>
    group_by(fromnode) |>
    filter(max(n()) > 1) |>
    group_split()

  # get a dendritic network to traverse
  net <- add_toids(net, return_dendritic = FALSE) |>
    add_topo_sort()

  outlets <- lapply(all_div, function(d, g, net) {
    main <- d$id[d$divergence == 1]
    divs <- d$id[d$divergence == 2]

    # need to pass main as the first start
    starts <- c(main, divs)

    paths <- navigate_network_dfs_internal(g, starts, reset = FALSE)

    out <- unlist(lapply(paths[2:length(paths)], function(x) {
      lapply(x, function(x2) tail(unlist(x2, recursive = TRUE, use.names = FALSE), 1))
    }))

    if(length(out) > 1) {
      main <- unlist(paths[1], recursive = TRUE, use.names = FALSE)

      out_net <- net |>
        filter(.data$id %in% out & .data$toid %in% main) |>
        left_join(select(net, all_of(c(id, to_topo_sort = topo_sort))),
                         by = c("toid" = "id")) |>
        filter(.data$to_topo_sort == max(.data$to_topo_sort))

      out <- unique(out_net$id)
    }

    out

  }, g = make_index_ids(net), net = net)

  outlets <- unlist(outlets)

  return <- net$toid[net$id %in% outlets]

  mutate(x, return_divergence = ifelse(id %in% return, 1, 0))

}
