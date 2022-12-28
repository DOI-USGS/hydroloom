# hydroloom Attributes
id <- "id"
toid <- "toid"
tonode <- "tonode"
fromnode <- "fromnode"
divergence <- "divergence"
wbid <- "wbid"
total_da_sqkm <- "tot_da_sqkm"
da_sqkm <- "da_sqkm"
length_km <- "length_km"
pathlength_km <- "pathlength_km"
arbolate_sum <- "arbolate_sum"
topo_sort <- "topo_sort"
up_topo_sort <- "up_topo_sort"
dn_topo_sort <- "dn_topo_sort"
dn_minor_topo_sort <- "dn_minor_topo_sort"
terminal_topo_sort <- "terminal_topo_sort"
terminal_flag <- "terminal_flag"
terminal_id <- "terminal_id"
start_flag <- "start_flag"
levelpath <- "levelpath"
up_levelpath <- "up_levelpath"
dn_levelpath <- "dn_levelpath"
stream_level <- "stream_level"
dn_stream_level <- "dn_stream_level"
stream_order <- "stream_order"
dendritic_stream_order <- "dendritic_stream_order"
feature_type <- "feature_type"
feature_type_code <- "feature_type_code"
vector_proc_unit <- "vector_proc_unit"
raster_proc_unit <- "raster_proc_unit"
reachcode <- "reachcode"
reach_measure <- "reach_measure"
from_measure <- "from_measure"
to_measure <- "to_measure"

good_names <- c(id, toid, fromnode, tonode, divergence, wbid,
                total_da_sqkm, da_sqkm, length_km, pathlength_km, arbolate_sum,
                topo_sort, up_topo_sort, dn_topo_sort, dn_minor_topo_sort,
                terminal_topo_sort, terminal_flag, terminal_id, start_flag,
                levelpath, up_levelpath, dn_levelpath,
                stream_level, dn_stream_level, stream_order, dendritic_stream_order,
                feature_type, feature_type_code, vector_proc_unit, raster_proc_unit,
                reachcode, reach_measure, from_measure, to_measure)

hnd <- as.list(rep("", length(good_names)))
names(hnd) <- good_names

hnd$id <- "shared network identifier for catchment divide and flowpath"
hnd$toid <- "indicates to the downstream id. May or may not be dendritic"
hnd$topo_sort <- "Similar to hydrosequence in NHDPlus. Large topo_sort values
                  are upstream of small topo_sort values. Note that there are
                  many valid topological sort orders of a directed graph."
hnd$levelpath <- "provides an identifier for the collection of flowpaths
                  that make up a single mainstem flowpath of a drainage
                  basin"
hnd$terminal_flag <- "1 for network terminous 0 for within network"
hnd$terminal_id <- "id of terminal catchment for entire drainage basin"

# TODO: Complete documentation of names

hydroloom_name_definitions <- as.character(hnd)

# input names that should be changed to replacement names
hydroloom_name_map <- c(
  comid = id,
  nhdplusid = id,
  featureid = id,
  toid = toid,
  tocomid = toid,
  tonode = tonode,
  fromnode = fromnode,
  divergence = divergence,
  wbareacomi = wbid,

  totdasqkm = total_da_sqkm,
  areasqkm = da_sqkm,
  lengthkm = length_km,
  pathlength = pathlength_km,
  arbolatesu = arbolate_sum,

  hydroseq = topo_sort,
  uphydroseq = up_topo_sort,
  dnhydroseq = dn_topo_sort,
  dnminorhyd = dn_minor_topo_sort,

  terminalpa = terminal_topo_sort,
  terminalfl = terminal_flag,
  startflag = start_flag,

  levelpathi = levelpath,
  levelpathid = levelpath,
  uplevelpat = up_levelpath,
  dnlevelpat = dn_levelpath,

  streamleve = stream_level,
  dnlevel = dn_stream_level,
  streamrde = stream_order,
  streamcalc = dendritic_stream_order,

  ftype = feature_type,
  fcode = feature_type_code,
  vpuid = vector_proc_unit,
  rpuid = raster_proc_unit,

  reachcode = reachcode,
  reach_meas = reach_measure,
  frommeas = from_measure,
  tomeas = to_measure)

hydroloom_env <- new.env()

assign("hydroloom_name_map", hydroloom_name_map, envir = hydroloom_env)

assign("good_names", good_names, envir = hydroloom_env)

check_names <- function(x, req_names, context) {
  if(!all(req_names %in% names(x)))
    stop(paste(context, "requires", paste(req_names, collapse = ", "),
               "hydroloom attributes."), call. = FALSE)
}

#' @importFrom dplyr filter select left_join all_of any_of bind_rows group_by
#' @importFrom dplyr ungroup n rename row_number arrange desc distinct mutate
#' @importFrom dplyr everything as_tibble pull group_split tibble bind_cols
#' @importFrom sf "st_geometry<-" st_drop_geometry st_geometry st_as_sf st_sf
#' @importFrom sf st_coordinates st_crs st_join st_reverse st_transform
#' @importFrom future.apply future_lapply future_sapply

#' @title create an hy fabric object
#' @description converts a compatible dataset into a fabric s3 class
#' @inheritParams add_levelpaths
#' @param clean logical if TRUE, geometry and non-hydroloom compatible attributes
#' will be removed.
#' @return hy object with attributes compatible with the hydroloom package.
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#'
#' hy(x)
#'
#' hy(x, clean = TRUE)[1:10,]
#'
#' attr(hy(x), "orig_names")
#'
hy <- function(x, clean = FALSE) {

  orig_names <- names(x)

  g <- NULL
  geom_name <- NULL
  if(inherits(x, "sf")) {
    geom_name <- attr(x, "sf_column")
    g <- st_geometry(x)
    x <- drop_geometry(x)
  }

  x <- align_names(x)

  keep_names <- orig_names

  if(clean) {

    keep_names <- orig_names[which(names(x) %in% good_names)]

    x <- select(x, all_of(names(x)[names(x) %in% good_names]))

    if(!is.null(geom_name))
      orig_names <- orig_names[!orig_names %in% geom_name]

  } else if(!is.null(g)) {

    x <- st_sf(x, geom = g)

  }

  if("toid" %in% names(x)) {
    x$toid <- replace_na(x$toid, 0)
  }

  # strip tbl
  if(inherits(x, "sf")) {
    x <- st_sf(as_tibble(x))
  } else {
    x <- as_tibble(x)
  }

  attr(x, "orig_names") <- stats::setNames(names(x), keep_names)

  class(x) <- c("hy", class(x))

  x
}

#' is hy?
#' @description test if object is a valid according to the hy s3 class
#' @param x object to test
#' @return logical TRUE if valid
#' @export
#'
is.hy <- function(x) {

  if(!inherits(x, "hy")) {
    message("no hy class attribute")
    return(FALSE)
  }

  if("toid" %in% names(x) && any(is.na(x$toid))) {
    message("some na toids")
    return(FALSE)
  }

  if(!"orig_names" %in% names(attributes(x))) {
    message("no original names attribute")
    return(FALSE)
  }

  TRUE
}

#' reverse hy to original names
#' @description renames hy object to original names and removes hy object
#' attributes.
#' @inheritParams add_levelpaths
#' @export
#' @examples
#' x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
#' x <- hy(x)
#'
#' hy_reverse(x)
#'
hy_reverse <- function(x) {

  if(!is.hy(x)) stop("must be an hy object")

  orig_names <- attr(x, "orig_names")

  attr(x, "orig_names") <- NULL

  rep_names <- names(orig_names)[match(names(x), orig_names)]

  names(x)[which(names(x) %in% orig_names)] <- rep_names[!is.na(rep_names)]

  class(x) <- class(x)[!class(x) == "hy"]

  if(inherits(x, "sf")) {
    attr(x, "sf_column") <- names(orig_names)[orig_names == attr(x, "sf_column")]
    x <- st_sf(x)
  }

  x

}
