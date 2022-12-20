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
total_length_km <- "total_length_km"
topo_sort <- "topo_sort"
up_topo_sort <- "up_topo_sort"
dn_topo_sort <- "dn_topo_sort"
dn_minor_topo_sort <- "dn_minor_topo_sort"
terminal_topo_sort <- "terminal_topo_sort"
terminal_flag <- "terminal_flag"
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
                total_da_sqkm, da_sqkm, length_km, pathlength_km, total_length_km,
                topo_sort, up_topo_sort, dn_topo_sort, dn_minor_topo_sort,
                terminal_topo_sort, terminal_flag, start_flag,
                levelpath, up_levelpath, dn_levelpath,
                stream_level, dn_stream_level, stream_order, dendritic_stream_order,
                feature_type, feature_type_code, vector_proc_unit, raster_proc_unit,
                reachcode, reach_measure, from_measure, to_measure)

hnd <- setNames(rep("", length(good_names)), good_names)

hydroloom_name_definitions <- hnd

# TODO: type out definitions for all package attributes

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
  arbolatesu = total_length_km,

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

required_atts_error <- function(context, required_atts) {
  stop(paste(context, "requires", paste(required_atts, collapse = ", "),
             "hydroloom attributes."))
}

#' @importFrom dplyr filter select left_join all_of any_of bind_rows group_by
#' @importFrom dplyr ungroup n rename row_number arrange desc distinct
#' @importFrom sf "st_geometry<-" st_drop_geometry st_geometry st_as_sf st_sf
#' @importFrom sf st_coordinates st_crs st_join st_reverse st_transform

#' @title create an hy fabric object
#' @description converts a compatible dataset into a fabric s3 class
#' @inheritParams navigate_hydro_network
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
    x$toid[is.na(x$toid)] <- 0
  }

  # strip tbl
  if(inherits(x, "sf")) {
    x <- st_sf(as.data.frame(x))
  } else {
    x <- as.data.frame(x)
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

  if("toid" %in% names(x) & any(is.na(x$toid))) {
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
#' @inheritParams navigate_hydro_network
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

  if(inherits(x, "sf"))
    x <- st_sf(x)

  x

}
