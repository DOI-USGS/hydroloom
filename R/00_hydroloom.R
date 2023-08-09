# hydroloom Attributes
id <- "id"
toid <- "toid"
tonode <- "tonode"
fromnode <- "fromnode"
divergence <- "divergence"
wbid <- "wbid"
total_da_sqkm <- "total_da_sqkm"
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
levelpath_outlet_id <- "levelpath_outlet_id"
up_levelpath <- "up_levelpath"
dn_levelpath <- "dn_levelpath"
stream_level <- "stream_level"
dn_stream_level <- "dn_stream_level"
stream_order <- "stream_order"
stream_calculator <- "stream_calculator"
feature_type <- "feature_type"
feature_type_code <- "feature_type_code"
vector_proc_unit <- "vector_proc_unit"
raster_proc_unit <- "raster_proc_unit"
id_measure <- "id_measure"
aggregate_id <- "aggregate_id"
aggregate_id_measure <- "aggregate_id_measure"
aggregate_id_from_measure <- "aggregate_id_from_measure"
aggregate_id_to_measure <- "aggregate_id_to_measure"
point_id <- "point_id"
offset <- "offset"

indid <- "indid"
toindid <- "toindid"

good_names <- c(id, toid, fromnode, tonode, divergence, wbid,
                total_da_sqkm, da_sqkm, length_km, pathlength_km, arbolate_sum,
                topo_sort, up_topo_sort, dn_topo_sort, dn_minor_topo_sort,
                terminal_topo_sort, terminal_flag, terminal_id, start_flag,
                levelpath, up_levelpath, dn_levelpath, levelpath_outlet_id,
                stream_level, dn_stream_level, stream_order, stream_calculator,
                feature_type, feature_type_code, vector_proc_unit, raster_proc_unit,
                id_measure, aggregate_id, aggregate_id_measure,
                aggregate_id_from_measure, aggregate_id_to_measure, point_id, offset)

hnd <- as.list(rep("", length(good_names)))
names(hnd) <- good_names

hnd$id <- "shared network identifier for catchment divide and flowpath or flowline"
hnd$toid <- "indicates to the downstream id. May or may not be dendritic"
hnd$fromnode <- "indicates the node representing the nexus upstream of a catchment"
hnd$tonode <- "indicates the node represneting the nexus downstream of a catchment"
hnd$divergence <- "indicates whether a catchment is not downstream of a diversion (0),
                   the primary path downstream of a divergence (1),
                   or a minor path downstream of a diversion (2)."
hnd$wbid <- "waterbody id"
hnd$total_da_sqkm <- "total drainage area at the outlet of a catchment"
hnd$da_sqkm <- "local drainage area of a catchment"
hnd$length_km <- "length of a single catchment's flowpath"
hnd$pathlength_km <- "distance from the outlet of a catchment to the terminal outlet of a network"
hnd$arbolate_sum <- "total accumulated length of all upstream flowlines"
hnd$topo_sort <- "Similar to hydrosequence in NHDPlus. Large topo_sort values
                  are upstream of small topo_sort values. Note that there are
                  many valid topological sort orders of a directed graph."
hnd$up_topo_sort <- "topo sort value of the upstream mainstem"
hnd$dn_topo_sort <- "topo sort value of the downstream mainstem"
hnd$dn_minor_topo_sort <- "topo sort value of the downstream minor network element with the smallest id"
hnd$terminal_topo_sort <- "topo sort value of the outlet network element"
hnd$terminal_flag <- "1 for network terminous 0 for within network"
hnd$terminal_id <- "id of terminal catchment for entire drainage basin"
hnd$start_flag <- "1 for a headwater, 0 otherwise"
hnd$levelpath <- "provides an identifier for the collection of flowpaths
                  that make up a single mainstem flowpath of a drainage
                  basin"
hnd$up_levelpath <- "levelpath value of the upstream mainstem"
hnd$dn_levelpath <- "levelpath value of the downstream mainstem"
hnd$stream_level <- "starting at 1 for coastal terminals and 4 for inland terminals
                     increments by 1 for each smaller tributary level"
hnd$dn_stream_level <- "stream level of downstream mainstem network element"
hnd$stream_order <- "starting at 1 for headwaters increments by 1 for each larger
                     tributary level, divergences adopt stream order from upstream
                     but returning divergent network does not increment stream order"
hnd$stream_calculator <- "starting at 1 for headwaters and 0 along divirted paths
                          increments by 1 for each larger tributary level, does not
                          increment along diverted paths. Is equal to stream_order
                          along the dendritic network"
hnd$feature_type <- "descriptive feature type monicker"
hnd$feature_type_code <- "compact feature type identifier"
hnd$vector_proc_unit <- "identifier for processing units based on vector encapsulation"
hnd$raster_proc_unit <- "identifier for processing units based on raster encapsulation"
hnd$id_measure <- "interpolative linear reference measure along a single identified feature"
hnd$aggregate_id <- "aggregate identifier useful for 'reach' or 'flowpath' aggregation of flowlines"
hnd$aggregate_id_measure <- "interpolative linear reference measure along an aggregate feature"
hnd$aggregate_id_from_measure <- "interpolative linear reference for downstream end of a single
                                  feature that makes up an aggregate feature"
hnd$aggregate_id_to_measure <- "interpolative linear reference for the upstream end of a single
                                feature that makes up an aggregate feature"
hnd$point_id <- "identifier of hydrologic location point"
hnd$offset <- "offset distance from point to line in units of linear reference analysis units"
hnd$levelpath_outlet_id <- "id of outlet catchment of a levelpath"

hydroloom_name_definitions <- stats::setNames(as.character(hnd), names(hnd))
class(hydroloom_name_definitions) <- c("hydroloom_names", class(hydroloom_name_definitions))

#' @export
#' @noRd
print.hydroloom_names <- function(x, ...) {
  for(i in 1:length(x)) {
    cat(paste0('"', names(x)[i], '"', ": ", unname(x)[i], "\n"))
  }
}

# input names that should be changed to replacement names
hydroloom_name_map <- c(
  comid = id,
  nhdplusid = id,
  featureid = id,
  permanent_identifier = id,
  from_permanent_identifier = id,

  toid = toid,
  tocomid = toid,
  to_permanent_identifier = toid,
  tonode = tonode,
  fromnode = fromnode,
  divergence = divergence,
  wbareacomi = wbid,

  totdasqkm = total_da_sqkm,
  totda = total_da_sqkm,
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
  outletID = levelpath_outlet_id,
  uplevelpat = up_levelpath,
  dnlevelpat = dn_levelpath,

  streamleve = stream_level,
  dnlevel = dn_stream_level,
  streamorde = stream_order,
  streamcalc = stream_calculator,

  ftype = feature_type,
  fcode = feature_type_code,
  vpuid = vector_proc_unit,
  rpuid = raster_proc_unit,

  reachcode = aggregate_id,
  reach_meas = aggregate_id_measure,
  reachcode_measure = aggregate_id_measure,
  frommeas = aggregate_id_from_measure,
  tomeas = aggregate_id_to_measure)

hydroloom_env <- new.env()

assign("hydroloom_name_map", hydroloom_name_map, envir = hydroloom_env)

assign("good_names", good_names, envir = hydroloom_env)

check_names <- function(x, req_names, context) {
  if(!all(req_names %in% names(x)))
    stop(paste(context, "requires", paste(req_names, collapse = ", "),
               "hydroloom attributes."), call. = FALSE)
}

get_outlet_value <- function(x) {
  if(inherits(x$id, "character")) {
    ""
  } else {
    0
  }
}

#' @importFrom dplyr filter select left_join right_join all_of any_of bind_rows group_by
#' @importFrom dplyr ungroup n rename row_number arrange desc distinct mutate summarise
#' @importFrom dplyr everything as_tibble pull group_split tibble bind_cols lag case_when
#' @importFrom rlang :=
#' @importFrom sf "st_geometry<-" st_drop_geometry st_geometry st_geometry_type st_intersection
#' @importFrom sf st_cast st_linestring st_is_longlat st_transform st_segmentize st_buffer
#' @importFrom sf st_as_sf st_sf st_zm st_coordinates st_crs st_join st_reverse
#' @importFrom pbapply pblapply pbsapply pbapply pboptions

.data <- NULL

#' @title Create a hy Fabric S3 Object
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

    keep_names <- keep_names[c(which(keep_names != geom_name), which(keep_names == geom_name))]

    x <- st_sf(x, geom = g)

  }

  if("toid" %in% names(x)) {
    out_val <- get_outlet_value(x)

    x$toid <- replace_na(x$toid, out_val)
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

#' Is Valid `hy` Class?
#' @description test if object is a valid according to the hy s3 class
#' @param x object to test
#' @param silent logical should messages be emitted?
#' @return logical TRUE if valid
#' @export
#'
is.hy <- function(x, silent = FALSE) {

  if(!inherits(x, "hy")) {
    if(!silent)
      message("no hy class attribute")
    return(FALSE)
  }

  if("toid" %in% names(x) && any(is.na(x$toid))) {
    if(!silent)
      message("some na toids")
    return(FALSE)
  }

  if(!"orig_names" %in% names(attributes(x))) {
    if(!silent)
      message("no original names attribute")
    return(FALSE)
  }

  TRUE
}

#' Reverse `hy` to Original Names
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
