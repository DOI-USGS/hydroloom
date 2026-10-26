##### 00_hydroloom.R contains package wide variables and key package declarations. #####

#' @importFrom dplyr filter select left_join right_join all_of any_of bind_rows group_by
#' @importFrom dplyr ungroup n rename row_number between arrange desc distinct mutate summarise
#' @importFrom dplyr everything as_tibble pull group_split tibble bind_cols lag case_when
#' @importFrom data.table copy data.table as.data.table setnames
#' @importFrom tidyr unnest replace_na pivot_wider
#' @importFrom rlang :=
#' @importFrom sf "st_geometry<-" st_drop_geometry st_geometry st_geometry_type st_intersects
#' @importFrom sf st_cast st_linestring st_is_longlat st_transform st_segmentize st_buffer
#' @importFrom sf st_as_sf st_sf st_zm st_coordinates st_crs st_join st_reverse
#' @importFrom sf st_point st_sfc
#' @importFrom pbapply pblapply pbsapply pbapply pboptions
#' @importFrom RANN nn2
#' @importFrom stats setNames na.omit
#' @importFrom fastmap fastqueue faststack
#' @importFrom utils txtProgressBar setTxtProgressBar adist combn tail
#' @importFrom units set_units as_units
#' @importFrom methods as
#'

# hydroloom uses data.table for some key joins. This ensures the syntax is recognized
.datatable.aware <- TRUE

# hydroloom uses .data for masking in many dplyr functions
# hydroloom attributes are used for tidyselection elsewhere
.data <- NULL

# hydroloom Attributes
id <- "id"
toid <- "toid"
tonode <- "tonode"
fromnode <- "fromnode"
divergence <- "divergence"
divergence_fraction <- "divergence_fraction"
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
upmain <- "upmain"
downmain <- "downmain"
hydro_location_id <- "hydro_location_id"
distance_km <- "distance_km"
past_end_km <- "past_end_km"
id_from_measure <- "id_from_measure"
id_to_measure <- "id_to_measure"

indid <- "indid"
toindid <- "toindid"

# hydroloom S3 class names
hy_classes <- c("hy", "hy_topo", "hy_leveled", "hy_node", "hy_flownetwork")

# hydroloom_name_definitions is read from inst/extdata/hy_vocab.json and
# assigned into the namespace in .onLoad (R/zzz.R). This placeholder lets
# roxygen document and export the object.
hydroloom_name_definitions <- NULL

# declared here to go with hydroloom names.
#' @export
#' @noRd
print.hydroloom_names <- function(x, ...) {
  for (i in seq_along(x)) {
    cat(paste0(i, ' "', names(x)[i], '"', ": \n\t ", unname(x)[i], "\n"))
  }
}

hydroloom_env <- new.env()

check_names <- function(x, req_names, context) {
  if (!all(req_names %in% names(x)))
    stop(paste(context, "requires", paste(req_names, collapse = ", "),
      "hydroloom attributes."), call. = FALSE)
}
