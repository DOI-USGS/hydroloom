##### decomposition_viz.R -- manual visual inspection of decompose_network ####
#
# Renders the at-scale NLDI basin (USGS-08082500) decomposition as a
# flowline map colored by domain_id. The fixture is the same one the
# Layer 9 test (test_decomposition_at_scale.R) uses, cached under
# nhdplusTools_data_dir() so it does not need to be re-fetched.
#
# Manual / inspection only. Not run by `devtools::test()`. Run from the
# package root with:
#
#   /c/Users/dblodgett/AppData/Local/Programs/R/R-4.5.2/bin/Rscript.exe \
#     tests/testthat/manual/decomposition_viz.R
#
# Output: a PNG written to /tmp/hydroloom_decomposition_at_scale.png and
# a console summary of per-domain catchment counts.

devtools::load_all(quiet = TRUE)

library(sf)

# ---- locate the cached fixture -----------------------------------------

cache_dir <- nhdplusTools::nhdplusTools_data_dir()

gpkg <- file.path(cache_dir, "hydroloom_decomp_at_scale.gpkg")

if (!file.exists(gpkg)) {
  stop("at-scale fixture not cached at ", gpkg, "\n",
    "  run the Layer 9 test once to populate it: ",
    "devtools::test(filter = 'decomposition_at_scale')",
    call. = FALSE)
}

message("loading fixture: ", gpkg)

flowlines_sf <- sf::read_sf(gpkg, layer = "NHDFlowline_Network")
catchments_sf <- sf::read_sf(gpkg, layer = "CatchmentSP")
basin <- sf::read_sf(gpkg, layer = "basin")

flowlines_sf <- sf::st_filter(
  flowlines_sf,
  st_compatibalize(basin, flowlines_sf)
)

# Keep only catchments whose featureid matches a flowline comid.
catchments_sf <- catchments_sf[catchments_sf$featureid %in% flowlines_sf$comid, ]

message("flowlines loaded: ", nrow(flowlines_sf))
message("catchments loaded: ", nrow(catchments_sf))

# ---- enrich + decompose ------------------------------------------------

message("enriching network (hy -> add_toids -> add_levelpaths -> add_streamorder)")

src <- hy(flowlines_sf)

# Strip pre-existing decomposition columns (the cached NHDPlusV2 gpkg
# carries stream_order et al. straight from the source). Same drop set
# the test helper enrich_for_decomposition() uses.
drop_canonical <- c("stream_order", "topo_sort", "levelpath",
  "levelpath_outlet_id", "pathlength_km", "stream_level")

src[intersect(drop_canonical, names(src))] <- NULL

src <- add_toids(src, return_dendritic = TRUE)

# detect name/weight columns the same way the helper does
name_attribute <- if ("gnis_id" %in% names(src)) "gnis_id" else "GNIS_ID"

weight_attribute <- if ("arbolate_sum" %in% names(src)) {
  "arbolate_sum"
} else {
  "ArbolateSu"
}

src <- add_levelpaths(src,
  name_attribute = name_attribute,
  weight_attribute = weight_attribute)

src <- add_streamorder(src, status = FALSE)

message("decomposing network (trunk_threshold = 2000 sqkm)")

d <- decompose_network(src, trunk_threshold = 2000)

message("decomposition built: ", length(d$domains), " domains")

# ---- per-domain summary ------------------------------------------------

summary_df <- data.frame(
  domain_id = vapply(d$domains, \(x) x$domain_id, character(1)),
  type = vapply(d$domains, \(x) x$domain_type, character(1)),
  n_catchments = vapply(d$domains, \(x) nrow(x$catchments), integer(1)),
  trunk_parent = vapply(d$domains,
    \(x) x$trunk_domain_id %||% NA_character_, character(1)),
  stringsAsFactors = FALSE,
  row.names = NULL
)

summary_df <- summary_df[order(summary_df$type, -summary_df$n_catchments), ]

cat("\n=== domain summary ===\n")
print(summary_df, row.names = FALSE)

cat("\n=== totals ===\n")
cat("trunk domains:   ", sum(summary_df$type == "trunk"), "\n")
cat("compact domains: ", sum(summary_df$type == "compact"), "\n")
cat("total catchments:", sum(summary_df$n_catchments), "\n")
cat("source rows:     ", nrow(src), "\n")

# ---- trunk size distribution -------------------------------------------
#
# Tabulates trunk-domain catchment counts. With trunk_threshold = 100,
# each basin whose outlet total_da_sqkm exceeds 100 km² gets one trunk
# domain containing all catchments above the threshold. Basins at or
# below the threshold get no trunk -- just a single compact domain.

trunk_sizes <- summary_df$n_catchments[summary_df$type == "trunk"]

trunk_size_dist <- as.data.frame(
  table(n_catchments = trunk_sizes),
  stringsAsFactors = FALSE,
  responseName = "n_trunks"
)

trunk_size_dist$n_catchments <- as.integer(
  as.character(trunk_size_dist$n_catchments))

trunk_size_dist <- trunk_size_dist[order(trunk_size_dist$n_catchments), ]

cat("\n=== trunk size distribution ===\n")
print(trunk_size_dist, row.names = FALSE)

# ---- join domain_id back to catchment polygons -------------------------

idx <- d$catchment_domain_index

# Map catchment featureid -> domain_id through the enriched src ids.
# src$id is the hy-canonical id (was comid); catchments_sf$featureid
# is the raw NHDPlusV2 key. Both are integer-valued.
catch_id_chr <- as.character(catchments_sf$featureid)

domain_for_catch <- unname(idx[catch_id_chr])

type_for_domain <- setNames(summary_df$type, summary_df$domain_id)

plot_sf <- catchments_sf

plot_sf$domain_id <- domain_for_catch

plot_sf$domain_type <- unname(type_for_domain[domain_for_catch])

# Drop any catchments that didn't map (shouldn't happen, but be safe).
plot_sf <- plot_sf[!is.na(plot_sf$domain_id), ]

# Order so trunks draw on top of compacts.
plot_sf <- plot_sf[order(plot_sf$domain_type == "trunk"), ]

# ---- color palette -----------------------------------------------------

set.seed(20260408)

domain_levels <- unique(plot_sf$domain_id)

pal <- grDevices::hcl.colors(length(domain_levels), palette = "Spectral")

pal <- sample(pal)

names(pal) <- domain_levels

plot_sf$color <- pal[plot_sf$domain_id]

# ---- render ------------------------------------------------------------

# out_png <- normalizePath(
#   file.path("hydroloom_decomposition_at_scale.png"),
#   mustWork = FALSE)

# dir.create(dirname(out_png), showWarnings = FALSE, recursive = TRUE)

# grDevices::png(out_png, width = 1600, height = 1600, res = 150)

par(mar = c(0, 0, 2, 0))

plot(sf::st_geometry(plot_sf),
  col = plot_sf$color,
  border = NA,
  main = sprintf(
    "decompose_network(trunk_threshold=2000): %d domains (%d trunk, %d compact) on %d catchments",
    length(d$domains),
    sum(summary_df$type == "trunk"),
    sum(summary_df$type == "compact"),
    nrow(plot_sf)))

# grDevices::dev.off()

# message("wrote ", out_png)
