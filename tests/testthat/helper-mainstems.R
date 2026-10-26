ne <- "https://geoconnex.us/ref/mainstems/2408627"  # Northeast Creek
mc <- "https://geoconnex.us/ref/mainstems/2408909"  # Morgan Creek
nh <- "https://geoconnex.us/ref/mainstems/2408056"  # New Hope River

# NHDPlusV2 New Hope sample as a hy_topo with mainstem URIs as aggregate_id
read_v2 <- function() {

  e <- new.env()
  source(system.file("extdata", "new_hope_mainstems.R", package = "hydroloom"),
    local = e)

  sf::st_zm(add_toids(hy(e$new_hope_mainstems)))
}

# 3DHP New Hope sample renamed to hydroloom names, or NULL when the sample
# is not available
read_3dhp <- function(keep_geometry = FALSE) {

  e <- new.env()
  try(source(system.file("extdata", "new_hope_3dhp.R",
    package = "hydrogeofetch"), local = e), silent = TRUE)

  f <- e$sample_new_hope_3dhp
  if (is.null(f) || !file.exists(f)) return(NULL)

  d3 <- sf::read_sf(f)

  m <- match(d3$dnhydrosequence, d3$hydrosequence)
  d3$toid <- d3$id3dhp[m]
  d3$toid[is.na(d3$toid)] <- ""

  d3 <- if (keep_geometry) sf::st_zm(d3) else sf::st_drop_geometry(d3)

  dplyr::transmute(d3,
    id = id3dhp, toid = toid, aggregate_id = mainstemid,
    topo_sort = hydrosequence, length_km = lengthkm, divergence = divergence)
}
