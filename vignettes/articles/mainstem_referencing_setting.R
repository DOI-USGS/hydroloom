# Renders mainstem_referencing_setting.png, the setting figure shown at the
# top of the mainstem referencing article. The networks, confluence and point
# come from the article's own chunks, pulled in with knitr::purl() so the
# figure and the article cannot drift apart. Run from the package root.
#
# source("vignettes/articles/mainstem_referencing_setting.R")

library(hydroloom)
library(sf)

article <- "vignettes/articles/mainstem_referencing.Rmd"

# evaluate named chunks of the article in this environment
run_chunks <- function(rmd, labels) {
  tmp <- tempfile(fileext = ".R")
  knitr::purl(rmd, output = tmp, quiet = TRUE, documentation = 1L)
  lines <- readLines(tmp)
  heads <- grep("^## ----", lines)
  names(heads) <- sub("^## ----([^-]+)-+.*$", "\\1", lines[heads])
  ends <- c(heads[-1] - 1, length(lines))
  for (l in labels) {
    i <- which(names(heads) == l)
    eval(parse(text = lines[heads[i]:ends[i]]), envir = parent.frame())
  }
}

# v2, net_3dhp, the confluence ne_v2, the indexed point point_sf, the map
# window view, the 3DHP mainstems ms_3dhp and the waterbodies wb
run_chunks(article,
  c("mainstems", "v2", "threedhp", "confluences", "index", "map_setup"))

# the referenced confluence: upstream end of the first receiving flowline
confluence <- get_node(v2[v2$id == ne_v2$id, ], "start")

# IBM color-blind-safe palette; frac and pos place each label along the
# in-view piece of its mainstem
mainstems <- data.frame(
  uri = c(nh, ne, mc),
  label = c("New Hope River", "Northeast Creek", "Morgan Creek"),
  col = c("#648FFF", "#DC267F", "#785EF0"),
  frac = c(0.45, 0.25, 0.5),
  pos = c(4, 2, 1))
col_3dhp <- "#FE6100"
point_fill <- "#FFB000"

on <- function(u) v2[!is.na(v2$aggregate_id) & v2$aggregate_id == u, ]

# a point part way along the longest in-view piece of a mainstem
anchor <- function(x, frac) {
  line <- suppressWarnings(sf::st_crop(sf::st_geometry(x), view))
  line <- sf::st_cast(sf::st_line_merge(sf::st_union(line)), "LINESTRING")
  line <- line[which.max(sf::st_length(line))]
  sf::st_coordinates(sf::st_line_sample(line, sample = frac))
}

png("vignettes/articles/mainstem_referencing_setting.png",
  width = 7, height = 7, units = "in", res = 150)

oldpar <- par(mar = c(0.5, 0.5, 0.5, 0.5))

plot(view, border = NA, reset = FALSE)
plot(sf::st_geometry(wb), add = TRUE, col = "grey90", border = NA)
plot(sf::st_geometry(v2), add = TRUE, col = "grey80", lwd = 0.7)

for (i in seq_len(nrow(mainstems))) {
  m <- mainstems[i, ]
  plot(sf::st_geometry(on(m$uri)), add = TRUE, col = m$col, lwd = 3)
}

plot(sf::st_geometry(ms_3dhp), add = TRUE, col = col_3dhp, lwd = 2)

plot(sf::st_geometry(confluence), add = TRUE, pch = 17, col = "black",
  cex = 2)
plot(sf::st_geometry(point_sf), add = TRUE, pch = 21, bg = point_fill,
  col = "black", cex = 2)

for (i in seq_len(nrow(mainstems))) {
  m <- mainstems[i, ]
  a <- anchor(on(m$uri), m$frac)
  text(a[1], a[2], m$label, col = m$col, pos = m$pos, font = 2, cex = 1.1)
}

legend("bottomright", bty = "n", cex = 1,
  legend = c("Northeast Creek confluence",
    "the referenced point", "mainstems as drawn by 3DHP",
    "3DHP waterbodies", "other New Hope flowlines"),
  pch = c(17, 21, NA, 22, NA), pt.bg = c(NA, point_fill, NA, "grey90", NA),
  col = c("black", "black", col_3dhp, "grey70", "grey60"),
  lwd = c(NA, NA, 2, NA, 1), pt.cex = c(1.6, 1.6, NA, 2, NA))

# scale bar: 2 km, lower left
bb <- sf::st_bbox(view)
sx <- bb["xmin"] + 600
sy <- bb["ymin"] + 500
segments(sx, sy, sx + 2000, sy, lwd = 3)
segments(c(sx, sx + 2000), sy - 100, c(sx, sx + 2000), sy + 100, lwd = 2)
text(sx + 1000, sy, "2 km", pos = 3, cex = 1)

par(oldpar)

dev.off()
