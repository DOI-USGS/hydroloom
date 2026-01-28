reprex::reprex({
library(hydroloom)
library(dplyr)
library(sf)

x <- sf::read_sf(system.file("extdata/new_hope.gpkg",
                             package = "hydroloom")) |>
  select(id = COMID) |> sf::st_transform(5070)

names(x)

make_attribute_topology(x, min_distance = 10)
}, venue = "rtf")

plot(sf::st_geometry(rmapshaper::ms_simplify(x, keep = .5)), col = "darkblue")

reprex::reprex({
  library(hydroloom)
  library(dplyr)
  library(sf)

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg",
                               package = "hydroloom")) |>
    select(id = COMID) |> sf::st_transform(5070) |>
    make_attribute_topology(min_distance = 10)

  as.tbl(make_node_topology(x, add_div = TRUE))
}, venue = "rtf")

reprex::reprex({
  library(hydroloom)
  library(dplyr)
  library(sf)

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg",
                               package = "hydroloom")) |>
    select(id = COMID, GNIS_NAME, FTYPE) |>
    sf::st_transform(5070)

  x <- left_join(make_attribute_topology(x, min_distance = 10) |>
                   make_node_topology(add_div = TRUE),
                 x, by = "id")

  outlets <- filter(x, !tonode %in% fromnode)

  add_divergence(x, coastal_outlet_ids = outlets$id, inland_outlet_ids = NULL,
                 name_attr = "GNIS_NAME", type_attr = "FTYPE",
                 major_types = "StreamRiver")

}, venue = "rtf")

reprex::reprex({
  library(hydroloom)
  library(dplyr)
  library(sf)

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg",
                               package = "hydroloom")) |>
    select(id = COMID, GNIS_NAME, FTYPE) |>
    sf::st_transform(5070)

  x <- left_join(make_attribute_topology(x, min_distance = 10) |>
                   make_node_topology(add_div = TRUE),
                 x, by = "id")

  outlets <- filter(x, !tonode %in% fromnode)

  x <- add_divergence(x, coastal_outlet_ids = outlets$id, inland_outlet_ids = NULL,
                      name_attr = "GNIS_NAME", type_attr = "FTYPE",
                      major_types = "StreamRiver")

  add_streamorder(x) |> st_sf() |> st_drop_geometry() |> select(-GNIS_NAME, -FTYPE)

}, venue = "rtf")

reprex::reprex({
  library(hydroloom)
  library(dplyr)
  library(sf)

  x <- sf::read_sf(system.file("extdata/new_hope.gpkg",
                               package = "hydroloom")) |>
    select(id = COMID, GNIS_NAME, FTYPE, LENGTHKM) |>
    sf::st_transform(5070)

  x <- left_join(make_attribute_topology(x, min_distance = 10) |>
                   make_node_topology(add_div = TRUE),
                 x, by = "id")

  outlets <- filter(x, !tonode %in% fromnode)

  x <- add_divergence(x, coastal_outlet_ids = outlets$id, inland_outlet_ids = NULL,
                      name_attr = "GNIS_NAME", type_attr = "FTYPE",
                      major_types = "StreamRiver") |>
    add_toids()
  x <- mutate(x, arbolate_sum = accumulate_downstream(x, "LENGTHKM", quiet = TRUE))

  x <- add_levelpaths(x, name_attribute = "GNIS_NAME", weight_attribute = "arbolate_sum") |>
    st_sf() |> st_drop_geometry() |> select(-GNIS_NAME, -FTYPE, -LENGTHKM)

}, venue = "rtf")

x <- sf::read_sf(system.file("extdata/new_hope.gpkg",
                             package = "hydroloom")) |>
  select(id = COMID) |> sf::st_transform(5070)
x <- rmapshaper::ms_simplify(x, keep = .9)

x <- sf::st_sf(x)
f <- sf::st_buffer(filter(x, id == 8893556), dist = units::as_units(2, "km"))
plot(sf::st_geometry(f), border = NA)

plot(x["levelpath"], reset = FALSE, lwd = 2)

plot(sf::st_geometry(x), lwd = x$stream_order, col = "darkblue", add = TRUE)
plot(sf::st_geometry(x), lwd = x$stream_calculator, col = "magenta", add = TRUE)
plot(sf::st_geometry(filter(x, divergence == 2)), col = "magenta", add = TRUE, lwd = 2)

plot(sf::st_geometry(get_node(x, "start")), pch = "O", cex = .8, add = TRUE)
plot(sf::st_geometry(get_node(x, "end")), pch = "x", cex = .7, add = TRUE)
