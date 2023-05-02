## os datahub python

tf <- read_csv("data/tf_trees.csv")

wf <- sf::read_sf("/Users/julianflowers/Dropbox/My Mac (Julians-MBP-2)/Downloads/Download_2260238/wlff-2016_5042295/GB_WLF_V1_0.gdb")

gsp <- here::here("/Users/julianflowers/Dropbox/My Mac (Julians-MBP-2)/Downloads/opgrsp_mbtiles_gb/Data")
f <- list.files(gsp, "mbtiles", full.names = TRUE)

tf |>
  mutate(row = row_number()) |>
  select(row, everything()) |>
  filter(tf_id == 214)

key = "QQfpQgnuiTQLA3fErTbffq8G4VOGdP6b"

plot_os_land_cover <- function(lon, lat, key, buff = 1000, n = 10000L){

  # lon <- lon
  # lat <- lat

  ## load packages
  require(needs)

  needs(tidyverse, reticulate, mapview, sf, tictoc, colourvalues, leaflet)

  ## create virtual environment (this will need to be relative path)
  virtualenv_create("osmaps")
  Sys.setenv(RETICULATE_PYTHON = "/Users/julianflowers/.virtualenvs/osmaps/bin/python")
  key = key

  use_virtualenv("osmaps")


  py_install("osdatahub", pip = TRUE, envname = "osmaps")

  ## import modules

  osmaps <- import("osdatahub")
  Extent <- osmaps$Extent
  geojson <- import("geojson")
  places <- osmaps$PlacesAPI

  ## get os collections

  os_col <- osmaps$NGD$get_collections()
  # point <- c(lon, lat)


  collection = "lnd-fts-land-1"
  ngd = osmaps$NGD(key, collection)

  lon <- lon
  lat <- lat

  x1 <- lon - buff
  x2 <- lon + buff
  y1 <- lat - buff
  y2 <- lat + buff

  bbox <- c(x1, y1, x2, y2)


  extent = Extent$from_bbox(bbox, crs = "EPSG:27700")

  tic()
  results = ngd$query(max_results = n, extent)
  toc()

sf::sf_use_s2(FALSE)

gj <- geojson$dumps(results, sort_keys = TRUE) |>
  sf::read_sf()

tierb <- gj |>
  unnest("oslandcovertierb") |>
  select(oslandcovertierb) |>
  filter(!str_detect(oslandcovertierb, "Made|Under"))



mv <- tierb  |>
  #count(oslandcovertiera, oslandcovertierb) |>
  #unnest("oslandcovertierb")
  mapview(zcol = "oslandcovertierb", col.region = pal)

out <- list(sf = gj, map = mv, tiers = tierb)

}

library(sf)

tf_sf <- st_as_sf(tf, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(27700)

tf_sf_c <- st_coordinates(tf_sf) |>
  data.frame() |>
  mutate_all(~(round(.x, 0)))

p <- plot_os_land_cover(lon = tf_sf_c$X[101], lat = tf_sf_c$Y[101], key = key, buff = 1000)

p$
p$sf |>
  mutate(area = st_area(p$sf)) |>
  st_drop_geometry() |>
  unnest("oslandcovertierb") |>
  group_by(oslandcovertierb) |>
  summarise(sum_area = sum(area),
            prop_area = sum_area / 4000000) |>
  arrange(-prop_area)





tf_buff <- tf |>
  select(tf_id, lon, lat) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(27700) |>
  st_buffer(1000)

osl <- p$sf |>
  select(oslandcovertierb) |>
            st_transform(27700) |>
  unnest("oslandcovertierb")

osl |>
  st_join(tf_buff[86,], join = st_within) |>
  mapview(zcol = "oslandcovertierb", col.region = RColorBrewer::brewer.pal("RdOrGn", n = 11))


###

i <- 7

p <- plot_os_land_cover(lon = tf_sf_c$X[i], lat = tf_sf_c$Y[i], key = key, buff = 1000)

w <- plot_os_water_cover(lon = tf_sf_c$X[i], lat = tf_sf_c$Y[i], key = key, buff = 1000)



os_green <- st_intersection(p$sf |>
                  unnest("oslandcovertierb") |>
                  filter(!str_detect(oslandcovertierb, "Made|Under|Excav|Sand|Shin|Mud|Boul|Marsh")) |>
          st_transform(27700) , tf_buff[i,])


linear_wood <- st_intersection(wf, tf_buff[i,])

open_greenspace <- read_sf(f[1], layer = "greenspace_site")

green_space <- st_transform(open_greenspace, crs = 27700)

green_space  <- st_intersection(green_space, tf_buff[i,])

water <- st_transform(w$sf, crs = 27700) |>
  st_intersection(tf_buff[i,])

ggplot() +
  geom_sf(aes(colour = function., fill = NULL), data = green_space, lwd = 2) +
  geom_sf(aes(fill = oslandcovertierb), data = os_green) +
  geom_sf(data = linear_wood, colour= "darkgreen", lwd = 2, lty = "dashed") +
  geom_sf(data = water, fill = "blue") +
  scale_fill_manual(values = pal, name = NULL) +
  theme_void() +
  labs(title = "Green Infrastructure")


st_crs(wf)

wf_86 <- st_intersection(wf, tf_buff[86,]) |>
  mapview()


ggplot() +
  geom_sf() +
  theme_void()


mapview(zcol = "oslandcovertierb", col.region = pal)
library(colorRamps)


results1 = ngd$query(max_results = 5000L)

g <- geojson$dumps(results1, sort_keys = TRUE) |>
  sf::read_sf()

pluck(g, "oslandcovertierb") |>
  unique() |>
  enframe() |>
  unnest("value") |>
  pluck("value") |>
  unique() |>
  factor() ->
  land


pal <- colorspace::sequential_hcl(palette = "YlGnBu", rev = TRUE, n = length(unique(p$sf$oslandcovertierb)))

colorspace::hcl_palettes()

palette <- colorFactor(palette = pal, levels = land[1:6])
previewColors(palette, land[1:10])


plot_os_water_cover <- function(lon, lat, key, buff = 1000, n = 10000L){

  # lon <- lon
  # lat <- lat

  ## load packages
  require(needs)

  needs(tidyverse, reticulate, mapview, sf, tictoc, colourvalues, leaflet)

  ## create virtual environment (this will need to be relative path)
  virtualenv_create("osmaps")
  Sys.setenv(RETICULATE_PYTHON = "/Users/julianflowers/.virtualenvs/osmaps/bin/python")
  key = key

  use_virtualenv("osmaps")


  py_install("osdatahub", pip = TRUE, envname = "osmaps")

  ## import modules

  osmaps <- import("osdatahub")
  Extent <- osmaps$Extent
  geojson <- import("geojson")
  places <- osmaps$PlacesAPI

  ## get os collections

  os_col <- osmaps$NGD$get_collections()
  # point <- c(lon, lat)


  collection = "wtr-fts-water-1"
  ngd = osmaps$NGD(key, collection)

  lon <- tf_sf_c$X[86]
  lat <- tf_sf_c$Y[86]
  buff <- 1000
  n <- 1000L

  x1 <- lon - buff
  x2 <- lon + buff
  y1 <- lat - buff
  y2 <- lat + buff

  bbox <- c(x1, y1, x2, y2)


  extent = Extent$from_bbox(bbox, crs = "EPSG:27700")

  tic()
  results = ngd$query(max_results = n, extent)
  toc()

  sf::sf_use_s2(FALSE)

  gj <- geojson$dumps(results, sort_keys = TRUE) |>
    sf::read_sf()

  out <- list(sf = gj)

}
