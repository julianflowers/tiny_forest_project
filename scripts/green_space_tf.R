## create linked greenspace and tiny forest data

needs(sf, tidyverse, here, mapview)

here()

## process green space data and extract allotments / parks
gs_path <- "/Users/julianflowers/Dropbox/My Mac (Julians-MBP-2)/Downloads/OS Open Greenspace (GPKG) GB/data"

gf <- list.files(gs_path, "gpkg", full.names = TRUE)

gs_data <- read_sf(gf[1], layer = "GreenspaceSite")

allotments <- gs_data |>
  filter(str_detect(`function`, "Allotment|Park"))

allotments <- allotments |>
  mutate(gs_area = st_area(allotments),
         gs_centroid = st_centroid(allotments))


## tidy forest sf data

tf_data <- read_csv("data/tf_detail.csv")
tf_data
tf_ll <- read_csv("data/tf_lat_long.csv") |>
  mutate(title = str_remove_all(title, '\"'))

## remove mismatches
tf_data <- tf_data |>
  slice(-c(58, 59, 165, 167, 172, 174, 175, 178, 179))


tf_ll <- tf_ll |>
  slice(-c(171:177))

## create sf

tf_data_sf <- tf_data |>
  bind_cols(tf_ll) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326)

tf_data_sf |>
  ggplot() +
  geom_sf()

## save as shp

write_sf(tf_data_sf, "data/tf_shp.shp")

glimpse(tf_data_sf)

tf_data_buff <- st_buffer(tf_data_sf, 1000) |>
  st_transform(27700)

st_crs(allotments)
ol <- st_contains(y = st_centroid(allotments), tf_data_buff)

centroids <- st_centroid(allotments)

allot_near_tf <- st_distance(centroids, st_transform(tf_data_sf, 27700))

allot_near_tf_long <- allot_near_tf |>
  data.frame() |>
  bind_cols(id = centroids$id) |>
  pivot_longer(names_to = "green", values_to = "vals", cols = X1:X170) |>
  mutate(vals = units::drop_units(vals)) |>
  filter(vals < 2000)

ids <- pluck(allot_near_tf_long, "id") |>
  unique()

allotments |>
  filter(id %in% ids) |>
  mapview() +
  mapview(tf_data_buff, col.regions = "pink")


