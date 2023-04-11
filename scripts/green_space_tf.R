## create linked greenspace and tiny forest data

needs(sf, tidyverse, here, mapview, data.table, ggmap, tmap)

map_data(worldMapEnv)

ggmap::register_google(key = "AIzaSyBhAWbDyKX2chM2Jz0ZV22uT_8qN7YKL3Q")

here()

## process green space data and extract allotments / parks
gs_path <- "/Users/julianflowers/Dropbox/My Mac (Julians-MBP-2)/Downloads/OS Open Greenspace (GPKG) GB/data"
gs_file <- "/Users/julianflowers/Dropbox/My Mac (Julians-MBP-2)/Downloads/Priority_Habitats_Inventory_England_2210546699479199036.gpkg"


gf <- list.files(gs_path, "gpkg", full.names = TRUE)

gs_data <- read_sf(gf[1], layer = "GreenspaceSite") |>
  st_transform(4326)

f_data <- st_read(gs_file)


uk.maps <- map_data("world", region = "UK")

uk.maps |>
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = region)) +
  geom_point(data = tf_data, aes(x = lon, y = lat)) +
  theme_void()








head(f_data)

setDT(f_data)[, .N, by = (MainHabs)]

woodland <- f_data |>
  filter(str_detect(MainHabs, "Decid")) |>
  st_transform(crs = 4326)



woodland_sample <- sample_frac(woodland, 0.1)

allotments <- gs_data |>
  filter(str_detect(`function`, "Allotment|Park"))

allotments <- allotments |>
  mutate(gs_area = st_area(allotments),
         gs_centroid = st_centroid(allotments))


## tidy forest sf data

tf_data <- read_csv("data/tf_w_1.csv")


## create sf

tf_data_sf <- tf_data |>
  st_as_sf(coords = c("lon", "lat"), crs = 3426) |>
  st_transform(4326)

tf_data_sf |>
  ggplot() +
  geom_sf()

## save as shp

write_sf(tf_data_sf, "data/tf_shp.shp")

tf_data_sf <- read_sf("data/tf_shp.shp")

tf_data_buff <- st_buffer(tf_data_sf, 1000)

st_crs(tf_data_sf)

st_intersection(tf_data_buff, gs_data)

st_crs(tf_data_buff)

tf_wood <-st_intersection(tf_data_buff, woodland_sample)

tmap::tm_shape(st_geometry(tf_data_buff)) +
  tm_polygons(fill = "area")

get_map(c(lon = -.1, lat = 54), zoom = 2) |>
  ggmap() +
  coord_sf(crs = st_crs(3857)) +
  geom_sf(data = tf_data_buff, inherit.aes = FALSE, aes(colour = area) )





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

st_geometry(tf_data_buff)

allotments |>
  filter(id %in% ids) |>
  mapview() +
  mapview(st_geomtf_data_buff, col.regions = "pink")


