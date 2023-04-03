## create linked greenspace and tiny forest data

needs(sf, tidyverse, here, mapview)

here()

## process green space data and extract allotments / parks
gs_path <- "/Users/julianflowers/Dropbox/My Mac (Julians-MBP-2)/Downloads/OS Open Greenspace (GPKG) GB/data"
dw_file <- "/Users/julianflowers/Dropbox/My Mac (Julians-MBP-2)/Downloads/Priority_Habitats_Inventory_England_2210546699479199036.gpkg"

gf <- list.files(gs_path, "gpkg", full.names = TRUE)

gs_data <- read_sf(gf[1], layer = "GreenspaceSite")

dw_data <- st_read(dw_file)

woodland_data <- dw_data |>
  filter(str_detect(MainHabs, "Decid"))

allotments <- gs_data |>
  filter(str_detect(`function`, "Allotment|Park"))

allotments <- allotments |>
  mutate(gs_area = st_area(allotments),
         gs_centroid = st_centroid(allotments))


## tidy forest sf data

tf_data <- read_csv("data/tf_w_1.csv")

## remove mismatches
## create sf

tf_data_sf <- tf_data |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

tf_data_sf |>
  ggplot() +
  geom_sf()

tf_data_buff <- st_buffer(tf_data_sf, 1000) |>
  st_transform(27700)

tf_data_buff |>
  mutate(area = st_area(tf_data_buff),
         mean_area = mean(area) )


st_crs(allotments)
ol <- st_contains(y = st_centroid(allotments), tf_data_buff)

inter <-st_intersection(tf_data_buff, allotments) ## overlap between tf buffer and urban greenspace

wood <- st_intersection(tf_data_buff, woodland_data)

gs_buff <- select(inter, who:tfid, type = function.) |>
  bind_rows(select(wood, who:tfid, type = MainHabs))

gs_area <- gs_buff |>
  mutate(area = units::set_units(st_area(gs_buff), m2)) |>
  group_by(tfid) |>
  mutate(tot_area = sum(area),
         age = today() - when)

gs_area |>
  ggplot() +
  geom_density(aes(log(units::drop_units(area))), fill = "red", colour = "black", binwidth = 1.1)

gs_wide <- gs_area |>
  st_drop_geometry() |>
  group_by(tfid, type) |>
  summarise(total_area = sum(area)) |>
  mutate(total_area = units::drop_units(total_area)) |>
  pivot_wider(names_from = "type", values_from = "total_area", values_fn = sum, values_fill = 0)

gs_age <- select(gs_area, tfid, age, when, ) |>
  st_drop_geometry() |>
  distinct()

gs_wide <- gs_wide |>
  left_join(gs_age) |>
  janitor::clean_names()

gs_wide |>
  write_csv("tf_gs.csv")

gs_wide |>
  mutate(gs_area = deciduous_woodland + public_park_or_garden + allotments_or_community_growing_spaces) +
  ggplot() +
  geom_col(aes)

wood |>
  mapview(col.regions = "darkgreen") +
  mapview(tf_data_buff) +
  mapview(inter, col.regions = "brown") +
  mapview(tf_data_sf, col.regions = "black")

inter |>
  group_by(tfid, function.) |>
  summarise(area_1 = sum(area)) |>
  write_csv("gs_within_buf.csv")

View(inter)

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
  filter(id %in% ids)


|>
  mapview() +
  mapview(tf_data_buff, col.regions = "pink")


