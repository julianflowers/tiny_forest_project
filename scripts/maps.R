## green space in tf buffers

library(needs)
needs(tidyverse, sf, mapview, vegan, rvest, myScrapers, here, ggmap, fontawesome)

fontawesome::fa-

## tfs

tf <- read_csv("data/tf_detail.csv") |>
  mutate(plant_date = dmy(plant_date))
tf_ll <- read_csv("data/tf_lat_long.csv")

tf_ll_sf <- tf_ll |>
  st_as_sf(coords = c("long", "lat"), crs = 4326)

### 1k buffer
tf_ll_buff <- st_buffer(tf_ll_sf, 1000)

## greenspace
gs_path <- here::here("/Users/julianflowers/Dropbox/My Mac (Julians-MBP-2)/Downloads/OS Open Greenspace (GPKG) GB/data")

gf <- list.files(gs_path, "gpkg", full.names = TRUE)

gs_data <- read_sf(gf[1], layer = "GreenspaceSite")

gs_data <- gs_data |>
  mutate(area = st_area(gs_data))

st_crs(gs_data)

## join

join_tf_buff <- st_join(tf_ll_buff, gs_data, join = st_contains)
gs_filt <- gs_data |>
  st_filter(join_tf_buff, by = "id")

gs_filt |>
  mapview(zcol = "function") +
  mapview(tf_ll_buff) +
  mapview(join_tf_buff)

join_tf_buff |>
  group_by(title, `function`) |>
  summarise(gs_area = sum(area)) |>
  drop_na(gs_area) |>
  write_sf("data/tf_green_space.shp")

##

tf_gs <- read_sf("data/tf_green_space.shp") |> mutate(title = str_sub(title, 2, -2))

unique(tf_gs$title)
unique(tf$stub)

mapview(tf_gs, zcol = "function")

##

ggmap::register_google(key = "AIzaSyBhAWbDyKX2chM2Jz0ZV22uT_8qN7YKL3Q")
coords <- st_coordinates(tf_ll_sf) |>
  data.frame() |>
  bind_cols(year = year(tf$plant_date[1:177]),
            area = tf$plant_area[1:177]) |>
  mutate(label = fontawesome())
  drop_na()

ggmap(get_map(location = c(-1, 54), zoom = 6, maptype = "toner-lite")) +
  geom_point(data = coords, aes(x = X, y = Y, colour = factor(year), size = log(area)), show.legend = FALSE) +
  facet_wrap(~year, nrow = 2) +
  theme_void() +
  ggtitle("Number and location of tiny forests by year of planting") +
  theme(plot.title.position = "plot")

