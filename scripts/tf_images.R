## get satellite images from google maps
needs(magick, gt, gtExtras)

save_ggmap_images<- function(i){

Sys.setenv(GGMAP_GOOGLE_API_KEY= "AIzaSyBhAWbDyKX2chM2Jz0ZV22uT_8qN7YKL3Q")
library(needs)
needs(ggmap, tidyverse, sf, here, lubridate, vegan, data.table, mapview)

tf_w <- fread("data/tf_trees.csv") |>
  mutate(year = year(date),
         year_month = zoo::as.yearmon(date)) |>
  as_tibble()

tf_1 <- tf_w |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

tf_1 |>
  st_buffer(1000) %>%
  .[4,] |>
  mapview()

ggmap::get_googlemap(center = c(lon = tf_w$lon[i], lat = tf_w$lat[i]),
                          zoom = 19, maptype = "satellite") |>
  ggmap() -> p


ggsave(paste0(here::here(), "/images/tf_", i, ".png"), p)

}

get_ggmap_terrain_images<- function(i, zoom = 12){

  Sys.setenv(GGMAP_GOOGLE_API_KEY= "AIzaSyBhAWbDyKX2chM2Jz0ZV22uT_8qN7YKL3Q")
  library(needs)
  needs(ggmap, tidyverse, sf, here, lubridate, vegan, data.table, mapview)

  tf_w <- fread("data/tf_trees.csv") |>
    mutate(year = year(date),
           year_month = zoo::as.yearmon(date))

  tf_1 <- tf_w |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

  ggmap::get_googlemap(center = c(lon = tf_w$lon[i], lat = tf_w$lat[i]),
                       zoom = zoom, maptype = "terrain") |>
    ggmap() -> p

  p <- p +
    geom_point(aes(tf_w$lon[i], tf_w$lat[i]), pch = 10, size = 4, colour = "red" )


  ggsave(paste0(here::here(), "/images/tf_terrain_", i, ".png"), p)

}


get_ggmap_terrain_images(1, zoom = 16)

here::here()
map(.x = 1:170, ~(get_ggmap_terrain_images(.x)))


tf_w_im <- tf_w |>
  mutate(id = row_number())

terrain_files <- list.files(here::here("images"), "png", full.names = TRUE)[180:349] |>
  enframe()

sat_files <- list.files(here::here("images"), "png", full.names = TRUE)[3:179] |>
  enframe()

image_table <- sat_files |>
  mutate(img = str_extract(value, "tf_.*png"),
         id = parse_number(img)) |>
  arrange(id) |>
  slice(-c(171:177))

image_table_1 <- sat_files |>
  mutate(img1 = str_extract(value, "tf_.*png"),
         id = parse_number(img1),
         value1 = value) |>
  arrange(id)

image_table_1 |>
  tail()
j <- left_join(image_table, tf_w_im)

j |>
  left_join(image_table_1, by = "id")

j |>
  gt::gt() |>
  gt_img_rows(value, img_source = "local")



