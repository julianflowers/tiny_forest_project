needs(rgee, rgeeExtra, reticulate, tidyverse, RColorBrewer, leaflet, mapview, leaflet.extras2, furrr, future)
plan(multisession, workers = 4)
ee_Initialize(user = "julian.flowers12@gmail.com", drive = TRUE, gcs = TRUE)

tf <- read_csv("data/tf_lat_long.csv")

sent <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")
sent

i <- 7

point <- ee$Geometry$Point(c(tf$long[i], tf$lat[i]))$buffer(200)
bounds <- ee$Geometry$Point(c(tf$long[i], tf$lat[i]))$buffer(1000)

start <- "2018-01-01"
end <- "2023-01-01"

addNDVI <- function(image){
  ndvi = image$normalizedDifference(c('B8','B4'))
}



sent1 <- sent$filterDate(start, end)
sent1 <- sent1$filterBounds(bounds)
sent1 <- sent1$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 20))

#cat(unlist(sent1$first()$getInfo()))
#sent1_ndvi <- sent1$select("B8", "B4")

#cat(unlist(sent1$propertyNames()))


ic_d2 <- ee_get_date_ic(sent1)

ids <- pluck(ic_d2, "id")

images <- future_map(ids, ee$Image)
ndvi <- map(images, addNDVI)
ndvi_summary <- purrr::map_dfr(ndvi, ~ee_extract(.x, y = point, fun = ee$Reducer$median(), sf = TRUE))

ndvi_summary |>
  write_csv(paste0("data/ndvi",i, ".csv" ))

ndvi_summary |>
  bind_cols(ic_d2) |>
  ggplot() +
  geom_point(aes(as.Date(time_start), nd)) +
  geom_smooth(aes(as.Date(time_start), nd), span = 0.2) +
  scale_x_date(date_breaks = "6 months")



i <- 8

calc_ndvi_s2 <- function(i){


  point <- ee$Geometry$Point(c(tf$long[i], tf$lat[i]))$buffer(200)
  bounds <- ee$Geometry$Point(c(tf$long[i], tf$lat[i]))$buffer(1000)

start <- "2018-01-01"
end <- "2023-01-01"

addNDVI <- function(image){
  ndvi = image$normalizedDifference(c('B8','B4'))
}



sent1 <- sent$filterDate(start, end)
sent1 <- sent1$filterBounds(bounds)
sent1 <- sent1$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 20))

#cat(unlist(sent1$first()$getInfo()))
#sent1_ndvi <- sent1$select("B8", "B4")

#cat(unlist(sent1$propertyNames()))


ic_d2 <- ee_get_date_ic(sent1)

ids <- pluck(ic_d2, "id")

images <- future_map(ids, ee$Image)
ndvi <- map(images, addNDVI)
ndvi_summary <- purrr::map_dfr(ndvi, ~ee_extract(.x, y = point, fun = ee$Reducer$median(), sf = TRUE))

ndvi_summary |>
  write_csv(paste0("data/ndvi",i, ".csv" ))

ndvi_summary |>
  bind_cols(ic_d2) |>
  ggplot() +
  geom_point(aes(as.Date(time_start), nd)) +
  geom_smooth(aes(as.Date(time_start), nd), span = 0.2) +
  scale_x_date(date_breaks = "6 months")

}

map(11:20, calc_ndvi_s2)
