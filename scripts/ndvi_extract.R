needs(rgee, rgeeExtra, reticulate, tidyverse, RColorBrewer, geojsonio, leaflet, mapview, leaflet.extras2, furrr, future)
plan(multisession, workers = 4)
reticulate::virtualenv_remove("rgee")
reticulate::virtualenv_create("rgee", system_site_packages = TRUE)
reticulate::use_python('/Users/julianflowers/.virtualenvs/rgee/bin/python')
reticulate::use_virtualenv("rgee")
reticulate::py_install(packages = c("earthengine-api", "geemap"), pip = TRUE,
                       envname = "rgee")
ee <- import("earthengine_api")
geemap <- import("geemap")
ee_Initialize(user = "julian.flowers12@gmail.com", drive = TRUE, gcs = TRUE)

tf <- read_csv("data/tf_lat_long.csv")

tf |>
  mutate(row = row_number()) |>
  filter(str_detect(title, "Five"))

sent <- ee$ImageCollection("COPERNICUS/S2_SR")
sent

i <- 90

point <- ee$Geometry$Point(c(tf$long[i], tf$lat[i]))$buffer(200)
bounds <- ee$Geometry$Point(c(tf$long[i], tf$lat[i]))$buffer(1000)

start <- "2018-01-01"
end <- "2023-01-01"

addNDVI <- function(image){
  return(image$addBands(image$normalizedDifference(c('B8','B4'))$rename("ndvi")))
}



green$getInfo()

sent1 <- sent$filterDate(start, end)
sent1 <- sent1$filterBounds(bounds)
sent1 <- sent1$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 10))

ndvi <- sent1$map(addNDVI)
green <- ndvi$qualityMosaic('ndvi')
sent1min <- sent1$min()
sent1med <- sent1$median()

visParams <- list(bands = c('B4', 'B3', 'B2'), min = 300, max = 1600)

Map$centerObject(bounds)
Map$addLayer(sent1$mosaic()$clip(bounds), visParams = visParams, "mosaic") +
  Map$addLayer(sent1min$clip(bounds), visParams = visParams, "min") +
  Map$addLayer(sent1med$clip(bounds), visParams = visParams, "median")

Map$addLayer(green$select('ndvi')$clip(point), visParams = list(min = -0.5, max = 0.8,  palette = viridis::viridis(10, direction = 1),
                                                  opacity = 0.8), 'ndvi')

Map$addLegend(visParams = list(min = -0.5, max = 0.8,  palette = viridis::viridis(10, direction = 1),
                               opacity = 0.8))

rgee::ee_extract(green, y = bounds)


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
