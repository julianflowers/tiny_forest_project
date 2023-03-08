needs(rgee, rgeeExtra, reticulate, tidyverse, RColorBrewer, leaflet, mapview, leaflet.extras2, furrr, future)
plan(multisession, workers = 4)
ee_Initialize(user = "julian.flowers12@gmail.com", drive = TRUE, gcs = TRUE)

rgeeExtra::

tf <- read_csv("tf_lat_long.csv")

sent <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")
sent_2 <- ee_extra_scaleAndOffset(sent)

sent

i <- 1

point <- ee$Geometry$Point(c(tf$long[1], tf$lat[1]))$buffer(200)
bounds <- ee$Geometry$Point(c(tf$long[1], tf$lat[1]))$buffer(1000)

addNDVI <- function(image){
  ndvi = image$normalizedDifference(c('B8','B4'))
}

sent1 <- sent$filterDate(start, end)
sent1 <- sent1$filterBounds(bounds) 
sent1 <- sent1$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 20))
#cat(unlist(sent1$first()$getInfo()))
#sent1_ndvi <- sent1$select("B8", "B4")


ic_d2 <- ee_get_date_ic(sent1)

ids <- pluck(ic_d2, "id")

images <- future_map(ids, ee$Image)
ndvi <- map(images, addNDVI)
ndvi_summary<- purrr::map_dfr(ndvi, ~ee_extract(.x, y = bounds, fun = ee$Reducer$mean(), sf = TRUE))
