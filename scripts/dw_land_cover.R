## 10 m land-cover
Sys.setenv(RETICULATE_PYTHON = "/Users/julianflowers/.virtualenvs/geemap/bin/python")

library(needs)
needs(reticulate, tidyverse, rgee, raster, terra, mapview, stars, sf, prismatic, tidyrgee, mapview)

## set virtual env
reticulate::use_virtualenv("geemap")

## import python packages
import("ee")
gm <- import("geemap")
eem <- import("eemont")


## initialize ee
ee$Initialize()


## load tf data
df <- read_csv("data/tf_w_1.csv")

## sf

df_sf <- df |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)




## function to extract lat-lon

get_coords <- function(df, id){

  tf <- filter(df, tfid == id)
  lon <- tf$lon
  lat <- tf$lat
  out <- list(lon, lat)

}

## function to extract planting data
get_plant_date <- function(df, id){

  plant_date <- filter(df, tfid == id) |>
    pluck("when") |>
    as.character()


}


x <- get_coords(df, id = 214)
lat <- x[[2]]
lon <- x[[1]]
end_date <- get_plant_date(df, id = 214)
start_date <- "2019-01-01"


## function to visualise dynamicworld data

plot_dw_images <- function(start_date, end_date, lon, lat, buf){

  require(tidyrgee)


  dw_ic <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")
  point <- ee$Geometry$Point(lon, lat)
  buffer <- ee$Geometry$Point(lon, lat)$buffer(buf)

  dw_1 <- dw_ic$filterDate(start_date, end_date)
  dw_1 <- dw_1$filterBounds(buffer)

  #dates <- rgee::ee_get_date_ic(dw_1)

  dw_pal = c('#419BDF', '#397D49', '#88B053', '#7A87C6','#E49635', '#DFC35A',
        '#C4281B', '#A59B8F','#B39FE1')

  visParams <- list(min = 0, max = 8, palette = dw_pal)

  Map$setCenter(lon, lat, zoom = 15)
  Map$addLayer(dw_1$select('label')$median()$clip(buffer), visParams = list(min = 0, max = 8, palette = dw_pal), opacity = 0.7, name = "lc") +
    Map$addLayer(point)


}



####

plot_dw_images("2019-01-01", end_date, x[[1]], x[[2]], 1000)

####


plot_s2_veg_images <- function(start_date, end_date, lon, lat, buf){

  s2_ic <- ee$ImageCollection("COPERNICUS/S2_SR")
  point <- ee$Geometry$Point(lon, lat)
  buffer <- ee$Geometry$Point(lon, lat)$buffer(1000)

  s2_1 <- s2_ic$filterDate(start_date, end_date)
  s2_1 <- s2_1$filterBounds(buffer)
  s2_1 <- s2_1$filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 10))
  s2_1 <- s2_1$scale()$index(c('NDVI', 'NDSI', 'EVI'))

  #dates <- rgee::ee_get_date_ic(s2_1)

  veg_pal <- c(
    "#FFFFFF", "#CE7E45", "#DF923D", "#F1B555", "#FCD163", "#99B718", "#74A901",
    "#66A000", "#529400", "#3E8601", "#207401", "#056201", "#004C00", "#023B01",
    "#012E01", "#011D01", "#011301"
  )


  visParams <- list(min = -.5, max = .9, palette = veg_pal)

  Map$setCenter(lon, lat, zoom = 15)
  Map$addLayer(s2_1$median()$select('NDVI')$clip(buffer), visParams = visParams, opacity = 0.7, name = "nvdi") +
    Map$addLayer(point)

}

plot_s2_veg_images("2020-01-01", d1, x[[1]], x[[2]], buf = 1000)




point <- ee$Geometry$Point(x[[1]], x[[2]])
buffer <- ee$Geometry$Point(x[[1]], x[[2]])$buffer(1000)

s2_ic <- ee$ImageCollection("COPERNICUS/S2_SR")

s2_1 <- s2_ic$filterDate(start_date, end_date)
s2_1 <- s2_1$filterBounds(buffer)
s2_1 <- s2_1$filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 10))








