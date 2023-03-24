

extract_ndvi <- function(x, start_date, end_date){

  extract_ndvi <- function(start_date, end_date, x) {
    require(lubridate); require(tidyverse)
    start_date <- start_date
    end_date <- end_date
    i <- x
    point <- ee$Geometry$Point(c(tfs$long[i], tfs$lat[i]))
    buff <- ee$Geometry$Point(c(tfs$long[i], tfs$lat[i]))$buffer(200)

    addNDVI <- function(image){
      return(image$addBands(image$normalizedDifference(c('B8','B4'))$rename("ndvi")))
    }

    g_ic <- geemap$create_timeseries("COPERNICUS/S2_SR", start_date = start_date, end_date = end_date,
                                       region = buff,
                                       bands = c("B8", "B4", "B3", "B2"),
                                       frequency = 'month', reducer = 'median')

    ndvi <- g_ic$map(addNDVI)

    green <- ndvi$qualityMosaic('ndvi')

    geemap$download_ee_image(image = green, filename = paste0("green_", x, "_", start_date, ".tif"),
                             region = buff, scale = 1, crs = "EPSG:4326", num_threads = 6)
  }


}

extract_ndvi(path = "/Users/julianflowers/Library/CloudStorage/GoogleDrive-julian.flowers12@gmail.com/My Drive/tiny_forest_project 2/data",
             x = 10,
             start_date = "2022-06-01",
             end_date = "2022-09-01")

