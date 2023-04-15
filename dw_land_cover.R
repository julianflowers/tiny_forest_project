## 10 m land-cover

library(needs)

library(rgeeExtra)

needs(reticulate, tidyverse, rgee, raster, terra, mapview, stars, sf, prismatic, tidyrgee, rgeeExtra)

reticulate::use_virtualenv("geemap")
Sys.setenv(RETICULATE_PYTHON = "/Users/julianflowers/.virtualenvs/geemap/bin/python")

import("ee")
import("geemap")
import("eemont")

rgee::ee_Initialize(user = "julian.flowers12@gmail.com", drive = TRUE, gcs = TRUE)

df <- read_csv("data/tf_w_1.csv")

dw_ic <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")

get_coords <- function(df, id){

  tf <- filter(df, tfid == id)
  lon <- tf$lon
  lat <- tf$lat
  out <- list(lon, lat)

}

x <- get_coords(df, 85)
x

point <- ee$Geometry$Point(x[[1]], x[[2]])
buffer <- ee$Geometry$Point(x[[1]], x[[2]])$buffer(1000)

start_date <- "2019-01-01"
end_date <- as.character(today())

dw_1 <- dw_ic$filterDate(start_date, end_date)
dw_1 <- dw_1$filterBounds(buffer)

dates <- rgee::ee_get_date_ic(dw_1)

visParams <- list(min = 0, max = 8, palette = c("419bdf",
                                                "397d49",
                                                "88b053",
                                                "7a87c6",
                                                "e49635",
                                                "dfc35a",
                                                "c4281b",
                                                "a59b8f",
                                                "b39fe1"))

veg_pal <- palette = c(
  "#FFFFFF", "#CE7E45", "#DF923D", "#F1B555", "#FCD163", "#99B718", "#74A901",
  "#66A000", "#529400", "#3E8601", "#207401", "#056201", "#004C00", "#023B01",
  "#012E01", "#011D01", "#011301"
)

dw_pal = c('#419BDF', '#397D49', '#88B053', '#7A87C6','#E49635', '#DFC35A',
        '#C4281B', '#A59B8F','#B39FE1')

Map$setCenter(x[[1]], x[[2]], zoom = 16)
Map$addLayer(dw_1$first()$select('label')$clip(buffer), visParams, name = "dw")
