library(needs)

#remotes::install_github("r-earthengine/rgeeExtra")

#library(rgeeExtra)

needs(reticulate, tidyverse, rgee, raster, terra, mapview, stars, sf, prismatic, tidyrgee, rgeeExtra)

reticulate::virtualenv_list()
reticulate::virtualenv_remove("geemap")
reticulate::virtualenv_create("geemap")
reticulate::use_virtualenv("geemap")

# py_install("earthengine-api", envname = "geemap", pip = TRUE)
# py_install("geemap", envname = "geemap", pip = TRUE)
# py_install("IPython", pip = TRUE, envname = "geemap") #install the IPython package
# py_install("planet", pip = TRUE, envname = "geemap")
# py_install("plotly", pip = TRUE, envame = "geemap")
# py_install("ipyleaflet", pip = TRUE, envame = "geemap")
# py_install("geedim", pip = TRUE, envame = "geemap")
# py_install("eemont", pip = TRUE, envname = "geemap")
# py_install("ee_extra", pip = TRUE, envname = "geemap")


Sys.setenv(RETICULATE_PYTHON = "/Users/julianflowers/.virtualenvs/geemap/bin/python")

ee <- import("ee")
#leaflet <- import("ipyleaflet")
plotly <- import("plotly")
#planet <- import("planet")
#ipy <- import("IPython")
geemap <- import("geemap")
#folium <- import("folium")
ee_extra <- import("ee_extra")

os <- import("os")
geedim <- import("geedim")
import("eemont")


ee$Authenticate()

library(rgee)
ee_Initialize()

df <- read_csv("data/tf_trees.csv")                    ## import tf data

s2 <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED") ## import image collection

id <- 214

tf_pd <- df |>
  filter(tf_id == id) |>
  pluck("date") |>
  as.character()

lat <- df |>
  filter(tf_id == id) |>
  pluck("lat")

lon <- df |>
  filter(tf_id == id) |>
  pluck("lon")

point <- ee$Geometry$Point(lon, lat)$buffer(100)

tf <- ee$Geometry$Point(lon, lat)$buffer(1000)

visParams <- list(min = -.5, max = .8, palette = c("red","yellow","green"))

start_date <- "2023-01-01"
end_date <- "2023-04-01"

s2_filt <- s2$filterBounds(tf)
s2_filt <- s2$filterDate(start_date, end_date)
s2_filt <- s2_filt$filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 10))



s2_tidy <- tidyrgee::as_tidyee(s2_filt)

s2_quarterly <- s2_tidy$vrt |>
  mutate(yq = zoo::as.yearqtr(time_start))

s2_quarterly |>
  print(n = 48)

s2_spring_summer <- s2_quarterly |>
  filter(str_detect(yq, ("Q2|Q3")))




## Map
Map$centerObject(tf)
Map$addLayer(tf, name = "Buffer") +
  Map$addLayer(point, name = "Loc") +
  Map$addLayer(s2_filt$median()$clip(tf), visParams = list(min = 0, max = 3000, bands = list("B4", "B3", "B2"), gamma = c(0.9, 1.1, 1)), name = "RGB")
s2_spring_summer$yq

e1 <- ee$Image(s2_quarterly$id[37])$select("B8", "B5", "B4", "B3", "B2")
e2 <- ee$Image(s2_quarterly$id[38])$select("B8", "B5", "B4", "B3", "B2")
e3 <- ee$Image(s2_quarterly$id[39])$select("B8", "B5", "B4", "B3", "B2")
e4 <- ee$Image(s2_quarterly$id[40])$select("B8", "B5", "B4", "B3", "B2")
e5 <- ee$Image(s2_quarterly$id[41])$select("B8", "B5", "B4", "B3", "B2")
e6 <- ee$Image(s2_quarterly$id[42])$select("B8", "B5", "B4", "B3", "B2")
e7 <- ee$Image(s2_quarterly$id[43])$select("B8", "B5", "B4", "B3", "B2")
e8 <- ee$Image(s2_quarterly$id[44])$select("B8", "B5", "B4", "B3", "B2")
e9 <- ee$Image(s2_quarterly$id[45])$select("B8", "B5", "B4", "B3", "B2")
e10 <- ee$Image(s2_quarterly$id[46])$select("B8", "B5", "B4", "B3", "B2")
e11 <- ee$Image(s2_quarterly$id[47])$select("B8", "B5", "B4", "B3", "B2")
e12 <- ee$Image(s2_quarterly$id[48])$select("B8", "B5", "B4", "B3", "B2")


ic1 <- ee$ImageCollection$fromImages(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12)
em1 <- ic1()

Map$addLayer(ic1$median()$clip(buffer))

, visParams = list(min = 0, max = 65000, bands = c("B4", "B3", "B2")))

r <- map(c(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12), ee_as_raster, region = tf)

map(r, raster)

median(r[1:6])

r[[2]]

i <- 5

add_ndvi <- function(i){
      rn <- (r[[i]]$B8 - r[[i]]$B4) / (r[[i]]$B8 + r[[i]]$B4)
}

map(1:6, add_ndvi) |>
  brick()

plot(rn, col =  c(
  "#FFFFFF", "#CE7E45", "#DF923D", "#F1B555", "#FCD163", "#99B718", "#74A901",
  "#66A000", "#529400", "#3E8601", "#207401", "#056201", "#004C00", "#023B01",
  "#012E01", "#011D01", "#011301"
))

