needs(rgee, reticulate, tidyverse, RColorBrewer, leaflet, mapview, leaflet.extras2, furrr, future)

plan(multisession)

getwd()
tf <- read_csv("tf_lat_long.csv")

tf |>
  DT::datatable()


reticulate::virtualenv_remove("rgee")
reticulate::virtualenv_create("rgee", system_site_packages = TRUE, python = "/usr/local/bin/python3.10")
reticulate::py_install(c("earthengine-api", "poetry"), pip = TRUE, envname = "rgee")
use_python("/Users/julianflowers/.virtualenvs/rgee/bin/python")
import("poetry")
reticulate::py_install(c("geedim", "ee_extra", "regex", "jsbeautifier", "geemap"), pip = TRUE, envname = "rgee")

reticulate::use_virtualenv("rgee")
reticulate::virtualenv_list()

reticulate::import("poetry", convert = TRUE)

ee <- import("ee")
ee_extra <- import("ee_extra")
geemap <- import("geemap")

ee_clean_pyenv()
ee_install()
ee_check()



ee_Initialize(user = "julian.flowers12@gmail.com", drive = TRUE, gcs = TRUE)

ee_get_earthengine_path()

srtm <- ee$Image("USGS/SRTMGL1_003")
viz <- list(
  max = 4000,
  min = 0,
  palette = c("#000000","#5AAD5A","#A9AD84","#FFFFFF")
)
Map$addLayer(
  eeObject = srtm,
  visParams =  viz,
  name = 'SRTM'
)



## dynamic world

dw <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")

start <- "2020-01-01"
end <- "2022-12-31"

point <- ee$Geometry$Point(c(tf$long[87], tf$lat[87]))$buffer(200)
bounds <- ee$Geometry$Point(c(tf$long[87], tf$lat[87]))$buffer(1000)

bounds$getInfo()

dw1 <- dw$filterDate(start, end)
dw1 <- dw1$filterBounds(bounds)$select("label")

ic_d <- ee_get_date_ic(dw1)

dw1$first()$getInfo()


VIS_PALETTE = c('#419BDF', '#397D49', '#88B053', '#7A87C6','#E49635', '#DFC35A', 
                '#C4281B', '#A59B8F','#B39FE1')


Map$centerObject(point, zoom = 15) 

image <- ee$Image(ic_d$id[434])$select("label")

Map$addLayer(image$clip(bounds), opacity = 0.7, visParams = list(min = 0, max = 8, 
                                                        palette = VIS_PALETTE))

Map$addLayer(image$clip(point), opacity = 0.7, visParams = list(min = 0, max = 8, 
                                                                 palette = VIS_PALETTE))

## dw trees and grass (1 and 2)

dw2 <- dw1$filterBounds(bounds)$select(c("trees", "grass", "built"))


ic_d1 <- ee_get_date_ic(dw2)

image1 <- ee$Image(ic_d1$id[16])$select("trees")

unlist(image1$max)

dwMasked <- image1$updateMask(image1$gte(0.1))

Map$addLayer(dwMasked$clip(bounds), opacity = 0.7, 
             visParams = list(min = 0, max = 1, 
                              palette = viridis::mako(10, direction = -1)))


## sentinel images

sent <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")
rgeeExtra::ee_Image_spectralIndex(sent$first(),index = "NDVI")

addNDVI <- function(image){
  ndvi = image$normalizedDifference(c('B8','B4'))
}

sent1 <- sent$filterDate(start, end)
sent1 <- sent1$filterBounds(bounds) 
sent1 <- sent1$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 20))
cat(unlist(sent1$first()$getInfo()))
#sent1_ndvi <- sent1$select("B8", "B4")


ic_d2 <- ee_get_date_ic(sent1)

ids <- pluck(ic_d2, "id")

images <- map(ids, ee$Image)
ndvi <- map(images, addNDVI)


s <- map(ndvi[1], ~.x$select('nd')$median())

ndvi_summary_1 <- purrr::map_dfr(ndvi, ~ee_extract(.x, y = bounds, fun = ee$Reducer$mean(), sf = TRUE))

ndvi_ts <- ndvi_summary_1 |>
  bind_cols(ic_d2)

ndvi_ts |>
  mutate(year_mon = zoo::as.yearmon(as.Date(str_sub(time_start, 1, 10)))) |>
  group_by(year_mon) |>
  summarise(ndvi = mean(nd)) |>
  ggplot(aes(year_mon, ndvi)) +
  geom_point() +
  geom_line() +
  zoo::scale_x_yearmon()
  geom_smooth(method = 'loess', span = 0.5)
  
ndvi_ts |>
  ggplot() +
  geom_sf(aes(color = nd))

image2 <- ee$Image(ic_d2$id[2])
image3 <- ee$Image(ic_d2$id[214])

i2_ndvi <- addNDVI(image2)
i3_ndvi <- addNDVI(image3)




image2$getInfo()

ndvi_Masked_1 <- i2_ndvi$updateMask(i2_ndvi$gte(0.7))
ndvi_Masked_2 <- i3_ndvi$updateMask(i2_ndvi$gte(0.7))


m <- Map$addLayer(ndvi_Masked_1$clip(bounds), opacity = 0.8, 
             visParams = list(min = -1, max = 1, 
                              palette = brewer.pal("Greens", n = 9)))

m1 <- Map$addLayer(ndvi_Masked_2$clip(bounds), opacity = 0.8, 
                  visParams = list(min = -1, max = 1, 
                                   palette = brewer.pal("Greens", n = 9)))

m2 <- Map$addLayer(image3$clip(bounds), visParams = list(bands = c("B4", "B3", "B2"), 
                                                         min = 0, max = 1800))



m | m1

mapview(m1)



ee_extract(i3_ndvi, bounds)
rgee::ee_as_raster(i3_ndvi)

leaflet() %>% 
  addTiles() %>% 
  setView(tf$long[87], tf$lat[87], 15) %>% 
  addTiles(
    urlTemplate = m1$rgee$tokens,
    layerId = "leaflet_false_color",
    options = leaflet::tileOptions(opacity = 1)
  )
