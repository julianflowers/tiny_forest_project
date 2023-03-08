library(rgee);library(tidyverse, quietly = TRUE);library(reticulate);library(raster);library(mapview);library(stars);library(RStoolbox);library(RColorBrewer);library(googleCloudStorageR)

reticulate::virtualenv_list()

reticulate::virtualenv_create("rgee")
reticulate::use_virtualenv("rgee")

reticulate::use_python('/Users/julianflowers/.virtualenvs/geemap/bin/python')

import("ee")

ee_Initialize(user = "julian.flowers12@gmail.com", drive = T, gcs =T)

aoi = ee$Geometry$Point(0.152687, 52.221545)
start_date = '2020-01-01'
end_date = '2020-06-02'
CLOUD_FILTER = 35
CLD_PRB_THRESH = 50
NIR_DRK_THRESH = 0.15
CLD_PRJ_DIST = 1
BUFFER = 50

s2_sr_col = (ee$ImageCollection('COPERNICUS/S2_SR')
               $filterBounds(aoi)
               $filterDate(start_date, end_date)
               $filter(ee$Filter$lte('CLOUDY_PIXEL_PERCENTAGE', CLOUD_FILTER)))

# Import and filter s2cloudless.
s2_cloudless_col = (ee$ImageCollection('COPERNICUS/S2_CLOUD_PROBABILITY')
                    $filterBounds(aoi)
                    $filterDate(start_date, end_date))

t <- ee_get_date_ic(s2_sr_col) |>
  mutate(id1 = str_remove(id, "COPERNICUS/S2_SR/"))
t1 <- ee_get_date_ic(s2_cloudless_col) |>
  mutate(id1 = str_remove(id, "COPERNICUS/S2_CLOUD_PROBABILITY/"))

tj <- left_join(t, t1, by = "id1")


intersect(t$id1, t1$id1)

imt <- ee$Image(tj$id.x[1])$select("B4")
imt
cat(unlist(imt$getInfo()))
imt1 <- ee$Image(tj$id.y[1])

cat(unlist(imt1$getInfo()))

imt1$bandNames()$getInfo()


s2VisParams = list(bands = c('probability'), min = 0, max = 3000, palette = viridis::cividis(15));


rgee::Map$centerObject(aoi)
rgee::Map$addLayer(imt1$clip(aoi), visParams = s2VisParams)
