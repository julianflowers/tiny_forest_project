## load libraries

library(needs)
needs(rgee, tidyverse, reticulate, googleCloudStorageR, )


library(rgee);library(tidyverse, quietly = TRUE);library(reticulate);library(raster);library(mapview);library(stars);library(RStoolbox);library(RColorBrewer);library(googleCloudStorageR)
library(cptcity);library(pryr);library(rgdal);library(terra)

cpt

## set up virtual environment and install python packages
reticulate::virtualenv_list()
reticulate::virtualenv_remove("rgee")
reticulate::virtualenv_create("rgee")

reticulate::use_virtualenv("rgee")
reticulate::py_install("geemap", pip = TRUE, envname = "rgee")
reticulate::py_install("geedim", pip = TRUE, envname = "rgee")

reticulate::use_python('/Users/julianflowers/.virtualenvs/geemap/bin/python')

## import python modules
import("ee")
geemap <- import("geemap")
import("geedim")

## initialise
ee_Initialize(user = "julian.flowers12@gmail.com", drive = T, gcs =T)

library(rgeeExtra)
SAK <- "~/Downloads/eemapping-7cdee64d8d3e.json"
ee_utils_sak_copy(sakfile = SAK)
ee_utils_sak_validate()


## set initial parameters (location, date range, cloud cover)
EE_geom <- ee$Geometry$Point(c(-1.215472, 51.769139))$buffer(2000)
start <- "2019-01-01"
end <- "2023-01-01"
ccover <- 35

sr <- ee$ImageCollection("COPERNICUS/S2_SR")

## image collection filters
s2 <- sr$filterBounds(EE_geom)
s2 <- s2$filterDate(start, end)
s2 <- s2$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', ccover))

## load image collections
dataset <- ee$ImageCollection('COPERNICUS/S2_SR')
dataset1 <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")



## clean cloud images
S2_clean <- function(img) {
  # Calculate the NDVI
  ndvi_values <- img$normalizedDifference(c("B8","B4"))

  # Extract the quality band
  img_qa <- img$select("SCL")

  # Create a mask considering: cloud shandows, medium&high clouds, cirrus
  cloud_mask <- img_qa$eq(list(3, 8, 9, 10))$reduce(ee$Reducer$sum())$gt(0)

  # Mask pixels with value zero.
  ndvi_values %>%
    ee$Image$updateMask(cloud_mask) %>%
    ee$Image$copyProperties(img, list("system:time_start"))
}

# Create a monthly composite
ndvi_composite <- sr$
  filterDate('2019-01-01', '2019-12-31')$
  filter(ee$Filter$calendarRange(1, field = "month"))$
  filterBounds(EE_geom)$
  map(S2_clean)$
  median()

rgee::ee_get_date_ic(ndvi_composite)

# Display results
Map$setCenter(lon = -1.215472, lat = 51.769139, zoom = 14)
Map$addLayer(
  eeObject = ndvi_composite$clip(EE_geom),
  visParams = list(
    min = -0.4 ,
    max = 0.9 ,
    palette = brewer.pal("RdYlGn", n = 9),
    opacity = 0.7
  ))


## export images to google drive
geemap$ee_export_image_collection(s2_ndvi, out_dir = "/Users/julianflowers/Library/CloudStorage/GoogleDrive-julian.flowers12@gmail.com/My Drive",
                                    region = EE_geom, crs = "EPSG:4326", scale = 10)

## set drive
path <- here::here("/Users/julianflowers/Library/CloudStorage/GoogleDrive-julian.flowers12@gmail.com/ndvi")

## list files
tifs <- list.files(path, "tif", full.names = TRUE)
tifs

## import single raster
r1 <- raster(tifs[50], band = 17)
s1 <- terra::rast(tifs[1], band)
s2 <- terra::rast(tifs[140], lyrs = 17)

## import all rasters
st <- map(tifs, ~(raster(.x, band = 17)))

## stack rasters
st1 <- stack(st)

## function to calculate raster stats for ndvi
calc_stats <- function(f){

  require(raster)
  r <- raster(f, band = 17)
  mean = cellStats(r, stat = "mean")
  max = cellStats(r, stat = "max")
  min = cellStats(r, stat = "min")
  sd = cellStats(r, stat = "sd")

  out <- list(mean = mean, min = min, max = max, sd = sd)

}

## test
out <- calc_stats(tifs[1])


## apply across all tifs
ndvi_stats <- map(tifs, calc_stats)


## create data frame

ndvi_stats <- enframe(ndvi_stats) |>
  unnest(value) |>
  mutate(stats = rep(c("mean", "max", "min", "sd"), 164)) |>
  unnest("value")

## calculate quantiles

quants <- ndvi_stats |>
  filter(stats == "mean") |>
  summarise(q = quantile(value, probs = c(0, 0.1, 0.5, 0.9)))


## create classication matrix
matrix <- c(0, quants$q[1], 1,
            quants$q[1], quants$q[2], 2,
            quants$q[2], quants$q[3], 3,
            quants$q[3], quants$q[4], 4,
            quants$q[4], Inf, 5

)


m <- matrix(matrix, ncol = 3, byrow = TRUE)

## reclassify
st3 <- reclassify(st1, m)

## plot
plot(st3$layer.102, col = brewer.pal("PRGn", n = 5))

mapview(st3$layer.111, col.region = brewer.pal("PRGn", n = 5))


## dynamic world

dw_pal <- c("#419BDF", "#397D49", "#88B053", "#7A87C6", "#E49635", "#DFC35A", "#C4281B", "#A59B8F", "#B39FE1")

dw <- dataset1$filterBounds(EE_geom)
dw <- dw$filterDate(start, end)

dates <- rgee::ee_get_date_ic(dw)

months <- dates |>
  mutate(monyear = zoo::as.yearmon(time_start))

geemap$ee_export_image_collection(dw, out_dir = "/Users/julianflowers/Library/CloudStorage/GoogleDrive-julian.flowers12@gmail.com/My Drive/dw",
                                  region = EE_geom, crs = "EPSG:27700", scale = 10)

dw_path <- here("/Users/julianflowers/Library/CloudStorage/GoogleDrive-julian.flowers12@gmail.com/My Drive/dw")
dw_tifs <- list.files(dw_path, "tif", full.names = T)
dw_tifs

GDALinfo(dw_tifs[2])

## import all rasters

dw_size <- map(dw_tifs, object.size)

dw_size


geemap$image_cell_size(dw_tifs[1])$getInfo()

dws <- map(dw_tifs, raster, band = 10)

dws_stack <- stack(dws)
scale(dws_stack)

dw_df <- as.data.frame(dws_stack, xy = TRUE)



dw_df_long <- dw_df |>
  pivot_longer(names_to = "variable", values_to = "value", cols = layer.1:layer.176)

dw_df_long_dt <- data.table::setDT(dw_df_long)

dw_df_long_dt[value != 0, .N, by = .(x, y, value)][, max := max(N), by = .(x, y)][max == N] |>
  ungroup() |>
  group_by(x, y, value) |>
  mutate(mode = n()) |>
  st_as_sf(coords = c("x", "y"), crs = 27700) |>
  #mapview(col.region = dw_pal)
  ggplot() +
  geom_sf(aes(colour = factor(value)), shape = 15) +
  viridis::scale_color_viridis(discrete = TRUE, option = "turbo", direction = -1) +
  coord_sf()


dw_df_long |>
  arrange(variable)

p <- ggplot() +
  geom_raster(data = dw_df_long , aes(x = x, y = y, color = factor(value))) +
  facet_wrap(~variable)

calc_mode <- function(x){

  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]

}


p +
  scale_fill_manual(values = dw_pal)
dws_full <- map(dw_tifs, stack)

glimpse(dws_stack)

modal(dws_stack[1:20])

modal(dws_stack[1], na.rm = TRUE) |>
  plot()


dw1 <- raster(dw_tifs[6], band = 10)

dw1 |>
  mapview::mapview(col.region = dw_pal)
