library(reticulate);library(magick);library(rgee)
Sys.setenv(RETICULATE_PYTHON = '/Users/julianflowers/.virtualenvs/ee/bin/python')
virtualenv_list()
#py = use_python(Sys.getenv("RETICULATE_PYTHON"))

virtualenv_remove("rgee")
virtualenv_create("rgee")
use_virtualenv("rgee", required = TRUE)
py_install("earthengine-api", pip = TRUE, envname = "rgee")
py_install("geedim", pip = TRUE, envname = "rgee")
py_install("IPython", pip = TRUE, envname = "rgee")

ee <- import("ee")
ee$Initialize()
library(rgee)

ee_Initialize(drive = TRUE, gcs = TRUE)
roi <- ee4 
  
  
  Polygon(
  list(
  c(-122.275, 37.891),
  c(-122.275, 37.868),
  c(-122.240, 37.868),
  c(-122.240, 37.891)))

col = ee$Filter$And(
  
  ee$Filter$bounds(ee$Geometry$Point(0.1313, 52.1951)), 
  ee$Filter$date('2021-04-02', '2021-04-03')
  
  
)

ee_rand

dwcol = ee$ImageCollection('GOOGLE/DYNAMICWORLD/V1')$filter(col)
s2col = ee$ImageCollection('COPERNICUS/S2')$filter(col)


dw_s2_col <- ee$Join$saveFirst('s2_img')$
  apply(dwcol, s2col, ee$Filter$equals(list(
    leftField = "system:index", 
    rightField = "system:index")
  )
)


dwImage = ee$Image(dw_s2_col$first())
s2Image = ee$Image(dwImage$get('s2_img'))



Map$setCenter(0.1313, 52.1951, 12)
Map$addLayer(s2Image)

ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2016-01-01", "2016-01-31")$
  first()
import("datetime")
#folium <- import("folium")

ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2017-01-01", "2017-01-31")$
  first()

ee_s2$get("system:time_start") %>% eedate_to_rdate() # good!

ee_get_date_img(ee_s2)

library(sf)
nc <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
  st_transform(4326) %>%
  sf_as_ee()

ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2016-01-01", "2016-01-31")$
  filterBounds(nc)

ee_get_date_ic(ee_s2)


Define a Image or ImageCollection: Terraclimate
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate("2001-01-01", "2002-01-01")$
  map(function(x){
    date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
    name <- ee$String$cat("Terraclimate_pp_", date)
    x$select("pr")$reproject("EPSG:4326")$set("RGEE_NAME", name)
  })

# Define a geometry
nc <- st_read(
  dsn = system.file("shape/nc.shp", package = "sf"),
  stringsAsFactors = FALSE,
  quiet = TRUE
)

# Extract values
ee_nc_rain <- ee_extract(
  x = terraclimate,
  y = nc,
  scale = 250,
  fun = ee$Reducer$mean(),
  sf = FALSE
)

colnames(ee_nc_rain) <- sprintf("%02d", 1:12)
ee_nc_rain$name <- nc$NAME

library(tidyverse)
ee_nc_rain %>%
  pivot_longer(-name, names_to = "month", values_to = "pr") %>%
  ggplot(aes(x = as.integer(month), y = pr, color = pr)) +
  geom_line(alpha = 0.8, size = 2) +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal() +
  transition_states(name) +
  shadow_mark(size = 0.4, colour = "grey")





createTimeBand <-function(img) {
  year <- ee$Date(img$get('system:time_start'))$get('year')$subtract(1991L)
  ee$Image(year)$byte()$addBands(img)
}

collection <- ee$
  ImageCollection('NOAA/DMSP-OLS/NIGHTTIME_LIGHTS')$
  select('stable_lights')$
  map(createTimeBand)

collection

col_reduce <- collection$reduce(ee$Reducer$linearFit())
col_reduce <- col_reduce$addBands(
  col_reduce$select('scale'))
ee_print(col_reduce)


Map$setCenter(9.08203, 47.39835, 3)
Map$addLayer(
  eeObject = col_reduce,
  visParams = list(
    bands = c("scale", "offset", "scale"),
    min = 0,
    max = c(0.18, 20, -0.18)
  ),
  name = "stable lights trend"
)

library(sf)

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate("2001-01-01", "2002-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("pr")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("PP_%02d",1:12)) # rename the bands of an image

ee_nc_rain <- ee_extract(x = terraclimate, y = nc["NAME"], sf = FALSE)

ee_nc_rain %>%
  pivot_longer(-NAME, names_to = "month", values_to = "pr") %>%
  mutate(month, month=gsub("PP_", "", month)) %>%
  ggplot(aes(x = month, y = pr, group = NAME, color = pr)) +
  geom_line(alpha = 0.4) +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal()


mask <- system.file("shp/arequipa.shp", package = "rgee") %>%
  st_read(quiet = TRUE) %>%
  sf_as_ee()
region <- mask$geometry()$bounds()

col <- ee$ImageCollection('MODIS/006/MOD13A2')$select('NDVI')

col <- col$map(function(img) {
  doy <- ee$Date(img$get('system:time_start'))$getRelative('day', 'year')
  img$set('doy', doy)
})
distinctDOY <- col$filterDate('2013-01-01', '2014-01-01')


filter <- ee$Filter$equals(leftField = 'doy', rightField = 'doy')
join <- ee$Join$saveAll('doy_matches')
joinCol <- ee$ImageCollection(join$apply(distinctDOY, col, filter))
comp <- joinCol$map(function(img) {
  doyCol = ee$ImageCollection$fromImages(
    img$get('doy_matches')
  )
  doyCol$reduce(ee$Reducer$median())
})

visParams = list(
  min = 0.0,
  max = 9000.0,
  bands = "NDVI_median",
  palette = c(
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
  )
)

rgbVis <- comp$map(function(img) {
  do.call(img$visualize, visParams) %>%
    ee$Image$clip(mask)
})


gifParams <- list(
  region = region,
  dimensions = 600,
  crs = 'EPSG:3857',
  framesPerSecond = 10
)

dates_modis_mabbr <- distinctDOY %>%
  ee_get_date_ic %>% # Get Image Collection dates
  '[['("time_start") %>% # Select time_start column
  lubridate::month() %>% # Get the month component of the datetime
  '['(month.abb, .) # subset around month abbreviations

animation <- rgee::ee_utils_gif_creator(rgbVis, gifParams, mode = "wb")
animation %>%
  ee_utils_gif_annotate(
    text = "NDVI: MODIS/006/MOD13A2",
    size = 15, color = "white",
    location = "+10+10"
  ) %>%
  ee_utils_gif_annotate(
    text = dates_modis_mabbr,
    size = 30,
    location = "+290+350",
    color = "white",
    font = "arial",
    boxcolor = "#000000"
  ) # -> animation_wtxt

# ee_utils_gif_save(animation_wtxt, path = "raster_as_ee.gif")


# Define an study area
EE_geom <- ee$Geometry$Point(c(-70.06240, -6.52077))$buffer(5000)

l8img <- ee$ImageCollection('LANDSAT_LC08_C02_T2_L2') %>% 
  ee$ImageCollection$filterDate('2021-06-01', '2021-12-01') %>% 
  ee$ImageCollection$filterBounds(EE_geom) %>% 
  ee$ImageCollection$first()


gcs_l8_name  <- "l8demo2" # name of the image in GCS.
BUCKET_NAME <- "rgee_examples" # set here your bucket name
task <- ee_image_to_gcs(
  image = l8img$select(sprintf("SR_B%s",1:5)),
  region = EE_geom,
  fileNamePrefix = gcs_l8_name,
  timePrefix = FALSE,
  bucket = BUCKET_NAME,
  scale = 10,
  formatOptions = list(cloudOptimized = TRUE) # Return a COG rather than a TIFF file.
)
task$start()
ee_monitoring()


ee_user_info()

## 1. Download small images
dem <- ee$Image("WWF/HydroSHEDS/03CONDEM")
roi <- ee$Geometry$Point(-73,-12)$buffer(1000)$bounds()
# TIP: Use the dsn argument to specify where to save the raster.
dem_cusco_raster <- ee_as_raster(dem, roi)
dem_cusco_stars <- ee_as_stars(dem, roi)

## 2. Download large images
dem <- ee$Image("WWF/HydroSHEDS/03CONDEM")
roi <- ee$Geometry$Point(-73,-12)$buffer(10**5)$bounds()
# Map$centerObject(roi)
# Map$addLayer(dem$clip(roi))

# You need to upload your Google Drive credentials, rgee will make
# it automatically for you if you forgot it!
ee_Initialize(drive = TRUE)

dem_cusco_raster <- ee_as_raster(image = dem, region = roi, via = "drive")
dem_cusco_stars <- ee_as_stars(image = dem, region = roi, via = "drive")

# Clean a Google Drive folder
ee_clean_container(name = "rgee_backup", type = "drive")



elv = rgee$Image('USGS/SRTMGL1_003')
point <- rgee$Geometry$Point(-72.28525, 42.36103)
scale <- 1000
elev <- elv$sample(point, scale)
elev$first()

roi <- point$buffer(10^6)



ef <- elev$first()
ef <- ef$get('elevation')
ef <- ef$getInfo
print(ef)
py_to_r(ef)

py_list_attributes(elv)
as.character(ef[1])

m <- import_main()

builtins <- import_builtins()

builtins$print(ef)

py_save_object(ef, "test.txt")

m$r$ef()

print(ef)

cloud = rgee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 35)
lc = rgee$ImageCollection('COPERNICUS/S2_HARMONIZED')

lcd <- lc$filterDate('2021-10-15', '2021-10-25')
lcdp <- lcd$filterBounds(point)

s2 <- lcdp$filter(cloud)

s2image <- rgee$Image(s2$first())

s2VisParams = {bands: ['B4', 'B3', 'B2'], min: 0, max: 3000}


folium$Map$add_ee

$Map$addLayer(s2Image,'Sentinel-2 Image')
Map.centerObject(geometry, 13);

py_get_attr(lc)
info <- lc$getInfo()


img <- rgee$Image('LANDSAT/LT05/C01/T1_SR/LT05_034033_20000913')

img.info <- img$getInfo() 
img.info$bands

data <- lc$select('LST_Day_1km', 'QC_Day')

means <- data$mean()
means



roi <- ee$Geometry$Point(c(-122.2575, 37.8795)) %>%
  ee$Geometry$buffer(10000)
blocks <- ee$FeatureCollection("TIGER/2010/Blocks")
subset <- blocks$filterBounds(roi)
Map$centerObject(roi)
Map$addLayer(subset)

# Case 2: Display an Image
image <- ee$Image("CGIAR/SRTM90_V4")
band_viz = list(
  min = 0,
  max = 4000,
  palette = c(
    '0D0887', '5B02A3',
    '9A179B', 'CB4678',
    'EB7852', 'FBB32F',
    'F0F921'
  )
)
Map$setCenter()
Map$addLayer(image, band_viz)

# Case 3: Display an ImageCollection
ee_search_display("COPERNICUS/S2")
ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2016-01-01", "2016-01-31")$
  filterBounds(nc) %>%
  ee_get(0:5)

rgbVis <- list(
  min = 0,
  max = 3000,
  bands = c('B4', 'B3', 'B2')
)
Map$centerObject(nc)
Map$addLayers(ee_s2, rgbVis)

# Case 4: Edit
library(mapedit)
m1 <- Map$addLayer(image, band_viz)
my_geometry <- m1 %>% ee_as_mapview() %>% editMap()

ee_Initialize(drive = TRUE)
ee_user_info()

# Region of interest
roi <- ee$Geometry$Point(c(-122.2575, 37.8795)) %>%
  ee$Geometry$buffer(10000)

## 1. Download a small FeatureCollections
blocks <- ee$FeatureCollection("TIGER/2010/Blocks")
subset <- blocks$filterBounds(roi)
sf_subset <- ee_as_sf(x = subset, maxFeatures = 10000)
plot(sf_subset["countyfp10"])


## 2. Download a Large FeatureCollections
region <- ee$Geometry$Rectangle(-119.224, 34.669, -99.536, 50.064)
# ee_help(ee$FeatureCollection$randomPoints)
ee_randomPoints <- ee$FeatureCollection$randomPoints(region, 50000)
sf_random <- ee_as_sf(x = ee_randomPoints, via = "drive")


geom <- ee$Geometry$Rectangle(-10,-10,10,10)
Map$addLayer(geom)
ee_print(geom)

# Feature
feature <- ee$Feature(geom, list(rgee = "ee_print", data = TRUE))
ee_print(feature)

# FeatureCollection
featurecollection <- ee$FeatureCollection(feature)
ee_print(featurecollection)

# Image
srtm <- ee$Image("CGIAR/SRTM90_V4")
ee_print(srtm)

srtm_clip <- ee$Image("CGIAR/SRTM90_V4")$clip(geom)
srtm_metadata <- ee_print(srtm_clip)
srtm_metadata$img_bands_names

# ImageCollection
object <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filter(ee$Filter()$eq("WRS_PATH", 44))$
  filter(ee$Filter()$eq("WRS_ROW", 34))$
  filterDate("2014-03-01", "2014-08-01")$
  aside(ee_print)


# Input imagery is a cloud-free Landsat 8 composite.
l8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1")

image <- ee$Algorithms$Landsat$simpleComposite(
  collection = l8$filterDate("2018-01-01", "2018-12-31"),
  asFloat = TRUE
)

ee_print(image)
# Use these bands for prediction.
bands <- c("B2", "B3", "B4", "B5", "B6", "B7", "B10", "B11")

# Load training points. The numeric property 'class' stores known labels.
points <- ee$FeatureCollection("GOOGLE/EE/DEMOS/demo_landcover_labels")

# This property of the table stores the land cover labels.
label <- "landcover"

# Overlay the points on the imagery to get training.
training <- image$select(bands)$sampleRegions(
  collection = points,
  properties = list(label),
  scale = 30
)

# Define visualization parameters in an object literal.
vizParams <- list(
  bands = c("B5", "B4", "B3"),
  min = 0,
  max = 1,
  gamma = 1.3
)

Map$centerObject(points, 10)
Map$addLayer(image, vizParams, "Image") +
  Map$addLayer(points, list(color = "yellow"), "Training points")

ee_print(training)


image <- ee$Image("LANDSAT/LT05/C01/T1_TOA/LT05_031034_20110619")
ndvi <- image$normalizedDifference(c("B4", "B3"))

# Detect edges in the composite$
canny <- ee$Algorithms$CannyEdgeDetector(ndvi, 0.7)

# Mask the image with itself to get rid of areas with no edges.
canny <- canny$updateMask(canny)

Map$setCenter(-101.05259, 37.93418, 13)
Map$addLayer(ndvi, list(min = 0, max = 1), "Landsat NDVI") +
  Map$addLayer(canny, list(min = 0, max = 1, palette = "FF0000"), "Canny Edges")


image1 <- ee$Image('srtm90_v4')
geom_params <-   list(
  scale = 30,
  crs = 'EPSG:4326',
  region = '[[-120, 35], [-119, 35], [-119, 34], [-120, 34]]'
)
path <- image1$getDownloadUrl(geom_params)
print(path)

# Method 02: Download a thumbnail image
image_local <- ee_as_thumbnail(image1)
plot(image_local)

vis_params = list(min = 0, max = 3000)
Map$addLayer(image1, vis_params)



VECTORIZATION_SCALE <- 500

image1 <- ee$Image("MCD12Q1/MCD12Q1_005_2001_01_01")
image2 <- image1$select("Land_Cover_Type_1")
image3 <- image2$reproject("EPSG:4326", NULL, 500)
image4 <- image3$focal_mode()
image5 <- image4$focal_max(3)$focal_min(5)$focal_max(3)
image6 <- image5$reproject("EPSG:4326", NULL, 500)

PALETTE <- list(
  "aec3d4", # water
  "152106", "225129", "369b47", "30eb5b", "387242", # forest
  "6a2325", "c3aa69", "b76031", "d9903d", "91af40", # shrub, grass, savannah
  "111149", # wetlands
  "cdb33b", # croplands
  "cc0013", # urban
  "33280d", # crop mosaic
  "d7cdcc", # snow and ice
  "f7e084", # barren
  "6f6f6f" # tundra
)

vis_params <- list(min = 0, max = 17, palette = PALETTE)

Map$setCenter(-113.41842, 40.055489, 6)
Map$addLayer(image2, vis_params, 'IGBP classification')+
  Map$addLayer(image3, vis_params, 'Reprojected')+
  Map$addLayer(image4, vis_params, 'Mode')+
  Map$addLayer(image5, vis_params, 'Smooth')+
  Map$addLayer(image6, vis_params, 'Smooth')

cover <- ee$Image("MODIS/051/MCD12Q1/2012_01_01")$select("Land_Cover_Type_1")
igbpPalette <- c(
  "aec3d4",
  "152106", "225129", "369b47", "30eb5b", "387242",
  "6a2325", "c3aa69", "b76031", "d9903d", "91af40",
  "111149",
  "cdb33b",
  "cc0013",
  "33280d",
  "d7cdcc",
  "f7e084",
  "6f6f6f"
)

Map$setCenter(-99.229, 40.413, 5)
Map$addLayer(
  eeObject = cover,
  visParams = list(min = 0, max = 17, palette = igbpPalette),
  name = "MODIS Land Cover"
)


elev <- ee$Image("srtm90_v4")
cover <- ee$Image("MCD12Q1/MCD12Q1_005_2001_01_01")$select("Land_Cover_Type_1")
blank <- ee$Image(0)

# Where (1 <= cover <= 4) and (elev > 1000), set the output to 1.
output <- blank$where(
  test = cover$lte(4)$And(cover$gte(1))$And(elev$gt(1000)),
  value = 1
)

# Output contains 0s and 1s.  Mask it with itself to get rid of the 0s.
result <- output$mask(output)
vis <- list(min = 0, max = 3000)
Map$setCenter(-113.41842, 40.055489, 6)
Map$addLayer(elev, vis, "SRTM") +
  Map$addLayer(result, list(palette = "00AA00"), "Land Cover")


# Load a cloudy Landsat 8 image.
image <- ee$Image("LANDSAT/LC08/C01/T1_TOA/LC08_044034_20130603")
Map$addLayer(
  eeObject = image,
  visParams = list(bands = c("B5", "B4", "B3"), min = 0, max = 0.5),
  name = "original image"
)

# Load another image to replace the cloudy pixels.
replacement <- ee$Image("LANDSAT/LC08/C01/T1_TOA/LC08_044034_20130416")

# Compute a cloud score band$
cloud <- ee$Algorithms$Landsat$simpleCloudScore(image)$select("cloud")

# Set cloudy pixels to the other image.
replaced <- image$where(cloud$gt(10), replacement)

# Display the result.
Map$centerObject(image, zoom = 9)
Map$addLayer(
  eeObject = replaced,
  visParams = list(
    bands = c("B5", "B4", "B3"),
    min = 0,
    max = 0.5
  ),
  name = "clouds replaced"
)
Footer

collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filter(ee$Filter$eq("WRS_PATH", 44))$
  filter(ee$Filter$eq("WRS_ROW", 34))$
  filterDate("2014-03-01", "2014-09-01")

count <- collection$size()
cat("Count: ", count$getInfo())
range <- collection$reduceColumns(
  ee$Reducer$minMax(),
  list("system:time_start")
)


col_min <- eedate_to_rdate(range$get("min"))
col_max <- eedate_to_rdate(range$get("max"))
cat("Date range: ", as.character(col_min), as.character(col_max))

sunStats <- collection$aggregate_stats("SUN_ELEVATION")
cat("Sun elevation statistics: ")
sunStats$getInfo()

# Sort by a cloud cover property, get the least cloudy image.
image <- ee$Image(collection$sort("CLOUD_COVER")$first())
cat("Least cloudy image: ")
image$getInfo()

# Limit the collection to the 10 most recent images.
recent <- collection$sort("system:time_start", FALSE)$limit(10)
cat("Recent images: ")
recent$getInfo()


gsw <- ee$Image("JRC/GSW1_1/GlobalSurfaceWater")
occurrence <- gsw$select("occurrence")



VIS_OCCURRENCE <- list(
  min = 0,
  max = 100,
  palette = c("red", "blue")
)

VIS_WATER_MASK <- list(
  palette = c("white", "black")
)


water_mask <- occurrence$gt(10)$selfMask()
Map$addLayer(occurrence$updateMask(occurrence$divide(100)), VIS_OCCURRENCE, "Water Occurrence (1984-2018)") +
  Map$addLayer(water_mask, VIS_WATER_MASK, "90% occurrence water mask", FALSE)
Footer


gsw = ee$Image('JRC/GSW1_1/GlobalSurfaceWater')
occurrence = gsw$select('occurrence')
change = gsw$select("change_abs")
roi = ee$Geometry$Polygon(
  list(
    c(-74.17213, -8.65569),
    c(-74.17419, -8.39222),
    c(-74.38362, -8.36980),
    c(-74.43031, -8.61293)
  )
)


VIS_OCCURRENCE = list(
  min = 0,
  max = 100,
  palette = c('red', 'blue')
)

VIS_CHANGE = list(
  min = -50,
  max = 50,
  palette = c('red', 'black', 'limegreen')
)

VIS_WATER_MASK = list(
  palette = c('white', 'black')
)

water_mask = occurrence$gt(90)$mask(1)


Map$addLayer(water_mask, VIS_WATER_MASK, '90% occurrence water mask', FALSE) +
  Map$addLayer(occurrence$updateMask(occurrence$divide(100)),  VIS_OCCURRENCE, "Water Occurrence (1984-2015)") +
  Map$addLayer(change, VIS_CHANGE,'occurrence change intensity')
Footer

collection <- ee$ImageCollection('USDA/NAIP/DOQQ')

# create an roi
polys <- ee$Geometry$Polygon(
  list(
    c(-99.29615020751953, 46.725459351792374),
    c(-99.2116928100586, 46.72404725733022),
    c(-99.21443939208984, 46.772037733479884),
    c(-99.30267333984375, 46.77321343419932)
  )
)
fc <- ee$FeatureCollection(polys)

# create a FeatureCollection based on the roi and center the map
centroid = polys$centroid()$coordinates()$getInfo()
lng <- centroid[1]; lat <- centroid[2]
print(sprintf("lng = %s, lat = %s", lng, lat))

# filter the ImageCollection using the roi
naip = collection$filterBounds(polys)
naip_2015 = naip$filterDate('2015-01-01', '2015-12-31')
mosaic = naip_2015$mosaic()

# print out the number of images in the ImageCollection
count = naip_2015$size()$getInfo()
cat("Count: ", count)

# add the ImageCollection and the roi to the map
vis = list(bands = c('N', 'R', 'G'))
Map$setCenter(lng, lat, 12)
Map$addLayer(mosaic,vis) +
  Map$addLayer(fc)

fromFT = ee$FeatureCollection("users/wqs/Pipestem/Pipestem_HUC10")
Map$centerObject(fromFT)
Map$addLayer(fromFT)

sf_fromFT <- ee_as_sf(fromFT, via = 'drive')
plot(sf_fromFT)


fromFT <- ee$FeatureCollection("users/wqs/Pipestem/Pipestem_HUC10")
centroid <- fromFT$geometry()$centroid()$coordinates()$getInfo()
lng <- centroid[1]
lat <- centroid[2]
print(sprintf("lng = %s, lat = %s", lng, lat))

count <- fromFT$size()$getInfo()
fromFT_list <- fromFT$toList(5)

map_list <- list()
for (r_index in seq_len(count)) {
  index <- r_index - 1
  feature <- ee$Feature(fromFT_list$get(index))
  name <- feature$get("system:index")$getInfo()
  fc <- fromFT$filter(ee$Filter$eq("system:index", name))
  Map$centerObject(fc, zoom = 9)
  map_list[[name]] <- Map$addLayer(fc, name = name)
}

Reduce(`+`, map_list)

image <- ee$Image("USDA/NAIP/DOQQ/m_4609915_sw_14_1_20100629")

# scale means resolution.
downConfig <- list(
  scale = 10,
  maxPixels = 1.0E13,
  driveFolder = "image"
)

task <- ee$batch$Export$image(image, "10m", downConfig)
task$start()

ee_monitoring()


startTime <- "2001-01-01"
endTime <- "2001-02-01"

lst <- ee$ImageCollection("FORA0125_H002")$
  filterDate(startTime, endTime)

# Get the time series at these points.
points <- ee$Geometry$Point(-85.16516, 30.85000)
collection <- ee$FeatureCollection(points)


# Extract the values by running reduceRegions over each image in the image collection.
fc_reduceRegions <- function(feature) {
  feature$reduceRegions(collection, "first")
}

values <- lst$map(fc_reduceRegions)$flatten()

# Turn the result into a feature collection and export it.
taskParams <- list(
  driveFolder = "image",
  driveFileNamePrefix = "TylerTest",
  fileFormat = "CSV"
)

MyTry <- ee$batch$Export$table(values, "lst_timeseries", taskParams)
MyTry$start()
ee_monitoring(MyTry)


## oil palm 
dataset = ee$ImageCollection('BIOPAMA/GlobalOilPalm/v1')

class <- dataset$select('classification')

mosaic <- class$mosaic()

classificationVis = list(
  min =  1,
  max = 3,
  palette =  c('ff0000','ef00ff', '696969')
)

mask = mosaic$neq(3)
mask = mask$where(mask$eq(0), 0.6)

Map$setCenter(-3.0175, 5.2745,12)
Map$addLayer(mosaic$updateMask(mask),
             classificationVis, 'Oil palm plantation type', TRUE)






ataset = ee$FeatureCollection('USDOS/LSIB/2017')

styleParams = c(
  fillColor = 'b5ffb4',
  color = '00909F',
  width = 3.0
)

visParams = list(
  fillColor = 'b5ffb4',
  color = '00909F',
  width = 3.0
)

countries = ataset$style(styleParams)


Map$setCenter(16.35, 48.83, 4)
Map$addLayer(countries, visParams = list(), name = 'USDOS/LSIB/2017')


