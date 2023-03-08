needs(sf, tidyverse, mapview)


path <- here::here("/Users/julianflowers/Dropbox/My Mac (Julians-MBP-2)/Downloads/oproad_gml3_gb/data")

gml <- list.files(path, "gml", full.names = TRUE)

gml_ta <- read_sf(gml[45], layer = "RoadNode")
gml_ta_road <- read_sf(gml[45], layer = "RoadLink")

mapview(gml_ta |>
  filter(formOfRoadNode == "roundabout")) +
  mapview(gml_ta_road)

gml_ta |>
  filter(formOfRoadNode == "roundabout") |>
  mutate(id = row_number())
  
gml_ta_road |>
  View()

intersect(gml_ta_road$gml_id, gml_ta$gml_id)
  filter(!is.na(inNetwork))

  
remotes::install_github('ropensci/osmdata')

library(osmdata)

q <- opq(bbox = "Cambridge, England") %>%
  add_osm_feature(key = 'junction', value = 'roundabout') 

q1 <- osmdata_sf(q) 


q2 <- q1$osm_lines |>
  sf::st_cast(to = "MULTILINESTRING") 

needs(sf, tidyverse, units, ggmap, ggspatial, mapview)
devtools::install_github('Chrisjb/basemapR')
q2_area <- q2 |>
  dplyr::mutate(
         length = sf::st_length(q2), 
         area = (length ^2)/ (4 * pi)) |>
  dplyr::arrange(-area) 


q2_area |>
  mapview()
  ggplot() +
  annotation_map_tile(zoomin = -1) +
  geom_sf(aes(colour = drop_units(area), size = drop_units(area))) +
  theme_void() +
  scale_color_viridis_c() 
  
 
## local nature reserves
  
lnr <- sf::read_sf("https://services.arcgis.com/JJzESW51TqeY9uat/arcgis/rest/services/Local_Nature_Reserves_England/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

## priority habitats south

#phs <- sf::read_sf("https://services.arcgis.com/JJzESW51TqeY9uat/arcgis/rest/services/Priority_Habitat_Inventory_England_South/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

p <- here::here("/Users/julianflowers/Library/CloudStorage/GoogleDrive-julian.flowers12@gmail.com/My Drive/Priority_Habitats_Inventory_England_-8288141411155469249")

shps <- list.files(p, "shp$", full.names = TRUE)

shps_ph <- sf::read_sf(shps[1])
/Users/julianflowers/Library/CloudStorage/GoogleDrive-julian.flowers12@gmail.com/My Drive/OS VectorMap District (ESRI Shape File) TL
p1 <- here::here("/Users/julianflowers/Library/CloudStorage/GoogleDrive-julian.flowers12@gmail.com/My Drive/OS VectorMap District (ESRI Shape File) TL/data")

f_gml <- list.files(p1, "shp", full.names = TRUE)

i <- 1

f1 <- read_sf(f_gml[i]) |>
  st_transform(crs = 4326)
f1_1 <- read_sf(f_gml[i], layer = "SurfaceWater_Line") |>
  st_transform(crs = 4326)
f1_2 <- read_sf(f_gml[i], layer = "SurfaceWater_Area") |>
  st_transform(crs = 4326)
f1_3 <- read_sf(f_gml[i], layer = "Foreshore") |>
  st_transform(crs = 4326)
f1_4 <- read_sf(f_gml[i], layer = "ElectricityTransmissionLine") |>
  st_transform(crs = 4326)
f1 |>
  ggplot() +
  geom_sf()

+
  mapview(f1_1) +
  mapview(f1_2) +
  mapview(f1_3) +
  mapview(f1_4, col.regions = "red")


p2 <- here::here("/Users/julianflowers/Library/CloudStorage/GoogleDrive-julian.flowers12@gmail.com")

f2 <- list.files(p2, "gpkg", full.names = TRUE)

read_sf(f2[1]) |>
  mapview()


### tagfinder
q <- "shop"
v = "music"
url <- glue::glue("http://tagfinder.herokuapp.com/api/tag?key=", {q}, "&value=", {v}, "&format=json_pretty&language=en")

jsonlite::fromJSON(url, simplifyDataFrame = TRUE)
