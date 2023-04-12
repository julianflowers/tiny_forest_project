## urban rural mapping of tfs

needs(sf, mapview, tidyverse)


lsoa_boundaries <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_2011_Boundaries_Super_Generalised_Clipped_BSC_EW_V4/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
built_up_areas <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/BUA_2022_GB/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

ur_sheet <- readODS::list_ods_sheets("data/Rural_Urban_Classification_2011_lookup_tables_for_small_area_geographies.ods")

urban_rural <- readODS::read_ods("data/Rural_Urban_Classification_2011_lookup_tables_for_small_area_geographies.ods", sheet = ur_sheet[3], skip = 2)

urban_rural <- urban_rural |>
  janitor::clean_names()

glimpse(urban_rural)

mapview(built_up_areas)

lsoa_ur <- lsoa_boundaries |>
  left_join(urban_rural, by = c("LSOA11CD" = "Lower Super Output Area 2011 Code"))


tf_w <- read_csv("data/tf_w_1.csv") |>
  mutate(year = year(when),
         year_month = zoo::as.yearmon(when))

tf_sf <- tf_w  |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

mapview(lsoa_ur, zcol = "Rural Urban Classification 2011 (10 fold)") +
  mapview(tf_sf)

st_crs(tf_sf)

linked <- st_join(tf_sf, lsoa_ur, join = st_within)

linked |>
  select(stub, `Rural Urban Classification 2011 (10 fold)`) |>
  st_drop_geometry() |>
  left_join(tf_w) |>
  arrange(tfid) |>
  #DT::datatable()
  write_csv("data/tf_w_2.csv")



filter(linked, tfid == 85)

