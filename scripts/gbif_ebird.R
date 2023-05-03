needs(tidyverse, vegan, tidyfast, rebird, rgbif)
remotes::install_github("ropensci/rebird")
path <- here::here()
source(paste0(path, "/scripts/nbn_buffer.R"))

Sys.setenv(key = "jdfggmo3b71s")
Sys.getenv(key)

glenmore <- get_nbn_buffer(lon = -3.764, lat = 57.1645, n = 40000, radius = 5)

glenmore |>
  filter(between(year, 2019, 2023),
         classs == "Aves") |>
  count(vernacularName, year, sort = TRUE) |>
  dt_pivot_wider(names_from = year, values_from = n) |>
  top_n(50, `2019`) |>
  arrange(-`2019`)

ebirdtaxonomy() |>
  filter(str_detect(sciName, "Loxia"))

nearestobs(species_code('loxia scotica'), 57, -4, key = "jdfggmo3b71s", )

ebirdsubregionlist(regionType = "country", key = "jdfggmo3b71s")

rebird::ebirdgeo(species = 'scocro1', lat = 57, lng = -4, key = "jdfggmo3b71s")

rgbif::occurrencecount(scientificname = "Tetrao urogallus", coordinatestatus = TRUE,
                year = 2020, maxlatitude = 60)

rgbif::occ_count(scientificname = "Tetrao urogallus", coordinatestatus = TRUE,
                 year = 2020, maxlatitude = 60)


occ_data(taxonKey = usage, country = "GB", hasCoordinate = TRUE, eventDate = '2016,2022')$data |>
  drop_na(decimalLatitude) |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
  select(scientificName, year) |>
  mutate(year = factor(year)) |>
  mapview::mapview(zcol = "year")

tx <- rgbif::name_backbone("Haliaeetus albicilla")

usage <- tx$usageKey

capercaillie <- rgbif::occ_search(scientificName =
                    "Tetrao urogallus", country = "GB", year = 2018:2023 )

capercaillie$data |>
  drop_na(decimalLatitude) |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
  select(scientificName, year) |>
  mutate(year = factor(year)) |>
  mapview::mapview(zcol = "year")


library(rgbif)
gbif_d_ct <- rgbif::occ_download(pred("taxonKey", 2473577), format = "SIMPLE_CSV")

occ_download_wait('0187041-230224095556074')

occ_download_cached('0187041-230224095556074')

occ_count_country("GB")

# should give the same result
occ_count(scientificName="Aves")
occ_search(scientificName="Aves",limit=0)$meta$count

tf <- tempdir()

d <- occ_download_get('0187041-230224095556074', tf)


occ_download_import(d, tf) |>
  drop_na(decimalLatitude) |>
  filter(between(year, 2018, 2021)) |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
  select(scientificName)
  mapview::mapview()




