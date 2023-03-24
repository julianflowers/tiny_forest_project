## explore tiny forest data


## load libraries
library(needs)
needs(tidyverse, here, janitor, units, sf, mapview)
library(tidyverse, warn.conflicts = TRUE)

## load scraped tiny forest details (101 tfs)
tf <- read_csv("data/tf-data-1.csv")


## pivot wider to split out total area and classroom area
tf_details <- dplyr::select(tf, id:lon)
tf_details <- tf_details |>
  group_by(id) |>
  mutate(row_id = row_number()) |>
  pivot_wider(names_from = "row_id", names_prefix = "area", values_from = "area") |>
  rename(area = area1, class_area = area2)

## tf area distribution

tf_details |>
  mutate(area = units::set_units(area, m2),
         class = units::set_units(area, m2)) |>
  ggplot() +
  geom_density(aes(area)) +
  labs(title = "Tiny forest areas (N = 101)") +
  theme(plot.title.position = "plot")

## tf map

uk <- map_data(map = "world", region = "UK")


uk_sf <- st_as_sf(uk, coords = c("long", "lat"), crs = 4326)

tf_details_sf <- st_as_sf(tf_details, coords = c("lon" ,"lat"), crs = 4326)


## interactive
tf_details_sf |>
  mapview(col.regions = tf_details_sf$area)


## static
tf_details_sf |>
  #st_transform(27700) |>
  ggplot() +
  ggspatial::annotation_map_tile(zoomin = 3) +
  #geom_sf(data = uk_bound, size = 0.2)
  geom_sf(aes(colour = area)) +
  coord_sf(crs = 27700) +
  viridis::scale_colour_viridis(direction = -1, option = "viridis") +
    theme_void()





## earth watch files

p <- here("data")

f <- list.files(p, "csv", full.names = T)

csvs <- map(f, data.table::fread)

csvs <- map(csvs, janitor::clean_names) ## convert variable names to lower snake case

csvs <- map(csvs, janitor::remove_empty) ## remove empty rows

basenames <- map(f, basename)


## tree data

tree_data <- csvs[[12]] |>
  filter(tiny_forest_id !=84)

trees_per_tf <- tree_data |>
  group_by(tiny_forest_id) |>
  summarise(n_trees = sum(species_quantity))

trees_per_tf |>
  ggplot() +
  geom_density(aes(n_trees))

## tree density

tf_details <- tf_details |>
  full_join(tree_data, by = c("id" = "tiny_forest_id")) |>
  group_by(id) |>
  mutate(n_trees = sum(species_quantity),
            area = area,
            tree_density = units::set_units(n_trees / area, m2),
         n_species = n_distinct(species_name)
         ) |>
  distinct()

## tree counts wide data and save as csv

tf_details_wide <- tf_details |>
  #dplyr::select(-c(species_name_latin)) |>
  pivot_wider(names_from = "species_name", values_from = "species_quantity", values_fill = 0)

tf_details_wide |>
  write_csv("tf-data-1.csv")




## tree variation

tree_data |>
  full_join(tf_details, by = c("tiny_forest_id" = "id")) |>
  group_by(species_name.y) |>
  mutate(tot_trees = n()) |>
  filter(!str_detect(tiny_forest_name.x, "Earth")) |>
  ggplot() +
  geom_tile(aes(fct_reorder(tiny_forest_name.x, -tiny_forest_id), fct_reorder(species_name.x, -tot_trees), fill = species_quantity.x)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() +
  labs(x = "",
       y = "") +
  scale_fill_distiller(name = "Count", direction = 1)

## carbon
### tagged trees

tags <- csvs[[10]] |>
  filter(tiny_forest_id != 84) |>
  arrange(tiny_forest_id, tag_id)

csvs[[3]] |>
  glimpse()

carbon <- csvs[[3]] |>
  filter(tiny_forest_id != 84) |>
  arrange(tiny_forest_id, tag_id)

## tree_heights

carbon_stats <- carbon |>
  group_by(species_name) |>
  summarise(mean_height = mean(species_height_cm, na.rm = TRUE),
            sd = sd(species_height_cm, na.rm = TRUE),
            n = n(),
            se = sd / sqrt(n),
            mean_stem = mean(stem_1_ddh_mm, na.rm = TRUE),
            se_stem = sd(stem_1_ddh_mm, na.rm = TRUE) / sqrt(n))

carbon |>
  group_by(species_name) |>
  mutate(median = median(species_height_cm, na.rm = TRUE)) |>
  ggplot() +
  geom_boxplot(aes(fct_reorder(species_name, median), species_height_cm)) +
  coord_flip() +
  labs(x= "",
       title = "Variation in tree heights by species") +
  theme(plot.title.position = "plot")


## ground dwellers and pollinators

ground_dwellers <- csvs[[6]] |>
  filter(tiny_forest_id !=84)

ground_dwellers |>
  arrange(tiny_forest_id)

pollinators <- csvs[[7]] |>
  filter(tiny_forest_id !=84) |>
  arrange(tiny_forest_id)


## national fit count data from ceh
## https://catalogue.ceh.ac.uk/documents/13aed7ac-334f-4bb7-b476-4f1c3da45a13

p1 <- here("/Users/julianflowers/Library/CloudStorage/GoogleDrive-julian.flowers12@gmail.com/My Drive/dissertation/13aed7ac-334f-4bb7-b476-4f1c3da45a13/data")

files <- list.files(p1, "csv", full.names = TRUE)

pollinator_csv <- map_dfr(files, read_csv)

map(test, colnames)

pollinator_csv <- pollinator_csv |>
  dplyr::select(country, location_code:year, contains("habitat"),
                contains("target_"), contains("floral_unit"),
                bumblebees:all_insects_total)


### read in national grid and gb shape files

p2 <- here("/Users/julianflowers/Library/CloudStorage/GoogleDrive-julian.flowers12@gmail.com/My Drive/dissertation/Download_2209102")
sp <- list.dirs(p2)
shps <- map(sp, ~(list.files(.x, "shp", full.names = TRUE)))

grids <- map(shps[[2]], read_sf)
grids
bounds <- map(shps[[3]], read_sf)
bounds

## combined bounds
uk_bounds <- st_union(bounds[[1]], bounds[[2]], bounds[[3]], bounds[[4]])

mapview(uk_bounds)

ggplot() +
  geom_sf(data = uk_bounds) +
  coord_sf()


## plot ceh pollinator data

poll_map <- grids[[3]] |>
  left_join(pollinator_csv, by = c("PLAN_NO" = "X1km_square"))

poll_map <- poll_map |>
  filter(!is.na(country))

poll_map |>
  ggplot() +
  geom_sf(data = uk_bounds) +
  geom_sf(aes(fill = all_insects_total))

## join butterfly datasets and remove dummy data
## look at each data set separately

butterflies_1 <- csvs[[1]] |>
  filter(tiny_forest_id !=84)
butterflies_2 <- csvs[[2]] |>
  filter(tiny_forest_id !=84)

butterflies <- csvs[[1]] |>
  inner_join(csvs[[2]], by = c("tiny_forest_id", "tiny_forest_name")) |>
  filter(tiny_forest_id !=84)

View(butterflies_2)


map2(x = csvs, y = basenames, ~mutate(.x, .y))




map(csvs, ~(mutate(.x, basenames)))

csvs <- map(csvs, clean_names)
csvs <- map(csvs, filter, tiny_forest_id != 84)

csvs[[12]] |>
  arrange(tiny_forest_id) |>
  ggplot() +
  geom_point(aes(tiny_forest_name, fct_rev(species_name), colour = species_quantity, size = species_quantity), shape=15) +
 # coord_flip() +
  #scale_color_distiller(palette = brewer.pal(name = "Greens", n = 9)) +
  ggthemes::theme_few() +
  theme(axis.text.x.bottom = element_text(angle = 45, hjust = 1))


csvs[[7]] |>
  arrange(tiny_forest_id) |>
  ggplot(aes(fct_rev(tiny_forest_name), species, fill = qty)) +
  geom_tile() +
  coord_flip() +
  ggthemes::theme_few() +
  theme(axis.text.x.bottom = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "") +
  scale_fill_viridis_c(direction = -1)

csvs[[6]] |>
  arrange(tiny_forest_id) |>
  ggplot(aes(fct_rev(tiny_forest_name), species, fill = qty)) +
  geom_tile() +
  coord_flip() +
  ggthemes::theme_few() +
  theme(axis.text.x.bottom = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "") +
  scale_fill_viridis_c(direction = -1)

csvs[[5]] |>
  arrange(tiny_forest_id) |>
  dplyr::select(soil_compaction)

|>
  summarise(mean = mean(soil_compaction, na.rm = TRUE))

map_df(csvs[c(1, 2, 3, 4, 5, 6, 7, 14)], ~(dplyr::select(.x, tiny_forest_id, contains("date")))) |>
  arrange(tiny_forest_id) |>
  mutate(date = lubridate::dmy(event_date)) |>
  filter(date > "2022-06-01") |>
  distinct() |>
  ggplot() +
  geom_point(aes(date, factor(tiny_forest_id)))


csvs[[8]] |>
  dplyr::select(contains("tiny"), latitude, longitude) |>
  distinct() |>
  print(n = 148)
  filter(depth == "Surface")
