## explore tiny forest data

## earth watch files

p <- here("data")

f <- list.files(p, "csv", full.names = T)

csvs <- map(f, data.table::fread)

csvs <- map(csvs, janitor::clean_names) ## convert variable names to lower snake case

csvs <- map(csvs, janitor::remove_empty) ## remove empty rows

basenames <- map(f, basename)


## load libraries
library(needs)
needs(tidyverse, here, janitor, units, sf, mapview)
library(tidyverse, warn.conflicts = TRUE)

## load scraped tiny forest details (176 tfs)
tf <- read_csv("data/tf_w_2.csv")

## woodland/ green space

tf1 <- csvs[[31]] |>
  left_join(tf, by = c("tfid"))

## tf area distribution

tf |>
  ggplot() +
  geom_density(aes(area)) +
  labs(title = paste("Tiny forest areas (N = ", nrow(tf), ")")) +
  ggthemes::theme_base() +
  theme(plot.title.position = "plot")

## tf map

uk <- map_data(map = "world", region = "UK")


uk_sf <- st_as_sf(uk, coords = c("long", "lat"), crs = 4326)

tf_details_sf <- st_as_sf(tf, coords = c("lon" ,"lat"), crs = 4326)


## interactive
tf_details_sf |>
  mapview(zcol = "area")

ggplot(uk_sf) +
  geom_sf() +
  geom_sf(data = tf_details_sf, aes(color = factor(year))) +
  scale_colour_viridis_d() +
  coord_sf() +
  ggthemes::theme_base()



## tree data

library(vegan)

tree_data <- read_csv("https://media.githubusercontent.com/media/julianflowers/tiny_forest_project/main/data/setup_tree_species_partial.csv") |>
  filter(`Tiny Forest ID` !=84) |>
  rename(tfid = `Tiny Forest ID`) |>
  left_join(tf1)

tree_data |>
  janitor::clean_names() |>
  select(-c(species_id, species_name_latin)) |>
  pivot_wider(names_from = "species_name", values_from = "species_quantity", values_fill = 0) |>
  janitor::clean_names() -> tree_data_wide

tree_data_species <- select(tree_data_wide, apple_crab:last_col()) |>
  drop_na()

tree_data_covars <- select(tree_data_wide, tfid:year_month)

sn <- specnumber(tree_data_species)
div <- diversity(tree_data_species)

ur <- tree_data_covars$rural_urban_classification_2011_10_fold


tree_adonis <- adonis2(tree_data_species ~ ur, na.action = na.omit, permutations = 9999, parallel = 4)

tree_adonis

plot(rda(tree_data_species))

tree_mds <- metaMDS(tree_data_species, distance = "bray", k=2, noshare = TRUE,
        trymax=250, engine = c("monoMDS"), plot=FALSE, autotransform = FALSE)


tree_mds


scrs.2d <- as.data.frame(scores(tree_mds, display = "site")) |>
  bind_cols(tree_data_covars)


scrs.2d |>
  drop_na() |>
  ggplot() +
  geom_point(aes(NMDS1, NMDS2, colour = factor(rural_urban_classification_2011_10_fold), shape = factor(year), size = 1.5))


u <- umap::umap(tree_data_species)

u_clus <- dbscan::hdbscan(u$layout, minPts = 5)

u$layout |>
  bind_cols(tree_data_covars) |>
  bind_cols(cluster = u_clus$cluster) |>
  data.frame() |>
  ggplot() +
  geom_point(aes(...1, ...2, shape = factor(rural_urban_classification_2011_10_fold), colour = factor(cluster), size = 2))



pairwise.adonis <- function(x, factors, sim.method, p.adjust.m)
{
  library(vegan)
  set.seed(1)
  factors <- factors
  co = as.matrix(combn(unique(factors),2))
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()

  for(elem in 1:ncol(co)){
    ad = adonis(x[factors %in%
                    c(as.character(co[1,1]),as.character(co[2,1])),] ~
                  factors[factors %in%
                            c(as.character(co[1,1]),as.character(co[2,1]))] , model =
                  c("raw"), permutations = 999, method =sim.method, autotransform =
                  FALSE);
    pairs = c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  p.adjusted = p.adjust(p.value,method=p.adjust.m)
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted)
  return(pairw.res)
}

pairwise.adonis(tree_data_species, ur, sim.method="bray", p.adjust.m
                ='bonferroni')


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
