## tiny-forest-biodiversity

needs(tidyverse, vegan, broom, ggridges, umap)

tiny_forest_biodiversity <- read_csv("data/tiny_forest_biodiversity.csv")


tiny_forest_biodiversity |>
  filter(id == 92)

## counts by class, location

class_loc <- tiny_forest_biodiversity |>
  filter(between(year, 1990, 2022)) |>
  count(id, classs, year, sort = TRUE) |>
  arrange(year,id) |>
  pivot_wider(names_from = "classs", values_from = "n", values_fill = 0)

area_diversity <- class_loc |>
  nest(.by = year) |>
  mutate(div = map(data, diversity, "shannon"))

area_umap <- class_loc |>
  nest(.by = year) |>
  mutate(diss = map(data, ~(umap::umap(.x))))

area_umap$diss[[13]]$layout |>
  plot()


area_rtsne <- class_loc |>
  nest(.by = year) |>
  mutate(diss = map(data, ~(Rtsne::Rtsne(.x, perplexity = 10))))

area_rtsne$diss[[13]]$Y |>
  plot()


area_diversity |>
  unnest("div") |>
  group_by(year) |>
  mutate(id = row_number(),
         med = median(div)) |>
  ggplot(aes(y = factor(year), x = div), fill = div) +
  stat_density_ridges(scale = 2, quantile_lines = TRUE, quantiles = 2, alpha = 0.7) +
  scale_fill_viridis_d(option = "C") +
  theme_ridges()

area_diversity |>
  unnest("div") |>
  group_by(year) |>
  mutate(id = row_number(),
         mean = mean(div)) |>
  select(year, mean) |>
  distinct() |>
  ggplot(aes(year, mean)) +
  geom_line(lty = "dotted") +
  geom_point() +
  geom_smooth(method = "gam") +
  ggthemes::theme_few()




area_umap <- class_loc |>
  nest(.by = year) |>
  mutate(umap = map(data, umap))


## birds

birds <- tiny_forest_biodiversity |>
  filter(between(year, 1990, 2022),
         classs == "Aves")

birds <- sf::st_as_sf(birds, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

birds_wide <- birds |>
  count(year, id, vernacularName) |>
  pivot_wider(names_from = "vernacularName", values_from = "n", values_fill = 0)

birds_nest <- birds_wide[, -2] |>
  nest(.by = c(year)) |>
  arrange(year)

birds_nest$data[[30]]

bird_l <- birds_nest |>
  mutate(spec_no = map(data, vegan::specnumber)) |>
  unnest("spec_no") |>
  select(-data) |>
  pivot_wider(names_from = "year", values_from = "spec_no")

bird_l
