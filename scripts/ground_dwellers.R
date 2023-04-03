## ground dwellers

needs(vegan, tidyverse)

## earth watch files

p <- here("data")

f <- list.files(p, "csv", full.names = T)

csvs <- map(f, read_csv)

csvs <- map(csvs, janitor::clean_names) ## convert variable names to lower snake case

csvs <- map(csvs, janitor::remove_empty) ## remove empty rows

basenames <- map(f, basename)

ground_dwellers <- csvs[[6]] |>
  filter(tiny_forest_id !=84) |>
  arrange(tiny_forest_id)

gd_wide <- ground_dwellers |>
  select(-tile_id, -tile_location, -contains("submitted"), -survey_id, -contains("event")) |>
  pivot_wider(names_from = "species", values_from = "qty", values_fn = sum, values_fill = 0)


## biodiversity

### richness

gd <- gd_wide |>
  select(3:ncol(gd_wide))

gd_r <- vegan::specnumber(gd)

gd_sn <- gd |>
  bind_cols(sn = gd_r)

hist(gd_sn$sn)

### diversity - Shannon-Wiener

gd_d <- gd |>
  diversity()

gd_d1 <- gd |>
  diversity(index = "simpson")

gd_sn <- gd_sn |>
  bind_cols(shannon = gd_d)

hist(gd_sn$shannon...15)

## zero diversity

gd_sn |>
  filter(simpson == 0)

## Bray-Curtis

gd_df <- bind_cols(id = gd_wide$tiny_forest_name, gd_sn)

hist(gd_df$`Snails and slugs`)

aov(sn ~ factor(id), data = gd_df) |>
  TukeyHSD() |>
  broom::tidy()

gd_bray <- vegdist(gd, "bray")

variance.check2 <- betadisper(gd_bray, gd_wide$tiny_forest_name,
                              type=c("centroid"), bias.adjust= FALSE)

anova(variance.check2)

gd.adonis1 <- adonis2(gd ~ gd_wide$tiny_forest_name, model = c("raw"),
                       permutations = 99999, method = "bray", autotransform = FALSE)


gd.adonis1

summary(gd.adonis1)

## rda

rda <- vegan::rda(gd)

plot(rda)

summary(rda)
