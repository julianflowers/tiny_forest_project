## calculate diversity metrics

###############
#.
#. 1. TF trees
#
###############

needs(vegan, tidyverse, GGally)

trees_df <- read_csv("data/tf_trees.csv")

## ground dwellers

gd <- read_csv("https://media.githubusercontent.com/media/julianflowers/tiny_forest_project/main/data/ground_dwellers_june.csv")

gd_w <- gd |>
  rename(tf_id = 1) |>
  select(tf_id, Species, Qty) |>
  pivot_wider(names_from = "Species", values_from = "Qty", values_fn = sum, values_fill = 0) |>
  arrange(tf_id) |>
  filter(tf_id !=84)

## gd bd

sn_gd <- specnumber(gd_w[, -1])
div_gd <- diversity(gd_w[, -1])
gd_r <- bind_cols(richness = sn_gd, shannon = div_gd, tf_id = gd_w[,1])

## pollinators

pc <- read_csv("https://media.githubusercontent.com/media/julianflowers/tiny_forest_project/main/data/pollinators_timed_count_june.csv")

pc_w <- pc |>
  rename(tf_id = 1) |>
  select(tf_id, Species, Qty) |>
  pivot_wider(names_from = "Species", values_from = "Qty", values_fn = sum, values_fill = 0) |>
  arrange(tf_id) |>
  filter(tf_id !=84)

sn_pc <- specnumber(pc_w[, -1])
div_pc <- diversity(pc_w[, -1])

pc_r <- bind_cols(richness = sn_pc, shannon = div_pc, tf_id = pc_w[,1])

## select tree data (presence-absence)
trees <- df[, 6:83]

## calculate richness
sn <- specnumber(trees)
simpson <- diversity(trees, index = "simpson")

df_div <- df[, 1:5] |>
  bind_cols(richness = sn, shannon = div, simpson = simpson)

## join datasets

join_r <- pc_r |>
  full_join(gd_r, by = "tf_id") |>
  left_join(df_div, by = "tf_id")


my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

ggpairs(join_r[, c(2, 5, 11)], lower = list(continuous = my_fn))


pairs(join_r[c(1, 2, 4, 5, 10, 11)],
      panel = panel.smooth)

## min richness
df_div |>
  slice_min(richness)

## check 314

df_div |>
  filter(tf_id == 92)

## look for communities

df_div <- df_div |>
  mutate(lat2 = round(lat, 0))

trees1 <- bind_cols(lat = df_div$lat2, trees)

ad <- adonis2(sn ~ factor(df_div$lat2), permutations = 9999, parallel = 6, method = "jaccard")

dist <- vegdist(trees, method = "raup")
hclust(dist, method = "ward.D") |>
  plot()
