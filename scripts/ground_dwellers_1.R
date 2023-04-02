## ground_dweller bd

gd <- read_csv("https://media.githubusercontent.com/media/julianflowers/tiny_forest_project/main/data/ground_dwellers_june.csv") |>
  janitor::clean_names()

tf_gs <- read_csv("data/tf_gs.csv")

gd <- gd |>
  inner_join(tf_gs, by = c("tiny_forest_id" = "tfid")) |>
  arrange(tiny_forest_id) |>
  select(when, event_date, tiny_forest_id, species, qty,
         deciduous_woodland:allotments_or_community_growing_spaces) |>
  mutate(event_date = dmy(event_date),
         age_at_survey = event_date - when) |>
  select(-c(when:event_date)) |>
  pivot_wider(names_from = "species", values_from = "qty",
              values_fn = sum, values_fill = 0) |>
  janitor::clean_names() |>
  mutate(gs = deciduous_woodland + public_park_or_garden + allotments_or_community_growing_spaces) |>
  group_by(tiny_forest_id) |>
  mutate(study_id = row_number()) |>
  select(-c(2:4))

gd_species <- gd |>
  ungroup() |>
  select(4:15)

gd_age <- gd$age_at_survey

gd_gs <- gd$gs

gd_gs_df <- data.frame(gd_gs, sn)

sn <- vegan::specnumber(gd_species)
sn_df <- data_frame(gd_age, sn, gd_gs)
div <- vegan::diversity(gd_species)

broom::glance(glm(sn ~ gd_age + gd_gs , data = sn_df, family = "poisson" ))


ggplot(sn_df)
  geom_point(aes(gd_age, sn)) +
  geom_smooth(aes(gd_age, sn), method = "gam")

ggplot(gd_gs_df) +
  geom_point(aes(gd_gs, sn)) +
  geom_smooth(aes(gd_gs, sn), method = "gam")



## bray curtis


