## pollinators

poll <- read_csv("https://media.githubusercontent.com/media/julianflowers/tiny_forest_project/main/data/pollinators_timed_count_june.csv") |>
  janitor::clean_names()


tf_gs <- read_csv("data/tf_gs.csv")

poll <- poll |>
  inner_join(tf_gs, by = c("tiny_forest_id" = "tfid")) |>
  arrange(tiny_forest_id) |>
  select(when, event_date, tiny_forest_id, species, qty,
         deciduous_woodland:allotments_or_community_growing_spaces, flower_quantity, rain:wind) |>
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

poll_species <- poll |>
  ungroup() |>
  select(hoverfly:solitary_bee)

poll_species

poll_cov <- poll |>
  select(flower_quantity:age_at_survey, gs)

poll_sn <- vegan::specnumber(poll_species)

poll_df <- data.frame(poll_sn, poll_cov)

poll_df <- poll_df |>
  mutate(age_at_survey = as.numeric(age_at_survey),
         id = factor(tiny_forest_id)) |>
  mutate_at(.vars = c("gs", "age_at_survey"), scale) |>

  mutate_if(is.character, factor)

library(lme4);library(glmmLasso)
poll_df_mod <- glm(poll_sn ~ .,  data = poll_df |> select(-tiny_forest_id), family = "poisson")
poll_df_mod_glmm <- lme4::glmer(poll_sn ~ gs + age_at_survey + (1|tiny_forest_id),  data = poll_df, family = poisson)

broom::tidy(poll_df_mod, exponentiate = TRUE, conf.int = TRUE)

summary(poll_df_mod_glmm)

glmer(poll_sn ~ gs + age_at_survey + rain + wind + cloud + flower_quantity + (1|id), data = poll_df, family = "poisson") |>
  summary()

### permanova

ad_1 <- adonis2(poll_species ~ gs + age_at_survey, method = "bray", data = poll_df, permutations = 9999
                , parallel = 4)

class(ad_1)

str(ad_1)


### nmds

nmds.2d <- metaMDS(poll_species, distance = "bray", k=2, noshare = TRUE,
                        trymax=250, engine = c("monoMDS"), plot=FALSE, autotransform = FALSE)

nmds.2d$points
scrs.2d <- as.data.frame(nmds.2d$points)

scrs.2d <- cbind(scrs.2d, cov = poll_cov)
scrs.2d

scrs.2d |>
  ggplot() +
  geom_point(aes(MDS1, MDS2, colour = as.numeric(cov.gs), size = as.numeric(cov.age_at_survey))) +
  scale_color_viridis_c()

### dbstats

library(dbstats)

dbstats::dbglm()
