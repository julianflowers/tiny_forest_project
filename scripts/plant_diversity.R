
## plant diversity

library(vegan);library(janitor);library(broom);library(lme4);library(gamm4)

df <- read_csv("data/plant_bsbi_matrix.csv")

plants <- df |>
  select(14:last_col())

plants_sn <- plants |>
  vegan::specnumber() |>
  bind_cols(df[, 1:13]) |>
  clean_names()

plants_div <- plants |>
  vegan::diversity() |>
  bind_cols(df[, 1:13]) |>
  clean_names()

colnames(df)

options(digits = 3)

base_mod <- glm(x1 ~ 1, data = plants_div, family = "poisson")
AIC(base_mod)
tidy(base_mod, exponentiate = TRUE, conf.int = TRUE)

mod1 <- glmer(x1 ~ 1 + (1|tfid), data = plants_sn, family = "poisson")
AIC(mod1)
tidy(mod1, exponentiate = TRUE, conf.int = TRUE)

mod2 <- glmer(x1 ~ 1 + year_x + (1|tfid), data = plants_sn, family = "poisson")
AIC(mod2)
tidy(mod2, exponentiate = TRUE, conf.int = TRUE) |>
  mutate_if(is.numeric, ~(round(.x, 4)))

mod3 <- glmer(x1 ~ 1 + rural_urban_classification_2011_10_fold + (1|grid), data = plants_sn, family = "poisson")
AIC(mod3)
tidy(mod3, exponentiate = TRUE, conf.int = TRUE) |>
  mutate_if(is.numeric, ~(round(.x, 4)))

mod <- glmer(x1 ~ rural_urban_classification_2011_10_fold + year_x + (1|tfid), data = plants_sn, family = "poisson")
broom.mixed::tidy(mod, exponentiate = TRUE, conf.int = TRUE) |>
  mutate_if(is.numeric, ~(round(.x, 4)))

lme4::ranef(mod)

mod_gam <- gamm4::gamm4(x1 ~ 1 + s(area) + year_x + rural_urban_classification_2011_10_fold, random = ~(1|tfid), data = plants_div, family = "poisson")

broom.mixed::tidy(mod_gam$mer, exponentiate = TRUE, conf.int = TRUE) |>
  mutate_if(is.numeric, ~(round(.x, 4)))



plot(mod_gam$gam, select = 2)
plot(mod_gam$mer)

broom.mixed::tidy(mod, exponentiate = TRUE, conf.int = TRUE) |>
  mutate_if(is.numeric, ~(round(.x, 4)))
