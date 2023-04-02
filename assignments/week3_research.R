library(needs)
needs(dabestr, tidyverse, gtsummary, broom, AER, vegan)

######## access files

path <- here::here("~/Downloads")                        ## set directory
f <- list.files(path, "csv", full.names = TRUE)

f

## list files
read_csv(f[10])


x <- read_csv(f[29])                         ## mudskipper data stored to object 'x'

## calculate the mean no of mudskippers for just Estuary location

mean(x$Mudskippers[x$Location == "Estuary"]) ## 'base' R way

x |>                                      ## the 'tidyverse' way - take data
  filter(Location == "Estuary") |>        ## filter by Location = Estuary
  summarise(mean(Mudskippers))            ## summarise with the mean



shapiro.test(x$Mudskippers)               ## check for normality

hist(x$Mudskippers)                       ## draw a histogram of mudskipper counts

## perform a t-test - right where you have 2 groups or variables and data is independent and identically distributed (IID)
## if you don't think IID holds - use a non-parametric test i.e. Kruskal-Wallis

## The tilde notation (~) means 'formula' and is used where you have
## dependent (e.g. Mudskipper counts) and dependent (response) variables
## Put the dependent  variables on the LEFT and independent variables on the RIGHT

t.test(Mudskippers ~ Location, data = mudskipper, paired = F)

kruskal.test(Mudskippers ~ Location, data = mudskipper) |>
  tidy()

t_test_result <- t.test(Mudskippers ~ Location, data = mudskipper, paired = F)

tidy(t_test_result) ## gives a more readable output

## gtsummary gives publication ready table outputs (https://yuzar-blog.netlify.app/posts/2022-10-31-gtsummary/)
gtsummary::tbl_summary(x, by = Location,
                       statistic = list(all_continuous() ~ "{mean} ({sd})",
                                                          all_categorical() ~ "{n} / {N} ({p}%)")) %>%
  add_p(test = list(Mudskippers ~ "t.test"))


gtsummary::tbl_summary(x, by = Location,
                       statistic = list(all_continuous() ~ "{mean} ({sd})",
                                        all_categorical() ~ "{n} / {N} ({p}%)")) %>%
  add_p(test = list(Mudskippers ~ "kruskal.test"))

############

dog_stress <- read_csv(f[32])

t.test(dog_stress$stress_before, dog_stress$Stress_after) |>
  tidy()

lm(Stress_after ~ factor(Dog) + stress_before, data = dog_stress) |>.    ## lm
  anova() |>
  tidy()


nested <- dog_stress |>
  nest_by(Dog)


models <- nested |>                                                     ## stratified
  mutate(mod = list(lm(Stress_after ~ stress_before, data = data)))

models %>%
  summarise(broom::glance(mod))                                         ## tidy


############

leaves <- read_csv(f[32])

leaves

leaves |>                                                               ## how many in each location
  count(Location)

t.test(Mean_size ~ Location, data = leaves) |>
  glance()

aov(Mean_size ~ Location, data = leaves) |>
  glance()

#############

## white clover example for anova

clover.data <- MOD003371_White_clover_data <- read_csv("~/Dropbox/My Mac (Julians-MBP-2)/Downloads/MOD003371 White clover data.csv") ## load data
names(clover.data)
attach(clover.data)

### two-way orthogonal ANOVA (2x2) ##

two.way.1 <- aov(Biomass1~Temperature*Moisture)
summary(two.way.1)
tidy(two.way.1)

two.way.2 <- aov(Biomass2~Temperature*Moisture)
summary(two.way.2)
tidy(two.way.2)

## simple boxplots ##
plot(Biomass1 ~ Temperature * Moisture)

plot(Biomass2 ~ Temperature * Moisture, data = clover.data)

## plot the interaction ##

with(clover.data, interaction.plot(Temperature, Moisture, Biomass1))
interaction.plot(Temperature, Moisture, Biomass2)

par(mfrow=c(2,2))
plot(two.way.1)
plot(two.way.2)

norm.test.1 <- shapiro.test(Biomass1)
norm.test.1

norm.test.2 <- shapiro.test(Biomass2)
norm.test.2

#############

# Week 5

#############
library(tidyverse);library(broom);library(gt)

data <- read_csv(f[10])

data

data$Grazing

data$Irrigation <- as.factor(data$Irrigation)

summary(data)

glimpse(data)

## calculate standard error function

length(data$Grazing)

se <- function(x)  sd(x)/sqrt(length(x))

se(data$Yield)


se(c(1,2,3,4, 567))

## test for normality with Shapiro Wilks test

s1 <- shapiro.test(data$Yield)
s1
hist(log(data$Yield))



## mean grazing

attach(data)
(mean.grazing <- tapply(Yield, Grazing, mean)) ## apply method = calculate mean yield for high and low grazing

mean_grazing <- data |>                        ## tidy method using group_by and summarise
  group_by(Grazing, Irrigation) |>
  summarise(mean_yield = mean(Yield))

mean_grazing

se_grazing <- data |>                          ## tidy method using group_by and summarise
  group_by(Grazing) |>
  summarise(se_y = se(Yield))

se_grazing

## both

grazing_stats <- data |>                          ## tidy method using group_by and summarise
  group_by(Grazing, Irrigation) |>
  summarise(se_y = se(Yield),
            sd_y = sd(Yield),
            mean_y = mean(Yield),
            median = median(Yield),
            max = max(Yield))

grazing_stats |>
  gt()

## using tapply for sd

tapply(Yield, Grazing, sd)

data |>                          ## tidy method using group_by and summarise
  group_by(Grazing) |>
  summarise(se_y = se(Yield),
            mean_y = mean(Yield),
            sd_y = sd(Yield))

detach(data)

boxplot(factor(data$Yield) ~ data$Grazing, ylab = "Sorghum yield", xlab = "Grazing intensity",
        main = "Plot for week 5")

data %>%                                ## ggplot method
  ggplot() +
  geom_boxplot(aes(Grazing, Yield), fill = "grey") +
  ggthemes::theme_few()


### anova

data |>                          ## tidy method using group_by and summarise
  group_by(Irrigation) |>
  summarise(se_y = se(Yield),
            mean_y = mean(Yield),
            sd_y = sd(Yield))

boxplot(Yield~Irrigation)


## reorder category order
boxplot(Yield~factor(Irrigation,levels=c("Low","Medium","High")),
     ylab="Yield of sorghum (t/ha)", xlab="Irrigation level")


data %>%                                ## ggplot method
  ggplot() +
  geom_boxplot(aes(factor(Irrigation, levels=c("Low","Medium","High")), Yield), fill = "grey") +
  labs(x = "Irrigation level") +
  ggthemes::theme_few()

## one way aov

aov_y <- aov(Yield ~ GrazingIrrigation, data = data)

aov_y

summary(aov_y)

tidy(aov_y)



TukeyHSD(aov_y) |>
  tidy() |>
  filter(adj.p.value < 0.05)




TukeyHSD(aov_y) |>
  tidy() |>
  filter(adj.p.value < 0.05) |>
  ggplot() +
  geom_point(aes(contrast, estimate), size = 2) +
  geom_errorbar(aes(x = contrast, ymin = conf.low, ymax = conf.high, width = .1)) +
  ggthemes::theme_base() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### 2 way anova


summary <- data |>                          ## tidy method using group_by and summarise
  group_by(GrazingIrrigation) |>
  summarise(se_y = se(Yield),
            mean_y = mean(Yield),
            sd_y = sd(Yield))

## plot means and se
summary |>
  ggplot() +
  geom_col(aes(GrazingIrrigation, mean_y), fill = "grey") +
  geom_errorbar(aes(x = GrazingIrrigation, ymin = mean_y - sd_y, ymax = mean_y + sd_y, width = 0.1)) +
  ggthemes::theme_few() +
  scale_x_discrete(guide = guide_axis(n.dodge=2))



aov_2 <- aov(Yield ~ Grazing + Irrigation + GrazingIrrigation)

summary(aov_2)
tidy(aov_2)

aov_2_1 <- aov(Yield ~ Grazing + Irrigation)
tidy(aov_2_1)

anova(aov_2, aov_2_1)

par(mfrow = c(2,2))
plot(aov_2)

dev.off()
library(ggfortify)

autoplot(aov_2_1) +
  ggthemes::theme_few()


##### week 6

set.seed(2023)

data <- data |>
  janitor::clean_names()

data_wide <- data |>
  select(-grazing, -irrigation) |>
  pivot_wider(names_from = "grazing_irrigation", values_from = "yield")

data_long <- data_wide |>
  pivot_longer(names_to = "Group", values_to = "Measurement", cols = 1:6) |>
  unnest("Measurement")

data_long

groups <- data |>
  pluck("grazing_irrigation") |>
  unique()


data %>%
  janitor::clean_names() |>
  rename(Group = grazing_irrigation, Measurement = yield)

data1 <- data |>
  select(Group = 3, Measurement = 4)

library(dabestr)

all_vs_control <- data1 |>
  dabest(x = Group, y = Measurement,
          idx = c(groups[6], groups[2], groups[3],
                  groups[4], groups[5], groups[1]),
         paired = FALSE)

mean_diff <- all_vs_control %>%
  mean_diff()

mean_diff$result$bootstraps
str(mean_diff)

plot1 <-plot(mean_diff,
             rawplot.ylabel = "Yield (tonne / ha)",
             rawplot.type = "sinaplot",
             theme = ggplot2::theme_classic(),
             effsize.markersize = 3,
             rawplot.markersize = 3,
             rawplot.groupwidth = 0.30,
             rawplot.ylim = c(min(data$yield),max(data$yield)),
             effsize.ylim = NULL,
             effsize.ylabel = "Mean difference",
             tick.fontsize = 8,
             axes.title.fontsize = 10)
plot1


## Week 7 analysis

wk7 <- read_csv(f[12])

wk7 |>
  summary()

### code book

code_book <- data.frame(name = c("plotID", "date", "plotsize", "spcode", "species",
                                 "cover", "utme", "utmn", "elev", "tci", "streamdist", "disturb", "beers"),
              desc = c("identifier for the location (plot) sampled.",
              "the date the sample was taken",
              "size of the sampled plots.",
              "a shorthand for tree species.",
              "species’ full name.",
              "estimated average cover of the tree crowns within the plot (in m2).",
              "coordinate easting (using gps).",
              "coordinate northing (using gps).",
              "elevation from sea level.",
              "tree-crop interface.",
              "distance from a stream.",
              "the type of disturbance by human activity." ,
              "beers consumed during fieldwork (standardised units, probably pints)."
              )
)

glimpse(wk7)

count(wk7, plotID)

with(wk7, plot(cover ~ elev))

wk7 |>
  filter(str_detect(species, "Fag")) |>
  ggplot(aes(elev, cover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~species) +
  ggthemes::theme_base()

beech <- wk7 |>
  filter(str_detect(species, "Fag"))

mod <-lm(cover ~ elev, data = wk7, subset = species == "Fagus grandifolia")

broom::augment(mod, beech)

broom::augment(mod, wk7 |> filter(str_detect(species, "Fag"))) |>
  ggplot() +
  geom_point()

 anova(mod)

 confint(mod)

 AIC(mod)

 mod2 <-lm(cover^0.5 ~ elev, data = wk7, subset = species == "Fagus grandifolia")

broom::glance(mod2)
AIC(mod2)

anova(mod, mod3)

mod3 <-lm(cover ~ disturb, data = wk7, subset = species == "Fagus grandifolia")

summary(mod3)
AIC(mod3)
anova(mod3)

anova(mod3, mod)

mod4 <- lm(cover ~ disturb*elev, data=wk7,
           subset=species=="Fagus grandifolia")

summary(mod4)

anova(mod4)
AIC(mod4)

mod5 <- update(mod4, ~.-disturb:elev)

anova(mod4, mod5)

step(mod4) |>
  broom::tidy()

needs(ggfortify, lme4, nlme)


autoplot(mod)


mod5 <- lm(cover ~ elev*tci*streamdist*disturb, subset = species == "Fagus grandifolia", data = wk7)

mod5 |>
  broom::tidy() |>
  arrange(p.value)

AIC(mod5)

stepwise <- step(mod5)
str(stepwise)

stepwise$anova$Step

stepwise$p.value


## using a multilevel / mixed modelling approach
library(lme4)

mod_l <- lmer(cover ~ tci + (1|plotID), data = wk7)
mod_l_2 <- lmer(cover ~ elev + (1|plotID), data = wk7)
mod_l_3 <- lmer(cover ~ elev + tci + (1|plotID), data = wk7)



anova(mod_l, mod_l_2, mod_l_3)

mod_l_1 <- nlme::lme(cover ~ tci, random = ~1|plotID, data = wk7)

AIC(mod_l)
confint(mod_l, method = "profile")

plot(mod_l)

mod_l_1 |>
  summary()
  broom.mixed::glance()

## week 8 glms

blue_tit <- read_csv(f[16])     ## load blue tit data

blue_tit |>
  mutate(site = factor(site))

blue_tit |>                     ## summarise
  summary()

### poisson regression
### we will run a few models - the first assumes the data is gaussian (normal) - family = "gaussian"
### including eggdate*site is equivalent to including eggdate, site and the interaction as independent variables
### because the dependent variable is counts of fledged birds (and small numbers) and includes vero counts..
### the poisson distribution is a better assumption so we use family = "poisson" in the glm
### in poisson distributions the mean and variance should be (roughly) equal to 1 - if the variance is greater
### data is said to be overdispersed so people tend to use alternative distributions like
### quasipoisson and negative binomial - i have included quasipoisson as an example
### using anova allows us to identify the best fitting model - its the one with the lowest AIC

mod_bt_1 <- glm(fledge.count ~ eggdate*site, data = blue_tit)
mod_bt_2 <- glm(fledge.count ~ eggdate*factor(site) -1, data = blue_tit, family = "poisson")
mod_bt_3 <- glm(fledge.count ~ eggdate*site, data = blue_tit, family = "quasipoisson") ## data is overdispersed
mod_bt_4 <- glm(fledge.count ~ eggdate + site, data = blue_tit, family = "poisson")

summary(mod_bt_4)

mod_bt_5 <- glm(fledge.count ~ site, data = blue_tit, family = "quasipoisson")

AIC(mod_bt_1)
dp <- dispersiontest(mod_bt_2)

dp$estimate
summary(mod_bt_2, dispersion = dp$estimate)
AIC(mod_bt_3)
AIC(mod_bt_5)


tidy(mod_bt_2, exponentiate = TRUE, conf.int = TRUE)

anova(mod_bt_4, test = "Chisq")

#site as a random variable

library(lme4)

mod_bt_6 <- glmer(fledge.count ~ eggdate + (1|site), data = blue_tit, family = "poisson")

broom.mixed::tidy(mod_bt_6)

## logistic regression
## where your dependent variable is yes/no eg fledged/ not-fledged - this fits the binomial distribution and is
## knows as logistic regression (this is the shape of the distribution)
## the binomial distribution fits heads/tails success rates of coin tosses for example
## for glm we need to recode the dependent variable (fledged) to 1 and 0.

blue_tit <- blue_tit |>
  mutate(fledged = case_when(fledge.binary == "Fledged" ~ 1,
                             TRUE ~ 0))

mod_lr_1 <- glm(fledged ~ eggdate + site, data = blue_tit, family = "binomial")

mod_lr_1 |>
  tidy(exponentiate = TRUE, conf.int = TRUE) ## this converts the data back to units of analysis (otherwise its log(unit))

mod_lr_2 <- glmer(fledged ~ eggdate + (1|site), data = blue_tit, family = "binomial")

mod_lr_2 |>
  tidy(exponentiate = TRUE, conf.int = TRUE)

## Week 9 - multidimensional data - gull communities

gulls <- read_csv(f[18])

habitat <- factor(gulls$Habitat)

species <- gulls |>
  select(-Habitat)



### sum across species and habitat types
gulls |>
  gather(species, count, 2:ncol(gulls)) |>
  group_by(Habitat, species) |>
  summarise(sum = sum(count)) |>
  spread(Habitat, sum)

### row sums - abundance

abundance <- data.frame(habitat, n = rowSums(species))
abundance |>
  group_by(habitat) |>
  summarise(mean = mean(n),
            sd = sd(n))

colSums(species) ## counts total obs of each species

### using vegan

sn <- specnumber(species)
sn <- data.frame(habitat, sn)

sn |>
  group_by(habitat) |>
  summarise(mean = mean(sn),
            sd = sd(sn))

gulls |>
  gather(species, count, 2:ncol(gulls))

### diversity

d <- diversity(species, index = "shannon")

d1 <- data.frame(habitat, d)


aov.sn <- aov(sn ~ habitat, data = sn)

aov.sn |>
  summary()

TukeyHSD(aov.sn) |>
  broom.mixed::tidy()

aov.d <- aov(d ~ habitat, data = d1)

aov.d |>
  summary()

TukeyHSD(aov.d) |>
  broom.mixed::tidy()

### transformations

species_transformed_4th <- sqrt(sqrt(species))
species_transformed_sqrt <- sqrt(species)
species_transformed_log <- log10(species + 1)

### distance matrices

gull.distance1 <- vegdist(species_transformed_4th, "bray")

variance.check1 <- betadisper(gull.distance1, habitat,
                              type=c("centroid"), bias.adjust= FALSE)

anova(variance.check1)

gull.distance2 <- vegdist(species, "bray")

variance.check2 <- betadisper(gull.distance2, habitat,
                              type=c("centroid"), bias.adjust= FALSE)

anova(variance.check2)


### permutations with adonis

gull.adonis1 <- adonis(species ~ habitat, model = c("raw"),
                       permutations = 9999, method = "bray", autotransform = FALSE)




gull.adonis1$aov.tab

summary(gull.adonis1)



pairwise.adonis <- function(x,factors, sim.method, p.adjust.m)
{
  library(vegan)
  set.seed(1)
  factors <- habitat
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


pairwise.adonis(species, habitat, sim.method="bray", p.adjust.m
                ='bonferroni')

sn |>
  str()


sn.adonis <- adonis(as.numeric(sn) ~ habitat, model = c("raw"), permutations =
                      9999, method = "euclidean", autotransform = TRUE, data = sn)


str(sn.adonis)

sn.adonis$aov.tab

summary(aov.sn)

### pca

pca_gulls <- princomp(species)

plot(pca_gulls)

gull_pca <- data.frame(pca_gulls$scores[,1:2], habitat)

rda <- vegan::rda(species)

plot(gull_pca[1:2])

summary(rda)

## nmds

library(vegan)
gull.nmds.2d <- metaMDS(species, distance = "bray", k=2, noshare = TRUE,
                        trymax=250, engine = c("monoMDS"), plot=FALSE, autotransform = FALSE)
scrs.2d <- as.data.frame(scores(gull.nmds.2d, display = "site"))

scrs.2d <- cbind(scrs.2d, habitat = habitat)
scrs.2d

gull.nmds.3d <- metaMDS(species, distance = "bray", k=3, noshare = TRUE,
                        trymax=250, engine = c("monoMDS"), plot=FALSE, autotransform = FALSE)
scrs.3d <- as.data.frame(scores(gull.nmds.3d, display = "site"))
scrs.3d <- cbind(scrs.3d, habitat = habitat)
scrs.3d
plot(gull.nmds.2d)

x<-as.numeric(scrs.2d$NMDS1)
y<-as.numeric(scrs.2d$NMDS2)

plot(y, x, ylab="nMDS2", xlab="nMDS1", xaxt="n", yaxt="n", cex=1.5)

# choose symbols for each habitat type #
pch_habitat <- c(15,16,17,18)[as.factor(habitat)]
# change symbols in plot #
plot(y, x, ylab="nMDS2", xlab="nMDS1", xaxt="n", yaxt="n", cex=1.5,
     pch=pch_habitat)
# change colours of symbols for each habitat type #
col_habitat <- c("red","blue","green","orange")[as.factor(habitat)]
# change symbols in plot #
plot(y, x, ylab="nMDS2", xlab="nMDS1", xaxt="n", yaxt="n", cex=1.5,
     pch=pch_habitat, col = col_habitat)
# produce simple legend and place at the bottom left #
legend("bottomleft", legend = levels(habitat),
       col=c("red","blue","green","orange"), pch=c(15,16,17,18), cex = 1.0,
       box.col="black")

pca.gull<-rda(species)
pca.scrs.2d <- as.data.frame(scores(pca.gull, display = "site"))
pca.scrs.2d <- cbind(pca.scrs.2d, Habitat = habitat)
pca.scrs.2d

pca.x<-as.numeric(pca.scrs.2d$PC1)
pca.y<-as.numeric(pca.scrs.2d$PC2)
pca.pch_habitat <- c(15,16,17,18)[as.factor(habitat)]
pca.col_habitat <- c("red","blue","green","orange")[as.factor(habitat)]
plot(pca.y,pca.x, ylab="PCA2", xlab="PCA1", cex=1.5, pch=pca.pch_habitat,
     col = pca.col_habitat)
legend("bottomleft", legend = levels(habitat),
       col=c("red","blue","green","orange"), pch=c(15,16,17,18), cex = 1.0,
       box.col="black")

scrs.2d |>
  ggplot() +
  geom_point(aes(NMDS1, NMDS2, shape = habitat, colour = habitat, size = 1.5)) +
  ggthemes::theme_base()


## umap

library(umap)

u <- umap::umap(species)

u$layout |>
  cbind(habitat) |>
  data.frame() |>
  ggplot() +
    geom_point(aes(V1, V2, shape = factor(habitat), colour = factor(habitat), size = 2))

