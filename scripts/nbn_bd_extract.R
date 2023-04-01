needs(tidyverse, vegan)
path <- here::here()
source(paste0(path, "/scripts/nbn_buffer.R"))
tfLL <- read_csv(paste0(path, "/data/tf_lat_long.csv"))
tfLL <- tfLL[-54,]

safe_buff <- safely(get_nbn_buffer)

nbn_bd_data_3 <- purrr::map(93:100, ~(safe_buff(tfLL$long[.x], tfLL$lat[.x], n = 30000)))

nbn_data_final <- map(nbn_bd_data, "result")

map(nbn_bd_data_3, "result")
nbn_bd_data

nbn_bd_data[[1]]$result |>
  count(classs, year) |>
  filter(year >= 2010) |>
  ggplot() +
  geom_tile(aes(year, classs, fill = n))

tychwood_nbn_year <- nbn_bd_data[[1]]$result |>
  filter(year >= 2010,
         classs == "Aves") |>
  select(year, vernacularName, species) |>
  count(vernacularName, year) |>
  pivot_wider(names_from = "vernacularName",
              values_from = "n",
              values_fn = sum,
              values_fill = 0) |>
  arrange(year)

year <- factor(tychwood_nbn_year$year)
species <- tychwood_nbn_year[,-1 ] |>
  janitor::clean_names()

sn <- tychwood_nbn_year |>
  vegan::specnumber() |>
  bind_cols(year)

tychwood_nbn_year |>
  vegan::diversity() |>
  bind_cols(year)


vegan::adonis2(species ~ year, model = c("raw"), permutations =
                                        9999, method = "euclidean", autotransform
                                      = TRUE)




pairwise.adonis <- function(x, factors, sim.method, p.adjust.m)
{
  library(vegan)
  set.seed(1)
  factors <- factors
  co = as.matrix(combn(unique(year),2))
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


pairwise.adonis(species, year, sim.method="bray", p.adjust.m
                ='bonferroni')


sp.nmds.2d <- metaMDS(species, distance = "bray", k=2, noshare = TRUE,
                        trymax=250, engine = c("monoMDS"), plot=FALSE, autotransform = FALSE)
scrs.2d <- as.data.frame(scores(sp.nmds.2d, display = "site"))

scrs.2d <- cbind(scrs.2d, year = year)
plot(scrs.2d)

x<-as.numeric(scrs.2d$NMDS1)
y<-as.numeric(scrs.2d$NMDS2)

scrs.2d |>
  ggplot(aes(NMDS1, NMDS2, col = year)) +
  geom_point() +
  scale_color_viridis_d()


pca.sp<-rda(species)
pca.scrs.2d <- as.data.frame(scores(pca.sp, display = "site"))
pca.scrs.2d <-cbind(pca.scrs.2d, year = year)
pca.scrs.2d

pca.scrs.2d |>
  ggplot(aes(PC1, PC2, col = year)) +
  geom_point() +
  scale_color_viridis_d()


library(umap)
library(reticulate)
iris.umap_learn <- umap(species, method="umap")

custom.settings = umap.defaults
custom.settings$n_neighbors = 3
custom.settings
u <- umap::umap(species, config = custom.settings)

u$layout |>
  cbind(year) |>
  data.frame() |>
  ggplot() +
  geom_point(aes(V1, V2,  colour = factor(year), size = 2))

