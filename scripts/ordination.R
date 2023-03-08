## ordination with vegan, factoextra, umap, tsne

needs(umap, factoextra, Rtsne, vegan, FactoMineR, dbscan, ggfortify)
remotes::install_github("gavinsimpson/ggvegan")


data(BCI)

data(dune)
data("dune.env")
dim(dune)
data("dune.phylodis")

dune.env

rarefy(BCI, 3)

diversity(BCI)/log(specnumber(BCI))


pm <- poll_map |>
  dplyr::select(PLAN_NO, date, habitat, bumblebees:insects_other, -geometry) |>
  mutate(date = lubridate::dmy(date), 
         year_mon = zoo::as.yearmon(date))

pm_habitats <- pm |>
  count(PLAN_NO, habitat, year_mon) 

|>
  group_by(PLAN_NO, year_mon) |>
  summarise()

pm_habitats |>
  count(habitat)

pm$geometry <- NULL 

pm_final <- pm |>
  group_by(PLAN_NO, year_mon) |>
  select(-date) |>
  mutate_if(is.numeric, sum) |>
  distinct() 



sp_richness <- specnumber(pm_final)

div <- enframe(diversity(ungroup(pm_final[,3:12])))

umap <- umap(pm_final[, -c(1:2, 13)])

umap$layout |>
  plot()

pca <- metaMDS(pm_final[, -c(1:2, 13)])
pca
scores(pca, display = "sites")

ggvegan::autoplot(pca)

aov(value ~ habitat, data = sp_richness) |>
  summary()

length(sp_richness)
nrow(pm_final)
nrow(pm_habitats)

sp_richness <- data.frame(sp_richness, pm_habitats, div)

dim(sp_richness)

sp_richness |>
  ggplot() +
  geom_violin(aes(habitat, sp_richness))

aov(sp_richness ~ habitat, data = sp_richness) |>
  summary()




pm |>
  select(-c(date, PLAN_NO, habitat)) |>
  t()
  nest(data = bumblebees:insects_other) |>
  #mutate(scale = map(data, scale)) |>
  mutate(diversity = map(data, vegan::diversity, "simpson")) 
  unnest("diversity")

pm <- data.frame(pm)
  
summary(pm)

pca <- FactoMineR::pm[4:13])

factoextra::get_pca(pca)$coord

factoextra::fviz_pca_var(pca)


scale_pm <- scale(pm[4:13])

um <- umap(scale_pm, preserve.seed = TRUE)

??Rtsne

tsne <- Rtsne::Rtsne(scale_pm, check_duplicates = FALSE, perplexity = 50)

plot(tsne$Y)

clust <- tsne$Y |>
  data.frame() |>
  dbscan::hdbscan(minPts = 30)

clust$cluster 

tsne$Y |>
  cbind(clust$cluster) |>
  data.frame() |>
  group_by(X3) |>
  mutate(medianX = median(X1), 
         medianY = median(X2)) |>
  ggplot() +
  geom_point(aes(X1, X2, colour = factor(X3))) +
  geom_point(aes(medianX, medianY), colour = "black", shape = "X", size = 5) +
  scale_color_viridis_d() +
  ggthemes::theme_few()
  
