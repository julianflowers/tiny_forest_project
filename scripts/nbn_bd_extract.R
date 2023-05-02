needs(tidyverse, vegan, data.table)
path <- here::here()
source(paste0(path, "/scripts/nbn_buffer.R"))

get_nbn_buffer(lon = -5.186, lat = 58.380, radius = 2000, n = 40000) -> inchnadamph

setDT(inchnadamph)[classs == "Aves" & vernacularName == "Wood Warbler" & between(year, 2010, 2020), .N, by = .(year)][order(-N)]|>
  top_n(100)
tfLL <- read_csv("data/tf_trees.csv")


tfLL[216, ]

ids <- tfLL$tf_id[-c(51, 170)]


# remove 51, check 73, 83, 84

safe_buff <- safely(get_nbn_buffer)


nbn_bd_data_1 <- purrr::map(1:10, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
nbn_bd_data_2 <- purrr::map(11:20, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
nbn_bd_data_3 <- purrr::map(21:30, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
nbn_bd_data_4 <- purrr::map(31:40, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
nbn_bd_data_5 <- purrr::map(41:50, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
nbn_bd_data_6 <- purrr::map(51:60, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
nbn_bd_data_7 <- purrr::map(61:70, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
nbn_bd_data_8 <- purrr::map(71:80, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
nbn_bd_data_9 <- purrr::map(81:90, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
nbn_bd_data_10 <- purrr::map(91:100, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
nbn_bd_data_11 <- purrr::map(101:110, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
nbn_bd_data_12 <- purrr::map(111:120, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
nbn_bd_data_13 <- purrr::map(121:130, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
nbn_bd_data_14 <- purrr::map(131:140, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
nbn_bd_data_15 <- purrr::map(141:150, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
nbn_bd_data_16 <- purrr::map(151:160, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
nbn_bd_data_17 <- purrr::map(161:170, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))
#nbn_bd_data_18 <- purrr::map(171:174, ~(safe_buff(tfLL$lon[.x], tfLL$lat[.x], n = 30000)))


res <- map(nbn_bd_data_17, "result")

map(res, dim)

nbn_data_final <- map(c(nbn_bd_data_1, nbn_bd_data_2, nbn_bd_data_3, nbn_bd_data_4, nbn_bd_data_5,
                        nbn_bd_data_6[2:10], nbn_bd_data_7, nbn_bd_data_8, nbn_bd_data_9,
                        nbn_bd_data_10,
                        nbn_bd_data_11, nbn_bd_data_12, nbn_bd_data_13,
                        nbn_bd_data_14, nbn_bd_data_15, nbn_bd_data_16,
                        nbn_bd_data_17[1:9]
                        ),  "result")

nbn_data_final |>
  saveRDS("buffer_bd.rds")

names(nbn_data_final)


nbn_data_final_1 <- map_dfr(1:168, ~(nbn_data_final[[.x]] |> mutate(tf = ids[.x])))

nbn_data_final_1 <- nbn_data_final_1[-1059949,]

setDT(nbn_data_final_1)

nbn_data_final_1[between(year, 2010, 2023), .N, by = .(tf, year)][order(tf, year)] |>
  ggplot() +
  geom_tile(aes(year, factor(tf), fill = log(N))) +
  scale_fill_viridis_c(direction = -1, option = "rocket") +
  ggthemes::theme_base()

nbn_data_final_1 |>
  fwrite("nbn_tf_data.csv")

nbn_data_recent <- nbn_data_final_1 |>
  filter(between(year, 2010, 2023))

nbn_data_recent <- nbn_data_recent |>
  setDT()

nbn_data_recent |>
  count(tf, year, classs) |>
  ggplot() +
  geom_histogram(aes(n, fill = classs)) +
  facet_wrap(~year + classs)
  ggplot(aes(reorder(tf, n), n)) +
  geom_col() +
  ggthemes::theme_base() +
  theme(plot.background = element_blank()) +
  coord_flip()

nbn_data_recent |>
  count(year) |>
  ggplot(aes(year, n)) +
  geom_col() +
  ggthemes::theme_base() +
  theme(plot.background = element_blank())

nbn_data_recent |>
  filter(tf == 85) |>
  count(classs, year) |>
  filter(classs == "Aves")

tychwood_nbn_year <- nbn_data_recent |>
  filter(tf == 85) |>
  filter(year >= 2010,
         classs == "Insecta") |>
  select(year, vernacularName, species) |>
  count(vernacularName, year) |>
  pivot_wider(names_from = "vernacularName",
              values_from = "n",
              values_fn = sum,
              values_fill = 0) |>
  arrange(year)

tychwood_nbn_year |>
  count(year)

year <- factor(tychwood_nbn_year$year)
species <- tychwood_nbn_year[,-1 ] |>
  janitor::clean_names()



sn <- tychwood_nbn_year |>
  vegan::specnumber() |>
  bind_cols(year)

tychwood_nbn_year |>
  vegan::diversity() |>
  bind_cols(year)

vegdist(species, "bray") |>
  hclust() |>
  plot()

vegan::adonis2(species ~ year, model = c("raw"), permutations =
                                        9999, method = "raup", autotransform
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

