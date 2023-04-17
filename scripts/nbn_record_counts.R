library(needs)
needs(tidyverse, tidyfast, data.table)
setDTthreads(6)

tfb_bd_nbn <- read_rds("buffer_bd.rds")

tfLL <- read_csv("data/tf_trees.csv")

nbn_tfb <- map_dfr(c(1:171), ~(tfb_bd_nbn[[.x]] |> mutate(id = tfLL$tf_id[.x]))) |>
  select(1:15) |>
  setDT()

nbn_tfb_class <- nbn_tfb[classs %in% c("Aves", "Insecta", "Magnoliopsida", "Mammalia"),]


dt_count(nbn_tfb_class[between(year, 2000, 2023)], classs ) |>
  arrange(-N)

## top 4 aves, insecta, magnoliopsida, mammalia

