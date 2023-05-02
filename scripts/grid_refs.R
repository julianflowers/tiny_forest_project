library(needs)
needs(furrr, future, tidyverse, tictoc, tidyfast, dtplyr, fs, sf, mapview, reticulate)

grids <- "/Users/julianflowers/Library/Mobile Documents/com~apple~CloudDocs/Downloads/Download_1948457"

grids <- list.dirs(grids)[2]

grid_reds <- list.files(grids, "shp", full.names = TRUE)[2] |>
  read_sf() |>
  st_transform(4326)

Sys.setenv(RETICULATE_PYTHON = "/Users/julianflowers/.virtualenvs/osmaps/bin/python")
use_virtualenv("osmaps")
py_install("OSGridConverter", pip = TRUE, envname = "osmaps")

osgrid <- import("OSGridConverter")


tf <- fs::dir_ls("data")[39] |>
  read_csv(lazy = TRUE)

get_grid_ref <- function(i){

  grid <- osgrid$latlong2grid(tf$lat[i],tf$lon[i])

   r <- str_flatten(grid)


   code <- str_extract(r, "[[:upper:]]{2}")

   match <- str_extract_all(r, "\\d{5}")

   gr <- paste0(code, match[[1]][1])

   out <- list(gr = gr)

}

gr <- map(1:176, safely(get_grid_ref) ) |>
  map("result") |>
  map_chr("gr")




rur_urb <- fs::dir_ls("data")[39] |>
  read_csv(lazy = TRUE) |>
  count(`Rural Urban Classification 2011 (10 fold)` ) |>
  mutate(rur_urb = factor(`Rural Urban Classification 2011 (10 fold)` ))

fct_infreq(rur_urb$rur_urb) |>
  fct_rev() |>
  levels()

rur_urb |>
  ggplot() +
  geom_col(aes(fct_infreq(rur_urb) |>
                 fct_rev(), n)) +
  coord_flip()


tf_sf <- st_as_sf(fs::dir_ls("data")[39] |>
           read_csv(lazy = TRUE), coords = c("lon", "lat"), crs = 4326)



