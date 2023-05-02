# script to extract bsbi observations for 1km grid squares

library(myScrapers);library(rvest);library(tidyverse);library(sf);library(reticulate)

grids <- "/Users/julianflowers/Library/Mobile Documents/com~apple~CloudDocs/Downloads/Download_1948457"

grids <- list.dirs(grids)[2]

list.files(grids, "shp", full.names = TRUE)[2] |>
  read_sf() |>
  st_transform(4326)

Sys.setenv(RETICULATE_PYTHON = "/Users/julianflowers/.virtualenvs/osmaps/bin/python")
use_virtualenv("osmaps")
py_install("OSGridConverter", pip = TRUE, envname = "osmaps")

osgrid <- import("OSGridConverter")

tf <- read_csv("data/tf_trees.csv")
tf

grid <- osgrid$latlong2grid(tf$lat[160],tf$lon[160])

r <- str(grid)



gr <- "SP30U"
gr1 <- "TL4760"

get_bsbi_data <- function(gr){

url <- paste0("https://database.bsbi.org/reports/sitetaxa.php?gridref=", gr, "&minfreq=1&minyear=2015&maxyear=2000&sortrecent=1")

  records <- read_html(url) |>
    html_nodes("section") |>
    html_text2() |>
    str_split("\\n")

  out <- list(records = records)

}

test <- get_bsbi_data("TL4760")
test |>
  enframe() |>
  unnest("value") |>
  unnest("value") |>
  slice(-c(168:173)) |>
  mutate(year = str_extract(value, "20\\d{2}"),
         value = str_remove(value, year),
         count = str_extract(value, "\\d.*$"),
         value = str_remove(value, count)) |>
  separate(count, c("past", "present", sep = " ")) |>
  select(-c(name, past)) |>
  select(-last_col()) |>
  pivot_wider(names_from = "value", values_from = "present", values_fn = list)

