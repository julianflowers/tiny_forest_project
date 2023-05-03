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
  map_chr("gr") |>
  map(~(str_sub(.x, 1, 6)))



tf <- flatten(gr) |>
  map_chr(1) |>
  bind_cols(tf)

get_bsbi_data <- function(gr){

  url <- paste0("https://database.bsbi.org/reports/sitetaxa.php?gridref=", gr, "&minfreq=1&minyear=2015&maxyear=2000&sortrecent=1")

  records <- read_html(url) |>
    html_nodes("section") |>
    html_text2() |>
    str_split("\\n") |>
    enframe() |>
    mutate(grid = gr) |>
    unnest("grid") |>
    unnest("value")

  out <- list(records = records)

}

test <- get_bsbi_data(gr[1])

plants <- map(gr, safely(get_bsbi_data))

plants <- plants |>
  map("result") |>
  map("records") |>
  list_rbind() |>
  filter(str_length(value) > 1) |>
  mutate(year = str_extract(value, "20\\d{2}"),
         species_tot = ifelse(is.na(year), parse_number(value), NA)) |>
  fill(species_tot, .direction = "up") |>
  filter(!str_detect(value, "Total")) |>
  slice(-169)

plants |>
  mutate(value = str_remove(value, "\\(20\\d{2}\\)"),
         value = str_remove(value, "[[:space:]]s\\.s\\.|[[:space:]]s\\.l\\.|[[:space:]]agg\\.|× |[[:space:]]auct\\., non Mill\\.")) |>
  separate_wider_delim(value, delim = " ", names = c("taxa1", "taxa2", "start", "end"), too_few = "align_start",
                    too_many = "merge") |>
  unite(taxa, c("taxa1", "taxa2")) |>
  select(-name, -start, -species_tot) |>
  pivot_wider(names_from = "taxa", values_from = "end", values_fn = \(x) sum(as.numeric(x), na.rm = TRUE), values_fill = 0) |>
  left_join(tf, by = c("grid" = "...1")) |>
  arrange(grid, year.x) |>
  mutate(id = paste(tfid, year.x, grid, sep = "-")) |>
  select(stub:id, everything()) |>
  write_csv("data/plant_bsbi_matrix.csv")






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



