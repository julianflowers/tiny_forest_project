## scraping the tiny forest website to get location and planting information

library(needs)
needs(rvest, tidyverse)
library(myScrapers)

url <- 'https://tinyforest.earthwatch.org.uk/tiny-forest-sites/'


## scrape web addresses for each tf
links <- get_page_links(url) %>%
  .[18:402] |>
  unique() |>
  enframe()

tf_df <- links |>
  mutate(url = paste0("https://tinyforest.earthwatch.org.uk/", value),
         stub = str_remove(value, "/tiny-forest-sites/8-tiny-forest/"),
         tf_id = parse_number(stub))



create_tf_table <- function(df, i){

  read_html(tf_df$url[i]) |>
  html_nodes("body") |>
  html_text2() |>
  str_split("\n") |>
  enframe() |>
  unnest("value") |>
  filter(str_detect(value, "m2") |
           str_detect(value, "GPS") |
           str_detect(value, "^\\d{2}\\D.*20\\d{2}")|
           str_detect(value, "Species")) |>
    mutate(tf_id = df$tf_id[i])

}

tf_table <- map_dfr(1:180, ~(create_tf_table(tf_df, .x)))

tf_table_planted <- tf_table |>
  group_by(tf_id) |>
  mutate(n = n(),
         id = row_number()) |>
  filter(n > 3) |>
  mutate(metric = case_when(id == 1 ~ "plant_date",
                            id == 2 ~ "area",
                            id == 3 & n == 5 ~ "class area",
                            id == 3 & n == 4 ~ "trees",
                            id == 4 & n == 5 ~ "trees",
                            TRUE ~ "gps"))

tf_table_planted |>
  ungroup() |>
  mutate(value = case_when(metric == "plant_date" ~ lubridate::dmy() ))



tf_table_tidy <- tf_table_planted |>
  select(-c(n, name, id)) |>
  pivot_wider(names_from = "metric", values_from = "value", values_fn = list ) |>
  unnest("gps") |>
  #unnest("class area") |>
  unnest("area") |>
  unnest("plant_date") |>
  unnest("trees") |>
  select(-`class area`) |>
  mutate(date = dmy(plant_date),
         area = parse_number(area),
         trees = str_remove(trees, "Species Planted in the Forest:"),
         gps = str_remove(gps, "GPS:")) |>
  separate(gps, c("lat", 'lon'), sep = ",.") |>
  mutate(trees = str_split(trees, "\\|"))

tf_presence_absence <- tf_table_tidy |>
  ungroup() |>
  unnest("trees") |>
  pivot_wider(names_from = "trees", values_from = "trees", values_fill = NA) |>
  mutate_at(.vars = 7:84, ~(ifelse(is.na(.x), 0, 1))) |>
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) |>
  select(-plant_date) |>
  janitor::clean_names()

tf_presence_absence |>
  write_csv("data/tf_trees.csv")
