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
           str_detect(value, "^\\d{2}.*20\\d{2}")|
           str_detect(value, "Species")) |>
    mutate(tf_id = df$tf_id[i])

}

tf_table <- map_dfr(1:180, ~(create_tf_table(tf_df, .x)))

tf_table <- tf_table |>
  group_by(tf_id) |>
  mutate(n = n(),
         id = row_number(),
         metric = case_when(id == 1 ~ "plant date",
                            id == 2 ~ "area",
                            id == 3 & n == 5 ~ "class area",
                            id == 3 & n == 4 ~ "trees",
                            id == 4 & n == 5 ~ "trees",
                            TRUE ~ "gps"))

tf_table |>
  filter(tf_id == 379)

tf_table_tidy <- tf_table |>
  select(-c(n, id, name)) |>
  pivot_wider(names_from = "metric", values_from = "value", values_fn = list ) |>
  unnest("gps") |>
  unnest("trees") |>
  unnest("class area") |>
  unnest("area") |>
  unnest("plant date") |>
  mutate(date = dmy(`plant date`),
         area = parse_number(area),
         class_area = parse_number(`class area`),
         trees = str_remove(trees, "Species Planted in the Forest:"))

tf_table_tidy |>
  DT::datatable()

