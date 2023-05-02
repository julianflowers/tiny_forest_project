## urban diversity framework scores https://www.sciencedirect.com/science/article/pii/S0006320720308119?via%3Dihub#bb0010

needs(readxl, tidyverse)

xls <- tempfile()

curl::curl_download("https://ars.els-cdn.com/content/image/1-s2.0-S0006320720308119-mmc2.xlsx", xls)

urban_bio <- read_xlsx(xls)

urban_bio |>
  count(Class) |>
  print(n = 27)
