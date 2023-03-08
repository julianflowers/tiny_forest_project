## working wth CEH landcover 2021 10m resolution data set - data from CEH

library(sf);library(tidyverse);library(fs);library(here)

here::here()

fs::dir_ls(here())

fs::dir_tree(here())


path <- here::here("/Users/julianflowers/Dropbox/FME_333C3E30_1674940727338_8308 2/data/a22baa7c-5809-4a02-87e0-3cf87d4e223a")
lc_21 <- list.files(path, "tif*", full.names = TRUE)

lc_21_stars <- raster::raster(lc_21[1])

head(lc_21_stars)

stars::st_downsample(lc_21_stars, 10)

lc_21_stars$gblcm10m2021.tif

head(lc_21_stars$gblcm10m2021_1)

tiny_forests1 <- read_csv("~/Desktop/tiny-forest1.csv")



tiny_forests1 |>
  DT::datatable()

tiny_forests1_bd <- read_csv("/Users/julianflowers/Dropbox/Mac (2)/Desktop/tiny_forst_biodiversity.csv")



tiny_forests1_sf <-tiny_forests1 |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(27700)

five_trees <- tiny_forests1_sf |>
  filter(str_detect(urls, "five")) 
buff <- st_buffer(five_trees, 100)



lc_21_five_trees <- raster::crop(lc_21_stars, buff)

mapview::mapview(lc_21_five_trees, col.regions = viridis::viridis(21))

buff1 <- st_buffer(tiny_forests1_sf[71,], 20)

mapview::mapview(buff) +
  mapview::mapview(buff1, col.region = "black") +
  mapview::mapview(lc_21_five_trees)

sf::st_geometry(tiny_forests1_sf)

ids <- tiny_forests1$id |>
  unique()

tiny_forests1_sf |>
  filter(id == ids[99]) |>
  st_geometry() |>
  st_buffer(1000) |>
  sf::write_sf("test.shp")

tiny_forests1_sf |>
  filter(id == ids[99]) |>
  st_geometry() |>
  sf::write_sf("test1.shp")

test_buf <- sf::st_read("test.shp") |>
  st_transform(27700)
test_point <- sf::st_read("test1.shp") |>
  st_transform(27700)

extent <- sf::st_bbox(test_buf)

cropped <- st_crop(lc_21_stars, extent) 

b1 <- cropped |>
  slice(band, 1)

class(b1)

b1[[1]] <- fact

mapview::mapView(b1, alpha = 0.4, col.regions = viridis::turbo(21)) +
  mapview::mapView(test_point, col.regions = "black")

ggplot() +
  stars::geom_stars(data = b1) +
  geom_sf(data = test_point, colour = "red", show.legend = F) +
  scale_fill_viridis_b(option = "rocket") +
  coord_sf(crs = 27700)

point <- sf::st_coordinates(test_point)

cropped_df <- as.data.frame(cropped) |>
  arrange(x, y) |>
  pivot_wider(names_from = "band", values_from = "gblcm10m2021.tif") |>
  filter(near(x, point[1], tol = 20), near(y, point[2], tol = 20))
  
cropped_df

pnt = st_sample(st_as_sfc(st_bbox(cropped), 10))
stars::st_extract(st_coordinates(test_point))


## value of trees

temp <- tempfile()
curl::curl_download("https://www.adeptnet.org.uk/sites/default/files/media/2023-01/The%20Value%20of%20Trees_Appendix2%20Species%20Ecosystem%20Servicesf.xlsx", temp)

value_of_trees <- readxl::read_xlsx(temp)

value_of_trees

## species selection matrix

tmp <- tempfile()
curl::curl_download("https://www.adeptnet.org.uk/sites/default/files/media/2023-01/The%20Value%20of%20Trees_Appendix1_Species%20Selection%20Matrix_May22f.xlsx", tmp)

species_selection <- readxl::read_xlsx(tmp, skip = 1)

species_selection |>
  slice(-150) |>
  select(`Botanical Name`, `Common Name`) |>
  print(n = 150)

value_of_trees |>
  filter(Age == 10) |>
  ggplot() +
  geom_col(aes(`Total pollution removal (g)`, reorder(`Common name`, `Total pollution removal (g)`))) +
  labs(y = "") +
  theme(axis.text.y = element_text(size = 3))

