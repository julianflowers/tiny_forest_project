get_nbn_buffer <- function(lon, lat, radius = 1, n = 10000){
  
  require(dplyr)
  require(jsonlite)
  require(tictoc)
  
  tic()
  
  base_url <- "https://records-ws.nbnatlas.org/occurrences/search?q=*:*&"
  search <- paste0(base_url, "lat=", lat, "&lon=", lon, "&radius=", radius, "&pageSize=", n)
  df <- fromJSON(search, simplifyDataFrame = TRUE)
  
  toc()
  
  df$occurrences |>
    select(kingdom:genus, contains("decimal"), year, month, dataProviderName, speciesGroups, vernacularName, species)
  
  
}