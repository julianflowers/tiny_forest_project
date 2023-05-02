

get_bryophyte_nbn <- function(startIndex=0){

  url <- paste0("https://records-ws.nbnatlas.org/occurrences/search?q=data_provider_uid%3Adp74&pageSize=40000&startIndex=", startIndex)
  tictoc::tic()
  test <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)
  tictoc::toc()
  #Sleep(20)

  out <- list(results = test)


}

t2 <- get_bryophyte_nbn(startIndex = 10001)

bryo <- data.frame(t2$results$occurrences) |>
  dplyr::select(-assertions)

data.table::as.data.table(data.frame((bryo)))

