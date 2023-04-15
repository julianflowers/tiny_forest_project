remotes::install_github("julianflowers/srUtils", force = TRUE)

library(srUtils)

forest_climate <- "(Urban OR Suburban OR City OR Cities OR Neighbourhood OR Neighborhood OR Borough OR Periurban OR  Street$) AND (Forest$ OR Tree$ OR Deciduous OR Coniferous OR Conifer$ OR Green_Space$ OR Greenspace OR Park)"

forest_carbon <- 'Carbon OR Climate_Change OR Nature_Based_Solution$) AND
(Management OR Mitigation OR Planting$ OR Best practice$ OR Conservation OR Recommendation$ OR  Strateg* OR Solution$ OR  application$ OR Planning  OR Regeneration OR Com- munity OR USA OR Canada)'

q <- "Nature"



search_woslite(api_key = "e4879094187828aa948bf61348587b8059f6814a", query = "(biodiversity OR species diversity OR species richness) AND (urban forest OR tiny forest OR community garden* OR allotment* OR green space)")

library(myScrapers)

ncbi_key = "bd86b3e3500c581cbc6ee0896f551bae0408"
search <- "(urban forest OR nature based solution*[tw] OR tiny forest[tw] OR alloment* OR community garden*) AND (biodiversity OR species richness OR species diversity) review[pt]"
start = 2000
end = 2023

res <- pubmedAbstractR(search = search, start = start, end = end, ncbi_key = ncbi_key, n = 164)

res$abstracts[108,]$title

res$abstracts[9,2] |>
  gt::gt()

res$abstracts[12,2] |>
  gt::gt()

res$abstracts[26,2] |>
  gt::gt()


### semantic scholar

library(semanticscholar)

Sys.setenv(SEMANTICSCHOLAR_API = "VTSXBRoNTI8FuXeLygko36FiJbktDUef5Ddq0xYT")
semanticscholar::S2_api()

ss <- semanticscholar::S2_search_papers("urban forest nature based solution", limit = 100)

ss_ids <- pluck(ss$data, "paperId")

details <- map(ss_ids, semanticscholar::S2_paper)

titles <- map(details, "title")
abstracts <- map(details, "abstract")
year <- map(details, "year")
paperId <- map(details, "paperId")
journal <- map(details, "venue")
doi <- map(details, "doi")
cited_by <- map(details, "numCitedBy")
citing <- map(details, "numCiting")
refs <- map(details, "references")

df <- data_frame(paperId, year, journal, titles, abstracts,
                 doi, refs, cited_by, citing) |>
  unnest("paperId") |>
  unnest("year") |>
  unnest("journal") |>
  unnest("paperId") |>
  unnest("titles") |>
  unnest("abstracts") |>
  unnest("doi") |>
  unnest("cited_by") |>
  unnest("citing") |>
  mutate(refs_ids = map(refs, "paperId")) |>
  unnest("refs_ids") |>
  mutate(det = map(refs_ids, S2_paper))

df |>
  View()







