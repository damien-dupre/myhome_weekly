# libraries --------------------------------------------------------------------
library(dplyr)
library(glue)
library(httr2)
library(jsonlite)
library(readr)
library(tibble)
library(tidyr)

# custom functions -------------------------------------------------------------
n_adds <- function(json){
  requ <- request("https://api.myhome.ie/search") |> 
    req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36") |> 
    req_body_json(fromJSON(json)) |> 
    req_perform()
  
  resp <- resp_body_json(requ, encoding = "windows-1252")$ResultCount
}

scraper <- function(json){
  requ <- request("https://api.myhome.ie/search") |> 
    req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36") |> 
    req_body_json(fromJSON(json)) |> 
    req_perform()
  
  resp <- resp_body_json(requ, encoding = "windows-1252")$SearchResults |> 
    unlist() |> 
    enframe()
}

# data scraping ----------------------------------------------------------------
df <- c(1080, 887, 618, 737, 1464, 1463, 2013, 2448, 2362, 2414, 2670, 2648, 2594, 2515, 2535, 2781, 2712, 2771, 2888, 3091, 3254, 3587, 3944, 3969, 4054, 4011) |>
  as_tibble_col(column_name = "region_id") |>
  mutate(json_page = glue(
    '{{
      "ApiKey": "5f4bc74f-8d9a-41cb-ab85-a1b7cfc86622",
      "CorrelationId": "e4e14c46-53e6-463f-9bdc-f67785bd4915",
      "Page": 1,
      "SearchRequest": {{"RegionId": {region_id}, "ChannelIds": [1], "PropertyClassIds": [1]}}
    }}'
  )) |>
  rowwise() |>
  mutate(n_pages = ceiling(n_adds(json_page)/20))|>
  filter(n_pages > 0) |>
  group_by(region_id) |>
  summarise(json_pages = glue(
    '{{
      "ApiKey": "5f4bc74f-8d9a-41cb-ab85-a1b7cfc86622",
      "CorrelationId": "e4e14c46-53e6-463f-9bdc-f67785bd4915",
      "Page": {1:n_pages},
      "SearchRequest": {{"RegionId": {region_id}, "ChannelIds": [1], "PropertyClassIds": [1]}}
    }}'
  )) |>
  mutate(n_pages = row_number()) |>
  group_by(region_id, n_pages) |>
  summarise(json_data = scraper(json_pages)) |>
  ungroup() |>
  unnest(cols = c(json_data))

write_csv(df, glue("data/myhome_{Sys.Date()}.csv"))
