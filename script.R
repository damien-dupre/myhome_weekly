# libraries --------------------------------------------------------------------
library(dplyr)
library(glue)
library(httr2)
library(jsonlite)
library(readr)
library(tibble)
library(tidyr)
library(purrr)

# options ----------------------------------------------------------------------
options(encoding = "windows-1252")

# constants --------------------------------------------------------------------
API_BASE_URL <- "https://api.myhome.ie/search"
API_KEY <- "5f4bc74f-8d9a-41cb-ab85-a1b7cfc86622"
CORRELATION_ID <- "e4e14c46-53e6-463f-9bdc-f67785bd4915"
USER_AGENT <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36"
RESULTS_PER_PAGE <- 20

# helper functions -------------------------------------------------------------
create_base_request <- function() {
  request(API_BASE_URL) |>
    req_user_agent(USER_AGENT)
}

create_json_payload <- function(region_id, page = 1) {
  list(
    ApiKey = API_KEY,
    CorrelationId = CORRELATION_ID,
    Page = page,
    SearchRequest = list(
      RegionId = region_id,
      ChannelIds = list(1),
      PropertyClassIds = list(1)
    )
  )
}

# optimized API functions ------------------------------------------------------
get_total_results <- function(region_id) {
  tryCatch({
    payload <- create_json_payload(region_id)
    
    response <- create_base_request() |>
      req_body_json(payload) |>
      req_perform()
    
    resp_body_json(response, encoding = "windows-1252")$ResultCount %||% 0
  }, error = function(e) e)
}

scrape_page <- function(region_id, page) {
  tryCatch({
    payload <- create_json_payload(region_id, page)
    
    response <- create_base_request() |>
      req_body_json(payload) |>
      req_perform()
    
    search_results <- resp_body_json(response, encoding = "windows-1252")$SearchResults
    
    if (is.null(search_results) || length(search_results) == 0) {
      return(tibble(name = character(), value = character()))
    }
    
    search_results |>
      unlist() |>
      enframe()
  }, error = function(e) {tibble(name = character(), value = character())})
}

# main scraping function ------------------------------------------------------
scrape_region_data <- function(region_ids) {
  region_info <- tibble(region_id = region_ids) |>
    mutate(
      total_results = map_dbl(region_id, get_total_results),
      n_pages = ceiling(total_results / RESULTS_PER_PAGE)
    ) |>
    filter(n_pages > 0)
  
  page_combinations <- region_info |>
    select(region_id, n_pages) |>
    rowwise() |>
    summarise(
      region_id = region_id,
      page = list(1:n_pages),
      .groups = "drop"
    ) |>
    unnest(page)
  
  results <- page_combinations |>
    mutate(data = map2(region_id, page, ~scrape_page(.x, .y))) |>
    select(-page) |>
    unnest(data)
  
  return(results)
}

# main execution ---------------------------------------------------------------
region_ids <- c(1080, 887, 618, 737, 1464, 1463, 2013, 2448, 2362, 2414, 
                2670, 2648, 2594, 2515, 2535, 2781, 2712, 2771, 2888, 
                3091, 3254, 3587, 3944, 3969, 4054, 4011)

# Execute scraping
df <- scrape_region_data(region_ids)

# Save results
write_csv(df, glue("data/myhome_{Sys.Date()}.csv"))
