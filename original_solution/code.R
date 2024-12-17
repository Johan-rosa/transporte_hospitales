
# Pacakges ----------------------------------------------------------------
library(googleway)
library(tidyverse)
library(readxl)

# Keys --------------------------------------------------------------------
key <- Sys.getenv("API_KE")

# Testing googleway -------------------------------------------------------

raw_origen <- readxl::read_excel("original_solution/places.xlsx", "origen")
raw_destino <- readxl::read_excel("original_solution/places.xlsx", "destino")

origen <- tibble(
    nombre = rep(raw_destino$nombre, each = nrow(raw_origen)),
    origen = rep(raw_origen$provincia, times = nrow(raw_destino))
  )  %>%
  left_join(raw_origen %>% select(origen = provincia, lat, lon))

destino <- tibble(
  nombre = rep(raw_destino$nombre, each = nrow(raw_origen)),
  origen = rep(raw_origen$provincia, times = nrow(raw_destino))
)  %>%
  left_join(raw_destino %>% select(nombre, lat, lon))


get_distance_time <- function(origen, destino, time = NULL, sleep = 3) {
  query_result <- google_distance(
    origins = origen,
    destinations = destino,
    mode = "driving",
    departure_time = time,
    key = key
  )
  
  result <- query_result$rows$elements |> as.data.frame()
    
  rsult_data <- tibble::tibble(
    distance_value = result$distance$value,
    distance_text = result$distance$text,
    duration_value = result$duration$value,
    duration_text = result$duration$text
  )
  
  output <- tibble::tibble(
    origin_address = query_result$origin_addresses,
    destination_address = query_result$destination_addresses
  ) |> bind_cols(rsult_data)
  
  Sys.sleep(sleep)
  output
  
}

query_result <- map(
  seq_along(origen$nombre),
  ~get_distance_time(origen[.x, c("lat", "lon")], destino[.x, c("lat", "lon")], sleep = 2)
)

x <- bind_rows(query_result) %>%
  bind_cols(origen[, c("nombre", "origen")], .) 

clipr::write_clip(x)

# Test --------------------------------------------------------------------

get_distance_time(select(origen, lat, lon)[1, ], select(destino, lat, lon)[1, ])
