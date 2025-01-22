library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(googleway)

# Functions -----------------------------------------------------------------------------------

get_distance_time <- function(
    origen,
    destino,
    key = Sys.getenv("API_KEY"),
    time = NULL,
    sleep = 2,
    mode = "driving",
    ...
) {
  query_result <- googleway::google_distance(
    origins = origen,
    destinations = destino,
    key = key,
    mode = mode,
    departure_time = time,
    ...
  )
  
  result <- query_result$rows$elements |> as.data.frame()
  
  rsult_data <- tibble::tibble(
    distance_value = result$distance$value,
    distance_text = result$distance$text,
    duration_value = result$duration$value,
    duration_text = result$duration$text
  )
  
  polyline <- googleway::google_directions(origen, destino, key = key) |> 
    googleway::direction_polyline()
  
  output <- tibble::tibble(
    origin_address = query_result$origin_addresses,
    destination_address = query_result$destination_addresses,
    polyline = polyline
  ) |> 
    dplyr::bind_cols(rsult_data)
  
  Sys.sleep(sleep)
  print(jsonlite::toJSON(output))
  output
}

# Import data ---------------------------------------------------------------------------------

hospitales <- readRDS("data/hospitales.rds")
hospitales_nuevos <- readRDS("data/hospitales_nuevos.rds")

map_municipios <- readRDS("data/municipios_sf.rds")

centroid_municipio <- map_municipios |>
  st_drop_geometry() |>
  select(
    id,
    region_code,
    region_label,
    provincia_code,
    provincia_label,
    municipio_code,
    municipio_label,
    lat = centroid_y,
    lng = centroid_x
  ) |>
  as_tibble()

adjusted_centroids <- readRDS("data/municipio_adjusted_cengroid.rds")

centroid_municipio <- centroid_municipio |>
  anti_join(adjusted_centroids, by = c("id")) |>
  bind_rows(adjusted_centroids)

origin_destination_base <- expand_grid(
  id = centroid_municipio$id,
  hospital = hospitales$name
)

origin_destination <- origin_destination_base |>
  mutate(origin = map(id, \(id_municipio) select(filter(centroid_municipio, id == id_municipio), lat, lng))) |>
  mutate(destination = map(hospital, \(hospital) select(filter(hospitales, name == hospital), lat, lng)))

# Do not run this code every time, it takes time and tokens
# origin_destination <- origin_destination |>
#   mutate(
#     direccion_distance = map2(
#       origin,
#       destination,
#       possibly(get_distance_time, otherwise = NA),
#       .progress = TRUE
#     )
#   )
#
# saveRDS(origin_destination, "data/origin_destination_time.rds")

origin_destination <- readRDS("data/origin_destination_time.rds")

# Hospitales nuevos ---------------------------------------------------------------------------

origin_destination_hospitales_nuevos_base <- expand_grid(
  id = centroid_municipio$id,
  hospital = hospitales_nuevos$name
)

origin_destination_hospitales_nuevos <- origin_destination_hospitales_nuevos_base |>
  mutate(origin = map(id, \(id_municipio) select(filter(centroid_municipio, id == id_municipio), lat, lng))) |>
  mutate(destination = map(hospital, \(hospital) select(filter(hospitales_nuevos, name == hospital), lat, lng)))

# # Do not run this code every time, it takes time and tokens
# origin_destination_hospitales_nuevos <- origin_destination_hospitales_nuevos |>
#   mutate(
#     direccion_distance = map2(
#       origin,
#       destination,
#       possibly(get_distance_time, otherwise = NA),
#       .progress = TRUE
#     )
#   )
# 
# saveRDS(origin_destination_hospitales_nuevos, "data/origin_destination_time_hospitales_nuevos.rds")

