library(sf)
library(dplyr)

distance_time <- readRDS("data/municipios_distance_time_all.rds")
map_municipios <- readRDS("data/municipios_sf.rds")

division_territorial <- map_municipios |>
  st_drop_geometry() |>
  as_tibble() |> 
  select(id, region_label, provincia_label) 

distance_time |>
  left_join(division_territorial, by = "id") |>
  writexl::write_xlsx("outputs/distance_time.xlsx")
