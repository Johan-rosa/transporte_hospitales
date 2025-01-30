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

minimo_con_hipotetico <- distance_time |>
  group_by(id) |>
  slice_min(duration_value) |>
  ungroup() |> 
  left_join(division_territorial, by = "id")

writexl::write_xlsx(con_hipotetico, "outputs/distance_time_con_hipoteticos.xlsx")

minimo_sin_hipoteticos <- distance_time |>
  filter(hospita_existente) |> 
  group_by(id) |>
  slice_min(duration_value) |>
  ungroup() |> 
  left_join(division_territorial, by = "id")


minimo_tt_sin_hipoteticos <- distance_time |>
  filter(hospita_existente, stringr::str_detect(hospital_type, "área", negate = TRUE)) |> 
  group_by(id) |>
  slice_min(duration_value) |>
  ungroup() |> 
  left_join(division_territorial, by = "id")

minimo_tt_con_hipotetico <- distance_time |>
  filter(stringr::str_detect(hospital_type, "área", negate = TRUE)) |>
  group_by(id) |>
  slice_min(duration_value) |>
  ungroup() |> 
  left_join(division_territorial, by = "id")

writexl::write_xlsx(minimo_sin_hipoteticos, "outputs/distance_time_solo_existentes.xlsx")

minimo_con_hipotetico |> distinct(hospital_type)
minimo_sin_hipoteticos |>
  select(
    id,
    region = region_label, 
    provincia = provincia_label, 
    municipio = municipio_label, 
    tiempo_pre = duration_value,
    distance_pre = distance_value,
    hospital_type_pre = hospital_type
  ) |>
    mutate(
      hospital_type_pre = ifelse(stringr::str_detect(hospital_type_pre, "área"), "AS", "CT")
    ) |>
  left_join(
    minimo_con_hipotetico |> 
    select(
      id,
      tiempo_post = duration_value,
      distance_post = distance_value,
      hospital_type_post = hospital_type
    ) |>
      mutate(
        hospital_type_post = ifelse(stringr::str_detect(hospital_type_post, "área"), "AS", "CT")
      ),
     by = "id"
  ) |>
  left_join(select(minimo_tt_sin_hipoteticos, id, tiempo_tt_pre = duration_value, distance_tt_pre = distance_value)) |> 
  left_join(select(minimo_tt_con_hipotetico, id, tiempo_tt_post = duration_value, distance_tt_post = distance_value,)) |>
  writexl::write_xlsx("outputs/comparacion_pre_y_post_by_municipio.xlsx")







