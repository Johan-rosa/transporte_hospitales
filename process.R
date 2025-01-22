library(googleway)
library(dplyr)
library(tidyr)
library(purrr)
library(leaflet)
library(sf)
library(ggplot2)
library(highcharter)

# Import data ---------------------------------------------------------------------------------

hospitales_existentes <- readRDS("data/hospitales.rds")
hospitales_nuevos <- readRDS("data/hospitales_nuevos.rds")

hospitales <- bind_rows(hospitales_existentes, hospitales_nuevos)

map_municipios <- readRDS("data/municipios_sf.rds")
map_municipios_json <- geojsonio::geojson_list(map_municipios)

municipio_labels <- map_municipios |>
  sf::st_drop_geometry() |>
  select(id, municipio_label) |> 
  as_tibble()

municipios_distance_time_hospitales <- readRDS("data/origin_destination_time.rds")
municipios_distance_time_hospitales <- left_join(municipio_labels, municipios_distance_time_hospitales) |>
  left_join(
    select(hospitales, hospital_id = id, name, hospita_existente = existe, hospital_type = type),
    by = c("hospital" = "name")
  ) |>
  relocate(hospital_id, .before = hospital)

municipios_distance_time_hospitales_nuevos <- readRDS("data/origin_destination_time_hospitales_nuevos.rds")
municipios_distance_time_hospitales_nuevos <- left_join(municipio_labels, municipios_distance_time_hospitales_nuevos) |>
  left_join(
    select(hospitales, hospital_id = id, name, hospita_existente = existe, hospital_type = type),
    by = c("hospital" = "name")
  ) |>
  relocate(hospital_id, .before = hospital)

municipios_distance_time_hospitales <- municipios_distance_time_hospitales |>
  unnest(direccion_distance) |>
  unnest(origin, names_sep = "_") |>
  unnest(destination, names_sep = "_")

municipios_distance_time_hospitales_nuevos <- municipios_distance_time_hospitales_nuevos |>
  unnest(direccion_distance) |>
  unnest(origin, names_sep = "_") |>
  unnest(destination, names_sep = "_")

municipios_distance_time_hospitales_nuevos_all <- municipios_distance_time_hospitales |>
  bind_rows(municipios_distance_time_hospitales_nuevos)

# Mapa de hospitales --------------------------------------------------------------------------

custom_icons <- icons(
  iconUrl = ifelse(
    hospitales$type == "Hospital traumatológico", 
    "https://maps.google.com/mapfiles/ms/icons/red-dot.png", 
    "https://maps.google.com/mapfiles/ms/icons/blue-dot.png"
  ),
  iconWidth = 38, iconHeight = 38,
  iconAnchorX = 22, iconAnchorY = 37
)

hospitales |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addMarkers(icon = custom_icons) |>
  addLegend(
    position = "bottomright",
    title = "Type",
    colors = c("#fd7567", "#6991fd"),
    labels = c("Definitive Care Hospitals (3)", "Trauma Resuscitation Unit (12)"),
    opacity = 1
  )


# Visualización tiempos -----------------------------------------------------------------------

time_distance_min <- municipios_distance_time_hospitales |>
  group_by(id, municipio_label) |>
  slice_min(duration_value) |>
  ungroup() |>
  mutate(log_duration_value = log(duration_value))

time_distance_min_all <- municipios_distance_time_hospitales_nuevos_all |>
  group_by(id, municipio_label) |>
  slice_min(duration_value) |>
  ungroup() |>
  mutate(log_duration_value = log(duration_value))


time_distance_min |>
  summarise(
    when = "before",
    across(
      .cols = duration_value, 
      .fns = list(mean = mean, max = max, min = min)
    )
  ) |>
  bind_rows(
    time_distance_min_all |>
      summarise(
        when = "after",
        across(
          .cols = duration_value, 
          .fns = list(mean = mean, max = max, min = min)
        )
      )
  ) |>
  mutate(across(where(is.numeric), \(x) {x / 60 / 60}))


highchart(type = "map") %>%
  hc_add_series(
    mapData = map_municipios_json,
    data = select(
      time_distance_min, id, 
      value = duration_value, 
      duration_text, 
      municipio_label,
      distance_text
      ) |>
      filter(!is.na(value)),
    type = 'map',
    joinBy = c('id', 'id'),
    value = "value",
    name = 'Tiempo y distancia: ',
    dataLabels = list(enabled = FALSE),
    tooltip = list(
      useHTML = TRUE,
      pointFormat = "<b>{point.municipio_label}</b> <br> {point.duration_text} ({point.distance_text})"
    )
  ) %>%
  hc_colorAxis(
    dataClassColor = "numberSince",
    labels = list(
      formatter = JS(
        "function() {
          return (this.value / 60 / 60).toFixed(1) + ' h'; // Convert minutes to hours
        }"
      )
    )
  )

highchart(type = "map") %>%
  hc_add_series(
    mapData = map_municipios_json,
    data = select(
      time_distance_min, 
      id, 
      value = duration_value, 
      duration_text, 
      municipio_label,
      distance_text
    ) |>
      filter(!is.na(value)),
    type = 'map',
    joinBy = c('id', 'id'),
    value = "value",
    name = 'Tiempo y distancia: ',
    dataLabels = list(enabled = FALSE),
    tooltip = list(
      useHTML = TRUE,
      pointFormat = "<b>{point.municipio_label}</b> <br> {point.duration_text} ({point.distance_text})"
    )
  ) %>%
  hc_colorAxis(
    dataClassColor = "category",
    dataClasses = list(
      list(to = 20 * 60, color = "#fee5d9", name = "20 min"),
      list(from = 20 * 60, to = 60 * 60, color = "#fcae91", name = "1 h"),
      list(from = 60 * 60, to = 90 * 60, color = "#fb6a4a", name = "1.5 h"),
      list(from = 90 * 60, to = 120 * 60, color = "#de2d26", name = "2 h"),
      list(from = 120 * 60, to = 180 * 60, color = "#a50f15", name = "3 h"),
      list(from = 180 * 60, color = "#67000d", name = "+3 h")
    )
  )

highchart(type = "map") %>%
  hc_add_series(
    mapData = map_municipios_json,
    data = select(
      time_distance_min_all, 
      id, 
      value = duration_value, 
      duration_text, 
      municipio_label,
      distance_text
    ) |>
      filter(!is.na(value)),
    type = 'map',
    joinBy = c('id', 'id'),
    value = "value",
    name = 'Tiempo y distancia: ',
    dataLabels = list(enabled = FALSE),
    tooltip = list(
      useHTML = TRUE,
      pointFormat = "<b>{point.municipio_label}</b> <br> {point.duration_text} ({point.distance_text})"
    )
  ) %>%
  hc_colorAxis(
    dataClassColor = "category",
    dataClasses = list(
      list(to = 20 * 60, color = "#fee5d9", name = "20 min"),
      list(from = 20 * 60, to = 60 * 60, color = "#fcae91", name = "1 h"),
      list(from = 60 * 60, to = 90 * 60, color = "#fb6a4a", name = "1.5 h"),
      list(from = 90 * 60, to = 120 * 60, color = "#de2d26", name = "2 h"),
      list(from = 120 * 60, to = 180 * 60, color = "#a50f15", name = "3 h"),
      list(from = 180 * 60, color = "#67000d", name = "+3 h")
    )
  )



# map_municipios |>
#   leaflet() |>
#   addProviderTiles(providers$CartoDB.Positron) |>
#   addPolygons(weight = 1, fill = NA, color = "#fd7567") |> 
#   addCircleMarkers(
#     lng = ~centroid_x,
#     lat = ~centroid_y,
#     radius = 3,
#     stroke = FALSE,
#     fillOpacity = 0.8
#   )
