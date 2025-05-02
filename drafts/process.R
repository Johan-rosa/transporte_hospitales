library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(leaflet)
library(ggplot2)
library(googleway)
library(geojsonio)
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

# saveRDS(municipios_distance_time_hospitales_nuevos_all, "data/municipios_distance_time_all.rds")

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
division_territorial <- map_municipios |>
  st_drop_geometry() |>
  as_tibble() |> 
  select(id, region_label, provincia_label)

division_territorial

time_distance_min <- municipios_distance_time_hospitales |>
  group_by(id, municipio_label) |>
  slice_min(duration_value) |>
  ungroup() |>
  mutate(log_duration_value = log(duration_value)) |>
  left_join(division_territorial, by = "id")

time_distance_min_all <- municipios_distance_time_hospitales_nuevos_all |>
  group_by(id, municipio_label) |>
  slice_min(duration_value) |>
  ungroup() |>
  mutate(log_duration_value = log(duration_value)) |> 
  left_join(division_territorial, by = "id")


data <- municipios_distance_time_hospitales_nuevos[1223, ]

polyline <- pull(data , polyline) |> 
  googleway::decode_pl() 

polyline |>
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) |>
  addMarkers(
    data = data,
    lat = ~origin_lat,
    lng = ~origin_lng,
    popup = ~glue::glue("Trayecto de {distance_text}, {duration_text}")
  ) |> 
  addMarkers(
    data = data,
    lat = ~destination_lat,
    lng = ~destination_lng,
    popup = ~glue::glue("Trayecto de {distance_text}, {duration_text}")
  ) |> 
  addPolylines(
    lng = ~lon,
    lat = ~lat,
    color = "blue",
    weight = 3,
    opacity = 0.8,
    label = "Route"
  )


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

map1 <- highchart(type = "map") %>%
  hc_chart(events = list(load = JS("function() { this.myChartId = 'chart2'; }"))) |> 
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
  ) |>
  hc_plotOptions(series = list(
    point = list(
      events = list(
        mouseOver = JS(
          "function() {
            var chart1 = Highcharts.charts.filter(chart => chart.myChartId === 'chart1')[0];
            if (chart1) {
              chart1.series[0].data[this.index].setState('hover');
              chart1.tooltip.refresh(chart1.series[0].data[this.index]);
            }
          }"
        ),
        mouseOut = JS(
          "function() {
            var chart1 = Highcharts.charts.filter(chart => chart.myChartId === 'chart1')[0];
            if (chart1) {
              chart1.series[0].data[this.index].setState('');
              chart1.tooltip.hide();
            }
          }"
        )
      )
    )
  ))

saveRDS(map1, "outputs/color_map_original.rds")

map2 <- highchart(type = "map") %>%
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
      list(from = 20 * 60, to = 60 * 60, color = "#fcae91", name = "1.0 h"),
      list(from = 60 * 60, to = 90 * 60, color = "#fb6a4a", name = "1.5 h"),
      list(from = 90 * 60, to = 120 * 60, color = "#de2d26", name = "2.0 h"),
      list(from = 120 * 60, to = 180 * 60, color = "#a50f15", name = "3.0 h"),
      list(from = 180 * 60, color = "#67000d", name = "+3 h")
    )
  ) |>
  hc_chart(events = list(load = JS("function() { this.myChartId = 'chart1'; }"))) %>%
  hc_plotOptions(series = list(
    point = list(
      events = list(
        mouseOver = JS(
          "function() {
            var chart2 = Highcharts.charts.filter(chart => chart.myChartId === 'chart2')[0];
            if (chart2) {
              chart2.series[0].data[this.index].setState('hover');
              chart2.tooltip.refresh(chart2.series[0].data[this.index]);
            }
          }"
        ),
        mouseOut = JS(
          "function() {
            var chart2 = Highcharts.charts.filter(chart => chart.myChartId === 'chart2')[0];
            if (chart2) {
              chart2.series[0].data[this.index].setState('');
              chart2.tooltip.hide();
            }
          }"
        )
      )
    )
  ))

saveRDS(map2, "outputs/color_map_hipotetico.rds")

htmltools::browsable(htmltools::tagList(map1, map2))
# Stats -------------------------------------------------------------------

summarise_time <- function(seconds) {
  if (!is.numeric(seconds) || any(seconds < 0)) {
    stop("Input must be a non-negative numeric vector.")
  }
  
  hours <- seconds %/% 3600
  minutes <- (seconds %% 3600) %/% 60
  remaining_seconds <- round(seconds %% 60)
  
  pad <- \(x) stringr::str_pad(x, width = 2, pad = "0")
  glue::glue("{pad(hours)}:{pad(minutes)}:{pad(remaining_seconds)}")
}

# Example usage
summarise_time(c(4767, 3264, 15671, 10752, 284)) 


time_distance_min_all |>
  summarise(
    across(
      .cols = duration_value, 
      .fns = list(mean = mean, max = max, min = min, sd = sd),
      na.rm = TRUE
    ),
    .by = "provincia_label"
  ) |>
  mutate(across(where(is.numeric), summarise_time))

time_distance_min |>
  summarise(
    across(
      .cols = duration_value, 
      .fns = list(mean = mean, max = max, min = min)
    ),
    .by = "provincia_label"
  ) |>
  mutate(across(where(is.numeric), summarise_time)) |>
  View()

  

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
