library(googleway)
library(jsonlite)
library(dplyr)
library(tidyr)
library(sf)
library(purrr)
library(leaflet)

# Import data ---------------------------------------------------------------------------------

hospitales <- readRDS("data/hospitales.rds")
hospitales <- hospitales |>
  mutate(type = c(rep("Hospital traumatológico", 3), rep("Hospital con área de shok", 12))) |>
  unnest(latlong)

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
  )

origin_destination_base <- expand_grid(
  id = centroid_municipio$id,
  hospital = hospitales$name
)

origin_destination <- origin_destination_base |>
  mutate(origin = map(id, \(id_municipio) select(filter(centroid_municipio, id == id_municipio), lat, lng))) |>
  mutate(destination = map(hospital, \(hospital) select(filter(hospitales, name == hospital), lat, lng)))

origin_destination <- origin_destination |>
  mutate(
    direccion_distance = map2(
      origin,
      destination,
      possibly(get_distance_time, otherwise = NA),
      .progress = TRUE
    )
  )


saveRDS(origin_destination, "data/origin_destination_time.rds")

origin_destination |>
  unnest(c(origin, destination), names_sep = "_") |>
  unnest(direccion_distance)

centroid_municipio |>
  semi_join(filter(origin_destination, is.na(direccion_distance)) |> distinct(id))

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
  

map_municipios |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(weight = 1, fill = NA, color = "#fd7567") |> 
  addCircleMarkers(
    lng = ~centroid_x,
    lat = ~centroid_y,
    radius = 3,
    stroke = FALSE,
    fillOpacity = 0.8
  )
