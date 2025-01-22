library(googleway)
library(dplyr)
library(tidyr)
library(purrr)
library(leaflet)
library(sf)
library(ggplot2)

# Import data ---------------------------------------------------------------------------------

hospitales <- readRDS("data/hospitales.rds")
hospitales <- hospitales |>
  mutate(type = c(rep("Hospital traumatológico", 3), rep("Hospital con área de shok", 12))) |>
  unnest(latlong)

map_municipios <- readRDS("data/municipios_sf.rds")
distance_matrix <- readRDS("data/municipo_hospital_distance_matrix.rds")
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

map_municipios <- map_municipios |>
  left_join(slice_min(distance_matrix, duration_value, by = id))

map_m
