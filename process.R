library(googleway)
library(jsonlite)
library(dplyr)
library(sf)
library(leaflet)

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
    lng = centroid_y
  )

origin_destination_base <- expand_grid(
  id = centroid_municipio$id,
  hospital = hospitales$name
)

origin_destination_base |>
  mutate(origin = map(id, \(id_municipio) select(filter(centroid_municipio, id == id_municipio), lng, lat))) |>
  mutate(destination = map(hospital, \(hospital) select(filter(hospitales, name == hospital), lng, lat)))
  


map_municipios |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(lng = ~centroid_x, lat = ~centroid_y)
  
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
  
