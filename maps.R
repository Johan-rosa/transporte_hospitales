
# Packages ----------------------------------------------------------------


# Import data -------------------------------------------------------------
hospitales_existentes <- readRDS("data/hospitales.rds")
hospitales_nuevos <- readRDS("data/hospitales_nuevos.rds")

hospitales <- bind_rows(hospitales_existentes, hospitales_nuevos)

map_municipios <- readRDS("data/municipios_sf.rds")
map_municipios_json <- geojsonio::geojson_list(map_municipios)

municipios_distance_time <- readRDS("data/municipios_distance_time_all.rds")

# Mapa de hospitales ------------------------------------------------------

hospitales <- hospitales |>
  mutate(
    emoji = if_else(type == "Hospital con área de shock", "🚑", "🏥")
  )

hospitales |> 
  leaflet()  |>
  addProviderTiles(providers$CartoDB.Positron) |> 
  addLabelOnlyMarkers(
    ~lng,
    ~lat,
    label = ~emoji,
    labelOptions = labelOptions(
      noHide = TRUE,
      textOnly = TRUE,
      style = list(
        "font-size" = "24px",
        "text-align" = "center"
      )
    )
  )

# Centroide municipios ----------------------------------------------------

municipios_distance_time |>
  slice(1, .by = id) |> 
  leaflet() |> 
  addProviderTiles(providers$CartoDB.Positron) |> 
  addCircleMarkers(
    lat = ~origin_lat,
    lng = ~origin_lng,
    radius = 3,
    opacity = 1,
    weight = 2,
    fillOpacity =  0.6,
    label = ~municipio_label
  )
