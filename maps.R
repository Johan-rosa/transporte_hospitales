
# Packages ----------------------------------------------------------------
library(leaflet)
library(dplyr)
library(purrr)

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
    type = ifelse(type == "Hospital traumatológico", "Hospital traumatológico", "Hospital con área de shock"),
    emoji = if_else(type == "Hospital traumatológico", "🏥",  "🚑")
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
  ) |>
  addCircleMarkers(
    ~lng,
    ~lat,
    label = ~name,
    radius = 15,
    fillOpacity = 0, 
    stroke = FALSE,
  )

# Centroide municipios ----------------------------------------------------

ll_municipios <- municipios_distance_time |>
  slice(1, .by = id) |> 
  leaflet() |> 
  addProviderTiles(providers$CartoDB.Positron) |> 
  addCircleMarkers(
    lat = ~origin_lat,
    lng = ~origin_lng,
    radius = 4,
    weight = 2,
    fillColor = "#225EA8",
    fillOpacity = 1,
    stroke = FALSE,
    label = ~municipio_label
  )

# Polylines -----------------------------------------------------------------------------------

polylines_str <- municipios_distance_time |>
  filter(hospita_existente) |>
  slice_min(order_by = duration_value, by = id) |>
  select(municipio_label, polyline, duration_value)

polyline_df <- polylines_str$polyline |>
  setNames(polylines_str$municipio_label) |>
  map(googleway::decode_pl)

pal <- colorNumeric(
  palette = "YlOrRd", 
  domain = polylines_str$duration_value,
  reverse = FALSE
)

ll_plyline <- ll_municipios

for (index in seq_along(polylines_str$polyline)) {
  current_polyline <- polyline_df[[index]]

  ll_plyline <- ll_plyline |>
    addPolylines(
      data = current_polyline,
      lng = ~lon,
      lat = ~lat,
      weight = 3,
      opacity = 0.8,
      color = pal(polylines_str$duration_value[index])
    )
}

duration_legend <- htmltools::tags$div(
  style = "
        position: relative;
        background: white;
        padding: 6px 10px;
        border-radius: 5px;
        box-shadow: 0 0 15px rgba(0,0,0,0.2);
        font-size: 12px;
        text-align: center;
        line-height: 1.2;
        width: 300px;
      ",
  htmltools::tags$div("Tiempodel viaje", style = "text-align: left;"),
  htmltools::tags$div(
    style = "
          background: linear-gradient(to right, #ffffb2, #fecc5c, #fd8d3c, #f03b20, #bd0026);
          height: 12px;
          margin-top: 4px;
          margin-bottom: 4px;
          border-radius: 4px;
        "
  ),
  htmltools::tags$div(
    style = "display: flex; justify-content: space-between;",
    htmltools::tags$span("15m"),
    htmltools::tags$span("1h"),
    htmltools::tags$span("3h"),
    htmltools::tags$span("5h")
  )
)

ll_plyline |>
  addControl(
    html = duration_legend,
    position = "bottomright"
  )



# Legend hospital type ----------------------------------------------------

# Create hospital legend HTML
legend_html <- "<div style='padding:10px'><b>Tipo de hospital</b><br>"
hospital_types <- distinct(hospitales, type, emoji)

for (i in seq_len(nrow(hospital_types))) {
  legend_html <- paste0(
    legend_html,
    hospital_types$emoji[i], " - ", hospital_types$type[i], "<br>"
  )
}

emoji_legend_html <- paste0(legend_html, "</div>")

# Add custom legend
ll_colorplet_polyline |>
  addControl(
    html = emoji_legend_html,
    position = "bottomleft"
  )

# Colorplet routes map ------------------------------------------------------------------------

pal_green_red <- colorNumeric(
  palette = rev(RColorBrewer::brewer.pal(5, "RdYlGn")),
  domain = polylines_str$duration_value
)

pal_green_red <- colorNumeric(
  palette = colorRampPalette(c("#1a9850", "#fee08b", "#f46d43", "#d73027", "#a50026"))(100),
  domain = polylines_str$duration_value
)

duration_legend <- htmltools::tags$div(
  style = "
        position: relative;
        background: white;
        padding: 6px 10px;
        border-radius: 5px;
        box-shadow: 0 0 15px rgba(0,0,0,0.2);
        font-size: 12px;
        text-align: center;
        line-height: 1.2;
        width: 300px;
        margin-bottom: 30px;
      ",
  htmltools::tags$div("Tiempodel viaje", style = "text-align: left;"),
  htmltools::tags$div(
    style = "
          background: linear-gradient(to right, #1a9850, #fee08b, #f46d43, #d73027, #a50026);
          height: 12px;
          margin-top: 4px;
          margin-bottom: 4px;
          border-radius: 4px;
        "
  ),
  htmltools::tags$div(
    style = "display: flex; justify-content: space-between;",
    htmltools::tags$span("15m"),
    htmltools::tags$span("1h"),
    htmltools::tags$span("3h"),
    htmltools::tags$span("5h")
  )
)

ll_colorplet <- map_municipios |>
  left_join(polylines_str) |> 
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    stroke = TRUE,
    layerId = ~id,
    weight = 1,
    fillOpacity = 0.6,
    fillColor = ~pal_green_red(duration_value),
    color = "white"
  ) |>
  addCircleMarkers(
    lng = ~centroid_x,
    lat = ~centroid_y,
    stroke = FALSE,
    fillOpacity = 1,
    radius = 3
  )

ll_colorplet_polyline <- ll_colorplet

for (index in seq_along(polylines_str$polyline)) {
  current_polyline <- polyline_df[[index]]
  
  ll_colorplet_polyline <- ll_colorplet_polyline |>
    addPolylines(
      data = current_polyline,
      lng = ~lon,
      lat = ~lat,
      weight = 2,
      opacity = 0.8,
      color = "gray"
    )
}

ll_colorplet_polyline |>
  addLabelOnlyMarkers(
    data = hospitales,
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
  ) |> 
  addControl(
    html = emoji_legend_html,
    position = "bottomright"
  ) |> 
  addControl(
    position = "bottomleft",
    html = duration_legend
  )

