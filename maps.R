
# Packages ----------------------------------------------------------------
library(leaflet)
library(dplyr)
library(purrr)

# Import data -------------------------------------------------------------
hospitales_existentes <- readRDS("data/hospitales.rds")
hospitales_nuevos <- readRDS("data/hospitales_nuevos.rds")

hospitales <- bind_rows(hospitales_existentes, hospitales_nuevos) |>
  mutate(
    type = ifelse(
      type == "Hospital traumatológico",
      "Hospital traumatológico",
      "Hospital con área de shock"
    ),
    emoji = if_else(type == "Hospital traumatológico", "🏥",  "🚑")
  )

map_municipios <- readRDS("data/municipios_sf.rds")
map_municipios_json <- geojsonio::geojson_list(map_municipios)

municipios_distance_time <- readRDS("data/municipios_distance_time_all.rds") |>
  mutate(
    hospital_type = ifelse(
      hospital_type == "Hospital traumatológico",
      "Hospital traumatológico",
      "Hospital con área de shock"
    )
  )

min_distance_time_existing <- municipios_distance_time |>
  filter(hospita_existente) |>
  slice_min(duration_value, by = id)

min_distance_time_all <- municipios_distance_time |>
  slice_min(duration_value, by = id) 

min_distance_time_trauma_existing <- municipios_distance_time |>
  filter(hospita_existente, hospital_type == "Hospital traumatológico") |> 
  slice_min(duration_value, by = id)

min_distance_time_trauma_all <- municipios_distance_time |>
  filter(hospital_type == "Hospital traumatológico") |> 
  slice_min(duration_value, by = id)


# functions -----------------------------------------------------------------------------------

plot_polyline <- function(map, data) {
  polylines_df <- data |>
    dplyr::pull(polyline) |>
    purrr::map(googleway::decode_pl)
  
  for (index in seq_along(polylines_df)) {
    current_polyline <- polylines_df[[index]]
    
    map <- map |>
      leaflet::addPolylines(
        data = current_polyline,
        lng = ~lon,
        lat = ~lat,
        weight = 2,
        opacity = 0.8,
        color = "gray"
      )
  }
  
  map
}

add_hospital_legend <- function(plot, data, position = "bottomleft") {
  legend_html <- "<div style='padding:10px'><b>Tipo de hospital</b><br>"
  hospital_types <- distinct(data, type, emoji)
  
  for (i in seq_len(nrow(hospital_types))) {
    legend_html <- paste0(
      legend_html,
      hospital_types$emoji[i], " - ", hospital_types$type[i], "<br>"
    )
  }
  
  emoji_legend_html <- paste0(legend_html, "</div>")
  
  # Add custom legend
  plot |>
    addControl(
      html = emoji_legend_html,
      position = position
    )
}


plot_time_distance <- function(
    map_data,
    hospitales,
    color_pal
) {
  map_data |>
    leaflet() |>
    addProviderTiles(providers$CartoDB.Positron) |>
    addPolygons(
      stroke = TRUE,
      layerId = ~id,
      weight = 1,
      fillOpacity = 0.6,
      fillColor = ~color_pal(duration_value),
      color = "white"
    ) |>
    plot_polyline(map_data) |>
    addCircleMarkers(
      lat = ~origin_lat,
      lng = ~origin_lng,
      radius = 4,
      weight = 2,
      fillColor = "#225EA8",
      fillOpacity = 1,
      stroke = FALSE,
      label = ~municipio_label
    ) |>
    add_hospital_legend(hospitales) |>
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
    addCircleMarkers(
      data = hospitales,
      ~lng,
      ~lat,
      label = ~name,
      radius = 15,
      fillOpacity = 0, 
      stroke = FALSE,
    ) |>
    addLegend(
      position = "bottomright",
      pal = color_pal,
      title = "Duración (min)",
      values = map_data$duration_value,
      labFormat = labelFormat(
        transform = function(x) {
          mins <- x / 60
          (mins %/% 10) * 10
        }
      )
    )
}


# Maps ----------------------------------------------------------------------------------------

pal_time_all <- colorNumeric(
  palette = colorRampPalette(c("#1a9850", "#fee08b", "#f46d43", "#d73027", "#a50026"))(100),
  domain = c(0, min_distance_time_existing$duration_value)
)

pal_time_trauma <- colorNumeric(
  palette = colorRampPalette(c("#1a9850", "#fee08b", "#f46d43", "#d73027", "#a50026"))(100),
  domain = c(0, min_distance_time_trauma_existing$duration_value)
)


## Tiempo de viajes con hospitales existentes
map_municipios |> 
  left_join(min_distance_time_existing) |>
  plot_time_distance(hospitales = hospitales, color_pal = pal_time_all)

## Tiempo de viaje con hospitales hipoteticos
map_municipios |> 
  left_join(min_distance_time_all) |>
  plot_time_distance(hospitales = hospitales, color_pal = pal_time_all)

## Tiempo de viajes hacia hospital traumatológico existente
map_municipios |> 
  left_join(min_distance_time_trauma_existing) |>
  plot_time_distance(
    hospitales = filter(hospitales, type == "Hospital traumatológico"),
    color_pal = pal_time_trauma
  )

## Tiempo de viajes hacia hospital traumatológico con hipoteticos
map_municipios |> 
  left_join(min_distance_time_trauma_all) |>
  plot_time_distance(
    hospitales = filter(hospitales, type == "Hospital traumatológico"),
    color_pal = pal_time_trauma
  )





# Data wrangling ------------------------------------------------------------------------------

hospitales <- hospitales


# Mapa de hospitales ------------------------------------------------------


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

ll_municipios

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
  domain = municipios_distance_time$duration_value
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
  ) |>
  addLegend(
    position = "topright",
    colors = pal()
  )

