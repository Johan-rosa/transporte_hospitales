
# Packages ----------------------------------------------------------------
library(leaflet)
library(dplyr)
library(purrr)
library(mapview)

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

add_polyline <- function(map, data) {
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

add_duration_polygon <- function(map, duration_data, color_pal, show_legend = FALSE) {
  map <- map |>
    addPolygons(
      data = duration_data,
      stroke = TRUE,
      layerId = ~id,
      weight = 1,
      fillOpacity = 0.6,
      fillColor = ~color_pal(duration_value),
      color = "white"
    )
  
  if (show_legend) {
    map <- map |>
      addLegend(
        position = "bottomright",
        pal = color_pal,
        title = "Duración (min)",
        values = duration_data$duration_value,
        labFormat = labelFormat(
          transform = function(x) {
            mins <- x / 60
            (mins %/% 10) * 10
          }
        )
      )
  }
  
  map
}

add_hospital_layer <- function(
    map, 
    hospitales, 
    show_legend = FALSE, 
    position = "bottomleft",
    size = "24px"
) {
  map <- map |> 
    addLabelOnlyMarkers(
      data = hospitales,
      ~lng,
      ~lat,
      label = ~emoji,
      labelOptions = labelOptions(
        noHide = TRUE,
        textOnly = TRUE,
        style = list(
          "font-size" = size,
          "text-align" = "center"
        )
      )
    )
  
  if (show_legend) map <- add_hospital_legend(map, hospitales)
  
  map
}


plot_map_distance <- function(data, color_pal, hospitales = NULL, show_polylines = FALSE) {
  map <- leaflet() |> 
    add_duration_polygon(
      data, 
      color_pal,
      show_legend = TRUE
    )
  
  if (!is.null(hospitales)) {
    map <- map |> 
      add_hospital_layer(hospitales, show_legend = TRUE, size = "15px")
  }
  
  if (show_polylines) {
    map <- map |> 
      add_polyline(data)
  }
  
  map
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


save_map <- function(map, file) {
  mapshot(map, file = file.path("outputs/mapas/new-plots", file))
}


# Solo existentes y de todo tipo ------------------------------------------

trayecto_existentes <- map_municipios |> 
  left_join(min_distance_time_existing) |> 
  plot_map_distance(
    pal_time_all,
    hospitales = filter(hospitales, existe),
    show_polylines = TRUE
  )

save_map(trayecto_existentes, "trayecto-solo_existentes-todo_tipo.png")

poligonos_existentes <- map_municipios |> 
  left_join(min_distance_time_existing) |> 
  plot_map_distance(pal_time_all)

save_map(poligonos_existentes, "poligonos-solo_existentes-todo_tipo.png")

leyenda_existentes <- map_municipios |> 
  left_join(min_distance_time_existing) |> 
  plot_map_distance(
    pal_time_all,
    hospitales = filter(hospitales, existe)
  )

save_map(leyenda_existentes, "leyenda-solo_existentes-todo_tipo.png")

# Con hipoteticos y de todo tipo ------------------------------------------

trayecto_todos <- map_municipios |> 
  left_join(min_distance_time_all) |> 
  plot_map_distance(
    pal_time_all,
    hospitales = hospitales,
    show_polylines = TRUE
  )

save_map(trayecto_todos, "trayecto-con_hipoteticos-todo_tipo.png")

leyenda_todos <- map_municipios |> 
  left_join(min_distance_time_all) |> 
  plot_map_distance(
    pal_time_all,
    hospitales = hospitales,
    show_polylines = TRUE
  )

save_map(leyenda_todos, "leyenda-con_hipoteticos-todo_tipo.png")

poligonos_todos <- map_municipios |> 
  left_join(min_distance_time_all) |> 
  plot_map_distance(
    pal_time_all,
  )

save_map(poligonos_todos, "poligonos-con_hipoteticos-todo_tipo.png")

# Solo existentes y traumatológicos ---------------------------------------

trayecto_trauma_existentes <- map_municipios |> 
  left_join(min_distance_time_trauma_existing) |> 
  plot_map_distance(
    pal_time_trauma,
    hospitales = filter(hospitales, existe, type == "Hospital traumatológico"),
    show_polylines = TRUE
  )

save_map(trayecto_trauma_existentes, "trayecto-solo_existentes-solo_traumatologicos.png")


leyenda_trauma_existentes <- map_municipios |> 
  left_join(min_distance_time_trauma_existing) |> 
  plot_map_distance(
    pal_time_trauma,
    hospitales = filter(hospitales, existe, type == "Hospital traumatológico"),
  )

save_map(leyenda_trauma_existentes, "leyenda-solo_existentes-solo_traumatologicos.png")

poligonos_trauma_existentes <- map_municipios |> 
  left_join(min_distance_time_trauma_existing) |> 
  plot_map_distance(pal_time_trauma)

save_map(poligonos_existentes, "poligonos-solo_existentes-solo_traumatologicos.png")

# # Con hipoteticos y solo traumatologicos --------------------------------

trayecto_trauma_hipoteticos <- map_municipios |> 
  left_join(min_distance_time_trauma_all) |> 
  plot_map_distance(
    pal_time_trauma,
    hospitales = filter(hospitales, type == "Hospital traumatológico"),
    show_polylines = TRUE
  )

save_map(trayecto_trauma_hipoteticos, "trayecto-con_hipoteticos-solo_traumatologicos.png")

leyenda_trauma_hipoteticos <- map_municipios |> 
  left_join(min_distance_time_trauma_all) |> 
  plot_map_distance(
    pal_time_trauma,
    hospitales = filter(hospitales, type == "Hospital traumatológico")
  )

save_map(leyenda_trauma_existentes, "leyenda-con_hipoteticos-solo_traumatologicos.png")

poligonos_trauma_hipoteticos <- map_municipios |> 
  left_join(min_distance_time_trauma_all) |> 
  plot_map_distance(pal_time_trauma)

save_map(poligonos_trauma_hipoteticos, "poligonos-con_hipoteticos-solo_traumatologicos.png")

