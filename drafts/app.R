library(shiny)
library(bslib)
library(highcharter)
library(leaflet)

distance_time_all <- readRDS("data/municipios_distance_time_all.rds")
map_municipios <- readRDS("data/municipios_sf.rds")


map_hipotetico <- readRDS("outputs/color_map_hipotetico.rds")
map_original <- readRDS("outputs/color_map_original.rds")


custom_theme <- bs_theme(
)

ui <- page(
  theme = custom_theme,
  div(
    class = "p-4 mb-4 shadow-sm",
    h1("Transporte terrestre")
  ),
  div(
    class = "p-3",
    layout_column_wrap(
      width = 1 / 2,
      card(card_header("Tiempo original"), card_body(map_original), full_screen = TRUE),
      card(card_header("Tiempo hipotetico"), card_body(map_hipotetico), full_screen = TRUE),
    ),
    card(
      full_screen = TRUE,
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          "ruta",
          div(
            class = "d-flex gap-2",
            selectInput(
              "municipio", 
              NULL, 
              choices = unique(distance_time_all$municipio_label), 
              selectize = TRUE
            ),
            selectInput("hospital", NULL, choices = unique(distance_time_all$hospital), width = "500px")
          )
        )
      ),
      card_body(
        leafletOutput("polyline_map")
      )
    )
  )
)

server <- function(input, output, session) {
  output$polyline_map <- renderLeaflet({
    map_municipios |>
      leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        stroke = TRUE,
        layerId = ~id,
        weight = 1,
        fillOpacity = 0.1,
        fillColor = "white",
        color = "gray"
      )
  })
  
  selected_direction <- reactive({
    req(input$municipio, input$hospital)
    distance_time_all |>
      filter(
        municipio_label == input$municipio,
        hospital == input$hospital
      )
  })

  polyline <- reactive({
    req(selected_direction())
    pull(selected_direction(), polyline) |> 
      googleway::decode_pl() 
  })
  
  observeEvent(input$polyline_map_shape_click, {
    req(input$polyline_map_shape_click)
    municipio <- map_municipios |>
      filter(id == input$polyline_map_shape_click$id) |>
      pull(municipio_label)
    
    updateSelectInput(session, "municipio", selected = municipio)
  })
  
  observe({
    req(polyline())
    leafletProxy("polyline_map") |>
      removeShape(c("line", "origin", "destination")) |>
      addPolylines(
        layerId = "line",
        data = polyline(),
        lng = ~lon,
        lat = ~lat,
        color = "blue",
        weight = 3,
        opacity = 0.8,
        label = "Route"
      ) |>
      addMarkers(
        layerId = "origin",
        data = selected_direction(),
        lat = ~origin_lat,
        lng = ~origin_lng,
        popup = ~glue::glue("Trayecto de {distance_text}, {duration_text}")
      ) |> 
      addMarkers(
        layerId = "destination",
        data = selected_direction(),
        lat = ~destination_lat,
        lng = ~destination_lng,
        popup = ~glue::glue("Trayecto de {distance_text}, {duration_text}")
      )
  })
}

shinyApp(ui, server)
