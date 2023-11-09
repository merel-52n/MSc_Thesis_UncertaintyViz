source("global.R", local = T)
source("kriging_slicewise.R", local = T)
source("pixmap.R", local = T)

#### UI ####
ui <- fluidPage(
  
  navbar <- navbarPage(
    title = "Uncertainty Viz in ECV Forecasts",
    id = "navbar",
    
    # Page 0 "Home" content
    tabPanel(title = "Home", value = "tab0",
             
             mainPanel(
               img(src="https://icisk.eu/wp-content/uploads/2022/01/icisk_logo_full.png", style = "height: 50%; width: 50%; margin: 1px 1px;"),
               h1("Uncertainty visualization questionnaire"),
               p("This webpage exhibits some examples for the visualization of uncertainty in temperature and precipitation forecasts for Living Lab Andalusia in Spain 🇪🇸 in the I-CISK project."),
               p("Navigate through the top bar to explore the different visualizations. You use these later for the questionnaire.")
             )
    ),
    # Page 1 "Map 1" content
    tabPanel( title = "Map 1", value = "tab1",
              mainPanel(
                p("This page depicts possible outcomes of the temperature forecast for June 2030. Click on the play-button ▶️️ in the slider to view the animation."),
                sliderInput("scenario", "Scenario", min = 1, max = length(years), step = 1, value = 1, animate = animationOptions(interval = 800, loop = TRUE)),
                h1("Hypothetical Outcome Map"),
                leafletOutput("map")
              )
    ),
    # Page 2 "Map 2" content
    
    tabPanel( title = "Map 2", value = "tab2",
              mainPanel(
                p("This page depicts two maps: the left one displays the temperature forecast for June 2030.
                  The map on the right shows the associated uncertainty of the forecast.")),
              column(width = 6, h1("Forecast"), leafletOutput("map2_1")),
              column(width = 6, h1("Forecast Uncertainty"), leafletOutput("map2_2"))
    ),
    # Page 3 "Map 3" content
    tabPanel( title = "Map 3", value = "tab3",
              mainPanel(
                p("This page depicts highest and lowest possibilities of the temperature forecast for June 2030.
                  The left map displays the highest possible temperature, and the right one the highest.")),
              column(width = 6, h1("Highest predicted temperature"), leafletOutput("map3_1")),
              column(width = 6, h1("Lowest predicted temperature"), leafletOutput("map3_2"))
    ),
    
    # Page 4 "Map 4" content
    tabPanel( title = "Map 4", value = "tab4",
              mainPanel(
                p("This page depicts possible outcomes of the temperature forecast for June 2030. 
                  For each region, "),
                sliderInput("year4", "Select Year", min = years[1], max = years[length(years)], step = 1, value = years[1]),
                h1("Pixelated map"),
                plotOutput("map4")
              )
    )
    
  ) # close navbarpage
) # close UI

#### Server ####
server <- function(input, output, session) {
  # Tab 1 config
  output$map <- renderLeaflet({
    year <- years[1]
    map_data <- get(paste0("kriged_slices_", year))
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addMouseCoordinates() |>
      addStarsImage(map_data["mean_temp_pred", , , 6], layerId = "Temperature", colors = pal, opacity = 0.7) |>
      addLegend(pal = pal_legend, values = 18:30, title = "Temperature (°C)", position = "bottomright", opacity = 1,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) |>
      addPolygons(data = andalucia, fill = FALSE, color = "red", weight = 2)
  })
  
  # Use observeEvent to update the layer when the input changes
  # corresponds to above map 1
  observeEvent(input$scenario, {
    selected_scenario_index <- input$scenario
    year <- years[selected_scenario_index]
    map_data <- get(paste0("kriged_slices_", year))
    leafletProxy("map", data = map_data) |>
      clearGroup("Temperature") |>  # Clear the existing starsImage layer
      addStarsImage(map_data["mean_temp_pred", , , 6], layerId = "Temperature", colors = pal, opacity = 0.7)
  })
  
  # Tab 2 config
  output$map2_1 <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addMouseCoordinates() |>
      addStarsImage(mean_temps_june["mean_temp_pred"], layerId = "Temperature", colors = pal, opacity = 0.7) |>
      addLegend(pal = pal_legend, values = 18:30, title = "Temperature (°C)", position = "bottomright", opacity = 1,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) |>
      addPolygons(data = andalucia, fill = FALSE, color = "red", weight = 2)
  })
  
  output$map2_2 <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addMouseCoordinates() |>
      addStarsImage(mean_temps_june["mean_temp_difference"], layerId = "Temperature", colors = pal2, opacity = 0.7) |>
      addLegend(pal = pal2_legend, values = 0.3:4.3, title = "Possible difference (°C)", position = "bottomright", opacity = 1,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) |>
      addPolygons(data = andalucia, fill = FALSE, color = "red", weight = 2)
  })
  
  # set zoom level back to 7 when tab for map2 tab is selected
  observe({
    if (input$navbar == "tab2") {
      leafletProxy("map2_1") |>
        setView(lng = -4.61, lat = 37.41, zoom = 7)
      leafletProxy("map2_2") |>
        setView(lng = -4.61, lat = 37.41, zoom = 7)
    }
  })
  
  observe({ # Observer to respond to zoom / pan of map1 and apply to map2
    coords <- input$map2_1_bounds
    
    if (!is.null(coords)) {
      tproxy <- leafletProxy("map2_2") |>
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })
  
  observe({ # Observer to respond to zoom / pan of map1 and apply to map2
    coords <- input$map2_2_bounds
    
    if (!is.null(coords)) {
      tproxy <- leafletProxy("map2_1") |>
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })
  
  # Tab 3 config
  output$map3_1 <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addMouseCoordinates() |>
      addStarsImage(max_temps_june["mean_temp_pred"], layerId = "Temperature", colors = pal, opacity = 0.7) |>
      addLegend(pal = pal_legend, values = 18:30, title = "Temperature (°C)", position = "bottomright", opacity = 1,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) |>
      addPolygons(data = andalucia, fill = FALSE, color = "red", weight = 2)
  })
  
  output$map3_2 <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addMouseCoordinates() |>
      addStarsImage(min_temps_june["mean_temp_pred"], layerId = "Temperature", colors = pal, opacity = 0.7) |>
      addLegend(pal = pal_legend, values = 18:30, title = "Temperature (°C)", position = "bottomright", opacity = 1,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) |>
      addPolygons(data = andalucia, fill = FALSE, color = "red", weight = 2)
  })
  
  # set zoom level back to 7 when tab for map2 tab is selected
  observe({
    if (input$navbar == "tab3") {
      leafletProxy("map3_1") |>
        setView(lng = -4.61, lat = 37.41, zoom = 7)
      leafletProxy("map3_2") |>
        setView(lng = -4.61, lat = 37.41, zoom = 7)
    }
  })

  observe({ # Observer to respond to zoom / pan of map3_1 and apply to map3_2
    coords <- input$map3_1_bounds
    
    if (!is.null(coords)) {
      tproxy <- leafletProxy("map3_2") |>
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })
  
  observe({ # Observer to respond to zoom / pan of map3_2 and apply to map3_1
    coords <- input$map3_2_bounds
    
    if (!is.null(coords)) {
      tproxy <- leafletProxy("map3_1") |>
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })
  
  # Tab 4 config
  output$map4 <- renderPlot({
    year <- input$year4
    map_data <- get(paste0("unifPixMap_", year))
    view(map_data)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
