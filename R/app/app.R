source("global.R", local = T)
source("kriging_slicewise.R", local = T)
source("pixmap.R", local = T)

#### UI ####
ui <- fluidPage(
  
  navbar <- navbarPage(
    title = "Uncertainty Viz in ECV projections",
    id = "navbar",
    
    # Page 0 "Home" content
    tabPanel(title = "Home", value = "tab0",
             
             mainPanel(
               img(src="https://icisk.eu/wp-content/uploads/2022/01/icisk_logo_full.png", style = "height: 50%; width: 50%; margin: 1px 1px;"),
               h1("Uncertainty visualization questionnaire"),
               p("This webpage exhibits some examples for map visualization of uncertainty in temperature projections for Living Lab Andalusia in Spain 🇪🇸 in the I-CISK project."),
               p("Navigate through the top bar to explore the different visualizations. You use these to answer the questionnaire.")
             )
    ),
    
    # Page 1 "Map 1" content
    tabPanel( title = "Map 1", value = "tab1",
              mainPanel(
                p("This page depicts possible outcomes of the temperature projection for June 2030. 
                Click on the play-button ▶️️ in the slider to view the animation. 
                Each slide represents a possible outcome of the projection model."),
                sliderInput("outcome", "Outcome", min = 1, max = length(years), step = 1, value = 1, animate = animationOptions(interval = 800, loop = TRUE)),
                h1("Hypothetical Outcome Map"),
                leafletOutput("map")
              )
    ),
    # Page 2 "Map 2" content
    tabPanel( title = "Map 2", value = "tab2",
              mainPanel(
                p("This page depicts two maps: the left one displays the temperature projection for June 2030.
                  The map on the right shows the associated uncertainty of the projection. 
                  The associated uncertainty is the possible difference in °C betweeen the future observed temperature and the projected temperature.")),
              column(width = 6, h1("Projection")),
              column(width = 6, h1("Possible deviation")),
              uiOutput("map2")
    ),
    
    # Page 3 "Map 3" content
    tabPanel( title = "Map 3", value = "tab3",
              mainPanel(
                p("This page depicts highest and lowest possibilities of the temperature projection for June 2030.
                  The left map displays the highest possible mean temperature, and the right one the lowest. 
                  The real value will most likely be somewhere in between.")),
              column(width = 6, h1("Highest projected temperature")),
              column(width = 6, h1("Lowest projected temperature")),
              uiOutput("map3")
    ),
    
    # Page 4 "Map 4" content
    tabPanel( title = "Map 4", value = "tab4",
              mainPanel(
                p("This page depicts the mean temperature projection for June 2030 on a pixel map. 
                  This pixel map was made by breaking down areas into tiny dots (pixels) and giving each dot a value based on how confident we are in our estimate or its frequency in that area."
                  ),
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
      addPolygons(data = andalucia, fill = FALSE, color = "red", weight = 2) |>
      addCircleMarkers(lng = locationA$lon, lat = locationA$lat, radius = 10, color = "black", fill = FALSE, fillOpacity = 1) |>
      addCircleMarkers(lng = locationB$lon, lat = locationB$lat, radius = 10, color = "black", fill = FALSE, fillOpacity = 1)
    
  })
  
  # Use observeEvent to update the layer when the input changes
  # Corresponds to above map 1
  observeEvent(input$outcome, {
    selected_outcome_index <- input$outcome
    year <- years[selected_outcome_index]
    map_data <- get(paste0("kriged_slices_", year))
    leafletProxy("map", data = map_data) |>
      clearGroup("Temperature") |>  # Clear the existing starsImage layer
      addStarsImage(map_data["mean_temp_pred", , , 6], layerId = "Temperature", colors = pal, opacity = 0.7)
  })
  
  # Tab 2 config
  
  output$map2 <- renderUI({
    
    map2_1 <- leaflet() |>
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(minZoom=5, maxZoom=15)) |>
      addMouseCoordinates() |>
      addStarsImage(mean_temps_june["mean_temp_pred"], layerId = "Temperature", colors = pal, opacity = 0.7) |>
      addLegend(pal = pal_legend, values = 18:30, title = "Temperature (°C)", position = "bottomright", opacity = 1,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) |>
      addPolygons(data = andalucia, fill = FALSE, color = "red", weight = 2) |>
      addCircleMarkers(lng = locationA$lon, lat = locationA$lat, radius = 10, color = "black", fill = FALSE, fillOpacity = 1) |>
      addCircleMarkers(lng = locationB$lon, lat = locationB$lat, radius = 10, color = "black", fill = FALSE, fillOpacity = 1)
    
    map2_2 <- leaflet() |>
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(minZoom=5, maxZoom=15)) |>
      addMouseCoordinates() |>
      addStarsImage(mean_temps_june["mean_temp_difference"], layerId = "Temperature", colors = pal2, opacity = 0.7) |>
      addLegend(pal = pal2_legend, values = 0.3:4.3, title = "Possible deviation (°C)", position = "bottomright", opacity = 1,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) |>
      addPolygons(data = andalucia, fill = FALSE, color = "red", weight = 2) |>
      addCircleMarkers(lng = locationA$lon, lat = locationA$lat, radius = 10, color = "black", fill = FALSE, fillOpacity = 1)|>
      addCircleMarkers(lng = locationB$lon, lat = locationB$lat, radius = 10, color = "black", fill = FALSE, fillOpacity = 1)
    
    sync(map2_1, map2_2, sync.cursor = FALSE)
    
  })
  
  output$map3 <- renderUI({
    
    map3_1 <- leaflet() |>
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(minZoom=5, maxZoom=15)) |>
      addMouseCoordinates() |>
      addStarsImage(max_temps_june["mean_temp_pred"], layerId = "Temperature", colors = pal, opacity = 0.7) |>
      addLegend(pal = pal_legend, values = 18:30, title = "Temperature (°C)", position = "bottomright", opacity = 1,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) |>
      addPolygons(data = andalucia, fill = FALSE, color = "red", weight = 2) |>
      addCircleMarkers(lng = locationA$lon, lat = locationA$lat, radius = 10, color = "black", fill = FALSE, fillOpacity = 1)|>
      addCircleMarkers(lng = locationB$lon, lat = locationB$lat, radius = 10, color = "black", fill = FALSE, fillOpacity = 1)
    
    map3_2 <- leaflet() |>
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(minZoom=5, maxZoom=15)) |>
      addMouseCoordinates() |>
      addStarsImage(min_temps_june["mean_temp_pred"], layerId = "Temperature", colors = pal, opacity = 0.7) |>
      addLegend(pal = pal_legend, values = 18:30, title = "Temperature (°C)", position = "bottomright", opacity = 1,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) |>
      addPolygons(data = andalucia, fill = FALSE, color = "red", weight = 2) |>
      addCircleMarkers(lng = locationA$lon, lat = locationA$lat, radius = 10, color = "black", fill = FALSE, fillOpacity = 1)|>
      addCircleMarkers(lng = locationB$lon, lat = locationB$lat, radius = 10, color = "black", fill = FALSE, fillOpacity = 1)
    
    sync(map3_1, map3_2, sync.cursor = FALSE)
    
  })

  # Tab 4 config
  output$map4 <- renderPlot({
    
    map_data <- get(paste0("unifPixMap"))
    view(map_data) + 
      geom_point(data = locationA, aes(x = lon, y = lat), shape = 1, color = "blue", stroke = 2, size = 5) + 
      geom_point(data = locationB, aes(x = lon, y = lat), shape = 1, color = "blue", stroke = 2, size = 5) + 
      scale_shape(solid = FALSE)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
