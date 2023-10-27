source("global.R", local = T)
source("kriging_slicewise.R", local = T)

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
             )),
    
    # Page 1 "Map 1" content
    tabPanel( title = "Map 1", value = "tab1",
              mainPanel(
                p("This page depicts possible outcomes of the temperature forecast for the next year.")),
              sliderInput("year", "Select Year", min = years[1], max = years[length(years)], step = 1, value = years[1], animate = animationOptions(interval = 800, loop = TRUE)),
              leafletOutput("map")
    ),
    
    # Page 2 "Map 2" content
    tabPanel( title = "Map 2", value = "tab2",
              mainPanel(
                p("This page depicts possible outcomes of the temperature forecast for the next year."))
    ),
    # Page 3 "Map 3" content
    tabPanel( title = "Map 3", value = "tab3",
              mainPanel(
                p("This page depicts possible outcomes of the temperature forecast for the next year."))
    ),
    
    # Page 4 "Map 4" content
    tabPanel( title = "Map 4", value = "tab4",
              mainPanel(
                p("This page depicts possible outcomes of the temperature forecast for the next year."))
    )
    
  ) # close navbarpage
) # close UI

#### Server ####
server <- function(input, output, session) {
  # Change the map data based on the selected year
  # output$map <- renderLeaflet({
  #   year <- input$year
  #   map_data <- get(paste0("kriged_slices_", year))
  #   mapview(map_data["mean_temp_pred", , , 6], layer.name = "Temperature", na.color = NA)@map
  # })
  
  # Create an initial map
  output$map <- renderLeaflet({
    year <- 2010
    map_data <- get(paste0("kriged_slices_", year))
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addMouseCoordinates() |>
      addStarsImage(map_data["mean_temp_pred", , , 6], layerId = "Temperature", colors = pal, opacity = 0.7) |>
      addLegend(pal = pal, values = 18:30, title = "Temperature", position = "bottomright", opacity = 1) |>
      addPolygons(data = spain_LL, fill = FALSE, color = "red", weight = 2)
  })
  
  # Use observeEvent to update the layer when the input changes
  observeEvent(input$year, {
    year <- input$year
    map_data <- get(paste0("kriged_slices_", year))
    leafletProxy("map", data = map_data) |>
      clearGroup("Temperature") |>  # Clear the existing starsImage layer
      #clearControls() |> # Clear the legend
      addStarsImage(map_data["mean_temp_pred", , , 6], layerId = "Temperature", colors = pal, opacity = 0.7)
      #addLegend(pal = pal, values = 20:25, title = "Temperature", position = "bottomright")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
