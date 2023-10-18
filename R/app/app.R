source("global.R", local = T)
source("kriging_slicewise.R", local = T)

#### UI ####
ui <- fluidPage(

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
      
      # Page 1 "Map" content
      tabPanel( title = "Map", value = "tab1",
                mainPanel(
                  p("This page depicts possible outcomes of the temperature forecast for the next year.")),
                  sliderInput("year", "Select Year", min = 2010, max = 2015, step = 1, value = 2010, animate = animationOptions(interval = 800, loop = TRUE)),
                  leafletOutput("map")
                )
        )
    ))

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
          addStarsImage(map_data["mean_temp_pred", , , 6], layerId = "Temperature", opacity = 0.7)
      })
      
      # Use observeEvent to update the layer when the input changes
      observeEvent(input$year, {
        year <- input$year
        map_data <- get(paste0("kriged_slices_", year))
        leafletProxy("map", data = map_data) |>
          clearGroup("Temperature") |>  # Clear the existing image layer
          addStarsImage(map_data["mean_temp_pred", , , 6], layerId = "Temperature", opacity = 0.7)
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
