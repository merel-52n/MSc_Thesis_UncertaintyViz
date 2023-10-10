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
                mainPanel(p("This page depicts possible outcomes of the temperature forecast for the next year."))
                )
        )
    )
)

#### Server ####
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)