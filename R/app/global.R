# Load necessary libraries
library(devtools)
library(sf)
library(stars)
library(starsExtra)
library(data.table)
library(ggplot2)
library(gstat)
library(dplyr)
library(tidyr)
library(viridis)
library(leaflet)
library(leafem)
library(leafsync)
library(viridisLite)
library(shiny)
library(rsconnect)
library(Vizumap)
library(rnaturalearth)

# Configurable variables
years = 2010:2015
pixelsize = 0.05

# Color palettes for the different maps
pal = colorNumeric(
  viridisLite::inferno(32), 
  domain = 18:30, 
  rev = TRUE,
  na.color = "transparent"
)

pal_legend = colorNumeric(
  viridisLite::inferno(32), 
  domain = 18:30, 
  rev = FALSE,
  na.color = "transparent"
)

pal2 = colorNumeric(
  viridisLite::cividis(12), 
  domain = 0.3:4.3, 
  rev = TRUE,
  na.color = "transparent"
)

pal2_legend = colorNumeric(
  viridisLite::cividis(12), 
  domain = 0.3:4.3, 
  rev = FALSE,
  na.color = "transparent"
)

# Download Spain regions and crop to Andalucia
spain_states <- ne_states(country = "Spain", returnclass = "sf")
andalucia_states <- spain_states[spain_states$region == "Andalucía", ] |> select(name)
andalucia <- st_union(andalucia_states)

# View Andalucia shape
leaflet() |> 
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(data = st_union(andalucia_states), fill = FALSE)
