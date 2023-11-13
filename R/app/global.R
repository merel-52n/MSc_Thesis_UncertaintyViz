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

# Location A for in the Leaflet map as marker for reference
locationA <- data.frame(
  lon = -5.89,
  lat = 37.56
)

# View Andalucia shape with the marker for location A
leaflet() |> 
  addProviderTiles(providers$CartoDB.Positron) |>
  addMouseCoordinates() |>
  addPolygons(data = st_union(andalucia_states), fill = FALSE) |>
  addCircleMarkers(lng = locationA$lon, lat = locationA$lat, radius = 10, color = "black", fill = FALSE, fillOpacity = 1)

