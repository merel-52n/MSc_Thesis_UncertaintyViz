# Load necessary libraries
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
library(viridisLite)
library(shiny)

# Configurable variables
years = 2010:2015
pixelsize = 0.05

# Color palettes for the different maps
pal = colorNumeric(
  viridisLite::inferno(32), 
  domain = range(c(18:30)), 
  rev = TRUE,
  na.color = "transparent"
)

pal2 = colorNumeric(
  viridisLite::cividis(12), 
  domain = range(c(0.3:4.3)), 
  rev = TRUE,
  na.color = "transparent"
)
