##
# This script downloads the necessary data for the app. NB: running it will take a long time (more than half an hour, approx.)
##

suppressMessages(library(meteospain))
suppressMessages(library(sf))
suppressMessages(library(log4r))

# Initiate logger
debug_level <- "DEBUG"
logger <- logger(debug_level)

# Check if data directory exists (if not, create it)
data_folder <- "./data"
if (!dir.exists(data_folder)) {
  dir.create(data_folder)
}

convert_DMS_to_decimal = function(geometry){
  ##
  # Function that converts arcseconds/minutes lat/lon coordinates to decimal lat/lon coordinates
  # Meteo Ria API delivers data in arcseconds/minutes, so this function can be used to convert them
  # Input should be a list of point geometries
  ##
  g = st_coordinates(geometry)
  lon = paste0(as.character(g[1]), "00")
  lat = paste0(as.character(g[2]), "00")
  lon_deg = as.numeric(substring(lon, 1,2))
  lon_min = as.numeric(substring(lon, 4,5))
  lon_sec = as.numeric(substring(lon, 6,7))
  lat_deg = as.numeric(substring(lat, 1,2))
  lat_min = as.numeric(substring(lat, 4,5))
  lat_sec = as.numeric(substring(lat, 6,7))
  p_lon = lon_deg - lon_min/60 - lon_sec/3600
  p_lat = lat_deg + lat_min/60 + lat_sec/3600
  return(st_sfc(st_point(c(p_lon, p_lat)), crs = 4326))
}

save_meteo_data <- function(input_year) {
  ##
  # Function that downloads data from the Andalucia meteorological service
  # Link to data source: Red de Información Agroclimática de Andalucía (RIA)
  # https://www.juntadeandalucia.es/agriculturaypesca/ifapa/riaweb/web/ 
  # using R-package 'meteospain'
  # "input_year" is the desired year, for example 2022
  # The function downloads the data for the entire input year, so running takes quite some time
  ##
  
  # Create output file name including path
  output_file <- file.path(data_folder, paste("meteo_andalucia_", input_year, ".gpkg", sep = ""))
  
  # If the file exists already, stop download
  if (file.exists(output_file)) {
    debug(logger, paste0("Download cancelled. GeoPackage already exists: ", output_file, "\n"))
    return()
  }
  
  # Convert year to proper format
  input_year <- formatC(input_year, width = 4, flag = "0")
  
  # Define start and end dates for the entire year
  start_date <- as.Date(paste(input_year, "01", "01", sep = "-"))
  end_date <- as.Date(paste(input_year, "12", "31", sep = "-"))
  
  # Retrieve meteo data for Andalucia for the specified month and year
  ria_options <- ria_options(resolution = 'daily', start_date = start_date, end_date = end_date)
  debug(logger, paste0("Downloading data for ", input_year, "...\n"))
  meteo_ria <- get_meteo_from('ria', ria_options)
  
  # Convert lat/lon coordinates from arcmin/arcsec to decimal degrees
  debug(logger, "Converting coordinates...")
  for(i in c(1:length(meteo_ria$geometry))){
    meteo_ria$geometry[i] <- convert_DMS_to_decimal(meteo_ria$geometry[i])
  }
  
  # Save the dataframe as a GeoPackage file
  st_write(meteo_ria, dsn = output_file, driver="gpkg", append = FALSE)
  # Print a confirmation message
  debug(logger, paste0("GeoPackage created successfully: ", output_file, "\n"))
}

# Download all data for the years 2010-2015
# Change "years" if needed, can range from 2000 to 2022
# This takes a while! 
years <- 2010:2015

for (y in years){
  save_meteo_data(y)
}

