library(sf)
library(stars)
library(starsExtra)
library(data.table)
library(ggplot2)
library(gstat)
library(rnaturalearth)
library(dplyr)
library(tidyr)
library(viridis)
library(gganimate)
library(gifski)
library(transformr)
library(magick)
# See https://github.com/thomasp85/gganimate/issues/479 : so need dev version of transformr to reproduce
# install.packages("devtools")
#devtools::install_github("thomasp85/transformr")
#library(transformr) see https://stackoverflow.com/questions/68450668/how-can-i-animate-points-on-a-spatial-map-with-gganimate-sf-and-ggplot2 

# Set the path to the geopackage files
setwd("/home/merel/Documents/I-CISK/MSc_Thesis_UncertaintyViz/R/")
path <- "./data"

#### Load all GeoPackage files with meteorological data from meteo-RIA API from 2010 until 2022 ####
# Create separate dfs for mean temperature per month + summed precipitation per month
for (year in 2010:2022) {
  filename <- paste0("meteo_andalucia_", year, ".gpkg")
  filepath <- file.path(path, filename)
  df_name_temp <- paste0("df_mean_tmp_", year)
  df_name_precip <- paste0("df_mean_precip_", year)
  tmpData <- st_read(filepath)
  tmpData$month <- format(tmpData$timestamp, "%Y-%m")
  tmp_mean <- tmpData |>
    group_by(month, station_name) |>
    summarise(mean_temperature = mean(mean_temperature, na.rm = TRUE)) |>
    na.omit() ## added this
  assign(df_name_temp, tmp_mean)
  precip_mean <- tmpData |>
    group_by(month, station_name) |>
    summarise(sum_precipitation = sum(precipitation, na.rm = TRUE)) |>
    na.omit()
  assign(df_name_precip, precip_mean)
}

#### define target grid ####
grd <- make_grid(df_mean_tmp_2010, res = 0.1) # change res as needed

#### function form ##### 
sliced_krige <- function(year) {
  
  if (year < 2010 || year > 2022) {
    stop("Input year must be between 2010 and 2022")
  }
  # Construct the data frame name using 'year'
  df_name <- paste0("df_mean_tmp_", year)

  # iterate over all months
  kriged_slices_pred <- NULL
  kriged_slices_var  <- NULL
  
  timeCodes <- format(as.POSIXct(paste(year, "-01-01", sep=""))+0:11*31*24*3600, "%Y-%m")
  for(cur_month in timeCodes) { # cur_month <- timeCodes[7]
    cat("Processing:", cur_month, "\n")
    cur_emp_vgm <- variogram(mean_temperature~1, get(df_name)[get(df_name)$month == cur_month,])
    cur_mod_vgm <- fit.variogram(cur_emp_vgm, vgm(4, "Sph", 150, 0.1))
    
    # krige
    cur_kriged_slice <- krige(mean_temperature~1, get(df_name)[get(df_name)$month == cur_month,], grd, cur_mod_vgm)
    
    ## some visual debugging code
    #plot(cur_emp_vgm, cur_mod_vgm)
    #plot(get(df_name)[get(df_name)$month == cur_month,])
    #plot(cur_kriged_slice, col = rev(heat.colors(20)))
    
    # merge predictions
    cur_kriged_slice_pred <- cur_kriged_slice["var1.pred",]
    if (!is.null(kriged_slices_pred)) {
      kriged_slices_pred <- c(kriged_slices_pred, cur_kriged_slice_pred)
    } else {
      kriged_slices_pred <- cur_kriged_slice_pred
    }
    
    # merge kriging variances
    cur_kriged_slice_var <- cur_kriged_slice["var1.var",]
    if (!is.null(kriged_slices_var)) {
      kriged_slices_var <- c(kriged_slices_var, cur_kriged_slice_var)
    } else {
      kriged_slices_var <- cur_kriged_slice_var
    }
  }
  
  kriged_slices_pred <- st_redimension(kriged_slices_pred)
  kriged_slices_pred <- st_set_dimensions(kriged_slices_pred, 
                                          which = 3, 
                                          names="time", 
                                          values=timeCodes)
  attributes(kriged_slices_pred)$names <-  "mean_temp_pred"
  plot(kriged_slices_pred, col = rev(heat.colors(20)))
  
  kriged_slices_var <- st_redimension(kriged_slices_var)
  kriged_slices_var <- st_set_dimensions(kriged_slices_var, 
                                         which = 3, 
                                         names="time", 
                                         values=timeCodes)
  attributes(kriged_slices_var)$names <- "mean_temp_variance"
  plot(kriged_slices_var)
  
  # re-combine predictions and variance
  kriged_slices <- c(kriged_slices_pred,
                     kriged_slices_var)
  
  # Assign the kriged_slices to the variable with the input year
  kriged_slices_name <- paste0("kriged_slices_", year)
  assign(kriged_slices_name, kriged_slices, env=.GlobalEnv)
  
  #return(get(kriged_slices_name))
}

#### function form ##### 
sliced_krige_precip <- function(year) {
  
  if (year < 2010 || year > 2022) {
    stop("Input year must be between 2010 and 2022")
  }
  # Construct the data frame name using 'year'
  df_name <- paste0("df_mean_precip_", year)
  
  # iterate over all months
  kriged_slices_pred <- NULL
  kriged_slices_var  <- NULL
  
  timeCodes <- format(as.POSIXct(paste(year, "-01-01", sep=""))+0:11*31*24*3600, "%Y-%m")
  for(cur_month in timeCodes) { # cur_month <- timeCodes[7]
    cat("Processing:", cur_month, "\n")
    cur_emp_vgm <- variogram(sum_precipitation~1, get(df_name)[get(df_name)$month == cur_month,])
    cur_mod_vgm <- fit.variogram(cur_emp_vgm, vgm(4, "Sph", 150, 0.1))
    
    # krige
    cur_kriged_slice <- krige(sum_precipitation~1, get(df_name)[get(df_name)$month == cur_month,], grd, cur_mod_vgm)
    
    ## some visual debugging code
    #plot(cur_emp_vgm, cur_mod_vgm)
    #plot(get(df_name)[get(df_name)$month == cur_month,])
    #plot(cur_kriged_slice, col = rev(heat.colors(20)))
    
    # merge predictions
    cur_kriged_slice_pred <- cur_kriged_slice["var1.pred",]
    if (!is.null(kriged_slices_pred)) {
      kriged_slices_pred <- c(kriged_slices_pred, cur_kriged_slice_pred)
    } else {
      kriged_slices_pred <- cur_kriged_slice_pred
    }
    
    # merge kriging variances
    cur_kriged_slice_var <- cur_kriged_slice["var1.var",]
    if (!is.null(kriged_slices_var)) {
      kriged_slices_var <- c(kriged_slices_var, cur_kriged_slice_var)
    } else {
      kriged_slices_var <- cur_kriged_slice_var
    }
  }
  
  kriged_slices_pred <- st_redimension(kriged_slices_pred)
  kriged_slices_pred <- st_set_dimensions(kriged_slices_pred, 
                                          which = 3, 
                                          names="time", 
                                          values=timeCodes)
  attributes(kriged_slices_pred)$names <-  "sum_precip_pred"
  plot(kriged_slices_pred, col = rev(topo.colors(20)))
  
  kriged_slices_var <- st_redimension(kriged_slices_var)
  kriged_slices_var <- st_set_dimensions(kriged_slices_var, 
                                         which = 3, 
                                         names="time", 
                                         values=timeCodes)
  attributes(kriged_slices_var)$names <- "sum_precip_variance"
  plot(kriged_slices_var)
  
  # re-combine predictions and variance
  kriged_slices <- c(kriged_slices_pred,
                     kriged_slices_var)
  
  # Assign the kriged_slices to the variable with the input year
  kriged_slices_name <- paste0("kriged_slices_precip_", year)
  assign(kriged_slices_name, kriged_slices, env=.GlobalEnv)
  
  #return(get(kriged_slices_name))
}

#### Function that takes input variable precipitation or temperature ###
# doesn't work yet...
sliced_kriges <- function(year, variable_name) {
  
  if (year < 2010 || year > 2022) {
    stop("Input year must be between 2010 and 2022")
  }
  if (!(variable_name %in% c("temperature", "precipitation"))) {
    stop("Variable name must be either 'temperature' or 'precipitation'")
  }
  
  # Construct the data frame name using the input variables
  if (variable_name == "temperature") {
    variable_name <- "mean_temperature"
    df_name <- paste0("df_", "mean_tmp", "_", year)
    print(df_name)
  } else {
    variable_name <- "sum_precipitation"
    df_name <- paste0("df_", "mean_precip", "_", year)
  }
  
  # iterate over all months
  kriged_slices_pred <- NULL
  kriged_slices_var  <- NULL
  
  timeCodes <- format(as.POSIXct(paste(year, "-01-01", sep=""))+0:11*31*24*3600, "%Y-%m")
  for(cur_month in timeCodes) { # cur_month <- timeCodes[7]
    cat("Processing:", cur_month, "\n")
    print(paste(variable_name, "~1", sep=""))
    f <- paste(variable_name, "~1", sep="")
    cur_emp_vgm <- variogram(f, get(df_name)[get(df_name)$month == cur_month,])
    cur_mod_vgm <- fit.variogram(cur_emp_vgm, vgm(4, "Sph", 150, 0.1))
    
    # krige
    cur_kriged_slice <- krige(f, get(df_name)[get(df_name)$month == cur_month,], grd, cur_mod_vgm)
    
    ## some visual debugging code
    #plot(cur_emp_vgm, cur_mod_vgm)
    #plot(get(df_name)[get(df_name)$month == cur_month,])
    #plot(cur_kriged_slice, col = rev(heat.colors(20)))
    
    # merge predictions
    cur_kriged_slice_pred <- cur_kriged_slice["var1.pred",]
    if (!is.null(kriged_slices_pred)) {
      kriged_slices_pred <- c(kriged_slices_pred, cur_kriged_slice_pred)
    } else {
      kriged_slices_pred <- cur_kriged_slice_pred
    }
    
    # merge kriging variances
    cur_kriged_slice_var <- cur_kriged_slice["var1.var",]
    if (!is.null(kriged_slices_var)) {
      kriged_slices_var <- c(kriged_slices_var, cur_kriged_slice_var)
    } else {
      kriged_slices_var <- cur_kriged_slice_var
    }
  }
  
  kriged_slices_pred <- st_redimension(kriged_slices_pred)
  kriged_slices_pred <- st_set_dimensions(kriged_slices_pred, 
                                          which = 3, 
                                          names="time", 
                                          values=timeCodes)
  attributes(kriged_slices_pred)$names <-  paste0(variable_name, "_pred")
  plot(kriged_slices_pred, col = rev(heat.colors(20)))
  
  kriged_slices_var <- st_redimension(kriged_slices_var)
  kriged_slices_var <- st_set_dimensions(kriged_slices_var, 
                                         which = 3, 
                                         names="time", 
                                         values=timeCodes)
  attributes(kriged_slices_var)$names <- paste0(variable_name, "_variance")
  plot(kriged_slices_var)
  
  # re-combine predictions and variance
  kriged_slices <- c(kriged_slices_pred,
                     kriged_slices_var)
  
  # Assign the kriged_slices to the variable with the input year
  kriged_slices_name <- paste0("kriged_slices_", variable_name, "_", year)
  assign(kriged_slices_name, kriged_slices, env=.GlobalEnv)
  
  #return(get(kriged_slices_name))
}


# Run kriging interpolation for all years for temp and precip
for (year in 2010:2022) {
  sliced_krige(year)
}

for (year in c(2010:2011, 2013:2022)) { # 2012 causes strange error?
  sliced_krige_precip(year)
}

#### Plotting ####

# Get Spain shape to plot alongside one of the results (use st_crop for croppping out canary islands)
spain_mainland_bbox <- c(xmin = -10, xmax = 5, ymin = 35, ymax = 44)
spain <- ne_countries(scale = "medium", country = "Spain", continent = "Europe", returnclass = "sf") |>
  st_crop(spain_mainland_bbox)
spain_counties <- ne_states("spain", returnclass = "sf") |>
  st_crop(spain_mainland_bbox)
#spain_rivers <- ne_download(scale = 110, type = "rivers_lake_centerlines", category = "physical")

# Crop interpolation for plotting
cropped <- st_crop(kriged_slices_2011, spain)

# Now plot results with Spain
g <- ggplot() + 
  geom_sf() +
  coord_equal() +
  scale_fill_viridis() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0))

g_krig <- g + geom_sf(data = st_cast(spain, "MULTILINESTRING")) +
  geom_sf(data = st_cast(spain_counties, "MULTILINESTRING")) +
  # This plots the prediction for which month?
  geom_stars(data = cropped["mean_temp_pred"], aes(fill = mean_temp_pred, x = x, y = y)) +
  coord_sf(lims_method = "geometry_bbox")
  
# st_get_dimension_values(kriged_slices_2011, "time")

plot_krige <- function(year) {
  # Create a ggplot for each year
  g <- ggplot() + 
    geom_sf() +
    coord_equal() +
    scale_fill_viridis() +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0))
  
  g_krig <- g + geom_sf(data = st_cast(spain, "MULTILINESTRING")) +
    geom_stars(data = get(paste0("kriged_slices_", year))["mean_temp_pred"], aes(fill = mean_temp_pred, x = x, y = y)) +
    coord_sf(lims_method = "geometry_bbox")
  
  g_krig
  
  # Save the ggplot as a PNG file
  png_file <- paste0("./img/krige_animation_", year, ".png")
  ggsave(filename = png_file, plot = g_krig, width = 6, height = 6, units = "in")
}

## Plot results and save as png
for (year in 2010:2022) {
 plot_krige(year)
}

filepath = "./img"

png_files = list.files(filepath)
png_files = paste0(filepath, '/', png_files)
delay <- 0.3 #lookup speed in other paper
gifski(png_files = png_files, gif_file = "krige-animation.gif",
       delay = delay,
       progress = T)
image_read("krige-animation.gif")
