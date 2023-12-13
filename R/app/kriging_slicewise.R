# Set the path to the geopackage files
setwd("/home/merel/Documents/I-CISK/MSc_Thesis_UncertaintyViz/R/app")
path <- "./data"

#### Load all GeoPackage files with meteorological data from meteo-RIA API from 2010 until 2022 ####
# Create separate dfs for mean temperature per month + summed precipitation per month

for (year in years) {
  filename <- paste0("meteo_andalucia_", year, ".gpkg")
  filepath <- file.path(path, filename)
  df_name_temp <- paste0("df_mean_tmp_", year)
  df_name_precip <- paste0("df_mean_precip_", year)
  tmpData <- st_read(filepath)
  tmpData$month <- format(tmpData$timestamp, "%Y-%m")
  tmp_mean <- tmpData |>
    group_by(month, station_name) |>
    summarise(mean_temperature = mean(mean_temperature, na.rm = TRUE)) |>
    na.omit() 
  assign(df_name_temp, tmp_mean)
  precip_mean <- tmpData |>
    group_by(month, station_name) |>
    summarise(sum_precipitation = sum(precipitation, na.rm = TRUE)) |>
    na.omit()
  assign(df_name_precip, precip_mean)
}

#### define target grid ####
grd <- make_grid(andalucia, res = pixelsize)

# view grid with spanish LL
leaflet() |> 
  addProviderTiles(providers$CartoDB.Positron) |> 
  addMouseCoordinates() |> 
  addStarsImage(grd, opacity = 0.6) |> 
  addPolygons(data = andalucia, fill = FALSE, color = "red", weight = 2)

#### with prediction interval and difference #####
sliced_krige_withUnc <- function(year) {
  
  if (year < 2010 || year > 2022) {
    stop("Input year must be between 2010 and 2022")
  }
  # Construct the data frame name using 'year'
  df_name <- paste0("df_mean_tmp_", year)
  
  # iterate over all months
  kriged_slices_pred <- NULL
  kriged_slices_var  <- NULL
  kriged_slices_lowerbound <- NULL 
  kriged_slices_upperbound <- NULL
  kriged_slices_difference <- NULL
  
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
    
    # Calculate the standard deviation from kriging variance
    standard_deviation <- sqrt(cur_kriged_slice$var1.var)
    
    # Calculate the lower and upper bounds of the prediction interval
    lower_bound <- cur_kriged_slice$var1.pred - 1.96 * standard_deviation
    upper_bound <- cur_kriged_slice$var1.pred + 1.96 * standard_deviation
    difference <- 1.96 * standard_deviation
    
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
    print(kriged_slices_var)
    
    # merge upper bounds
    cur_kriged_slice_upperbound <- st_as_stars(upper_bound)
    if (!is.null(kriged_slices_upperbound)) {
      kriged_slices_upperbound <- c(kriged_slices_upperbound, cur_kriged_slice_upperbound)
    } else {
      kriged_slices_upperbound <- cur_kriged_slice_upperbound
    }
    print(kriged_slices_upperbound)
    
    # merge lower bounds 
    cur_kriged_slice_lowerbound <- st_as_stars(lower_bound)
    if (!is.null(kriged_slices_lowerbound)) {
      kriged_slices_lowerbound <- c(kriged_slices_lowerbound, cur_kriged_slice_lowerbound)
    } else {
      kriged_slices_lowerbound <- cur_kriged_slice_lowerbound
    }
    
    # merge differences
    cur_kriged_slice_difference <- st_as_stars(difference)
    if (!is.null(kriged_slices_difference)) {
      kriged_slices_difference <- c(kriged_slices_difference, cur_kriged_slice_difference)
    } else {
      kriged_slices_difference <- cur_kriged_slice_difference
    }
  }
  
  # redimension pred
  kriged_slices_pred <- st_redimension(kriged_slices_pred)
  kriged_slices_pred <- st_set_dimensions(kriged_slices_pred, 
                                          which = 3, 
                                          names="time", 
                                          values=timeCodes)
  attributes(kriged_slices_pred)$names <-  "mean_temp_pred"
  plot(kriged_slices_pred, col = rev(heat.colors(20)))
  
  # redimension var
  kriged_slices_var <- st_redimension(kriged_slices_var)
  kriged_slices_var <- st_set_dimensions(kriged_slices_var, 
                                         which = 3, 
                                         names="time", 
                                         values=timeCodes)
  attributes(kriged_slices_var)$names <- "mean_temp_variance"
  
  # redimension lower bound
  kriged_slices_lowerbound <- st_redimension(kriged_slices_lowerbound)
  kriged_slices_lowerbound <- st_set_dimensions(kriged_slices_lowerbound, 
                                                which = 3, 
                                                names="time", 
                                                values=timeCodes)
  attributes(kriged_slices_lowerbound)$names <- "mean_temp_lowerbound"
  
  plot(kriged_slices_lowerbound, col = rev(heat.colors(20)))
  
  # redimension upper bound
  kriged_slices_upperbound <- st_redimension(kriged_slices_upperbound)
  kriged_slices_upperbound <- st_set_dimensions(kriged_slices_upperbound, 
                                                which = 3, 
                                                names="time", 
                                                values=timeCodes)
  attributes(kriged_slices_upperbound)$names <- "mean_temp_upperbound"
  
  # redimension difference
  kriged_slices_difference <- st_redimension(kriged_slices_difference)
  kriged_slices_difference <- st_set_dimensions(kriged_slices_difference, 
                                                which = 3, 
                                                names="time", 
                                                values=timeCodes)
  attributes(kriged_slices_difference)$names <- "mean_temp_difference"
  
  # Configure x and y dimensions of the upper+lower+diff attributes to match the interpolation grid
  st_crs(kriged_slices_upperbound) <- st_crs(kriged_slices_pred)
  st_crs(kriged_slices_lowerbound) <- st_crs(kriged_slices_pred)
  st_crs(kriged_slices_difference) <- st_crs(kriged_slices_pred)
  
  kriged_slices_upperbound <- st_set_dimensions(kriged_slices_upperbound, which = "x", values = st_get_dimension_values(kriged_slices_pred, "x", center = FALSE))
  kriged_slices_upperbound <- st_set_dimensions(kriged_slices_upperbound, which = "y", values = st_get_dimension_values(kriged_slices_pred, "y", center = FALSE))
  kriged_slices_lowerbound <- st_set_dimensions(kriged_slices_lowerbound, which = "x", values = st_get_dimension_values(kriged_slices_pred, "x", center = FALSE))
  kriged_slices_lowerbound <- st_set_dimensions(kriged_slices_lowerbound, which = "y", values = st_get_dimension_values(kriged_slices_pred, "y", center = FALSE))
  kriged_slices_difference <- st_set_dimensions(kriged_slices_difference, which = "x", values = st_get_dimension_values(kriged_slices_pred, "x", center = FALSE))
  kriged_slices_difference <- st_set_dimensions(kriged_slices_difference, which = "y", values = st_get_dimension_values(kriged_slices_pred, "y", center = FALSE))
  
  # Restore crs (gets lost after st_set_dimensions?)
  st_crs(kriged_slices_upperbound) <- st_crs(kriged_slices_pred)
  st_crs(kriged_slices_lowerbound) <- st_crs(kriged_slices_pred)
  st_crs(kriged_slices_difference) <- st_crs(kriged_slices_pred)
  
  # re-combine predictions and variance
  kriged_slices <- c(kriged_slices_pred,
                     kriged_slices_var)
  
  # to allow merging with uncertainty attributes, set point info to FALSE
  attr(kriged_slices, "dimensions")$x$point = FALSE
  attr(kriged_slices, "dimensions")$y$point = FALSE
  
  # re-combine upper and lower bounds
  kriged_slices <- c(kriged_slices,
                     kriged_slices_lowerbound,
                     kriged_slices_upperbound,
                     kriged_slices_difference)
  print(kriged_slices)
  
  # crop to spain LL region
  kriged_slices <- st_crop(kriged_slices, andalucia)
  
  # Assign the kriged_slices to the variable with the input year
  kriged_slices_name <- paste0("kriged_slices_", year)
  assign(kriged_slices_name, kriged_slices, env=.GlobalEnv)
  
  #return(get(kriged_slices_name))
}

# Run kriging interpolation for all years for temp
for (year in years) {
  sliced_krige_withUnc(year)
}

cat("Interpolations finished for maps 1-3 \n")

cat("Performing caluclations on stars objects... \n")

# Store all data from month June for all years in one stars object to do some computations
# depends on config variable 'years' in global.R
combined_stars_data <- get(paste0("kriged_slices_", years[1]))[, , , 6]
for (i in 2:length(years)) {
  year <- years[i]
  dataset_name <- get(paste0("kriged_slices_", year))[, , , 6]
  combined_stars_data <- c(combined_stars_data, dataset_name)
}

mean_temps_june <- st_apply(combined_stars_data, c("x", "y"), mean)
min_temps_june <- st_apply(combined_stars_data, c("x", "y"), min)
max_temps_june <- st_apply(combined_stars_data, c("x", "y"), max)

# Example visualization for one dataset
map_data <- get(paste0("kriged_slices_", years[1]))
leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addMouseCoordinates() |>
  addStarsImage(map_data["mean_temp_pred", , , 6], layerId = "Temperature", colors = pal, opacity = 0.7) |>
  addLegend(pal = pal_legend, values = 18:30, title = "Temperature (°C)", position = "bottomright", opacity = 1,
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) |>
  addPolygons(data = andalucia, fill = FALSE, color = "red", weight = 2)

#### Save relevant items as .RData file ####
kriged_slices_list <- c()
for (year in years) {
  kriged_slices_list <- c(kriged_slices_list, (paste0("kriged_slices_", year)))
}
save(list = kriged_slices_list, file = "kriged_slices_data.RData", compress = FALSE)

kriged_means <- c("mean_temps_june", "min_temps_june", "max_temps_june")
save(list = kriged_means, file = "kriged_means.RData", compress = FALSE)
