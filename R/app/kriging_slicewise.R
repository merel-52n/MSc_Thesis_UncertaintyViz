# Set the path to the geopackage files
#setwd("/home/merel/Documents/I-CISK/MSc_Thesis_UncertaintyViz/R/app")
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
  
  # crop to andalucia LL shape
  kriged_slices <- st_crop(kriged_slices, andalucia)
  
  # Assign the kriged_slices to the variable with the input year
  kriged_slices_name <- paste0("kriged_slices_", year)
  assign(kriged_slices_name, kriged_slices, env=.GlobalEnv)
  
  #return(get(kriged_slices_name))
}

#### with prediction interval and differnce #####
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

# for (year in c(2010:2011, 2013:2022)) { # 2012 causes strange error?
#   sliced_krige_precip(year)
# }

#### Plotting ####

### 1. Get additional layers
# Get Spain and Portugal shape to plot alongside one of the results (use st_crop for croppping out Canary Islands + Azores)
# spain_mainland_bbox <- c(xmin = -10, xmax = 5, ymin = 35, ymax = 44)
# spain <- ne_countries(scale = "medium", country = "Spain", continent = "Europe", returnclass = "sf") |>
#   st_crop(spain_mainland_bbox) 
# 
# pt_mainland_bbox <- c(xmin = -9.52657060387, xmax = 36.838268541, ymin = -6.3890876937, ymax = 42.280468655)
# portugal <- ne_countries(scale = "medium", country = "Portugal", continent = "Europe", returnclass = "sf") |>
#   st_crop(pt_mainland_bbox)
# 
# # Download ocean layer to mask rectangle shape
# ocean <- ne_download(category = "physical", scale = "medium", type = "ocean", returnclass = "sf")

### 2. Plot kriging interpolation alongside above created layers
# plot_krige <- function(year, saveOption) {
#   data <- get(paste0("kriged_slices_", year))
#   
#   # Create a ggplot for each year
#   plot_krig <- ggplot() + 
#     geom_stars(data = data["mean_temp_pred", , , 6], aes(fill = mean_temp_pred, x = x, y = y)) +
#     scale_fill_viridis_c(name = "Predicted temperature (°C)", option = "C", direction = -1, na.value="transparent", limits=c(18,29)) +
#     geom_sf(data = st_cast(spain, "MULTILINESTRING")) +
#     geom_sf(data = ocean, fill = "lightblue") +
#     coord_sf(xlim=c(-9.248333, 0.2297222), ylim=c(34.285, 40.49611), expand = FALSE) +
#     ggtitle(paste0("Temperature predictions for July"))
#   
#   # Save the ggplot as a PNG file if desired
#   if (saveOption == TRUE) {
#     png_file <- paste0("./img/krige_animation_", year, ".png")
#     ggsave(filename = png_file, plot = plot_krig, width = 6, height = 6, units = "in")
#   }
#   return(plot_krig)
# }
# 
# ## Plot results and save as png
# for (year in 2010:2022) {
#  plot_krige(year, saveOption = TRUE)
# }
# 
# filepath = "./img"
# png_files = list.files(filepath)
# png_files = paste0(filepath, '/', png_files)
# delay = 0.5
# gifski(png_files = png_files, gif_file = "krige-animation.gif",
#        delay = delay,
#        progress = T)
# image_read("krige-animation.gif")

# Plot for 1 year

# ggplot() + 
#   geom_sf(data = spain) +
#   geom_stars(data = kriged_slices_2018["mean_temp_pred", , , 6], aes(fill = mean_temp_pred, x = x, y = y)) +
#   scale_fill_viridis_c(name = "Predicted temperature (°C)", option = "C", direction = -1, na.value="transparent", limits=c(18,29)) +
#   geom_sf(data = ocean, fill = "lightblue") +
#   geom_sf(data = portugal) +
#   coord_sf(xlim=c(-9.248333, 0.2297222), ylim=c(34.285, 40.49611), expand = FALSE)
#   #scale_fill_continuous(na.value="transparent")
# 
# ggplot() + 
#   geom_stars(data = kriged_slices_2010["mean_temp_pred"], aes(fill = mean_temp_pred, x = x, y = y)) +
#   facet_wrap(~time) +
#   geom_sf(data = st_cast(spain, "MULTILINESTRING")) +
#   geom_sf(data = ocean, fill = "lightblue") +
#   coord_sf(xlim=c(-9.248333, 0.2297222), ylim=c(34.285, 40.49611), expand = FALSE) +
#   scale_fill_continuous(na.value="transparent")

# interactive map version


# pal = hcl.colors(12, palette = "Inferno", rev = TRUE)
# pal_var = hcl.colors(12, palette = "Viridis")


# mapviewOptions(raster.palette = pal)

# temperature_map <- mapview(cropped_star["mean_temp_pred", , , 6], layer.name = "Temperature", na.color = NA, map.title = "July") |>
#   addLogo(img = "https://icisk.eu/wp-content/uploads/2022/01/icisk_logo_full.png", width=125, height=48)

map_data <- get(paste0("kriged_slices_", 2015))
leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addMouseCoordinates() |>
  addStarsImage(map_data["mean_temp_pred", , , 6], layerId = "Temperature", colors = pal, opacity = 0.7) |>
  addLegend(pal = pal_legend, values = 18:30, title = "Temperature (°C)", position = "bottomright", opacity = 1,
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  ) |>
  addPolygons(data = andalucia, fill = FALSE, color = "red", weight = 2)

# # leaflet version
# leaflet() |> 
#   addProviderTiles("OpenStreetMap") |> 
#   addStarsImage(cropped_star, col = pal, opacity = 0.8) |> 
#   addLegend(pal = pal, values = cropped_star["mean_temp_pred", , , 6]$mean_temp_pred) |> 
#   addLogo(img = "https://icisk.eu/wp-content/uploads/2022/01/icisk_logo_full.png", width=100, height=60)

# # sf version
# cropped <- st_crop(st_as_sf(kriged_slices_2010), st_as_sf(spain))
# ggplot() + 
#   geom_sf(data = cropped, aes(fill = mean_temp_pred.V1)) +
#   geom_sf(data = st_cast(spain, "MULTILINESTRING"))
#   


# # stars version
#library(mapview)

# library(leafem)
# leaflet() |> addProviderTiles("OpenStreetMap") |> addStarsImage(kriged_slices_2010)
#mapview(cropped_star)
# ggplot() + 
#   geom_sf(data = ocean, fill = "lightblue") +
#   geom_stars(data = cropped_star["mean_temp_pred"], aes(fill = mean_temp_pred, x = x, y = y)) +
#   geom_sf(data = st_cast(spain, "MULTILINESTRING")) +
#   coord_sf(xlim=c(-9.248333, 0.2297222), ylim=c(34.285, 40.49611), expand = FALSE) +
#   scale_fill_continuous(na.value="transparent")

#### stars appendix ####
# geom_stars plots only the 1st band...
# How to view 1 specific month
# See https://tmieno2.github.io/R-as-GIS-for-Economists/some-basic-operations-on-stars-objects.html
#kriged_slices_2010["mean_temp_pred", , , 6]$mean_temp_pred

# Get time values
#st_get_dimension_values(kriged_slices_2010, "time")

# #### function form ##### 
# sliced_krige_precip <- function(year) {
#   
#   if (year < 2010 || year > 2022) {
#     stop("Input year must be between 2010 and 2022")
#   }
#   # Construct the data frame name using 'year'
#   df_name <- paste0("df_mean_precip_", year)
#   
#   # iterate over all months
#   kriged_slices_pred <- NULL
#   kriged_slices_var  <- NULL
#   
#   timeCodes <- format(as.POSIXct(paste(year, "-01-01", sep=""))+0:11*31*24*3600, "%Y-%m")
#   for(cur_month in timeCodes) { # cur_month <- timeCodes[7]
#     cat("Processing:", cur_month, "\n")
#     cur_emp_vgm <- variogram(sum_precipitation~1, get(df_name)[get(df_name)$month == cur_month,])
#     cur_mod_vgm <- fit.variogram(cur_emp_vgm, vgm(4, "Sph", 150, 0.1))
#     
#     # krige
#     cur_kriged_slice <- krige(sum_precipitation~1, get(df_name)[get(df_name)$month == cur_month,], grd, cur_mod_vgm)
#     
#     ## some visual debugging code
#     #plot(cur_emp_vgm, cur_mod_vgm)
#     #plot(get(df_name)[get(df_name)$month == cur_month,])
#     #plot(cur_kriged_slice, col = rev(heat.colors(20)))
#     
#     # merge predictions
#     cur_kriged_slice_pred <- cur_kriged_slice["var1.pred",]
#     if (!is.null(kriged_slices_pred)) {
#       kriged_slices_pred <- c(kriged_slices_pred, cur_kriged_slice_pred)
#     } else {
#       kriged_slices_pred <- cur_kriged_slice_pred
#     }
#     
#     # merge kriging variances
#     cur_kriged_slice_var <- cur_kriged_slice["var1.var",]
#     if (!is.null(kriged_slices_var)) {
#       kriged_slices_var <- c(kriged_slices_var, cur_kriged_slice_var)
#     } else {
#       kriged_slices_var <- cur_kriged_slice_var
#     }
#   }
#   
#   kriged_slices_pred <- st_redimension(kriged_slices_pred)
#   kriged_slices_pred <- st_set_dimensions(kriged_slices_pred, 
#                                           which = 3, 
#                                           names="time", 
#                                           values=timeCodes)
#   attributes(kriged_slices_pred)$names <-  "sum_precip_pred"
#   plot(kriged_slices_pred, col = rev(topo.colors(20)))
#   
#   kriged_slices_var <- st_redimension(kriged_slices_var)
#   kriged_slices_var <- st_set_dimensions(kriged_slices_var, 
#                                          which = 3, 
#                                          names="time", 
#                                          values=timeCodes)
#   attributes(kriged_slices_var)$names <- "sum_precip_variance"
#   plot(kriged_slices_var)
#   
#   # re-combine predictions and variance
#   kriged_slices <- c(kriged_slices_pred,
#                      kriged_slices_var)
#   
#   # crop to spain mainland
#   kriged_slices <- st_crop(kriged_slices, st_as_sf(spain))
#   
#   # Assign the kriged_slices to the variable with the input year
#   kriged_slices_name <- paste0("kriged_slices_precip_", year)
#   assign(kriged_slices_name, kriged_slices, env=.GlobalEnv)
#   
#   #return(get(kriged_slices_name))
# }

