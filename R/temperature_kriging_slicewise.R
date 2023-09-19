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

# Set the path to the geopackage files
path <- "./data"

#### Load all GeoPackage files with meteorological data from meteo-RIA API from 2010 until 2022 ####
for (year in 2010:2022) {
  filename <- paste0("meteo_andalucia_", year, ".gpkg")
  filepath <- file.path(geopackage_path, filename)
  df_name <- paste0("df_mean_", year)
  tmpData <- st_read(filepath)
  tmpData$month <- format(tmpData$timestamp, "%Y-%m")
  tmp_mean <- tmpData |>
    group_by(month, station_name) |>
    summarise(mean_temperature = mean(mean_temperature, na.rm = TRUE)) |>
    na.omit() ## added this
  assign(df_name, tmp_mean)
}

#### define target grid ####
grd <- make_grid(df_mean_2012, res = 0.1) # change res as needed

#### for some slice ####

cur_month <- "2012-01"

cur_emp_vgm <- variogram(mean_temperature~1, df_mean_2012[df_mean_2012$month == cur_month,])
cur_mod_vgm <- fit.variogram(cur_emp_vgm, vgm("Lin"))

plot(cur_emp_vgm, cur_mod_vgm)

kriged_slices <- NULL
# krige
cur_kriged_slice <- krige(mean_temperature~1, df_mean_2012[df_mean_2012$month == cur_month,], grd, cur_mod_vgm)
plot(cur_kriged_slice, main = cur_month, col = rev(heat.colors(20)))

# iterate over all months
kriged_slices_pred <- NULL
kriged_slices_var  <- NULL

timeCodes <- format(as.POSIXct("2013-01-01")+0:11*31*24*3600, "%Y-%m")
for(cur_month in timeCodes) { # cur_month <- timeCodes[7]
  cat("Processing:", cur_month, "\n")
  cur_emp_vgm <- variogram(mean_temperature~1, df_mean_2013[df_mean_2013$month == cur_month,])
  cur_mod_vgm <- fit.variogram(cur_emp_vgm, vgm(4, "Sph", 150, 0.1))
  
  # krige
  cur_kriged_slice <- krige(mean_temperature~1, df_mean_2013[df_mean_2013$month == cur_month,], grd, cur_mod_vgm)
  
  ## some visual debuging code
  plot(cur_emp_vgm, cur_mod_vgm)
  plot(df_mean_2013[df_mean_2013$month == cur_month,])
  plot(cur_kriged_slice, col = rev(heat.colors(20)))
  
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

# Plot one of the results
spain <- ne_countries(scale = "medium", country = "Spain", continent = "Europe", returnclass = "sf")

# Crop out the canary islands
mainland_bbox <- c(xmin = -10, xmax = 5, ymin = 35, ymax = 44)
spain <- st_crop(spain, mainland_bbox)

g <- ggplot() + 
  geom_sf() +
  coord_equal() +
  scale_fill_viridis() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0))
g + geom_sf(data = st_cast(spain, "MULTILINESTRING")) +
  geom_stars(data = kriged_slices_pred, aes(fill = mean_temp_pred, x = x, y = y)) +
  coord_sf(lims_method = "geometry_bbox")

#### function form ##### 
sliced_krige <- function(year) {
  
  if (year < 2010 || year > 2022) {
    stop("Input year must be between 2010 and 2022")
  }
  # Construct the data frame name using 'year'
  df_name <- paste0("df_mean_", year)

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
    
    ## some visual debuging code
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

for (year in 2012:2014) {
  sliced_krige(year)  
}
sliced_krige(2010)

