# prepare Andalucia shape as SpatialPolygonsDataFrame, as this is required by Vizumap package
andalucia_sp <- andalucia_states |> 
  as_Spatial(cast = TRUE, IDs = paste0("ID", seq_along(from)))
cat("Starting pixelation for map 4... \n")
pix <- pixelate(andalucia_sp, id = "region")
cat("Pixelation complete \n")

create_pixmap <- function(sp_data, pix) {
  ##
  # Function that creates a pixelated map using the "Vizumap" package
  # variable "sp_data" must be a SpatialPolygonsDataFrame containing polygons,
  # that are to be assigned values
  # variable "pix" is a "pixel_df", which must be created with the "Vizumap" package, see their vignette on how to create such a df.
  ##
  
  # Load kriging predictions for mean temperautre in June as sf object
  df_mean_tmp_year <- st_as_sf(mean_temps_june, as_points = TRUE, merge = FALSE)
  
  # Prepare the kriging predictions in the correct format
  joined <- st_join(df_mean_tmp_year, andalucia_states)
  data <- joined |>
    group_by(name) |>
    summarize(
      mean_temp = mean(mean_temp_pred),
      variance = mean(mean_temp_variance)
    )
  
  print(colnames(data))
  colnames(data) <- c("name", "Temperature (°C)", "variance", "geometry")
  data <- read.uv(data = as.data.frame(data), estimate = "Temperature (°C)", error = "variance")
  
  df <- data.frame(region = sapply(slot(sp_data, "polygons"), function(x) slot(x, "ID")), name = unique(sp_data@data$name))
  data$region <- df[match(data$name, df$name), 1]
  data$region <- as.character(data$region)
  
  # Check that values in the shared column match
  all(data$region %in% pix$region)
  
  # Bootstrap values using uniform distribution and assign values for each polygon
  unifPixMap <- build_pmap(data = data, distribution = "uniform", pixelGeo = pix, id = "region", border = andalucia_sp, palette = "Reds")
  pixed_map <- paste0("unifPixMap")
  assign(pixed_map, unifPixMap, envir = .GlobalEnv)
}

create_pixmap(andalucia_sp, pix)

save(unifPixMap, file = "unifPixMap.RData", compress = FALSE)
