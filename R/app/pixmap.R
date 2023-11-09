# prepare Andalucia shape as SpatialPolygonsDataFrame, as this is required by Vizumap package
andalucia_sp <- andalucia_states |> 
  as_Spatial(cast = TRUE, IDs = paste0("ID", seq_along(from)))
cat("Starting pixelation for map 4... \n")
pix <- pixelate(andalucia_sp, id = "region")
cat("Pixelation complete \n")

create_pixmap <- function(year, sp_data, pix) {
  ##
  # Function that creates a pixelated map using the "Vizumap" package
  # variable "year" can be between 2010 and 2022
  # variable "sp_data" must be a SpatialPolygonsDataFrame containing polygons,
  # that are to be assigned values
  # variable "pix" is a "pixel_df", which must be created with the "Vizumap" package, see their vignette on how to create such a df.
  ##
  df_mean_tmp_year <- get(paste("df_mean_tmp_", year, sep = ""))
  
  # Filter the data based on the year, select june from each dataset
  joined <- st_join(df_mean_tmp_year, andalucia_states)
  data <- joined |>
    filter(month == paste(year, "-06", sep = "")) |>
    group_by(name) |>
    summarize(
      mean_temp = mean(mean_temperature),
      standard_deviation = sd(mean_temperature)
    )
  
  data <- read.uv(data = as.data.frame(data), estimate = "mean_temp", error = "standard_deviation")
  
  df <- data.frame(region = sapply(slot(sp_data, "polygons"), function(x) slot(x, "ID")), name = unique(sp_data@data$name))
  data$region <- df[match(data$name, df$name), 1]
  data$region <- as.character(data$region)
  
  # check that values in the shared column match
  all(data$region %in% pix$region)
  
  # bootstrap values using uniform distribution
  unifPixMap <- build_pmap(data = data, distribution = "uniform", pixelGeo = pix, id = "region", border = andalucia_sp, palette = "Reds")
  pixed_map <- paste0("unifPixMap_", year)
  assign(pixed_map, unifPixMap, envir = .GlobalEnv)
}

for (year in years) {
  create_pixmap(year, andalucia_sp, pix)  
}
