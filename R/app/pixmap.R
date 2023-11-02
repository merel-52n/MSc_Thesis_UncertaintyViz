library(Vizumap)
library(rnaturalearth)

# Download Spain regions and crop to Andalucia
spain <- ne_states(country = "Spain", returnclass = "sf")
andalucia <- spain[spain$region == "Andalucía", ] |> select(name)
leaflet() |> 
  addProviderTiles(providers$CartoDB.Positron) |> 
  addPolygons(data = andalucia, fill = FALSE)

andalucia_sp <- andalucia |> 
  as_Spatial(cast = TRUE, IDs = paste0("ID", seq_along(from)))
pix <- pixelate(andalucia_sp, id = "region")

joined <- st_join(df_mean_tmp_2010, andalucia)
andalucia_data <- joined |>
  filter(month == "2010-06") |>
  group_by(name) |>
  summarize(
    mean_temp = mean(mean_temperature),
    standard_deviation = sd(mean_temperature)
  ) 

andalucia_data <- read.uv(data = as.data.frame(andalucia_data), estimate = "mean_temp", error = "standard_deviation")

df <- data.frame(region = sapply(slot(andalucia_sp, "polygons"), function(x) slot(x, "ID")), name = unique(andalucia_sp@data$name))
andalucia_data$region <- df[match(andalucia_data$name, df$name), 1]
andalucia_data$region <- as.character(andalucia_data$region)

# check that values in shared column match
all(andalucia_data$region %in% pix$region)

# uniform distribution
unifPixMap <- build_pmap(data = andalucia_data, distribution = "uniform", pixelGeo = pix, id = "region", border = andalucia_sp)
view(unifPixMap)
