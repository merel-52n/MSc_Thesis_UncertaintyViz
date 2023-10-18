# Set the path to the geopackage files
geopackage_path <- "/home/merel/Documents/I-CISK/mockup-viz/data"

# Load all GeoPackage files with meteorological data from meteo-RIA API from 2010 until 2022

for (year in 2010:2022) {
  filename <- paste0("meteo_andalucia_", year, ".gpkg")
  filepath <- file.path(geopackage_path, filename)
  df_name <- paste0("df_", year)
  assign(df_name, st_read(filepath))
}

# Create a list with the names of all the dataframes
df_list <- list(df_2010, df_2011, df_2012, df_2013, df_2014, df_2015, df_2016, df_2017, df_2018, df_2019, df_2020, df_2021, df_2022)

# Merge the data frames using bind_rows
df_2010_2022 <- do.call(bind_rows, df_list)

# Convert the 'timestamp' column to a date format
df_2010_2022$timestamp <- as.Date(df_2010_2022$timestamp)

# Extract the month and year from the timestamp
df_2010_2022$year <- format(df_2010_2022$timestamp, "%Y")
df_2010_2022$month <- format(df_2010_2022$timestamp, "%Y-%m")

# Group the data by year and calculate the mean temperature
yearly_means_2010_2022 <- df_2010_2022 |> 
  group_by(year, station_name) |>
  summarise(mean_temperature = mean(mean_temperature, na.rm = TRUE)) |>
  as.data.frame()

# Group the data per month and calculate the mean temperature per month
monthly_means_2010_2022 <- df_2010_2022 |>
  group_by(month) |>
  summarise(mean_temperature = mean(mean_temperature, na.rm = TRUE)) |>
  as.data.frame()

# Load world country geometry as sf object with rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")
spain_cities <- data.frame(
  city = c("Malaga"),
  lat = c(36.7212746),
  lon = c(-4.4213988)
)
spain_cities <- st_as_sf(spain_cities, coords = c("lon", "lat"), remove = FALSE, 
                         crs = 4326, agr = "constant")

# Download ocean layer
ocean <- ne_download(category = "physical", scale = "medium", type = "ocean", returnclass = "sf")

# Add names of seas
ocean_names <- data.frame(
  name = c("Alboran Sea", "Gulf of Cádiz"),
  lon = c(-3.5, -7.8),
  lat = c(35.8, 36.0)
)

# Take subset of 2010 for plotting
yearly_means_filtered <- filter(yearly_means_2010_2022, year == 2010)

# Simplified function to plot station geometry
plot_station_locations <- function() {
  plot <- ggplot(world) + 
    geom_sf() +
    geom_sf(data = ocean, fill = "lightblue") +
    geom_text(data = ocean_names, aes(x = lon, y = lat, label = name),
              color = "darkblue", size = 3, angle = c(15, -10)) +
    geom_sf(data = yearly_means_filtered$geom) +
    #geom_sf(data = spain_cities, col = "red") +
    #geom_text_repel(data = spain_cities, aes(x = lon, y = lat, label = city), 
                    #fontface = "bold", nudge_x = c(-1, 0, -0.25, -1, 0), nudge_y = c(1, -0.75, -0.5, -0.5, 0)) +
    geom_text(aes(x = -4, y = 39.5, label = "Spain"), size = 5, color = "black") +
    coord_sf(xlim = c(-10.248333, 0.2297222), ylim = c(34.285, 40.49611), expand = FALSE) +
    ggtitle("Meteorological stations in Andalusia, Spain")
  return(plot)
}

plot_station_locations()

ggsave("/home/merel/Documents/I-CISK/MSc_Thesis_UncertaintyViz/stations.png", plot_station_locations(), width = 10, height = 8,  dpi = 1200)
