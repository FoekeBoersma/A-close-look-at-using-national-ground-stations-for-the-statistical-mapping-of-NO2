# Import necessary libraries
library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)
library(leaflet)
library(ggplot2)
library(GGally)
library(sp)
library(spatialEco)
library(yaml)

# Connect to YAML file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir)) # One level up in directory
config07_path <- file.path(config_dir, "config_07.yml")

# Read YAML configuration file
config07 <- yaml::yaml.load_file(config07_path)

# Define the parent directory (move four levels up)
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))

# Paths for input data based on YAML configuration
Amsterdam_NO2PredictionPerModel_dir <- normalizePath(file.path(parent_directory, config07$input_data$Amsterdam_NO2PredictionPerModel), winslash = "/")

# Import global dataset as an sf object, projected onto Amsterdam (if shapefile format is used)
global <- st_read(Amsterdam_NO2PredictionPerModel_dir)

global <- global %>%
  rename(
    predicted_NO2_RF = p_NO2_RF,
    predicted_NO2_LASSO = p_NO2_LA,
    predicted_NO2_RIDGE = p_NO2_RI,
    predicted_NO2_LightGBM = p_NO2_LG,
    predicted_NO2_XGBoost = p_NO2_X
  )

# Define output directory
out_location_dir <- normalizePath(file.path(parent_directory, config07$out_location), winslash = "/")

# Coordinates relating to Amsterdam
y <- 52.370216
x <- 4.852168
Amsterdam_point <- st_sfc(st_point(c(x, y)), crs = 4326) # Create point and set CRS as WGS84

# Convert Amsterdam point to planar coordinate system (EPSG 3035)
Amsterdam_point_3035 <- st_transform(Amsterdam_point, crs = 3035)

# Define rectangle around Amsterdam (specify extent)
rect_around_point <- function(x, xsize, ysize) {
  bbox <- st_bbox(x)
  bbox <- bbox + c(xsize / 2, ysize / 2, -xsize / 2, -ysize / 2)
  return(st_as_sfc(bbox))
}

Amsterdam_square_buffer <- rect_around_point(Amsterdam_point_3035, 30000, 30000)

# Ensure 'global' data matches Amsterdam_square_buffer CRS
global <- st_transform(global, crs = st_crs(Amsterdam_square_buffer))

# Spatial query - assign the data to the extent of Amsterdam
sp_query_Amsterdam <- spatial.select(Amsterdam_square_buffer, y = global, predicate = "contains")

sp_query_Amsterdam

# Map and export results
vars <- c("predicted_NO2_RF", "predicted_NO2_LASSO", "predicted_NO2_RIDGE", "predicted_NO2_LightGBM", "predicted_NO2_XGBoost")
breaks <- c(-100, 0, 15, 20, 25, 30, 35, 40, 45, 50, 100, 1000)
palette_colors <- c("#808080", "#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026" , "#808080")

for (model in vars) {
map <- tm_shape(sp_query_Amsterdam) + tm_polygons(fill = model, 
lwd = 0,  lwd.free = NA, palette = palette_colors, breaks=breaks, lwd.scale = tm_scale()) + tm_layout(legend.show = FALSE)  # Remove the legend
tmap_save(map, 
  width = 2000, 
  height = 2000, 
  units = "px", 
  filename = file.path(out_location_dir, paste0("Global_", model, ".jpg")))
}
# # Loop through the shapefiles and create a map for each
# for (model in vars) {
  
#   # Create the map
#   map <- tm_shape(sp_query_Amsterdam) + 
#     tm_borders(lwd = 0.5, col = "black", alpha = 0.1) +  # Adjust border transparency
#     tm_fill(col = model, palette = palette_colors, breaks = breaks, style = "fixed", 
#             fill_alpha = 0.7) +  # Set transparency for fill color
#     tm_layout(main.title = paste("NO2 Prediction -", model), 
#               legend.show = FALSE,  # Hide the legend
#               frame = FALSE)  # Remove the map frame
  
#   # Save the map as a JPG file
#   tmap_save(map, 
#             width = 1000, 
#             height = 1000, 
#             units = "px", 
#             filename = file.path(out_location_dir, paste0("Global_", model, ".jpg")))
# }