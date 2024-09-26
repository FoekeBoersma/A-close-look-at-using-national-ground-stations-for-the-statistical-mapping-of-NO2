# Load necessary libraries
library(sf)
library(raster)
library(ggplot2)
library(rgeos)
library(tidyverse)
library(sp)
library(leaflet)
library(terra)
library(stars)
library(dplyr)
library(yaml)
library(rgdal)
library(nngeo)     # For finding nearest features

# Connect to YAML configuration file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir))
config_path <- file.path(config_dir, "config_04.yml")
config <- yaml::yaml.load_file(config_path)

# Define output path
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
out_location_dir <- normalizePath(file.path(parent_directory, config$out_location), winslash = "/")

# IMPORT GEODATA
# Import area of interest at 100m resolution
amsterdam100m_grid_dir <- normalizePath(file.path(parent_directory, config$input_data$amsterdam100m_grid), winslash = "/")
grid <- readOGR(amsterdam100m_grid_dir)

# Data processing
grid_sf <- st_as_sf(grid)  # Convert grid to sf object
grid_3035 <- st_transform(grid_sf, crs = 3035)  # Project to EPSG: 3035

# Create centroids for each grid cell
grid_centroids <- gCentroid(grid, byid = TRUE)
grid_centroids_sf <- st_as_sf(grid_centroids)  # Convert to sf object
grid_centroids_sf$cenID <- seq(1, nrow(grid_centroids_sf))  # Assign unique ID
grid_centroids_3035 <- st_transform(grid_centroids_sf, crs = 3035)  # Project to planar coordinate system

# Step 1: Create buffers around centroids (100m radius)
centroid_layer <- st_buffer(grid_centroids_3035, dist = 100)  # 100m buffer

# Load buildings data
buildings_path <- normalizePath(file.path(parent_directory, config$input_data$polygonbuilding_studyArea), winslash = "/")
buildings <- st_read(buildings_path)  # Load the buildings shapefile
buildings_3035 <- st_transform(buildings, crs = 3035)  # Ensure same projection as grid and centroids

# Step 2: Clip buildings by each centroid's buffer (spatial join)
buildings_in_buffer <- st_intersection(buildings_3035, centroid_layer)  # Find buildings inside each buffer

# Step 3: Calculate the area of the buildings within each buffer
buildings_in_buffer$building_area <- st_area(buildings_in_buffer)  # Calculate building areas

# Step 4: Group by centroid ID and sum the building areas
building_area_per_buffer <- buildings_in_buffer %>%
  group_by(cenID) %>%
  summarize(total_building_area = sum(building_area))

# Step 5: Calculate the area of each 100m buffer (circle area = pi * r^2)
buffer_area <- pi * (100)^2  # Area of the 100m radius buffer

# Step 6: Calculate building density (total building area / buffer area) and assign it to the grid centroids
grid_centroids_df <- as.data.frame(grid_centroids_3035)  # Convert to data frame for joining
grid_centroids_df <- grid_centroids_df %>%
  left_join(building_area_per_buffer, by = "cenID") %>%
  mutate(bld_den100m = as.numeric(total_building_area) / buffer_area)  # Calculate building density

# Step 7: Replace NA values (for buffers with no buildings) with 0
grid_centroids_df$bld_den100m[is.na(grid_centroids_df$bld_den100m)] <- 0

grid_sf <- st_as_sf(grid_centroids_df)

# Step 8: Save the result as a shapefile
output_shapefile_path <- file.path(out_location_dir, 'grid_bldden_100m.shp')
sf::st_write(grid_sf, dsn = output_shapefile_path, driver = "ESRI Shapefile")

