# Load necessary libraries
library(sf)
library(ggplot2)
library(raster)
library(rgeos)
library(terra)      # Faster spatial processing
library(stars)      # For st_rasterize
library(tidyverse)  # Includes readr, dplyr, ggplot2, etc.
library(leaflet)    # For OSM mapping
library(yaml)       # For YAML configuration

# Connect to YAML file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir)) # One level up in directory
config_path <- file.path(config_dir, "config_06.yml")

# Read YAML configuration file
config <- yaml::yaml.load_file(config_path)

# Define the parent directory (move four levels up)
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))

# Paths for input data based on YAML configuration
amsterdam100m_grid_dir <- normalizePath(file.path(parent_directory, config$input_data$amsterdam100m_grid), winslash = "/")
amsterdam_predicting_no2_dir <- normalizePath(file.path(parent_directory, config$input_data$amsterdam_predicting_no2_allmodels), winslash = "/")

# Define output directory
out_location_dir <- normalizePath(file.path(parent_directory, config$out_location), winslash = "/")

# Import geodata (Area of Interest at 100m resolution)
grid <- st_read(amsterdam100m_grid_dir)

# Data processing

# Make grid data spatial (if not already in sf format)
grid_sf <- st_as_sf(grid)

# Load model predictions (using read_csv from readr, part of tidyverse)
model_predictions <- read_csv(amsterdam_predicting_no2_dir)

# Print first 3 rows for inspection
print(head(model_predictions, 3))

# Convert model predictions to sf object (setting appropriate CRS)
model_predictions_sf <- st_as_sf(model_predictions, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform to the same CRS as the grid
model_predictions_sf <- st_transform(model_predictions_sf, crs = st_crs(grid_sf))

# Join predictions with grid based on nearest feature
amsterdam_no2_predictions <- st_join(grid_sf, model_predictions_sf, join = st_nearest_feature)
amsterdam_no2_predictions <- amsterdam_no2_predictions %>%
  select(c(
            "nightlight_450", "nightlight_3150", "population_1000", 
            "population_3000", "road_class_2_25", "road_class_3_3000", 
            "road_class_3_300", "trop_mean_filt", "BldDen100", "NDVI", 
            "trafBuf25", "trafBuf50", "predicted_NO2_RF", "predicted_NO2_LASSO", 
            "predicted_NO2_RIDGE", "predicted_NO2_LightGBM", "predicted_NO2_XGBoost", 
            "geometry"))

# Export the result as a GeoPackage
st_write(amsterdam_no2_predictions, dsn = file.path(out_location_dir, 'Amsterdam_NO2PredictionPerModel2.gpkg'), driver = "GPKG")

