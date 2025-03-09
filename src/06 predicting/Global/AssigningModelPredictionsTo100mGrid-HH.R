# Load necessary libraries
library(sf)
library(raster)
library(ggplot2)
library(tidyverse)
library(sp)
library(terra)
library(dplyr)
library(yaml)
library(nngeo)     # For finding nearest features

# Connect to YAML configuration file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir))
config_path <- file.path(config_dir, "config_06.yml")
config <- yaml::yaml.load_file(config_path)

# Define output path
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
out_location_dir <- normalizePath(file.path(parent_directory, config$out_location), winslash = "/")

# Import area of interest at 100m resolution
hamburg100m_grid_dir <- normalizePath(file.path(parent_directory, config$input_data$hamburg100m_grid), winslash = "/")
grid <- st_read(hamburg100m_grid_dir)

## == data processing == ##

#make spatial
grid_sf <- st_as_sf(grid)

#Modelpredictions
modelPredictions <- read.csv(file.path(parent_directory, config$input_data$hamburg_predicting_no2_allmodels))
modelPredictions_sf <- st_as_sf(modelPredictions, coords=c("Longitude", "Latitude"), crs=4326)
#make crs similar
modelPredictions_sf <- st_transform(modelPredictions_sf, crs=st_crs(grid_sf))
Hamburg_NO2PredictionPerModel <- st_join(grid_sf, modelPredictions_sf, join = st_nearest_feature)

#export option
sf::st_write(Hamburg_NO2PredictionPerModel, dsn=file.path(out_location_dir, 'Hamburg_NO2PredictionPerModel.gpkg'), driver = "GPKG")

