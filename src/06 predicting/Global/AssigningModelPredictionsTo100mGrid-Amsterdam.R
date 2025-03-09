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

# IMPORT GEODATA
# Import area of interest at 100m resolution
# amsterdam100m_grid_dir <- normalizePath(file.path(parent_directory, config$input_data$amsterdam100m_grid), winslash = "/")
amsterdam100m_grid_dir <- normalizePath(file.path(parent_directory, config$input_data$amsterdam100m_greater_area), winslash = "/")
grid <- st_read(amsterdam100m_grid_dir)

## == data processing == ##

#make spatial
grid_sf <- st_as_sf(grid)

#Modelpredictions
# modelPredictions <- read.csv(file.path(parent_directory, config$input_data$amsterdam_predicting_no2_allmodels))
modelPredictions <- read.csv(file.path(parent_directory, config$input_data$amsterdam_predicting_no2_allmodels_ga)) # greater Amsterdam area
modelPredictions_sf <- st_as_sf(modelPredictions, coords=c("Longitude", "Latitude"), crs=4326)
#make crs similar
modelPredictions_sf <- st_transform(modelPredictions_sf, crs=st_crs(grid_sf))
amsterdam_NO2PredictionPerModel <- st_join(grid_sf, modelPredictions_sf, join = st_nearest_feature)

print(head(amsterdam_NO2PredictionPerModel))

if ("fid" %in% names(amsterdam_NO2PredictionPerModel)) {
  amsterdam_NO2PredictionPerModel <- dplyr::select(amsterdam_NO2PredictionPerModel, -fid)
}
#export option
sf::st_write(amsterdam_NO2PredictionPerModel, dsn=file.path(out_location_dir, 'Amsterdam_NO2PredictionPerModel_ga.gpkg'), driver = "GPKG")
