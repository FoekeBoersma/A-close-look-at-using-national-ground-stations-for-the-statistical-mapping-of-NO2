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
config_path <- file.path(config_dir, "config_06.yml")
config <- yaml::yaml.load_file(config_path)

# Define output path
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
out_location_dir <- normalizePath(file.path(parent_directory, config$out_location), winslash = "/")

# IMPORT GEODATA
# Import area of interest at 100m resolution
utrecht100m_grid_dir <- normalizePath(file.path(parent_directory, config$input_data$utrecht100m_grid), winslash = "/")
grid <- readOGR(utrecht100m_grid_dir)

## == data processing == ##

#make spatial
grid_sf <- st_as_sf(grid)

#Modelpredictions
modelPredictions <- read.csv(file.path(parent_directory, config$input_data$utrecht_predicting_no2_allmodels))
modelPredictions_sf <- st_as_sf(modelPredictions, coords=c("Longitude", "Latitude"), crs=4326)
#make crs similar
modelPredictions_sf <- st_transform(modelPredictions_sf, crs=st_crs(grid_sf))
Utrecht_NO2PredictionPerModel <- st_join(grid_sf, modelPredictions_sf, join = st_nearest_feature)

#export option
sf::st_write(Utrecht_NO2PredictionPerModel, dsn=file.path(out_location_dir, 'Utrecht_NO2PredictionPerModel.gpkg'), driver = "GPKG")

