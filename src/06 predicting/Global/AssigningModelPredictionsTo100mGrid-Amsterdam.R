#necessary modules
require(rgdal)
library(raster)
require(sf)
library(ggplot2)
library(raster)
library(rgeos)
library(tidyverse)
library(sp) #spatial operations
library(leaflet) #mapping in OSM
library(terra) #rasterize
library(stars) #necessary for st_rasterize
library(dplyr)
library(yaml)

#connect to yaml file
current_dir <- rstudioapi::getActiveDocumentContext()$path
# Move one level up in the directory
config_dir <- dirname(dirname(current_dir))
# Construct the path to the YAML configuration file
config_path <- file.path(config_dir, "config_06.yml")

# Read the YAML configuration file
config <- yaml.load_file(config_path)

# Use dirname() to get the parent directory
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))

amsterdam100m_grid <- config$input_data$amsterdam100m_grid
amsterdam100m_grid_dir <- normalizePath(file.path(parent_directory, amsterdam100m_grid ), winslash = "/")

amsterdam_predicting_no2_allmodels <- config$input_data$amsterdam_predicting_no2_allmodels
amsterdam_predicting_no2_allmodels_dir <- normalizePath(file.path(parent_directory, amsterdam_predicting_no2_allmodels ), winslash = "/")


## == define output path == ##
out_location <- config$out_location
out_location_dir <- normalizePath(file.path(parent_directory, out_location ), winslash = "/")


#IMPORT GEODATA

#import area of interest at 100m resolution
grid <- readOGR(amsterdam100m_grid_dir)

## == data processing == ##

#make spatial
grid_sf <- st_as_sf(grid)
#Modelpredictions
modelPredictions <- read.csv(amsterdam_predicting_no2_allmodels_dir)

# Print only the first 3 rows
print(head(modelPredictions, 3))

st

modelPredictions_sf <- st_as_sf(modelPredictions, coords=c("Longitude", "Latitude"), crs=4326)
#make crs similar
modelPredictions_sf <- st_transform(modelPredictions_sf, crs=st_crs(grid_sf))
Amsterdam_NO2PredictionPerModel <- st_join(grid_sf, modelPredictions_sf, join = st_nearest_feature)
Amsterdam_NO2PredictionPerModel <- Amsterdam_NO2PredictionPerModel %>% dplyr::select(-c( "fid",  "X",  "Unnamed..0"))
#export option
sf::st_write(Amsterdam_NO2PredictionPerModel, dsn = file.path(out_location_dir, 'Amsterdam_NO2PredictionPerModel.gpkg'), driver = "GPKG")
