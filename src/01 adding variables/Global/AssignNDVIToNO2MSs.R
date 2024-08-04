#necessary libraries
library(rgdal)
library(raster)
require(sf)
library(ggplot2)
library(raster)
library(rgeos)
library(tidyverse)
library(leaflet)
library(sp)
library(tmap) #for visualization purposes
library(bnspatial)
library(yaml)
library(rstudioapi)

## == IMPORT RASTER (TIF) WITH NDVI VALUES == ##
#connect to yaml file
current_dir <- rstudioapi::getActiveDocumentContext()$path
# Move one level up in the directory
config_dir <- dirname(dirname(current_dir))
# Construct the path to the YAML configuration file
config_path <- file.path(config_dir, "config.yml")
# Read the YAML configuration file
config <- yaml.load_file(config_path)

# Use dirname() to get the parent directory
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))

# Extract the NDVI map directory path from the config and correct the path format
ndvi_map_relative <- config$global$ndvi_map

# Construct the full path to the NDVI map directory based on the config file location
ndvi_map_dir <- normalizePath(file.path(parent_directory, ndvi_map_relative), winslash = "/")

# List .tif files in the directory with full paths
a <- list.files(ndvi_map_dir, pattern = '\\.tif$', full.names = TRUE)

#concatenate multiple vectors to single vector via "stack"-function.
b = stack(a)

## == import no2 dataset == ##
no2_dataset <- config$global$no2
no2_map_dir <- normalizePath(file.path(parent_directory, no2_dataset ), winslash = "/")

## == define output path == ##
out_location <- config$out_location
out_location_dir <- normalizePath(file.path(parent_directory, out_location ), winslash = "/")

## == IMPORT NO2 MEASUREMENT STATIONS == ##

#import csv (no2 measurement station data)
ms <- read.csv(file = no2_map_dir, sep = ";")
# make the SpatialPointsDataFrame object
coords <- ms[, c("Longitude", "Latitude")]
crs <- CRS("+proj=longlat +datum=WGS84") #define crs
ms <- as.data.frame(ms) #make dataframe

#filter only relevant data
ms <- ms %>% select(FID, Longitude, Latitude)

#make spatial points dataframe as several operations require this format
spdf <- SpatialPointsDataFrame(coords      = coords,
                               data        = ms, 
                               proj4string = crs)

## == ASSIGNING NDVI RASTER VALUES TO POINT DATASET == ##
#make crs of input datasets similar
st_poi <- st_as_sf(spdf)
st_poi <- st_transform(st_poi, crs = st_crs(b))

#extract raster NDVI values to points (no2 measurement stations)
points_NDVI <- raster::extract(b, st_poi, sp=TRUE) #sp=TRUE retains information of points dataset

#convert to crs = wgs84
points_NDVI <- as.data.frame(points_NDVI)
points_NDVI <- st_as_sf(points_NDVI, coords=c("Longitude", "Latitude"))
st_crs(points_NDVI) <- "+proj=longlat +datum=WGS84"

#rename column to "NDVI"
points_NDVI <- points_NDVI %>% rename(NDVI = mod13q1)

#visualize via tmap
tm_shape(points_NDVI)+
  tm_dots("NDVI")

#export option
sf::st_write(points_NDVI, dsn=out_location_dir, layer='ms_NDVI_global', driver = "ESRI Shapefile")

