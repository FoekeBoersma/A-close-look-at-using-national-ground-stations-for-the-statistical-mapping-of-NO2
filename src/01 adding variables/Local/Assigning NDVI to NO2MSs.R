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

## == connect with yaml file == ##

#connect to yaml file
current_dir <- rstudioapi::getActiveDocumentContext()$path
# Move one level up in the directory
config_dir <- dirname(dirname(current_dir))
# Construct the path to the YAML configuration file
config_path <- file.path(config_dir, "config.yml")
# Read the YAML configuration file
config <- yaml.load_file(config_path)

## == IMPORT RASTER (TIF) WITH NDVI VALUES == ##
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
ndvi_tif_dir <- config$global$ndvi_map
ndvi_tif_map <- normalizePath(file.path(parent_directory, ndvi_tif_dir ), winslash = "/")
#put tif files into list 
cur <- setwd(ndvi_tif_map)
#create list of all files in defined directory
# a = list.files(cur, pattern='.tif$')
a = list.files(cur, pattern='.tif$', full.names = TRUE)
#examine files in list
print(a)
#concatenate multiple vectors to single vector via "stack"-function.
b = stack(a)

## == define output path == ##
out_location <- config$out_location
out_location_dir <- normalizePath(file.path(parent_directory, out_location ), winslash = "/")
## == IMPORT SPATIAL POINT DATASET == ##

#import csv (no2 measurement station data)

no2_dataset <- config$local$no2
no2_map_dir <- normalizePath(file.path(parent_directory, no2_dataset ), winslash = "/")
ms_stations <- read.csv(file = no2_map_dir, sep= ",")

# make the SpatialPointsDataFrame object
coords <- ms_stations[, c("long", "lat")]
st_crs(b)
crs <- CRS("+proj=longlat +datum=WGS84")

ms_stations$M_id <- seq(1, nrow(ms_stations)) #assign unique identifier per measurement station - M_id

#filter only relevant data
ms_stations <- ms_stations %>% dplyr::select(M_id, long, lat)

spdf <- SpatialPointsDataFrame(coords      = coords,
                               data        = ms_stations, 
                               proj4string = crs)

## == ASSIGNING NDVI RASTER VALUES TO POINT DATASET == ##
#make crs of input datasets similar
st_poi <- st_as_sf(spdf)
st_poi <- st_transform(st_poi, crs = st_crs(b))

#extract raster NDVI values to points (no2 measurement stations)
points_NDVI <- raster::extract(b, st_poi, sp=TRUE) #sp=TRUE retains information of points dataset

#convert to crs = wgs84
points_NDVI <- as.data.frame(points_NDVI)
points_NDVI <- st_as_sf(points_NDVI, coords=c("long", "lat"))
st_crs(points_NDVI) <- "+proj=longlat +datum=WGS84"

#rename column to "NDVI"
points_NDVI <- points_NDVI %>% rename(NDVI = mod13q1)

#visualize via tmap
tm_shape(points_NDVI)+
  tm_dots("NDVI")

#select only relevant
points_NDVI <- points_NDVI %>% dplyr::select(NDVI, coords.x1, coords.x2)

#export option
sf::st_write(points_NDVI, dsn=out_location_dir, layer='ms_NDVI_local', driver = "ESRI Shapefile")
