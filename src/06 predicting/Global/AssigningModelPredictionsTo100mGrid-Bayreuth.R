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

#IMPORT GEODATA

#import area of interest at 100m resolution
grid <- readOGR('/TooBigData/grid100Bayreuth.gpkg')

## == data processing == ##

#make spatial
grid_sf <- st_as_sf(grid)

#Modelpredictions
modelPredictions <- read.csv('/TooBigData/SpatialPredictionPatterns/Predicting NO2-AllModelsBayreuth100_xy.csv')
modelPredictions_sf <- st_as_sf(modelPredictions, coords=c("Longitude", "Latitude"), crs=4326)
#make crs similar
modelPredictions_sf <- st_transform(modelPredictions_sf, crs=st_crs(grid_sf))
Bayreuth_NO2PredictionPerModel <- st_join(grid_sf, modelPredictions_sf, join = st_nearest_feature)

#export option
sf::st_write(Bayreuth_NO2PredictionPerModel, dsn='/TooBigData/Grid100/Bayreuth_NO2PredictionPerModel.gpkg', driver = "GPKG")
