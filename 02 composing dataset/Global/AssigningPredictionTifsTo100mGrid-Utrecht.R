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

#CRS
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs_32 <- CRS(sprintf(utmStr, 32))

#IMPORT GEODATA

#import area of interest at 100m resolution
grid <- readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Utrecht/AOI/grid100Utrecht.gpkg')

## == data processing == ##

#make spatial
grid_sf <- st_as_sf(grid)
# #switch to planar coordinate system 
# grid_32 <- st_transform(grid_sf, crs=crs_32)
# #create centroids in 100m grid
# grid_centroids <- gCentroid(grid,byid=TRUE)
# #to spatial dataframe
# grid_centroids_sf <- st_as_sf(grid_centroids)
# #unique ID where dissolve will be based on
# grid_centroids_sf$cenID <- seq(1,nrow(grid_centroids_sf))
# #switch to planar coordinate system 
# grid_centroids_32 <- st_transform(grid_centroids_sf, crs=crs_32)

#Modelpredictions
modelPredictions <- read.csv('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Utrecht/Predicting NO2-AllModelsUtrecht100_xy.csv')
modelPredictions_sf <- st_as_sf(modelPredictions, coords=c("Longitude", "Latitude"), crs=4326)
#make crs similar
modelPredictions_sf <- st_transform(modelPredictions_sf, crs=st_crs(grid_sf))
Hamburg_NO2PredictionPerModel <- st_join(grid_sf, modelPredictions_sf, join = st_nearest_feature)


#export option
sf::st_write(Hamburg_NO2PredictionPerModel, dsn='C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Utrecht/Utrecht_NO2PredictionPerModel.gpkg', driver = "GPKG")
