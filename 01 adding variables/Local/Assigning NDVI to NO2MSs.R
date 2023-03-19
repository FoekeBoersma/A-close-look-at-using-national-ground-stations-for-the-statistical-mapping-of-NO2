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

## == IMPORT RASTER (TIF) WITH NDVI VALUES == ##

#put tif files into list 
cur <- setwd("C:/Users/foeke/OneDrive/Documenten/april onwards/2022/tifs")
#create list of all files in defined directory
a = list.files(cur, pattern='.tif$')
#examine files in list
print(a)
#concatenate multiple vectors to single vector via "stack"-function.
b = stack(a)
st_crs(b)


## == IMPORT SPATIAL POINT DATASET == ##

#import csv (no2 measurement station data)
ms <- read.csv(file = 'C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForModelling/LocalModels/LocalModelData.csv', sep= ",")

# make the SpatialPointsDataFrame object
coords <- ms[, c("long", "lat")]
st_crs(b)
crs <- CRS("+proj=longlat +datum=WGS84")

ms$M_id <- seq(1, nrow(ms))
# ms_df <- as.data.frame(ms)

print(ms)

#filter only relevant data
ms <- ms %>% dplyr::select(M_id, long, lat)

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
points_NDVI <- st_as_sf(points_NDVI, coords=c("long", "lat"))
st_crs(points_NDVI) <- "+proj=longlat +datum=WGS84"

#rename column to "NDVI"
points_NDVI <- points_NDVI %>% rename(NDVI = mod13q1)

#visualize via tmap
tm_shape(points_NDVI)+
  tm_dots("NDVI")

#export option
sf::st_write(points_NDVI, dsn="C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-Local/NDVI", layer='ms_NDVI', driver = "ESRI Shapefile")
