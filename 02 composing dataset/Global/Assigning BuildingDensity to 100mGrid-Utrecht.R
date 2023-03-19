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

#IMPORT GEODATA

#import shapefile of buildingsNL 
buildings_32 <- readOGR('C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-BuildingDensity/polygonbuilding_32.shp')

#import area of interest
polygon <- readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Utrecht/AOI/polyUtrecht.gpkg')

#import area of interest at 100m resolution
grid <- readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Utrecht/AOI/grid100Utrecht.gpkg')


## == data processing == ##
#decrease size of buildings dataset
#make spatial dataframe
buildings_sf <- st_as_sf(buildings_32)
buildings_32 <- st_transform(buildings_sf, crs=st_crs(grid))
#clip buildings
class(grid)
grid_sf <- st_as_sf(grid)
clip_buildings <- buildings_32[grid_sf,]

#export option
#sf::st_write(clip_buildings, dsn='C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-SmallerGrids',layer='clipBuildings', driver = "ESRI Shapefile")


#create centroids in 100m grid
grd_centroids <- gCentroid(grid,byid=TRUE)

#export option
grd_centroids_sf <- st_as_sf(grd_centroids)
#unique ID where dissolve will be based on
grd_centroids_sf$cenID <- seq(1,nrow(grd_centroids_sf))

#sf::st_write(grd_centroids_sf, dsn='C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-SmallerGrids',layer='gridCentroids', driver = "ESRI Shapefile")


## == CREATE BUFFERS AROUND CENTROIDS == ##
#switch to planar coordinate system again
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs_32 <- CRS(sprintf(utmStr, 32))
grid_centroids_32 <- st_transform(grd_centroids_sf, crs=crs_32)

#create different buffers
centroids_100mBuf <- st_buffer(grid_centroids_32, 100)
#calculate surface (will be used later to calculate building density)
centroids_100mBuf$area <- st_area(centroids_100mBuf)

#export option
#sf::st_write(centroids_100mBuf, dsn='C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-SmallerGrids',layer='centroids_100mBuf', driver = "ESRI Shapefile")

## == calculate building density and assign to 100m grids/centroids == ##

#clip - building dataset will be assigned to buffers 
clip <- clip_buildings[centroids_100mBuf,]

#make clip spatial to export as shapefile
clip <- st_as_sf(clip)
#export option
#sf::st_write(clip, dsn="C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-SmallerGrids", layer='CLIP', driver = "ESRI Shapefile")

## == SELECT BUILDINGS WITHIN BUFFER X == ##
inter <- st_intersection(clip, centroids_100mBuf,  sp = TRUE)

#compute built area (m2) for each polygon in dataset
inter$BuiltArea <- st_area(inter)
#export option
sf::st_write(inter, dsn='C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Utrecht/AOI/interUtrecht.gpkg', driver = "GPKG")


