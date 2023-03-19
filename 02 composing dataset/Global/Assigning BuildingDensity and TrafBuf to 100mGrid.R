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
polygon <- readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Hamburg/AOI/polyHH.gpkg')

#import area of interest at 100m resolution
grid <- readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Hamburg/AOI/grid100HH.gpkg')


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
sf::st_write(inter, dsn='C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Hamburg/AOI/interHH.gpkg', driver = "GPKG")

#dissolve by common ID - polygons with similar buffer ID will be merged
#the area of these polygons will be aggregated via "SUM"
# dis <- inter %>% group_by(cenID) %>% summarize(sum(BuiltArea))

#export option
# sf::st_write(dis, dsn="C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-SmallerGrids", layer='DIS', driver = "ESRI Shapefile")
# 
# #import DIS, to avoid high computational time and power!
# dis <- readOGR('C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-SmallerGrids/DIS.shp')
# dis_BldDen <- as.data.frame(dis)
# print(dis_BldDen)
# dis_BldDen <- dis_BldDen %>% rename(BuiltArea = SUM_BuiltA)
# # dis_BldDen$area <- pi*(100)**2
# # view(dis_BldDen)
# #merge datasets via left_join
# #make normal dataframe to perform left_join
# grid_centroids_df <- as.data.frame(grd_centroids_sf)
# #assign info based on common M_id
# merge = left_join(grid_centroids_df, dis_BldDen, by = "cenID")
# 
# #create new column "building_density" dividing building surface per buffer by
# #total buffer surface
# 
# print(merge)
# merge$BldDen100 <- merge$BuiltArea/merge$MEAN_area
# #replace NA by 0 
# merge[is.na(merge)] <- 0
# view(merge)
# 
# #export option
# sf::st_write(merge, dsn="C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-SmallerGrids", layer='Centroids100_BldDen', driver = "ESRI Shapefile")
# #to wgs84
# st_crs(polygon)
# merge_sf <- st_as_sf(merge)
# merge84 <- st_transform(merge_sf, crs=st_crs(polygon))
# #export option
# sf::st_write(merge84, dsn="C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-SmallerGrids", layer='Centroids100_BldDen_wgs84', driver = "ESRI Shapefile")
# 
# 
# ## == assign to grid == ##
# #make grid as sf dataframe
# Grid100m_sf <- st_as_sf(grid)
# merge_sf <- st_as_sf(merge)
# #spatial join
# Grid100_BldDen <- st_join(Grid100m_sf, merge_sf, st_nearest_feature)
# 
# #export option
# sf::st_write(Grid100_BldDen, dsn='C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-SmallerGrids', layer='Grid100_BldDen', driver = "ESRI Shapefile")


