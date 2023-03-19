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
buildings_32 <- readOGR('C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-BuildingDensity/polygonbuilding_32_3000.shp')

## == IMPORT NO2 MEASUREMENT STATIONS == ##
ms_stations <- read.csv(file = 'C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/glo4var_2021.csv', sep= ";")

#create spatial dataframe from no2 measurement stations dataset
ms_stations_sf <- st_as_sf(ms_stations, coords = c("Longitude", "Latitude"))

st_crs(ms_stations_sf) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"

ms_stations_sf$M_id <- seq(1, 482) #import for later data processing (@ ## == CALCULATE AVERAGE TRAFFIC FOR EACH BUFFER == ##)
ms_stations_sf<- ms_stations_sf%>% dplyr::select(FID,geometry,M_id)

## == CREATE BUFFERS AROUND NO2 MEASUREMENT STATIONS == ##
##  First project data into a planar coordinate system 
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs_32 <- CRS(sprintf(utmStr, 32))
ms_stations_utm <- st_transform(ms_stations_sf, crs=crs_32)

#create different buffers

#initialize buffer parameters
bufs = list(100, 500, 1000)
#create for loop, thereby creating a different buffer per iteration.
buffer_vars = list()
#loop
for(i in bufs){
  buf_i <- st_buffer(ms_stations_utm, i)
  assign(paste("buf", i, "m", sep = ""), buf_i)
  
  #assign coordinate sustem
  buf_i <- st_as_sf(buf_i)
  st_crs(buf_i, crs=crs_32)
  st_crs(buf_i)
  
  buf_i$area <- st_area(buf_i)
  #store variable in loop
  buffer_vars[[paste("buf", i, "m", sep = "")]] <- buf_i
  
  #export option
  layer <- paste("buf", i, "m", sep = "")
  print(layer)
  
  sf::st_write(buf_i, dsn='C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-BuildingDensity',layer=layer, driver = "ESRI Shapefile")
  
}



#examine list where variables of previous loop were stored to.
summary(buffer_vars)
# 
# view(roads_utm)
st_crs(buffer_vars[[1]])

# #before loop, relevant datasets should have similar crs's.
# buildings_sf <- st_as_sf(buildings)
# buildings_sf <- st_transform(buildings_sf, crs=crs_32)

#export option
#sf::st_write(buildings_utm, dsn="C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-BuildingDensity", layer='buildings_32', driver = "ESRI Shapefile")

#transform crs of building dataset to that of buffers to perform spatial calculations
buildings_32_sf <-st_as_sf(buildings_32)



#initialize parameters used in for loop
j = 1
i = 1
merge_list = list()
bldden_ms_list = list()
measurement_stations <- as.data.frame(ms_stations_utm)

#while loop consists of several spatial operations:
  #'clip' for just selecting buildings in buffers
  #'intersect' for creating ID for features in corresponding buffer
  #'dissolve' to merge features into one, based on common ID (M_id)
  #'merge' for calculating building density per buffer
  
while(j <= length(buffer_vars)){
  #set to same crs to perform spatial operations
  buffer_vars[[j]] <- st_transform(buffer_vars[[j]], crs=st_crs(buildings_32_sf))
  #clip - building dataset will be assigned to buffers 
  clip <- buildings_32_sf[buffer_vars[[j]],]
  layer_clip  <- paste0('clip', bufs[[i]], '.shp')
  print(layer_clip)
  #make clip spatial to export as shapefile
  clip <- st_as_sf(clip)
  #export option
  sf::st_write(clip, dsn="C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-BuildingDensity/processing", layer=layer_clip, driver = "ESRI Shapefile")
  
  ## == SELECT ROADS WITHIN BUFFER X == ##
  int_j <- st_intersection(clip, buffer_vars[[j]],  sp = TRUE)
  
  #compute area (m2) for each polygon in dataset
  int_j$area <- st_area(int_j)
 
  layer_int  <- paste0('inter', bufs[[i]], '.shp')
  print(layer_int)
  sf::st_write(int_j, dsn="C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-BuildingDensity/processing", layer=layer_int, driver = "ESRI Shapefile")
  
  #dissolve by common ID - polygons with similar buffer ID will be merged
  #the area of these polygons will be aggregated via "SUM"
  
  dis <- int_j %>% group_by(M_id) %>% summarize(BuiltArea = sum(area))
  
  layer_dis  <- paste0('dis', bufs[[i]], '.shp')
  print(layer_dis)
  
  #export option
  sf::st_write(dis, dsn="C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-BuildingDensity/processing", layer=layer_dis, driver = "ESRI Shapefile")
  

  #merge datasets via left_join
  #make normal dataframe to perform left_join
  bufdata <- as.data.frame(buffer_vars[[j]])
  #assign info based on common M_id
  merge = left_join(bufdata, dis, by = "M_id")
  view(merge)
  
  #create new column "building_density" dividing building surface per buffer by
  #total buffer surface
  merge[[paste("BldDen", bufs[[i]], sep="")]] <- merge$BuiltArea/merge$area
  
  #drop irrelevant columns
  merge <- subset(merge, select = -c(FID, geometry.y))
  
  #replace NA by 0 
  merge[is.na(merge)] <- 0
  
  # #make spatial again
  # merge <- st_as_sf(merge)
  
  #store variable in loop
  merge_list[[paste0('merge', bufs[[i]])]] <- merge
  
  layer_mer  <- paste0('merge', bufs[[i]], '.shp')
  print(layer_mer)
  
  #export option
  sf::st_write(merge, dsn="C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-BuildingDensity/processing", layer=layer_mer, driver = "ESRI Shapefile")
  
  
  #next buffer dataset
  j = j+1
  i = i+1}


#examine
#summary(merge_list)

## == merge all data frames together and assign to measurement stations == ##

#make dataframe spatial
measurement_stations <- st_as_sf(measurement_stations)
measurement_stations <- st_transform(measurement_stations, crs=crs_32)

#join datasets in list 'merge_list' with each other, based on common M_id
bldden_per_buf <- merge_list %>% reduce(full_join, by='M_id')
#assign to measurement stations, again via common M_id
ms_bldden_per_buf <- left_join(measurement_stations, bldden_per_buf, by = "M_id")

#verify
#view(ms_bldden_per_buf)

#deselect irrelevant columns
ms_bldden_per_buf <- ms_bldden_per_buf %>% dplyr::select(M_id,  BldDen100, BldDen500, BldDen1000)
#verify
#view(ms_bldden_per_buf)

#export option
sf::st_write(ms_bldden_per_buf, dsn="C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-BuildingDensity", layer='BldDen_ms_StudyArea', driver = "ESRI Shapefile")
