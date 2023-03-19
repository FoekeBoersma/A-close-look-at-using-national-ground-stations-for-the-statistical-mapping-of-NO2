#necessary libraries
library(sf)
library(sfnetworks)
library(tidygraph)
library(tmap)
library(nngeo)
library(osmdata)
library(dplyr)
library(lwgeom)
library(raster)

library(rgdal)
library(utils)
library(ggplot2)
library(rgeos)
library(tidyverse)
library(leaflet) #mapping in OSM
library(bnspatial)
library(gstat)
library(geosphere) #geosphere::dist2Line
library(stars) #for st_rasterize
library(base) #sprintf
library(sfheaders) #converting to multistring

## == IMPORT ROADS DATASET, INCLUDING TRAFFIC VOLUME INFORMATION == ##

roads_Germany <- readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Germany/TrafficVolume_RoadsGermany.shp')

## == IMPORT NO2 MEASUREMENT STATIONS == ##
ms_stations <- read.csv(file = 'C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/glo4var_2021_allvars.csv', sep= ";")
#create spatial dataframe from no2 measurement stations dataset
ms_stations_sf <- st_as_sf(ms_stations, coords = c("Longitude", "Latitude"))
ms_stations_sf$M_id <- seq(1, nrow(ms_stations_sf)) #import for later data processing (@ ## == CALCULATE AVERAGE TRAFFIC FOR EACH BUFFER == ##)

st_crs(ms_stations_sf) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"

#filter to only German NO2 stations
#import Germany shapefile
germany <- read_sf('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/SpatialPlaces/Germany_Extended.shp')
plot(germany)

ms_stations_ger_alldata <- ms_stations_sf[germany,]

## == CREATE BUFFERS AROUND NO2 MEASUREMENT STATIONS == ##
##  First project data into a planar coordinate system 

crs_utm = 3035

ms_stations_ger<- ms_stations_ger_alldata %>% dplyr::select(geometry,M_id)
ms_stations_utm <- st_transform(ms_stations_ger, crs=crs_utm)

#create different buffers

#initialize buffer parameters
bufs = list(25, 50, 100, 400, 800)
#create for loop, thereby creating a different buffer per iteration.
buffer_vars = list()
#loop
for(i in bufs){
  buf_i <- st_buffer(ms_stations_utm, i)
  assign(paste("buf", i, "m", sep = ""), buf_i)
  
  #assign coordinate sustem
  buf_i <- st_as_sf(buf_i)
  st_crs(buf_i, crs=3035)
  st_crs(buf_i)
  #store variable in loop
  buffer_vars[[paste("buf", i, "m", sep = "")]] <- buf_i
  
  #export option
  layer <- paste("buf", i, "m", sep = "")
  print(layer)
  
  
  
  sf::st_write(buf_i, dsn='C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Germany/NO2',layer=layer, driver = "ESRI Shapefile")
  
}


## == CALCULATE AVERAGE TRAFFIC FOR EACH BUFFER == ##


#transform crs of roads to match buffer's
roads_Germany <- st_as_sf(roads_Germany)
roads_utm <- st_transform(roads_Germany, crs=crs_utm)
st_crs(roads_utm)

#examine list where variables of previous loop were stored to.
summary(buffer_vars)
colnames(roads_utm)

roads_utm <- roads_utm[, c("AvrgHrT", "geometry")]
roads_utm <- roads_utm %>% rename(AverageHourlyTraffic = AvrgHrT)

#initialize parameters used in for loop
j = 1
i = 1
merge_list = list()
measurement_stations <- as.data.frame(ms_stations_utm)

# st_crs(roads_utm)
# st_crs(buffer_vars[[1]])
summary(buffer_vars)
while(j <= length(buffer_vars)){
  #clip - building dataset will be assigned to buffers 
  clip <- roads_utm[buffer_vars[[j]],]
  layer_clip  <- paste0('clip', bufs[[i]], '.shp')
  print(layer_clip)
  sf::st_write(clip, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Germany/NO2", layer=layer_clip, driver = "ESRI Shapefile")
  ## == SELECT ROADS WITHIN BUFFER X == ##
  int_j <- st_intersection(clip, buffer_vars[[j]],  sp = TRUE)
  #compute length (m) for each polygon in dataset
  int_j$road_length <- st_length(int_j)
  #calculate accumulated traffic
  int_j$trafroad <- int_j$road_length*int_j$AverageHourlyTraffic
  
  layer_int  <- paste0('inter', bufs[[i]], '.shp')
  print(layer_int)
  sf::st_write(int_j, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Germany/NO2", layer=layer_int, driver = "ESRI Shapefile")
  
  #dissolve by common ID - polygons with similar buffer ID will be merged
  #the area of these polygons will be aggregated via "SUM"
  dis <- int_j %>% group_by(M_id) %>% summarize(TotalLength = sum(road_length), AccTraffic = sum(trafroad))
  #to get average traffic per buffer: Accumulated traffic buffer n / total road length buffer n
  dis[[paste("trafBuf", bufs[[i]], sep="")]] <- dis$AccTraffic / dis$TotalLength
  
  
  layer_dis  <- paste0('dis', bufs[[i]], '.shp')
  print(layer_dis)
  
  #export option
  sf::st_write(dis, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Germany/NO2", layer=layer_dis, driver = "ESRI Shapefile")
  
  #merge datasets via left_join
  merge = left_join(measurement_stations, dis, by = "M_id")
  
  #store variable in loop
  merge_list[[paste0('merge', bufs[[i]])]] <- merge
  
  layer_mer  <- paste0('merge', bufs[[i]], '.shp')
  print(layer_mer)
  
  #export option
  sf::st_write(merge, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Germany/NO2", layer=layer_mer, driver = "ESRI Shapefile")
  
  #next buffer dataset
  j = j+1
  i = i+1}



summary(merge_list)

#merge all data frames together
traffic_per_buf <- merge_list %>% reduce(full_join, by='M_id')
ms_traffic_per_buf <- left_join(measurement_stations, traffic_per_buf, by = "M_id")

ms_traffic_per_buf <- ms_traffic_per_buf %>% dplyr::select(M_id, trafBuf25, trafBuf50, 
                                                           trafBuf100, trafBuf400, trafBuf800, geometry.x)
ms_traffic_per_buf <- ms_traffic_per_buf %>% rename(geometry = geometry.x)

#rename variable
traffic_NO2_Germany <- ms_traffic_per_buf

sf::st_write(traffic_NO2_Germany, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Germany/NO2", layer='traffic_NO2_Germany', driver = "ESRI Shapefile")

# ## == ASSIGN TRAFFIC VALUES PER BUFFER TO MEASUREMENT STATIONS (ALL DATA) == ##
# ms_stations_alldata <- left_join(ms_stations_ger_alldata, ms_traffic_per_buf, by = "M_id", sp=T)
# ms_stations_alldata$geometry.y <- NULL
# view(ms_stations_alldata)
# 
# sf::st_write(ms_stations_alldata, dsn="C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject21/Germany/complete/NO2", layer='traffic_Germany', driver = "ESRI Shapefile")



### === NETHERLANDS === ###

## == IMPORT ROADS DATASET, INCLUDING TRAFFIC VOLUME INFORMATION == ##

roads_NL <- readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Netherlands/TrafficVolume_RoadsNetherlands.shp')



#filter to only Dutch NO2 stations
#import Germany shapefile
NL <- read_sf('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/SpatialPlaces/NL.shp')

NL <- st_transform(NL, crs = st_crs(ms_stations_sf))
ms_stations_nl_alldata <- ms_stations_sf[NL,]



## == CREATE BUFFERS AROUND NO2 MEASUREMENT STATIONS == ##
##  First project data into a planar coordinate system 



ms_stations_nl<- ms_stations_nl_alldata %>% dplyr::select(geometry,M_id)

# #export option
# sf::st_write(ms_stations_nl, dsn='C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject21/NL/NO2',layer='ms_stations_nl', driver = "ESRI Shapefile")

ms_stations_utm <- st_transform(ms_stations_nl, crs=3035)

#create different buffers

#initialize buffer parameters
bufs = list(25, 50, 100, 400, 800)
#create for loop, thereby creating a different buffer per iteration.
buffer_vars = list()
#loop
for(i in bufs){
  buf_i <- st_buffer(ms_stations_utm, i)
  assign(paste("buf", i, "m", sep = ""), buf_i)
  
  #assign coordinate sustem
  buf_i <- st_as_sf(buf_i)
  st_crs(buf_i, crs=3035)
  st_crs(buf_i)
  #store variable in loop
  buffer_vars[[paste("buf", i, "m", sep = "")]] <- buf_i
  
  #export option
  layer <- paste("buf", i, "m", sep = "")
  print(layer)
  
  
  
  sf::st_write(buf_i, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Netherlands/NO2",layer=layer, driver = "ESRI Shapefile")
  
}


## == CALCULATE AVERAGE TRAFFIC FOR EACH BUFFER == ##


#transform crs of roads to match buffer's
roads_NL <- st_as_sf(roads_NL)
roads_utm <- st_transform(roads_NL, crs=3035)
st_crs(roads_utm)

#examine list where variables of previous loop were stored to.
summary(buffer_vars)
# 
# view(roads_utm)


#initialize parameters used in for loop
j = 1
i = 1
merge_list = list()
measurement_stations <- as.data.frame(ms_stations_utm)

st_crs(roads_utm)
st_crs(buffer_vars[[1]])
summary(buffer_vars)
while(j <= length(buffer_vars)){
  #clip - building dataset will be assigned to buffers 
  clip <- roads_utm[buffer_vars[[j]],]
  layer_clip  <- paste0('clip', bufs[[i]], '.shp')
  print(layer_clip)
  sf::st_write(clip, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Netherlands/NO2", layer=layer_clip, driver = "ESRI Shapefile")
  ## == SELECT ROADS WITHIN BUFFER X == ##
  int_j <- st_intersection(clip, buffer_vars[[j]],  sp = TRUE)
  #compute length (m) for each polygon in dataset
  int_j$road_length <- st_length(int_j)
  #calculate accumulated traffic
  
  int_j$trafroad <- int_j$road_length*int_j$AvrgHrT
  
  layer_int  <- paste0('inter', bufs[[i]], '.shp')
  print(layer_int)
  sf::st_write(int_j, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Netherlands/NO2", layer=layer_int, driver = "ESRI Shapefile")
  
  #dissolve by common ID - polygons with similar buffer ID will be merged
  #the area of these polygons will be aggregated via "SUM"
  dis <- int_j %>% group_by(M_id) 
  
  dis <- dis %>% summarize(TotalLength = sum(road_length), AccTraffic = sum(trafroad))
  #to get average traffic per buffer: Accumulated traffic buffer n / total road length buffer n
  dis[[paste("trafBuf", bufs[[i]], sep="")]] <- dis$AccTraffic / dis$TotalLength
  
  
  layer_dis  <- paste0('dis', bufs[[i]], '.shp')
  print(layer_dis)
  
  #export option
  sf::st_write(dis, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Netherlands/NO2", layer=layer_dis, driver = "ESRI Shapefile")
  
  #merge datasets via left_join
  merge = left_join(measurement_stations, dis, by = "M_id")
  
  #store variable in loop
  merge_list[[paste0('merge', bufs[[i]])]] <- merge
  
  layer_mer  <- paste0('merge', bufs[[i]], '.shp')
  print(layer_mer)
  
  #export option
  sf::st_write(merge, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Netherlands/NO2", layer=layer_mer, driver = "ESRI Shapefile")
  
  #next buffer dataset
  j = j+1
  i = i+1}



summary(merge_list)

#merge all data frames together
traffic_per_buf <- merge_list %>% reduce(full_join, by='M_id')
ms_traffic_per_buf <- left_join(measurement_stations, traffic_per_buf, by = "M_id")

ms_traffic_per_buf <- ms_traffic_per_buf %>% dplyr::select(M_id, trafBuf25, trafBuf50, 
                                                           trafBuf100, trafBuf400, trafBuf800, geometry.x)
ms_traffic_per_buf <- ms_traffic_per_buf %>% rename(geometry = geometry.x)

#rename variable
traffic_NO2_NL <- ms_traffic_per_buf

sf::st_write(traffic_NO2_NL, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Netherlands/NO2", layer='traffic_NO2_NL', driver = "ESRI Shapefile")



## == Combine Traffic Volume on German roads with Traffic Volume on Dutch Roads via merge operation == ##
print(traffic_NO2_Germany)
Traffic_StudyArea = rbind(traffic_NO2_Germany, traffic_NO2_NL)

st_crs(traffic_NO2_Germany)
st_crs(traffic_NO2_NL)

print(Traffic_StudyArea)
print(traffic_NO2_NL)
# #export option
sf::st_write(Traffic_StudyArea, dsn='C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22',layer='Traffic_StudyArea', driver = "ESRI Shapefile")



