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
roads_Germany <- readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/Traffic/TrafficVolume_RoadsGermany.shp')

## == IMPORT NO2 MEASUREMENT STATIONS == ##
ms_stations <- read.csv(file = 'C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/GlobalModelData/InitialGlobalDataset.csv', sep= ";")

#create spatial dataframe from no2 measurement stations dataset
ms_stations_sf <- st_as_sf(ms_stations, coords = c("Longitude", "Latitude"))
ms_stations_sf$M_id <- seq(1, nrow(ms_stations_sf)) #import for later data processing (@ ## == CALCULATE AVERAGE TRAFFIC FOR EACH BUFFER == ##)
# assign coordinate system
st_crs(ms_stations_sf) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"

#filter to only German NO2 stations
#import Germany shapefile
germany <- read_sf('C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/SpatialPlaces/Germany.shp')
plot(germany)
#select measurement stations that are only in Germany
ms_stations_ger_alldata <- ms_stations_sf[germany,]

## == CREATE BUFFERS AROUND NO2 MEASUREMENT STATIONS == ##
##  First project data into a planar coordinate system 

#dfine coordinate reference system (planar)
crs_utm = 3035
#filter columns
ms_stations_ger<- ms_stations_ger_alldata %>% dplyr::select(geometry,M_id)
#apply projection system which is defined above
ms_stations_utm <- st_transform(ms_stations_ger, crs=crs_utm)

## == create for loop, thereby creating a different buffer per iteration == ##

#initialize buffer parameters
bufs = list(25, 50, 100, 400, 800)

#create emply list where loop results are stored to
buffer_vars = list()
#loop
for(i in bufs){
  buf_i <- st_buffer(ms_stations_utm, i)
  assign(paste("bufGermany", i, "m", sep = ""), buf_i)
  
  #assign coordinate sustem
  buf_i <- st_as_sf(buf_i)
  st_crs(buf_i, crs=3035)
  st_crs(buf_i)
  #store variable in loop
  buffer_vars[[paste("bufGermany", i, "m", sep = "")]] <- buf_i
  
  #export option
  layerGermany <- paste("bufGermany", i, "m", sep = "")
  print(layerGermany)
  #sf::st_write(buf_i, dsn='C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/toNO2s',layer=layerGermany, driver = "ESRI Shapefile")
  
}

## == CALCULATE AVERAGE TRAFFIC FOR EACH BUFFER == ##

#transform crs of roads to match buffer's
roads_Germany <- st_as_sf(roads_Germany)
roads_utm <- st_transform(roads_Germany, crs=crs_utm)
st_crs(roads_utm)

#examine list where variables of previous loop were stored to.
summary(buffer_vars)
colnames(roads_utm)
#filter dataset
roads_utm <- roads_utm[, c("AvrgHrT", "geometry")]
#rename
roads_utm <- roads_utm %>% rename(AverageHourlyTraffic = AvrgHrT)

#initialize parameters used in for loop
j = 1
i = 1
#create empty list were data processing datasets can be stored to 
merge_list = list()
measurement_stations <- as.data.frame(ms_stations_utm)


while(j <= length(buffer_vars)){
  #clip - roads will be assigned to buffers 
  clip <- roads_utm[buffer_vars[[j]],]
  layer_clipGermany  <- paste0('clipGermany', bufs[[i]], '.shp')
  print(layer_clipGermany)
  
  #sf::st_write(clipGermany, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/toNO2s", layer=layer_clipGermany, driver = "ESRI Shapefile")
  
  ## == SELECT ROADS WITHIN BUFFER X == ##
  intersect_j <- st_intersection(clip, buffer_vars[[j]],  sp = TRUE)
  #compute length (m) for each polygon in dataset
  intersect_j$road_length <- st_length(intersect_j)
  #calculate accumulated traffic
  intersect_j$trafroad <- intersect_j$road_length*intersect_j$AverageHourlyTraffic
  #assign to variable for export purposes
  layer_intersectGermany  <- paste0('interGermany', bufs[[i]], '.shp')
  print(layer_intersectGermany)
  #export option
  #sf::st_write(intersect_j, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/toNO2s", layer=layer_intersectGermany, driver = "ESRI Shapefile")
  
  #dissolve by common ID - polygons with similar buffer ID will be merged
  #the area of these polygons will be aggregated via "SUM"
  dissolve <- intersect_j %>% group_by(M_id) %>% summarize(TotalLength = sum(road_length), AccTraffic = sum(trafroad))
  #to get average traffic per buffer: Accumulated traffic buffer n / total road length buffer n
  dissolve[[paste("trafBuf", bufs[[i]], sep="")]] <- dissolve$AccTraffic / dissolve$TotalLength

  layer_dissolveGermany  <- paste0('dissolveGermany', bufs[[i]], '.shp')
  print(layer_dissolveGermany)
  
  #export option
  #sf::st_write(dissolve, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/toNO2s", layer=layer_dissolveGermany, driver = "ESRI Shapefile")
  
  #merge datasets via left_join
  mergeGermany = left_join(measurement_stations, dissolve, by = "M_id")
  
  #store variable in loop
  merge_list[[paste0('mergeGermany', bufs[[i]])]] <- mergeGermany
  
  layer_mergeGermany  <- paste0('mergeGermany', bufs[[i]], '.shp')
  print(layer_mergeGermany)
  
  #export option
  #sf::st_write(mergeGermany, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/toNO2s", layer=layer_mergeGermany, driver = "ESRI Shapefile")
  
  #next buffer dataset
  j = j+1
  i = i+1}

#examine
summary(merge_list)

## == joining traffic data with measurement stations and apply data cleaning == ##

#merge all data frames together
traffic_per_buf <- merge_list %>% reduce(full_join, by='M_id')
#join measurement stations with traffic data
ms_traffic_per_buf <- left_join(measurement_stations, traffic_per_buf, by = "M_id")

ms_traffic_per_buf

#filter to only relevant columns
ms_traffic_per_buf <- ms_traffic_per_buf %>% dplyr::select(M_id, trafBuf25, trafBuf50, 
                                                           trafBuf100, trafBuf400, trafBuf800, geometry)

#rename variable
traffic_NO2_Germany <- ms_traffic_per_buf

#sf::st_write(traffic_NO2_Germany, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/toNO2s", layer='traffic_NO2_Germany', driver = "ESRI Shapefile")

# ## == ASSIGN TRAFFIC VALUES PER BUFFER TO MEASUREMENT STATIONS (ALL DATA) == ##
# ms_stations_alldata <- left_join(ms_stations_ger_alldata, ms_traffic_per_buf, by = "M_id", sp=T)
# ms_stations_alldata$geometry.y <- NULL
# view(ms_stations_alldata)
# 
# sf::st_write(ms_stations_alldata, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/toNO2s", layer='traffic_Germany', driver = "ESRI Shapefile")

### === NETHERLANDS === ###

## == IMPORT ROADS DATASET, INCLUDING TRAFFIC VOLUME INFORMATION == ##
roads_NL <- readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/Traffic/TrafficVolume_RoadsNL.shp')

#filter to only Dutch NO2 stations
#import Netherlands shapefile
NL <- read_sf('C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/SpatialPlaces/NL.shp')
#apply crs
NL <- st_transform(NL, crs = st_crs(ms_stations_sf))
ms_stations_nl_alldata <- ms_stations_sf[NL,]

## == CREATE BUFFERS AROUND NO2 MEASUREMENT STATIONS == ##

#filter columns
ms_stations_nl<- ms_stations_nl_alldata %>% dplyr::select(geometry,M_id)

# #export option
# sf::st_write(ms_stations_nl, dsn='C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/toNO2s',layer='ms_stations_nl', driver = "ESRI Shapefile")

## Project data into a planar coordinate system 
ms_stations_utm <- st_transform(ms_stations_nl, crs=3035)

## == create different buffers == ##

#initialize buffer parameters
bufs = list(25, 50, 100, 400, 800)
#create for loop, thereby creating a different buffer per iteration.
buffer_vars = list()
#loop
for(i in bufs){
  buf_i <- st_buffer(ms_stations_utm, i)
  assign(paste("bufNL", i, "m", sep = ""), buf_i)
  
  #assign coordinate sustem
  buf_i <- st_as_sf(buf_i)
  st_crs(buf_i, crs=3035)
  st_crs(buf_i)
  #store variable in loop
  buffer_vars[[paste("bufNL", i, "m", sep = "")]] <- buf_i
  
  #export option
  layerNL <- paste("bufNL", i, "m", sep = "")
  print(layerNL)
 
  #export option
  #sf::st_write(buf_i, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/toNO2s",layer=layerNL, driver = "ESRI Shapefile")
  
}


## == CALCULATE AVERAGE TRAFFIC FOR EACH BUFFER == ##

#transform crs of roads to match buffer's
roads_NL <- st_as_sf(roads_NL)
roads_utm <- st_transform(roads_NL, crs=3035)
st_crs(roads_utm)

#examine list where variables of previous loop were stored to.
summary(buffer_vars)

#initialize parameters used in for loop
j = 1
i = 1
#create empty list where loop results will be stored to
merge_list = list()
measurement_stations <- as.data.frame(ms_stations_utm)

st_crs(roads_utm)
st_crs(buffer_vars[[1]])
summary(buffer_vars)

while(j <= length(buffer_vars)){
  #clip - building dataset will be assigned to buffers 
  clipNL <- roads_utm[buffer_vars[[j]],]
  layer_clipNL  <- paste0('clipNL', bufs[[i]], '.shp')
  print(layer_clipNL)
  #export option
  #sf::st_write(clip, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/toNO2s", layer=layer_clipNL, driver = "ESRI Shapefile")
  ## == SELECT ROADS WITHIN BUFFER X == ##
  intersect_j <- st_intersection(clipNL, buffer_vars[[j]],  sp = TRUE)
  #compute length (m) for each polygon in dataset
  intersect_j$road_length <- st_length(intersect_j)
  #calculate accumulated traffic
  intersect_j$trafroad <- intersect_j$road_length*intersect_j$AvrgHrT
  #assign specific buffer variable. This variable is variable in itself as buffer sizes change per loop.
  layer_intersectNL  <- paste0('interNL', bufs[[i]], '.shp')
  print(layer_intersectNL)
  #export option
  #sf::st_write(intersect_j, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/toNO2s", layer=layer_intersectNL, driver = "ESRI Shapefile")
  
  #dissolve by common ID - polygons with similar buffer ID will be merged
  #the area of these polygons will be aggregated via "SUM"
  dissolve <- intersect_j %>% group_by(M_id) 
  
  dissolve <- dissolve %>% summarize(TotalLength = sum(road_length), AccTraffic = sum(trafroad))
  #to get average traffic per buffer: Accumulated traffic buffer n / total road length buffer n
  dissolve[[paste("trafBuf", bufs[[i]], sep="")]] <- dissolve$AccTraffic / dissolve$TotalLength
  
  layer_dissolveNL  <- paste0('dissolveNL', bufs[[i]], '.shp')
  print(layer_dissolveNL)
  
  #export option
  #sf::st_write(dissolve, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/toNO2s", layer=layer_dissolveNL, driver = "ESRI Shapefile")
  
  #merge datasets via left_join
  mergeNL = left_join(measurement_stations, dissolve, by = "M_id")
  
  #store variable in loop
  merge_list[[paste0('mergeNL', bufs[[i]])]] <- mergeNL
  #assign specific buffer variable. This variable is variable in itself as buffer sizes change per loop.
  layer_mergeNL  <- paste0('mergeNL', bufs[[i]], '.shp')
  print(layer_mergeNL)
  
  #export option
  #sf::st_write(merge, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/toNO2s", layer=layer_mergeNL, driver = "ESRI Shapefile")
  
  #next buffer dataset
  j = j+1
  i = i+1}

summary(merge_list)

## == joining traffic data with measurement stations and apply data cleaning == ##

#merge all data frames together
traffic_per_buf <- merge_list %>% reduce(full_join, by='M_id')
#join measurement stations with traffic data
ms_traffic_per_buf <- left_join(measurement_stations, traffic_per_buf, by = "M_id")
#filter to only relevant columns

ms_traffic_per_buf
ms_traffic_per_buf <- ms_traffic_per_buf %>% dplyr::select(M_id, trafBuf25, trafBuf50, 
                                                           trafBuf100, trafBuf400, trafBuf800, geometry.x)
#rename
ms_traffic_per_buf <- ms_traffic_per_buf %>% rename(geometry = geometry.x)
#rename variable
traffic_NO2_NL <- ms_traffic_per_buf
#export option
#sf::st_write(traffic_NO2_NL, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/toNO2s", layer='traffic_NO2_NL', driver = "ESRI Shapefile")



## == Combine Traffic Volume on German roads with Traffic Volume on Dutch Roads via merge operation == ##

#via rowbind, combine Dutch and German traffic datasets which are projected onto the measurement stations
Traffic_StudyArea = rbind(traffic_NO2_Germany, traffic_NO2_NL)

Traffic_StudyArea

st_crs(traffic_NO2_Germany)
st_crs(traffic_NO2_NL)

# #export option
sf::st_write(Traffic_StudyArea, dsn='C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/Traffic/processed',layer='traffic_NO2_Global', driver = "ESRI Shapefile")



