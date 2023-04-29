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
roads_StudyArea <- readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/Traffic/TrafficVolume_StudyArea.shp')

## == IMPORT NO2 MEASUREMENT STATIONS == ##
ms_stations <- read.csv(file = 'C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/LocalModelData/LocalMeasurementStations.csv', sep= ",")

ms_stations <- ms_stations %>% rename(Longitude = long, Latitude = lat)

#create spatial dataframe from no2 measurement stations dataset
ms_stations_sf <- st_as_sf(ms_stations, coords = c("Longitude", "Latitude"))
ms_stations_sf$M_id <- seq(1, nrow(ms_stations_sf)) #import for later data processing (@ ## == CALCULATE AVERAGE TRAFFIC FOR EACH BUFFER == ##)
# assign coordinate system
st_crs(ms_stations_sf) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"

## == CREATE BUFFERS AROUND NO2 MEASUREMENT STATIONS == ##
##  First project data into a planar coordinate system 

#dfine coordinate reference system (planar)
crs_utm = 3035
#filter columns
ms_stations_sf<- ms_stations_sf %>% dplyr::select(geometry,M_id)
#apply projection system which is defined above
ms_stations_utm <- st_transform(ms_stations_sf, crs=crs_utm)

## == create for loop, thereby creating a different buffer per iteration == ##

#initialize buffer parameters
bufs = list(25, 50, 100, 400, 800)

#create emply list where loop results are stored to
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
  layerStudyArea <- paste("buf", i, "m", sep = "")
  print(layerStudyArea)
  sf::st_write(buf_i, dsn='C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/Local',layer=layerStudyArea, driver = "ESRI Shapefile")
  
}

## == CALCULATE AVERAGE TRAFFIC FOR EACH BUFFER == ##

#transform crs of roads to match buffer's
roads_StudyArea <- st_as_sf(roads_StudyArea)
roads_utm <- st_transform(roads_StudyArea, crs=crs_utm)
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
  layer_clip  <- paste0('clip', bufs[[i]], '.shp')
  print(layer_clip)
  
  sf::st_write(clip, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/Local", layer=layer_clip, driver = "ESRI Shapefile")
  
  ## == SELECT ROADS WITHIN BUFFER X == ##
  intersect_j <- st_intersection(clip, buffer_vars[[j]],  sp = TRUE)
  #compute length (m) for each polygon in dataset
  intersect_j$road_length <- st_length(intersect_j)
  #calculate accumulated traffic
  intersect_j$trafroad <- intersect_j$road_length*intersect_j$AverageHourlyTraffic
  #assign to variable for export purposes
  layer_intersect  <- paste0('inter', bufs[[i]], '.shp')
  print(layer_intersect)
  #export option
  sf::st_write(intersect_j, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/Local", layer=layer_intersect, driver = "ESRI Shapefile")
  
  #dissolve by common ID - polygons with similar buffer ID will be merged
  #the area of these polygons will be aggregated via "SUM"
  dissolve <- intersect_j %>% group_by(M_id) %>% summarize(TotalLength = sum(road_length), AccTraffic = sum(trafroad))
  #to get average traffic per buffer: Accumulated traffic buffer n / total road length buffer n
  dissolve[[paste("trafBuf", bufs[[i]], sep="")]] <- dissolve$AccTraffic / dissolve$TotalLength
  
  layer_dissolve  <- paste0('dissolve', bufs[[i]], '.shp')
  print(layer_dissolve)
  
  #export option
  sf::st_write(dissolve, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/Local", layer=layer_dissolve, driver = "ESRI Shapefile")
  
  #merge datasets via left_join
  merge = left_join(measurement_stations, dissolve, by = "M_id")
  
  #store variable in loop
  merge_list[[paste0('merge', bufs[[i]])]] <- merge
  
  layer_merge  <- paste0('merge', bufs[[i]], '.shp')
  print(layer_merge)
  
  #export option
  #sf::st_write(merge, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/Traffic/Local", layer=layer_merge, driver = "ESRI Shapefile")
  
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
traffic_NO2_StudyArea <- ms_traffic_per_buf

traffic_NO2_StudyArea

sf::st_write(traffic_NO2_StudyArea, dsn='C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/Traffic/processed',layer='traffic_NO2_Local', driver = "ESRI Shapefile")

