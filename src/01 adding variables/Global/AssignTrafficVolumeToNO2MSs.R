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

#connect to yaml file
current_dir <- rstudioapi::getActiveDocumentContext()$path
# Move one level up in the directory
config_dir <- dirname(dirname(current_dir))
# Construct the path to the YAML configuration file
config_path <- file.path(config_dir, "config.yml")
# Read the YAML configuration file
config <- yaml.load_file(config_path)

# Use dirname() to get the parent directory
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
traffic_study_ara_relative <- config$global$ traffic_volume_study_area
 

## == IMPORT NO2 MEASUREMENT STATIONS == ##
no2_dataset <- config$global$no2
no2_map_dir <- normalizePath(file.path(parent_directory, no2_dataset ), winslash = "/")

## == define output path == ##
out_location <- config$out_location
out_location_dir <- normalizePath(file.path(parent_directory, out_location ), winslash = "/")


## == IMPORT ROADS DATASET, INCLUDING TRAFFIC VOLUME INFORMATION == ##
roads_StudyArea <- normalizePath(file.path(parent_directory, traffic_study_ara_relative), winslash = "/")

## == IMPORT NO2 MEASUREMENT STATIONS == ##
ms_stations <- read.csv(file = no2_map_dir, sep= ";")

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
  assign(paste("bufStudyArea", i, "m", sep = ""), buf_i)
  
  #assign coordinate sustem
  buf_i <- st_as_sf(buf_i)
  st_crs(buf_i, crs=3035)
  st_crs(buf_i)
  #store variable in loop
  buffer_vars[[paste("bufStudyArea", i, "m", sep = "")]] <- buf_i
  
  #export option
  layerStudyArea <- paste("bufStudyArea", i, "m", sep = "")
  print(layerStudyArea)
  #sf::st_write(buf_i, dsn=out_location_dir,layer=layerStudyArea, driver = "ESRI Shapefile")
  
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
  layer_clipStudyArea  <- paste0('clipStudyArea', bufs[[i]], '.shp')
  print(layer_clipStudyArea)
  
  #sf::st_write(clipStudyArea, dsn=out_location_dir, layer=layer_clipStudyArea, driver = "ESRI Shapefile")
  
  ## == SELECT ROADS WITHIN BUFFER X == ##
  intersect_j <- st_intersection(clip, buffer_vars[[j]],  sp = TRUE)
  #compute length (m) for each polygon in dataset
  intersect_j$road_length <- st_length(intersect_j)
  #calculate accumulated traffic
  intersect_j$trafroad <- intersect_j$road_length*intersect_j$AverageHourlyTraffic
  #assign to variable for export purposes
  layer_intersectStudyArea  <- paste0('interStudyArea', bufs[[i]], '.shp')
  print(layer_intersectStudyArea)
  #export option
  #sf::st_write(intersect_j, dsn=out_location_dir, layer=layer_intersectStudyArea, driver = "ESRI Shapefile")
  
  #dissolve by common ID - polygons with similar buffer ID will be merged
  #the area of these polygons will be aggregated via "SUM"
  dissolve <- intersect_j %>% group_by(M_id) %>% summarize(TotalLength = sum(road_length), AccTraffic = sum(trafroad))
  #to get average traffic per buffer: Accumulated traffic buffer n / total road length buffer n
  dissolve[[paste("trafBuf", bufs[[i]], sep="")]] <- dissolve$AccTraffic / dissolve$TotalLength
  
  layer_dissolveStudyArea  <- paste0('dissolveStudyArea', bufs[[i]], '.shp')
  print(layer_dissolveStudyArea)
  
  #export option
  #sf::st_write(dissolve, dsn=out_location_dir, layer=layer_dissolveStudyArea, driver = "ESRI Shapefile")
  
  #merge datasets via left_join
  mergeStudyArea = left_join(measurement_stations, dissolve, by = "M_id")
  
  #store variable in loop
  merge_list[[paste0('mergeStudyArea', bufs[[i]])]] <- mergeStudyArea
  
  layer_mergeStudyArea  <- paste0('mergeStudyArea', bufs[[i]], '.shp')
  print(layer_mergeStudyArea)
  
  #export option
  #sf::st_write(mergeStudyArea, dsn=out_location_dir, layer=layer_mergeStudyArea, driver = "ESRI Shapefile")
  
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

sf::st_write(traffic_NO2_StudyArea, dsn=out_location_dir,layer='traffic_NO2_Global', driver = "ESRI Shapefile")

# ## == ASSIGN TRAFFIC VALUES PER BUFFER TO MEASUREMENT STATIONS (ALL DATA) == ##
# ms_stations_alldata <- left_join(ms_stations_ger_alldata, ms_traffic_per_buf, by = "M_id", sp=T)
# ms_stations_alldata$geometry.y <- NULL
# view(ms_stations_alldata)
# 
# sf::st_write(ms_stations_alldata, dsn=out_location_dir, layer='traffic_StudyArea', driver = "ESRI Shapefile")