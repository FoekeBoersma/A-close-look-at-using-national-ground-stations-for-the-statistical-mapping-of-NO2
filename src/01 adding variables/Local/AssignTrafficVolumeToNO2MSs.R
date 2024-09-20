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
library(yaml)

#crs used: 3035
#CRS 3035, also known as the Coordinate Reference System (CRS) with EPSG code 3035, is a specific projection used for mapping and spatial data analysis. 
#It is designed for use in Europe and is based on the Lambert Azimuthal Equal Area projection.


## == connect with yaml file == ##

#connect to yaml file
current_dir <- rstudioapi::getActiveDocumentContext()$path
# Move one level up in the directory
config_dir <- dirname(dirname(current_dir))
# Construct the path to the YAML configuration file
config_path <- file.path(config_dir, "config.yml")
# Read the YAML configuration file
config <- yaml.load_file(config_path)

## == IMPORT RASTER (TIF) WITH NDVI VALUES == ##
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))

## == define output path == ##
out_location <- config$out_location
out_location_dir <- normalizePath(file.path(parent_directory, out_location ), winslash = "/")

traffic_volume_netherlands_dataset <- config$global$traffic_volume_netherlands
traffic_volume_netherlands_dir <- normalizePath(file.path(parent_directory,traffic_volume_netherlands_dataset ), winslash = "/")

## == IMPORT ROADS DATASET, INCLUDING TRAFFIC VOLUME INFORMATION == ##
roads_NL <- readOGR(traffic_volume_netherlands_dir)

## == IMPORT NO2 MEASUREMENT STATIONS == ##

no2_dataset <- config$local$no2
no2_map_dir <- normalizePath(file.path(parent_directory, no2_dataset ), winslash = "/")
ms_stations <- read.csv(file = no2_map_dir , sep= ",")

#create spatial dataframe from no2 measurement stations dataset
ms_stations_sf <- st_as_sf(ms_stations, coords = c("long", "lat"))
st_crs(ms_stations_sf) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
ms_stations_sf$M_id <- seq(1, nrow(ms_stations_sf)) #unique identifier which will be used for later data processing
ms_stations_sf<- ms_stations_sf %>% dplyr::select(geometry,M_id) #filter columns
ms_stations_utm <- st_transform(ms_stations_sf, crs=3035)

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

  sf::st_write(buf_i, dsn=out_location_dir,layer=layer, driver = "ESRI Shapefile")
  
}

## == CALCULATE AVERAGE TRAFFIC FOR EACH BUFFER == ##

#transform crs of roads to match buffer's
roads_NL <- st_as_sf(roads_NL)
roads_utm <- st_transform(roads_NL, crs=3035)

#examine list where variables of previous loop were stored to.
summary(buffer_vars)

#initialize parameters used in for loop
j = 1
i = 1
merge_list = list()
measurement_stations <- as.data.frame(ms_stations_utm)

summary(buffer_vars)
while(j <= length(buffer_vars)){
  #clip - building dataset will be assigned to buffers 
  clip <- roads_utm[buffer_vars[[j]],]
  layer_clip  <- paste0('clip', bufs[[i]], '.shp')
  print(layer_clip)
  sf::st_write(clip, dsn=out_location_dir, layer=layer_clip, driver = "ESRI Shapefile")
  ## == SELECT ROADS WITHIN BUFFER X == ##
  intersect_j <- st_intersection(clip, buffer_vars[[j]],  sp = TRUE)
  #compute length (m) for each polygon in dataset
  intersect_j$road_length <- st_length(intersect_j)
  #calculate accumulated traffic
  intersect_j$trafroad <- intersect_j$road_length*intersect_j$AvrgHrT
  
  layer_intersect  <- paste0('intersect', bufs[[i]], '.shp')
  print(layer_intersect)
  sf::st_write(intersect_j, dsn=out_location_dir, layer=layer_intersect, driver = "ESRI Shapefile")
  
  #dissolve by common ID - polygons with similar buffer ID will be merged
  #the area of these polygons will be aggregated via "SUM"
  dissolve <- intersect_j %>% group_by(M_id) 
  
  dissolve <- dissolve %>% summarize(TotalLength = sum(road_length), AccTraffic = sum(trafroad))
  #to get average traffic per buffer: Accumulated traffic buffer n / total road length buffer n
  dissolve[[paste("trafBuf", bufs[[i]], sep="")]] <- dissolve$AccTraffic / dissolve$TotalLength
  
  #store 
  layer_dissolve  <- paste0('dissolve', bufs[[i]], '.shp')
  print(layer_dissolve)
  
  #export option
  sf::st_write(dissolve, dsn=out_location_dir, layer=layer_dissolve, driver = "ESRI Shapefile")
  
  #merge datasets via left_join
  merge = left_join(measurement_stations, dissolve, by = "M_id")
  
  #store variable in loop
  merge_list[[paste0('merge', bufs[[i]])]] <- merge

  layer_mer  <- paste0('merge', bufs[[i]], '.shp')
  print(layer_mer)

  #export option
  sf::st_write(merge, dsn=out_location_dir, layer=layer_mer, driver = "ESRI Shapefile")

  #next buffer dataset
  j = j+1
  i = i+1}

#verify if loop succeeded via summary
summary(merge_list)

#merge all data frames together
traffic_per_buf <- merge_list %>% reduce(full_join, by='M_id')
ms_traffic_per_buf <- left_join(measurement_stations, traffic_per_buf, by = "M_id")
ms_traffic_per_buf <- ms_traffic_per_buf %>% dplyr::select(M_id, trafBuf25, trafBuf50, 
                                                           trafBuf100, trafBuf400, trafBuf800, geometry.x)
ms_traffic_per_buf <- ms_traffic_per_buf %>% rename(geometry = geometry.x)

#export
sf::st_write(ms_traffic_per_buf, dsn=out_location_dir, layer='traffic_NO2_Local', driver = "ESRI Shapefile")
