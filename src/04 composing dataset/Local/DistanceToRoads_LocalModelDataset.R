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
library(yaml)

#connect to yaml file
current_dir <- rstudioapi::getActiveDocumentContext()$path
# Move one level up in the directory
config_dir <- dirname(dirname(current_dir))
# Construct the path to the YAML configuration file
config_path <- file.path(config_dir, "config_04.yml")

# Read the YAML configuration file
config <- yaml.load_file(config_path)

# Use dirname() to get the parent directory
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))

## == define output path == ##
out_location <- config$out_location
out_location_dir <- normalizePath(file.path(parent_directory, out_location ), winslash = "/")
##  First project data into a planar coordinate system (here UTM zone 32)
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs_32 <- CRS(sprintf(utmStr, 32))

#import datasets
modeling_dataset_local_processed <- config$input_data$modeling_dataset_local_processed
modeling_dataset_local_processed_dir <- normalizePath(file.path(parent_directory, modeling_dataset_local_processed ), winslash = "/") 
stations <- read.csv(modeling_dataset_local_processed_dir, sep=';')

#import roads
processed_traffic_dataset <- config$input$traffic_volume_study_area
processed_traffic_dataset_dir <- normalizePath(file.path(parent_directory, processed_traffic_dataset ), winslash = "/") 
roads <- st_read(processed_traffic_dataset_dir )

print(stations)
#make sf
roads_sf <- st_as_sf(roads)
stations$M_id <- seq(1, nrow(stations))
stations_xy <- stations[, c("M_id", "Longitude", "Latitude")]
stations_sf <- st_as_sf(stations, coords = c("Longitude", "Latitude"), crs=4326)

#crs should be same
roads_sf <- st_transform(roads_sf, crs= crs_32)
stations_sf <- st_transform(stations_sf, crs=st_crs(roads_sf))

distance <- st_distance(roads_sf, stations_sf)
print(distance)


#identify nearest point feature to point feature
nearest_points <- st_nearest_feature(stations_sf, roads_sf )
#calculate related distance between point and point feature
distance_points = st_distance(stations_sf, roads_sf[nearest_points,], by_element=TRUE)

#assign traffic values to line dataset, based on nearest distance
distance_to_road = cbind(stations_sf, st_drop_geometry(roads_sf)[nearest_points,])

#add distance column
distance_to_road$dist <- distance_points


distance_to_road_NO2 <- st_join(distance_to_road, stations_sf, join = st_nearest_feature)


# print(distance_to_road_NO2)

#filter only relevant columns
grep("dist", colnames(distance_to_road_NO2))
grep("trafBuf800.x", colnames(distance_to_road_NO2))
grep("lat", colnames(distance_to_road_NO2))
grep("M_id", colnames(distance_to_road_NO2))
distance_to_road_NO2 <- distance_to_road_NO2[, c(1:95, 127)] #127 = distance
names(distance_to_road_NO2) <- gsub('.x', '', names(distance_to_road_NO2), fixed=TRUE)
print(distance_to_road_NO2)

#replace NA by 0 
distance_to_road_NO2[is.na(distance_to_road_NO2)] <- 0


#export options
#shapefile
sf::st_write(distance_to_road_NO2, dsn=out_location_dir, layer='Local_ModellingDataset_distance', driver = "ESRI Shapefile")

#attach Longitude and Latitude columns 
distance_to_road_NO2_xy <- merge(distance_to_road_NO2, stations_xy, by = "M_id")

print(distance_to_road_NO2_xy)

#csv
write.csv(out_location_dir , 'Local_ModellingDataset_distance.csv',col.names = TRUE)
