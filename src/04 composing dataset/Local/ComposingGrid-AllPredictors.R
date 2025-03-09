# Load necessary libraries
library(sf)
library(raster)
library(ggplot2)
library(rgeos)
library(tidyverse)
library(sp)
library(leaflet)
library(terra)
library(stars)
library(dplyr)
library(yaml)
library(rgdal)
library(nngeo)     # For finding nearest features

# Connect to YAML configuration file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir))
config_path <- file.path(config_dir, "config_04.yml")
config <- yaml::yaml.load_file(config_path)

# Define output path
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
out_location_dir <- normalizePath(file.path(parent_directory, config$out_location), winslash = "/")

amsterdam100m_grid <- config$input_data$amsterdam100m_grid
amsterdam100m_grid_dir <- normalizePath(file.path(parent_directory, amsterdam100m_grid ), winslash = "/")

## == IMPORT GEODATA == ##

#import area of interest at 100m resolution
grid100_Amsterdam <- st_read(amsterdam100m_grid_dir)

## == data processing == ##

#make spatial
grid_sf <- st_as_sf(grid100_Amsterdam)
#switch to planar coordinate system 
grid_3035 <- st_transform(grid_sf, crs=3035)
#create centroids in 100m grid
grid_centroids <- gCentroid(grid100_Amsterdam,byid=TRUE)
#to spatial dataframe
grid_centroids_sf <- st_as_sf(grid_centroids)
#unique ID where dissolve will be based on
grid_centroids_sf$cenID <- seq(1,nrow(grid_centroids_sf))
#switch to planar coordinate system 
grid_centroids_3035 <- st_transform(grid_centroids_sf, crs=3035)

## == PREDICTORS 5 (AMSTERDAM) Local DATASET == ##

#first import all files in a single folder as a list 

tifs5 <- config$tifs$tifs5
tifs5_dir <- normalizePath(file.path(parent_directory, tifs5 ), winslash = "/")
rastlist <- list.files(path = tifs5_dir, pattern='.TIF$', all.files=TRUE, full.names=FALSE)

#define current working directory
setwd(tifs5_dir)

#import tif-files
rlist=list.files(getwd(), pattern="tif$", full.names=FALSE)
for(i in rlist) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) }

print(rlist)


## == extract to centroids == ## -

#initialize lists that will be used in loop
predictors = list(nightlight_450, 
                  nightlight_4950, population_1000, population_3000,  
                  road_class_1_5000,road_class_2_100, road_class_2_1000 , road_class_1_100,
                  road_class_2_5000, road_class_3_100, road_class_3_300)
prednames = list("nightlight_450", 
                 "nightlight_4950", "population_1000", "population_3000",  
                 "road_class_1_5000","road_class_2_100", "road_class_2_1000" , "road_class_1_100",
                 "road_class_2_5000", "road_class_3_100", "road_class_3_300")

#similar crs of all data used
grid_centroids_sf <- st_transform(grid_centroids_sf, crs=st_crs(predictors[[1]]))

#create empty list where loop results will be stored to.
centroid_predictors = list()
i=1
j=1

#While loop
while(i <= length(predictors))
{ 
  print(predictors[[i]])  
  centroid_pred <- raster::extract(predictors[[i]], grid_centroids_sf, sp=TRUE)
  #store variable in loop
  centroid_pred  <- as.data.frame(centroid_pred )
  centroid_predictors[[paste0('pred_', prednames[[j]])]] <- centroid_pred 
  i = i+1
  j = j+1
}

#examine
summary(centroid_predictors)

#merge all data frames together
centroids_5predictors <- centroid_predictors %>% reduce(full_join, by='cenID')

## == clean data == ##

#filter columns
centroids_5predictors <- centroids_5predictors %>% dplyr::select(cenID, coords.x1.x, coords.x2.x, nightlight_450, 
                                                                 nightlight_4950, population_1000, population_3000,  
                                                                 road_class_1_5000,road_class_2_100, road_class_2_1000 , road_class_1_100,
                                                                 road_class_2_5000, road_class_3_100, road_class_3_300)
#rename
centroids_5predictors <- centroids_5predictors %>% rename(Longitude = coords.x1.x, Latitude = coords.x2.x)


#remove unnecessary columns
centroids_5predictors <- centroids_5predictors %>% select(-contains("coords"))

#examine
print(centroids_5predictors)


## == TRAFFIC DATA == ##

#import traffic data

processed_traffic_dataset <- config$input$traffic_volume_study_area
processed_traffic_dataset_dir <- normalizePath(file.path(parent_directory, processed_traffic_dataset ), winslash = "/")

traffic <- st_read(processed_traffic_dataset_dir)
traffic_sf <- st_as_sf(traffic)
#similar crs are needed
traffic_sf <- st_transform(traffic_sf, crs=st_crs(grid_centroids_3035))
#filter to only relevant columns
traffic_sf <- traffic_sf[,c("AvrgHrT", "geometry")]


## == CALCULATE AVERAGE TRAFFIC FOR EACH BUFFER == ##


#initialize buffer parameters
bufs = list(25, 50)
#create for loop, thereby creating a different buffer per iteration.
buffer_vars = list()

for(i in bufs){
  buf_i <- st_buffer(grid_centroids_3035, i)
  assign(paste("buf", i, "m", sep = ""), buf_i)
  
  #assign coordinate system
  buf_i <- st_as_sf(buf_i)
  st_crs(buf_i, crs=3035)
  st_crs(buf_i)
  #store variable in loop
  buffer_vars[[paste("buf", i, "m", sep = "")]] <- buf_i
  
  layer <- paste("buf", i, "m", sep = "")
  
}

#make normal dataframe to perform left_join
grid_centroids_df <- as.data.frame(grid_centroids_sf)

i=1
j=1
#list where loop results will be stored to for each iteration
merge_list = list()

while(j <= length(buffer_vars)){
  #clip - traffic dataset will be assigned to buffers 
  clip_traffic <- traffic_sf[buffer_vars[[j]],]
  cat("clip traffic done:", "trafBuf", bufs[[i]])
  print("")
  ## == SELECT ROADS WITHIN BUFFER X == ##
  inter_traffic <- st_intersection(clip_traffic , buffer_vars[[j]],  sp = TRUE)
  #compute length (m) for each polygon in dataset
  inter_traffic$road_length <- st_length(inter_traffic)
  #calculate accumulated traffic
  inter_traffic$trafroad <- inter_traffic$road_length*inter_traffic$AvrgHrT
  cat("inter traffic done:", "trafBuf", bufs[[i]])
  
  #dissolve by common ID - polygons with similar buffer ID will be merged
  #the area of these polygons will be aggregated via "SUM"
  dis_traffic <-  inter_traffic %>% group_by(cenID) %>% summarize(TotalLength = sum(road_length), AccTraffic = sum(trafroad)) 
  
  #to get average traffic per buffer: Accumulated traffic buffer n / total road length buffer n
  dis_traffic[[paste("trafBuf", bufs[[i]], sep="")]] <- dis_traffic$AccTraffic / dis_traffic$TotalLength
  print("")
  cat("dis traffic done: ", "trafBuf",bufs[[i]])
  print("")
  #merge datasets via left_join
  merge_traffic = left_join(grid_centroids_df, dis_traffic, by = "cenID")
  
  #store variable in loop
  merge_list[[paste0('merge', bufs[[i]])]] <- merge_traffic
  i = i+1
  j = j+1
}


#merge all data frames together
traffic_per_buf <- merge_list %>% reduce(full_join, by='cenID')

traffic_bufs <- traffic_per_buf  %>% dplyr::select(cenID, trafBuf25,  trafBuf50)

## == merge data frames together== ##

datasets <- list(traffic_bufs, centroids_5predictors,  by="cenID")
#convert to dataframe
datasets <- as.data.frame(datasets)

datasets <- datasets %>% dplyr::select(cenID, nightlight_450, 
                                       nightlight_4950, population_1000, population_3000,  
                                       road_class_1_5000,road_class_2_100, road_class_2_1000 , road_class_1_100,
                                       road_class_2_5000, road_class_3_100, road_class_3_300, trafBuf50)


datasets <- merge(datasets, grid_centroids_3035, by= "cenID")
datasets_sf <- st_as_sf(datasets, crs=3035)

## == spatially join predictors with initial 100m spatial dataframe == ##

#crs should be similar 

#Centroids
Cen100_LocalPredictors <- st_join(grid_centroids_3035, datasets_sf, join=st_nearest_feature)
Cen100_LocalPredictors[is.na(Cen100_LocalPredictors)]<- 0

#create longitude and latitude columns

#convert to wgs 
Cen100_LocalPredictors_wgs <- st_transform(Cen100_LocalPredictors, crs=4326)
#extract latitude- and longitude-columns from geometry-column.
Cen100_LocalPredictors_wgs <- Cen100_LocalPredictors_wgs %>%
  mutate(Longitude = unlist(map(Cen100_LocalPredictors_wgs$geometry,1)),
         Latitude = unlist(map(Cen100_LocalPredictors_wgs$geometry,2)))

#Grid

#remove NA's and change to 0
datasets_sf[is.na(datasets_sf)] <- 0

Grid100_LocalPredictors <- st_join(grid_3035, datasets_sf, join=st_nearest_feature)
#remove NA's and change to 0
Grid100_LocalPredictors[is.na(Grid100_LocalPredictors)] <- 0

Grid100_LocalPredictors$cenID <- NULL
Grid100_LocalPredictors$fid <- NULL
Grid100_LocalPredictors
## == export options == ##
#shp - grid
sf::st_write(Grid100_LocalPredictors, dsn=file.path(out_location_dir, "Grid100_LocalPredictors_Amsterdam.gpkg"), driver = "GPKG")


