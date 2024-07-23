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
library(dplyr)


## == IMPORT GEODATA == ##

#import area of interest at 100m resolution
grid <- readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/grid100Hamburg.gpkg') #obtained via AreaOfInterest-HH.R

## == data processing == ##

#make spatial
grid_sf <- st_as_sf(grid)
#switch to planar coordinate system 
grid_3035 <- st_transform(grid_sf, crs=3035)
#create centroids in 100m grid
grid_centroids <- gCentroid(grid,byid=TRUE)
#to spatial dataframe
grid_centroids_sf <- st_as_sf(grid_centroids)
#unique ID where dissolve will be based on
grid_centroids_sf$cenID <- seq(1,nrow(grid_centroids_sf))
#switch to planar coordinate system 
grid_centroids_3035 <- st_transform(grid_centroids_sf, crs=3035)

print(grid_centroids_3035)

## == PREDICTORS 5 (Hamburg) GLOBAL DATASET == ##

#first import all files in a single folder as a list 
rastlist <- list.files(path = "C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/Hamburg/TIFFS/1", pattern='.TIF$', all.files=TRUE, full.names=FALSE)

#define current working directory
setwd("C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/Hamburg/TIFFS/1")

#import tif-files
rlist=list.files(getwd(), pattern="tif$", full.names=FALSE)
for(i in rlist) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) }

## == extract to centroids == ## - best 10/12 (excluding trafBuf100 and bldDen100)

#initialize lists that will be used in loop
predictors = list(population_3000,road_class_3_3000,population_1000,nightlight_450,road_class_2_25,nightlight_3150,road_class_3_300,trop_mean_filt )
prednames = list("population_3000","road_class_3_3000","population_1000","nightlight_450","road_class_2_25","nightlight_3150","road_class_3_300","trop_mean_filt")

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
centroids_5predictors <- centroids_5predictors %>% dplyr::select(cenID, coords.x1.x, coords.x2.x, population_3000,
                                                                 road_class_3_3000,population_1000,nightlight_450,
                                                                 road_class_2_25,nightlight_3150,
                                                                 road_class_3_300,trop_mean_filt)
#rename
centroids_5predictors <- centroids_5predictors %>% rename(Longitude = coords.x1.x, Latitude = coords.x2.x)


#remove unnecessary columns
centroids_5predictors <- centroids_5predictors %>% select(-contains("coords"))

#examine
print(centroids_5predictors)

## == BUILDING DENSITY == ##

#import DIS, to avoid high computational time and power!
dis <- readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/dissolve_BuildingDensity_Hamburg.gpkg')

dis_BldDen <- as.data.frame(dis)
print(dis_BldDen)
dis_BldDen <- dis_BldDen %>% rename(BuiltArea = SUM_BuiltA) #this option is maintained when the layer is imported


dis_BldDen$area <- pi*(100)**2 #this option is maintained when the layer is imported

#merge datasets via left_join
#make normal dataframe to perform left_join
grid_centroids_df <- as.data.frame(grid_centroids_sf)
#assign info based on common M_id

## == data processing == ##
print(grid_centroids_df)
print(dis_BldDen)

dis_BldDen$cenID <- as.numeric(dis_BldDen$cenID)
mergeBldDen = left_join(grid_centroids_df, dis_BldDen, by = "cenID")

#create new column "building_density" dividing building surface per buffer by
#total buffer surface

mergeBldDen$area <- pi*(100)**2
#calculate building density: built area in area x/ total area x
mergeBldDen$BldDen100 <- mergeBldDen$BuiltArea/mergeBldDen$area
#replace NA by 0 
mergeBldDen[is.na(mergeBldDen)] <- 0
#select only relevant columns
mergeBldDen <- mergeBldDen %>% dplyr::select(cenID, BldDen100)

## == TRAFFIC DATA == ##

#import traffic data

traffic <- readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/Traffic/TrafficVolume_StudyArea.shp') #obtained via AssignTrafficVolumeToRoads.R
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
#loop
for(i in bufs){
  buf_i <- st_buffer(grid_centroids_3035, i)
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

}

summary(buffer_vars)

## LOOP 

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

#merge traffic data frames together
summary(merge_list)
#merge all data frames together
traffic_per_buf <- merge_list %>% reduce(full_join, by='cenID')
#select only relevant columns
traffic_bufs <- traffic_per_buf  %>% dplyr::select(cenID, trafBuf25,  trafBuf50)

## == NDVI == ##

# TROUBLESHOOTING: Error in .local(.Object, ...) : Error in .rasterObjectFromFile(x, band = band, objecttype = "RasterLayer",  : 
# Cannot create a RasterLayer object from this file. (file does not exist)
# --> manually rerun setwd command to successfully make switch

#put tif files into list 
cur <- setwd("C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/NDVI")
#create list of all files in defined directory
a = list.files(cur, pattern='.tif$')
#examine files in list
print(a)
#concatenate multiple vectors to single vector via "stack"-function.
b = stack(a)
st_crs(b)

## == ASSIGNING NDVI RASTER VALUES TO POINT DATASET == ##

#crs must be similar between input data
grid_centroids_sf <- st_transform(grid_centroids_sf, crs=st_crs(b))
#extract raster NDVI values to points (no2 measurement stations)
points_NDVI <- raster::extract(b, grid_centroids_sf, sp=TRUE) #sp=TRUE retains information of points dataset
#convert to crs = wgs84
points_NDVI <- as.data.frame(points_NDVI)

#rename column to "NDVI"
points_NDVI <- points_NDVI %>% rename(NDVI = mod13q1)
#filter to only relevant columns
points_NDVI <- points_NDVI[,c("cenID", "NDVI")]

## == merge data frames together== ##

datasets <- list(traffic_bufs, mergeBldDen, centroids_5predictors, points_NDVI, by="cenID")
#convert to dataframae
datasets <- as.data.frame(datasets)
#select only relevant columns
datasets <- datasets %>% dplyr::select(cenID, nightlight_450, nightlight_3150, population_1000, population_3000, road_class_2_25, road_class_3_3000,  road_class_3_300, trop_mean_filt,  BldDen100, NDVI, trafBuf25, trafBuf50)
#merge predictor information with the centroid of every grid cell
datasets <- merge(datasets, grid_centroids_3035, by= "cenID")
#make spatial
datasets_sf <- st_as_sf(datasets, crs=3035)

## == spatially join predictors with initial 100m spatial dataframe == ##

#crs should be similar 

#Centroids
Cen100_GlobalPredictors <- st_join(grid_centroids_3035, datasets_sf, join=st_nearest_feature)
#replace NaN-values with 0
Cen100_GlobalPredictors[is.na(Cen100_GlobalPredictors)]<- 0

#create longitude and latitude columns

#convert to wgs 
Cen100_GlobalPredictors_wgs <- st_transform(Cen100_GlobalPredictors, crs=4326)
print(Cen100_GlobalPredictors_wgs)
#extract latitude- and longitude-columns from geometry-column.
Cen100_GlobalPredictors_wgs <- Cen100_GlobalPredictors_wgs %>%
  mutate(Longitude = unlist(map(Cen100_GlobalPredictors_wgs$geometry,1)),
         Latitude = unlist(map(Cen100_GlobalPredictors_wgs$geometry,2)))


#Grid

#remove NA's and change to 0
datasets_sf[is.na(datasets_sf)] <- 0
print(datasets_sf)

Grid100_GlobalPredictors <- st_join(grid_3035, datasets_sf, join=st_nearest_feature)
#remove NA's and change to 0
Grid100_GlobalPredictors[is.na(Grid100_GlobalPredictors)] <- 0

print(Grid100_GlobalPredictors)
Grid100_GlobalPredictors$cenID <- NULL
Grid100_GlobalPredictors$FID_1 <- NULL

## == export options == ##
#shp - centroids
#sf::st_write(Cen100_GlobalPredictors, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/Cen100_GlobalPredictors-HH.gpkg", driver = "GPKG")
#shp - grid format
sf::st_write(Grid100_GlobalPredictors, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/SpatialPredictionPatterns/Grid100_GlobalPredictors-HH.gpkg", driver = "GPKG")
Cen100_GlobalPredictors_wgs <- as.data.frame(Cen100_GlobalPredictors_wgs)
Cen100_GlobalPredictors_wgs <- Cen100_GlobalPredictors_wgs %>% dplyr::select(-c('geometry', 'cenID.x', "cenID.y"))
Cen100_GlobalPredictors_wgs
#csv
write.csv(Cen100_GlobalPredictors_wgs, 'C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/SpatialPredictionPatterns/grid100_GlobalPredictors-HH.csv')
