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

#connect to yaml file
current_dir <- rstudioapi::getActiveDocumentContext()$path
# Move one level up in the directory
config_dir <- dirname(dirname(current_dir))
# Construct the path to the YAML configuration file
config_path <- file.path(config_dir, "config_02.yml")
# Read the YAML configuration file
config <- yaml.load_file(config_path)

# Use dirname() to get the parent directory
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))

## == define output path == ##
out_location <- config_02$out_location
out_location_dir <- normalizePath(file.path(parent_directory, out_location ), winslash = "/")

#IMPORT GEODATA

## == IMPORT NO2 MEASUREMENT STATIONS == ##
no2_dataset <- config_02$local$no2
no2_map_dir <- normalizePath(file.path(parent_directory, no2_dataset ), winslash = "/")

## == INITIAL DATASET == ## - LOCAL

#import initial dataset
#import csv (no2 measurement station data)
ms_stations <- read.csv(file = no2_map_dir , sep= ",")

#filter to only relevant columns
ms_stations <- ms_stations %>% dplyr::select(Lopend_gemiddelde, long, lat)

## == PRECIPITATION == ##

#import precipitation data
#first import all files in a single folder as a list 
#define current working directory
prec_tif_dir <- config_02$global$precipitation_tifs
prec_tif_map <- normalizePath(file.path(parent_directory, prec_tif_dir ), winslash = "/")
setwd(prec_tif_map ) #TIFS are results of script "AssignPrecipitationToMSs.R"

rastlist_precipitation <- list.files(getwd(), pattern='.tif$', all.files=TRUE, full.names=FALSE)
for(i in rastlist_precipitation) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) }

print(rastlist_precipitation)

#assign precipitation data
#create lists that will be used in while loop
tifs <- list(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
tifnames <- list('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

#define column with unique identifier - this will be used as basis for merging data to the measurement stations
ms_stations$M_id <- seq(1, nrow(ms_stations))

# make the SpatialPointsDataFrame object
coords <- ms_stations[, c("long", "lat")]
crs <- CRS("+proj=longlat +datum=WGS84")

#create spatial dataframe where precipitation values can be extracted to
spdf_ms_stations <- SpatialPointsDataFrame(coords      = coords,
                                      data        = ms_stations, 
                                      proj4string = crs)
#for extraction, a sf dataframe is necessary
ms_stations_sf <- st_as_sf(spdf_ms_stations)
ms_stations_sf <- st_transform(ms_stations_sf, crs = st_crs(tifs[[1]]))

#create list where variables can be stored to during iterations
ms_precmonths = list()
#Begin with first element of lists 
i = 1
j = 1
#While loop
while(i <= length(tifs))
{ print(tifs[[i]])
  ms_prec <- raster::extract(tifs[[i]], ms_stations_sf, sp=TRUE)
  #store variable in loop
  ms_prec <- as.data.frame(ms_prec)
  
  ms_precmonths[[paste0('prec', tifnames[[j]])]] <- ms_prec
  i = i+1
  j = j+1
}
#examine
summary(ms_precmonths)

#merge all data frames together
prec_per_month <- ms_precmonths %>% reduce(full_join, by='M_id')
#filter relevant columns
prec_per_month <- prec_per_month %>% dplyr::select(M_id, jan, feb, mar, apr, may, jun, jul, aug, oct, sep, oct, nov, dec, long.y, lat.y)

#Export option
#sf::st_write(prec_per_month, dsn=out_location_dir, layer='prec_per_month', driver = "ESRI Shapefile")

#couple to initial dataset
ms_precipitation <- left_join(ms_stations_sf, prec_per_month, by = "M_id")
print(ms_precipitation)

## == BUILDING DENSITY == ##

#import building density data
processed_building_dataset <- config_02$local$building_processed
processed_building_dataset_dir <- normalizePath(file.path(parent_directory, processed_building_dataset ), winslash = "/")
buildingdensity_ms <- readOGR(processed_building_dataset_dir)
#make spatial
buildingdensity_ms <- st_as_sf(buildingdensity_ms)

#make spatial dataframe - thereafter, conversion to sf is possible
prec_coords <- ms_precipitation[c("long", "lat"), ]
ms_precipitation <- as.data.frame(ms_precipitation)
spdf_prec <- SpatialPointsDataFrame(coords      = coords,
                                      data        = ms_precipitation, 
                                      proj4string = crs)

#convert to sf to perform st_transform
ms_precipitation_sf <- st_as_sf(spdf_prec)
#to perform spatial join, crs's of input data need to be equal
buildingdensity_ms <- st_transform(buildingdensity_ms, crs=crs)

#spatial join
ms_prec_bd <- st_join(ms_precipitation_sf, buildingdensity_ms, st_nearest_feature)

#clean data
#get rid off NA values
ms_prec_bd <- tidyr::replace_na(ms_prec_bd, list(BldDen100=0, BldDen500=0, BldDen1000=0))
#drop irrelevant columns
ms_prec_bd <- subset(ms_prec_bd, select = -c(long.y, lat.y, geometry) )


#Export option
#sf::st_write(ms_prec_bd, dsn=out_location_dir, layer='ms_prec_bd', driver = "ESRI Shapefile")

## == NDVI == ##

#import ndvi data
processed_ndvi_dataset <- config_02$local$ndvi_processed
processed_ndvi_dataset_dir <- normalizePath(file.path(parent_directory, processed_ndvi_dataset ), winslash = "/")
NDVI_ms <- readOGR(processed_ndvi_dataset_dir ) #via AssignNDVIToNO2MSs.R

#make spatial
NDVI_ms <- st_as_sf(NDVI_ms)
NDVI_ms <- st_transform(NDVI_ms, crs=st_crs(ms_prec_bd))

#spatial join
ms_prec_bd_ndvi <- st_join(ms_prec_bd, NDVI_ms, st_nearest_feature)


#clean data
#drop irrelevant columns
ms_prec_bd_ndvi <- subset(ms_prec_bd_ndvi, select = -c(coords_x1, coords_x2, M_id.y) )

#export option
# #shapefile
# sf::st_write(ms_prec_bd_ndvi, dsn=out_location_dir, layer='ms_prec_bd_ndvi', driver = "ESRI Shapefile")


## == TRAFFIC VOLUME == ##


#import traffic volume data
processed_traffic_dataset <- config_02$local$traffic_processed
processed_traffic_dataset_dir <- normalizePath(file.path(parent_directory, processed_traffic_dataset ), winslash = "/")
trafficvolume_ms <- readOGR(processed_traffic_dataset_dir) #via "Assigning TrafficVolume to NO2MSs.R"
#make spatial
trafficvolume_ms <- st_as_sf(trafficvolume_ms)
trafficvolume_ms <- st_transform(trafficvolume_ms, crs=st_crs(ms_prec_bd_ndvi))

#spatial join
ms_prec_bd_ndvi_traf <- st_join(ms_prec_bd_ndvi, trafficvolume_ms, st_nearest_feature)

#clean data

#drop irrelevant columns
ms_prec_bd_ndvi_traf <- subset(ms_prec_bd_ndvi_traf, select = -c(M_id.x) )
colnames(ms_prec_bd_ndvi_traf)

#export options
#shapefile
# sf::st_write(ms_prec_bd_ndvi_traf, dsn=out_location_dir, layer='ms_prec_bd_ndvi_traf', driver = "ESRI Shapefile")
# #csv
# write.csv(ms_prec_bd_ndvi_traf, out_location_dir + 'ms_prec_bd_ndvi_traf.csv',col.names = TRUE)

## == import geodata (5 tifs) == ##

#define current working directory
setwd("C:/Users/foeke/OneDrive/Documenten/april onwards/2022/tifs/5TIFS")

#import tif-files
rlist=list.files(getwd(), pattern="tif$", full.names=FALSE)
for(i in rlist) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) }

print(rlist)


## == extract to measurement stations Amsterdam == ##

#initialize lists that will be used in loop
#list of variables being used
PredictorTifs <- list(elevation,industry_100,industry_1000,industry_25,industry_300,industry_3000,industry_50, industry_500 ,
                      industry_5000,nightlight_3150,nightlight_450,nightlight_4950,nightlight_900,OMI_mean_filt,population_1000,population_3000,population_5000   
                      ,radiation, road_class_1_100 , road_class_1_1000
                      ,road_class_1_25,road_class_1_300,road_class_1_3000, road_class_1_50  
                      ,road_class_1_500,  road_class_1_5000, road_class_2_100,  road_class_2_1000
                      ,road_class_2_25 ,road_class_2_300 , road_class_2_3000, road_class_2_50  
                      ,road_class_2_500 , road_class_2_5000, road_class_3_100,  road_class_3_1000
                      ,road_class_3_25 ,  road_class_3_300 , road_class_3_3000, road_class_3_50  
                      ,road_class_3_500 , road_class_3_5000, Rsp,temperature_2m_1 
                      ,temperature_2m_10, temperature_2m_11, temperature_2m_12, temperature_2m_2 
                      ,temperature_2m_3,  temperature_2m_4,  temperature_2m_5,  temperature_2m_6 
                      ,temperature_2m_7,  temperature_2m_8 , temperature_2m_9  ,trop_mean_filt   
                      ,wind_speed_10m_1 , wind_speed_10m_10, wind_speed_10m_11,wind_speed_10m_12
                      ,wind_speed_10m_2 , wind_speed_10m_3 , wind_speed_10m_4 , wind_speed_10m_5 
                      ,wind_speed_10m_6,  wind_speed_10m_7,  wind_speed_10m_8,  wind_speed_10m_9)

#list of names being used for assigning to variables
PredictorTifs_names <- list("elevation","industry_100","industry_1000","industry_25","industry_300","industry_3000","industry_50", 
                            "industry_500" ,"industry_5000","nightlight_3150","nightlight_450","nightlight_4950","nightlight_900","OMI_mean_filt","population_1000","population_3000","population_5000"   
                            ,"radiation" ,        "road_class_1_100" , "road_class_1_1000"
                            ,"road_class_1_25","road_class_1_300","road_class_1_3000", "road_class_1_50"  
                            ,"road_class_1_500",  "road_class_1_5000", "road_class_2_100",  "road_class_2_1000"
                            ,"road_class_2_25" ,"road_class_2_300" , "road_class_2_3000", "road_class_2_50"  
                            ,"road_class_2_500" , "road_class_2_5000", "road_class_3_100",  "road_class_3_1000"
                            ,"road_class_3_25" ,  "road_class_3_300" , "road_class_3_3000", "road_class_3_50"  
                            ,"road_class_3_500" , "road_class_3_5000", "Rsp"   ,            "temperature_2m_1" 
                            ,"temperature_2m_10", "temperature_2m_11", "temperature_2m_12", "temperature_2m_2" 
                            ,"temperature_2m_3",  "temperature_2m_4",  "temperature_2m_5",  "temperature_2m_6" 
                            ,"temperature_2m_7",  "temperature_2m_8" , "temperature_2m_9"  ,"trop_mean_filt"   
                            ,"wind_speed_10m_1" , "wind_speed_10m_10", "wind_speed_10m_11", "wind_speed_10m_12"
                            ,"wind_speed_10m_2" , "wind_speed_10m_3" , "wind_speed_10m_4" , "wind_speed_10m_5" 
                            ,"wind_speed_10m_6",  "wind_speed_10m_7",  "wind_speed_10m_8",  "wind_speed_10m_9")


#similar crs of all data used
print(ms_prec_bd_ndvi_traf)

ms_prec_bd_ndvi_traf <- st_transform(ms_prec_bd_ndvi_traf, crs=st_crs(PredictorTifs[[1]]))
Ms_M_id = ms_prec_bd_ndvi_traf[, "M_id"]
print(Ms_M_id)

MS_5TIFS = list()
i=1
j=1

#While loop
while(i <= length(PredictorTifs))
{ 
  print(PredictorTifs[[i]])  
  centroid_pred <- raster::extract(PredictorTifs[[i]], Ms_M_id, sp=TRUE)
  #store variable in loop
  centroid_pred  <- as.data.frame(centroid_pred )
  MS_5TIFS[[paste0('pred_', PredictorTifs_names[[j]])]] <- centroid_pred 
  i = i+1
  j = j+1
}

#examine
summary(MS_5TIFS)

#merge all data frames together
MS_5TIFS_all <- MS_5TIFS %>% reduce(full_join, by='M_id')


#clean data
#filter columns
MS_5TIFS_all <- MS_5TIFS_all %>% dplyr::select(M_id,elevation,industry_100,industry_1000,industry_25,industry_300,industry_3000,industry_50, industry_500 ,
                                                     industry_5000,nightlight_3150,nightlight_450,nightlight_4950,nightlight_900,OMI_mean_filt,population_1000,population_3000,population_5000   
                                                     ,radiation, road_class_1_100 , road_class_1_1000
                                                     ,road_class_1_25,road_class_1_300,road_class_1_3000, road_class_1_50  
                                                     ,road_class_1_500,  road_class_1_5000, road_class_2_100,  road_class_2_1000
                                                     ,road_class_2_25 ,road_class_2_300 , road_class_2_3000, road_class_2_50  
                                                     ,road_class_2_500 , road_class_2_5000, road_class_3_100,  road_class_3_1000
                                                     ,road_class_3_25 ,  road_class_3_300 , road_class_3_3000, road_class_3_50  
                                                     ,road_class_3_500 , road_class_3_5000, Rsp,temperature_2m_1 
                                                     ,temperature_2m_10, temperature_2m_11, temperature_2m_12, temperature_2m_2 
                                                     ,temperature_2m_3,  temperature_2m_4,  temperature_2m_5,  temperature_2m_6 
                                                     ,temperature_2m_7,  temperature_2m_8 , temperature_2m_9  ,trop_mean_filt   
                                                     ,wind_speed_10m_1 , wind_speed_10m_10, wind_speed_10m_11,wind_speed_10m_12
                                                     ,wind_speed_10m_2 , wind_speed_10m_3 , wind_speed_10m_4 , wind_speed_10m_5 
                                                     ,wind_speed_10m_6,  wind_speed_10m_7,  wind_speed_10m_8,  wind_speed_10m_9,coords.x1.x, coords.x2.x)



#rename
MS_5TIFS_all<- MS_5TIFS_all %>% rename(Longitude = coords.x1.x, Latitude=coords.x2.x)
#join based on column with unique identifier
MS_Amsterdam_AllPredictors = merge( MS_5TIFS_all,ms_prec_bd_ndvi_traf, by='M_id')
#verify
print(MS_Amsterdam_AllPredictors)

#crs 3035 - Lambert Azimuthal Equal Area projection
MS_Amsterdam_AllPredictors <- st_as_sf(MS_Amsterdam_AllPredictors)
MS_Amsterdam_AllPredictors <- st_transform(MS_Amsterdam_AllPredictors, crs=3035)

#export options
#shapefile
sf::st_write(MS_Amsterdam_AllPredictors, dsn=out_location_dir, layer='ModellingDataset-Local', driver = "ESRI Shapefile")
#for csv, spatial data is not necessary
MS_Amsterdam_AllPredictors<- as.data.frame(MS_Amsterdam_AllPredictors)
MS_Amsterdam_AllPredictors <- dplyr::select(MS_Amsterdam_AllPredictors, -c(M_id, geometry)) #remove geometry column
MS_Amsterdam_AllPredictors
#csv
write.csv(MS_Amsterdam_AllPredictors, out_location_dir + 'ModellingDataset-Local.csv',col.names = TRUE)
