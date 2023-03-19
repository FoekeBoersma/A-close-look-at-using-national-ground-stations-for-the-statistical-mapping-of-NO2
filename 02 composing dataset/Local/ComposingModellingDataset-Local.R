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


## == INITIAL DATASET == ## - LOCAL

#import initial dataset
#import csv (no2 measurement station data)
ms <- read.csv(file = 'C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForModelling/LocalModels/LocalMeasurementStations.csv', sep= ",")

#view(ms_sf)

## == PRECIPITATION == ##

#import precipitation data
#first import all files in a single folder as a list 
#define current working directory
setwd("C:/Users/foeke/OneDrive/Documenten/april onwards/2022/tifs/precipitation/tifsMonths")

rastlist_precipitation <- list.files(getwd(), pattern='.tif$', all.files=TRUE, full.names=FALSE)
for(i in rastlist_precipitation) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) }

print(rastlist_precipitation)

#assign precipitation data
#create lists that will be used in while loop
tifs <- list(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
tifnames <- list('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

#define column with unique identifier
ms$M_id <- seq(1, nrow(ms))
ms_nodata <- ms %>% dplyr::select(M_id, long, lat)
# make the SpatialPointsDataFrame object
coords <- ms[, c("long", "lat")]
crs <- CRS("+proj=longlat +datum=WGS84")

#create spatial dataframe where precipitation values can be extracted to
spdf_nodata <- SpatialPointsDataFrame(coords      = coords,
                                      data        = ms_nodata, 
                                      proj4string = crs)
#for extraction, a sf dataframe is necessary
ms_nodata <- st_as_sf(spdf_nodata)
ms_nodata <- st_transform(ms_nodata, crs = st_crs(tifs[[1]]))

#create list where variables can be stored to during iterations
ms_precmonths = list()
#Begin with first element of lists 
i = 1
j = 1
#While loop
while(i <= length(tifs))
{ print(tifs[[i]])
  ms_prec <- raster::extract(tifs[[i]], ms_nodata, sp=TRUE)
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
#sf::st_write(prec_per_month, dsn="C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-precipitation", layer='prec_per_month', driver = "ESRI Shapefile")

#couple to initial dataset
ms_precipitation <- left_join(ms, prec_per_month, by = "M_id")
#view(ms_precipitation)

## == BUILDING DENSITY == ##

#import building density data
buildingdensity_ms <- readOGR('C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-Local/BuildingDensity/BldDen_ms_StudyArea.shp')
#make spatial
buildingdensity_ms <- st_as_sf(buildingdensity_ms)

#make spatial dataframe - thereafter, conversion to sf is possible
prec_coords <- ms_precipitation[c("long", "lat"), ]
spdf_prec <- SpatialPointsDataFrame(coords      = coords,
                                      data        = ms_precipitation, 
                                      proj4string = crs)

#convert to sf to perform st_transform
ms_precipitation_sf <- st_as_sf(spdf_prec)
#to perform spatial join, crs's of input data need to be equal
buildingdensity_ms <- st_transform(buildingdensity_ms, crs=crs)

#spatial join
ms_prec_bd <- st_join(ms_precipitation_sf, buildingdensity_ms, st_nearest_feature)
#view(ms_prec_bd)

#clean data
#get rid off NA values
ms_prec_bd <- tidyr::replace_na(ms_prec_bd, list(BldDen100=0, BldDen500=0, BldDen1000=0))
#drop irrelevant columns
ms_prec_bd <- subset(ms_prec_bd, select = -c(long.y, lat.y, geometry) )

#Export option
#sf::st_write(ms_prec_bd, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForModelling", layer='ms_prec_bd', driver = "ESRI Shapefile")

## == NDVI == ##

#import NDVI data
NDVI_ms <- readOGR('C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-Local/NDVI/ms_NDVI.shp')
#examine
#view(NDVI_ms)

NDVI_ms <- st_as_sf(NDVI_ms)
NDVI_ms <- st_transform(NDVI_ms, crs=st_crs(ms_prec_bd))

#spatial join
ms_prec_bd_ndvi <- st_join(ms_prec_bd, NDVI_ms, st_nearest_feature)

#clean data
#drop irrelevant columns
ms_prec_bd_ndvi <- subset(ms_prec_bd_ndvi, select = -c(coords_x1, coords_x2, M_id.y) )
#Examine
#view(ms_prec_bd_ndvi)

#export option
# #shapefile
# sf::st_write(ms_prec_bd_ndvi, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForModelling", layer='ms_prec_bd_ndvi', driver = "ESRI Shapefile")


## == TRAFFIC VOLUME == ##


#import traffic volume data
trafficvolume_ms <- readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForModelling/LocalModels/TrafficLocal/traffic_NO2_Local.shp')
#make spatial
trafficvolume_ms <- st_as_sf(trafficvolume_ms)
trafficvolume_ms <- st_transform(trafficvolume_ms, crs=st_crs(ms_prec_bd_ndvi))

#spatial join
ms_prec_bd_ndvi_traf <- st_join(ms_prec_bd_ndvi, trafficvolume_ms, st_nearest_feature)

#clean data

#drop irrelevant columns
ms_prec_bd_ndvi_traf <- subset(ms_prec_bd_ndvi_traf, select = -c(M_id.x, M_id.y) )
#examine
#view(ms_prec_bd_ndvi_traf)

colnames(ms_prec_bd_ndvi_traf)

#drop irrelevant columns
ms_prec_bd_ndvi_traf <- subset(ms_prec_bd_ndvi_traf, select = -c(Vierweekse_1, Vierweekse_2 ,Vierweekse_3,Vierweekse_4,Vierweekse_5,Vierweekse_6,Vierweekse_7,Vierweekse_8,Vierweekse_9,Vierweekse_10,Vierweekse_11,Vierweekse_12,Vierweekse_13,Locatie, X, CodeW,Jaar))
#examine
print(ms_prec_bd_ndvi_traf)


#export options
#shapefile
# sf::st_write(ms_prec_bd_ndvi_traf, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForModelling", layer='ms_prec_bd_ndvi_traf', driver = "ESRI Shapefile")
# #csv
# write.csv(ms_prec_bd_ndvi_traf, 'C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForModelling/ms_prec_bd_ndvi_traf.csv',col.names = TRUE)



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

#view(centroids_5Tifs)

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

print(MS_5TIFS_all)


MS_Amsterdam_AllPredictors = merge( MS_5TIFS_all,ms_prec_bd_ndvi_traf, by='M_id')

print(MS_Amsterdam_AllPredictors)

#drop irrelevant columns
MS_Amsterdam_AllPredictors <- subset(MS_Amsterdam_AllPredictors, select = -c(Longitude, Latitude))
#rename other longitude/latitude (in wgs84)
MS_Amsterdam_AllPredictors <- MS_Amsterdam_AllPredictors %>% rename(Latitude = lat, Longitude= long)
#examine
print(MS_Amsterdam_AllPredictors)

#export options
#shapefile
sf::st_write(MS_Amsterdam_AllPredictors, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForModelling/LocalModels", layer='ModellingDataset-Local', driver = "ESRI Shapefile")
#csv
write.csv(MS_Amsterdam_AllPredictors, 'C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForModelling/LocalModels/ModellingDataset-Local.csv',col.names = TRUE)
