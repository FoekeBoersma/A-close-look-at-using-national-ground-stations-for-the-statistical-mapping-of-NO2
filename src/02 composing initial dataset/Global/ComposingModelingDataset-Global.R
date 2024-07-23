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


## == INITIAL DATASET == ## - global dataset

#import initial dataset - csv (no2 measurement station data)
ms_stations <- read.csv(file = 'C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/GlobalModelData/InitialGlobalDataset.csv', sep= ";")
#define column with unique identifier
ms_stations$M_id <- seq(1, nrow(ms_stations))

## == adding predictors to global dataset == ##

## == PRECIPITATION == ##

#import precipitation data
#first import all files in a single folder as a list 
#define current working directory
setwd("C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/Precipitation/TIFS")  #TIFS are results of script "AssignPrecipitationToMSs.R"

rastlist_precipitation <- list.files(getwd(), pattern='.tif$', all.files=TRUE, full.names=FALSE)
for(i in rastlist_precipitation) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) }

#assign precipitation data
#create lists that will be used in while loop
tifs <- list(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
tifnames <- list('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')


# make the SpatialPointsDataFrame object
coords <- ms_stations[, c("Longitude", "Latitude")]
crs <- CRS("+proj=longlat +datum=WGS84")

#create spatial dataframe where precipitation values can be extracted to
spdf_ms_stations <- SpatialPointsDataFrame(coords      = coords,
                                      data        = ms_stations, 
                                      proj4string = crs)
#for extraction, a sf dataframe is necessary
spdf_ms_stations <- st_as_sf(spdf_ms_stations)
spdf_ms_stations <- st_transform(spdf_ms_stations, crs = st_crs(tifs[[1]]))
spdf_nodata <- spdf_ms_stations[,("M_id")] #use only column containing unique identifier

#create list where variables can be stored to during iterations
ms_precmonths = list()
#Begin with first element of lists 
i = 1
j = 1
#While loop
while(i <= length(tifs))
{ ms_prec <- raster::extract(tifs[[i]], spdf_nodata, sp=TRUE)
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
# couple to initial dataset
prec <- merge(spdf_ms_stations,prec_per_month,  by='M_id')
colnames(prec)
#remove unnecessary columns
prec <- prec %>% dplyr::select(-contains("coords"))

#Export option
#sf::st_write(prec_per_month, dsn="C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject22-precipitation", layer='prec_per_month', driver = "ESRI Shapefile")

## == BUILDING DENSITY == ##

#import building density data
buildingdensity_ms <- readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/BuildingDensity/processed/BldDen_ms_StudyArea_Global.shp')
#make spatial
buildingdensity_ms_sf <- st_as_sf(buildingdensity_ms)

#to perform spatial join, crs's of input data need to be equal
buildingdensity_ms_sf <- st_transform(buildingdensity_ms_sf, crs=st_crs(prec))
#spatial join

ms_prec_bd <- st_join(prec, buildingdensity_ms_sf, st_nearest_feature)

#clean data
#get rid off NA values
ms_prec_bd <- tidyr::replace_na(ms_prec_bd, list(BldDen100=0, BldDen500=0, BldDen1000=0))
#drop irrelevant columns
ms_prec_bd <- subset(ms_prec_bd, select = -c(M_id.y) )

#Export option
#sf::st_write(ms_prec_bd, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForModelling", layer='ms_prec_bd', driver = "ESRI Shapefile")

## == NDVI == ##

#import NDVI data
NDVI_ms <- readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/NDVI/processed/ms_NDVI_global.shp') #via AssignNDVIToNO2MSs.R
#make spatial
NDVI_ms <- st_as_sf(NDVI_ms)
NDVI_ms <- st_transform(NDVI_ms, crs=st_crs(ms_prec_bd))

#spatial join
ms_prec_bd_ndvi <- st_join(ms_prec_bd, NDVI_ms, st_nearest_feature)

print(ms_prec_bd_ndvi)
#clean data
#drop irrelevant columns
ms_prec_bd_ndvi <- subset(ms_prec_bd_ndvi, select = -c(coords_x1, coords_x2, FID.y) )
#Examine
#view(ms_prec_bd_ndvi)

#export option
# #shapefile
# sf::st_write(ms_prec_bd_ndvi, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForModelling", layer='ms_prec_bd_ndvi', driver = "ESRI Shapefile")


## == TRAFFIC VOLUME == ##

#import traffic volume data
trafficvolume_ms <- readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/Traffic/processed/traffic_NO2_Global.shp') #via "Assigning TrafficVolume to NO2MSs.R"
#make spatial
trafficvolume_ms <- st_as_sf(trafficvolume_ms)
trafficvolume_ms <- st_transform(trafficvolume_ms, crs=st_crs(ms_prec_bd_ndvi))
#spatial join
ms_prec_bd_ndvi_traf <- st_join(ms_prec_bd_ndvi, trafficvolume_ms, st_nearest_feature)

#clean data

#drop irrelevant columns
ms_prec_bd_ndvi_traf <- subset(ms_prec_bd_ndvi_traf, select = -c(M_id.x, M_id) )
#rename
ms_prec_bd_ndvi_traf <- ms_prec_bd_ndvi_traf %>% rename(FID = FID.x)
#examine
view(ms_prec_bd_ndvi_traf)

colnames(ms_prec_bd_ndvi_traf)


ms_prec_bd_ndvi_traf <- replace_na(ms_prec_bd_ndvi_traf, list(trafBuf25=0, trafBuf50=0, trafBuf100=0, trafBuf400=0, trafBuf800=0))

#export options
#shapefile
sf::st_write(ms_prec_bd_ndvi_traf, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/GlobalModelData", layer='ModellingDataset-Global', driver = "ESRI Shapefile")
ms_prec_bd_ndvi_traf <- as.data.frame(ms_prec_bd_ndvi_traf)
ms_prec_bd_ndvi_traf <- dplyr::select(ms_prec_bd_ndvi_traf, -c(FID, geometry)) #remove geometry column
ms_prec_bd_ndvi_traf

#csv
write.csv(ms_prec_bd_ndvi_traf, 'C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/GlobalModelData/ModellingDataset-Global.csv',col.names = TRUE)


