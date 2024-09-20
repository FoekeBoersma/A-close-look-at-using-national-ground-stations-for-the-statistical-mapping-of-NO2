#necessary libraries
library(rgdal)
library(raster)
require(sf)
library(ggplot2)
library(raster)
library(rgeos)
library(tidyverse)
library(leaflet)
library(sp)
library(bnspatial)
library(gstat)
library(leaflet) #mapping in OSM
library(tmap)
library(gstat) #functionality for kriging
require(mapview)
library(stars) #necessary for st_rasterize
require(gridExtra)
require(raster)
library(rstudioapi)
library(yaml)

## == IMPORT GEODATA == ##
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
prec_map_relative <- config$global$precipitation

# Construct the full path to the NDVI map directory based on the config file location
precipitation_map_dir <- normalizePath(file.path(parent_directory, prec_map_relative), winslash = "/")

#import csv of monthly precipitation (2017)
#import measurement stations of "precipitation"
precipitation <- read.csv(precipitation, sep = ',')

## == define output path == ##
out_location <- config$out_location
out_location_dir <- normalizePath(file.path(parent_directory, out_location ), winslash = "/")

## == DATA PROCESSING == ##

#drop irrelevant columns (non-nummeric)
precipitation_processed <- subset(precipitation, select = -c(STATION, NAME, PRCP_ATTRIBUTES))

## == Create while loop with intention of creating precipitation tif per month == ##

#initialize lists that will be relevant for iterations in while loop
months = list("2017-01", "2017-02", "2017-03","2017-04","2017-05","2017-06",
              "2017-07","2017-08","2017-09","2017-10","2017-11","2017-12")

month_abr = list("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep",
                 "oct", "nov", "dec")

#create empty list where datasets, created in loop, will be stored to
list_months = list()
i = 1
j = 1
while(i <= length(months)){
  #filter month 
  precipitation_month <- subset(precipitation_processed, DATE == months[[i]])
  
  #make spatial dataframe
  precipitation_month_sf <- st_as_sf(precipitation_month, coords = c("LONGITUDE", "LATITUDE"))
  st_crs(precipitation_month_sf) <- "+proj=longlat +datum=WGS84"
  #convert to metric projection system
  precipitation_month_3035 <- st_transform(precipitation_month_sf, 3035)


  #verify is na are apparent
  #is.na(precipitation_month_3035$PRCP)
  #remove na-values
  precipitation_month_3035 <- precipitation_month_3035[!is.na(precipitation_month_3035$PRCP),]
  #verify
  #view(precipitation_jan_3035)

  #create training and testing datasets for examining kriging predictions
  dataset = sort(sample(nrow(precipitation_month_3035), nrow(precipitation_month_3035)*.8)) #training = 80% (0.8)
  train <- precipitation_month_3035[dataset,]
  test <- precipitation_month_3035[-dataset,]
  
  #export option
  #write.csv(train, paste0(out_location_dir, month_abr[[j]], '.csv'),col.names = TRUE)
  #write.csv(test, paste0(out_location_dir, month_abr[[j]], '.csv'),col.names = TRUE)
  
  #spatial data option
  train_sf <- st_as_sf(train)
  test_sf <- st_as_sf(test)
  layer_train  <- paste0('train_', month_abr[[j]], '.shp')
  #sf::st_write(train_sf, dsn=out_location_dir, layer=layer_train, driver = "ESRI Shapefile")
  layer_test  <- paste0('test_', month_abr[[j]], '.shp')
  #sf::st_write(test_sf, dsn=out_location_dir, layer=layer_test, driver = "ESRI Shapefile")

  #define area of interest
  bbox <- st_bbox(train)

  precipitation_grid <- train %>% 
    st_bbox() %>%     # determines bounding box coordinates from precipitation_pro_3035
    st_as_sfc() %>%   # creates sfc object from bounding box
    st_make_grid(     # create grid 1000 x 1000 pixel size
      cellsize = c(1000, 1000), 
      what = "centers") %>%
    st_as_sf(crs=st_crs(train)) # convert to sf object

  # Convert precipitation samples to SpatialPointsDataFrame
  precipitation_sp <- as(train, "Spatial")

  # Convert precipitation grid to SpatialPixelsDataFrame, the raster/grid equivalent in 
  # in the sp world. Necessary for further data processing
  precipitation_grid_sp <- as(as(precipitation_grid, "Spatial"), "SpatialPixels")

  ## == VARIOGRAM == ##

  # EXAMINE RANGE OPTIONS - CUTOFF

  #determine diagonal distance of bounding box - for cutoff 
  bbox <- st_bbox(train)

  #formula diagonal distance
  diagdis = sqrt((bbox$xmax-bbox$xmin)**2 + (bbox$ymax-bbox$ymin)**2)

  #determine average distance in m between points
  m <- st_distance(train)
  m/1000
  me <- mean(m)
  
  #general rule cutoff: diagonal distance / 3
  cutoff = diagdis / 3
  width = cutoff / 20 #distance interval over which point pairs are averaged in bins
  #show variogram
  plot(variogram(log(PRCP) ~ 1, train, cutoff = 160000, width=8000))

  ## ORDINARY KRIGING 

  #create sample variogram
  precipitation.variogram <- variogram(log(PRCP) ~ 1, train)
  #examine
  #plot(precipitation.variogram)
  #fit variogram model (fixed). 
  precipitation.vfit <- fit.variogram(precipitation.variogram, vgm(psill=0.1, "Sph", range=150000, nugget=0.02))

  #ordinary kriging
  lz.ok <- krige(log(PRCP) ~1, precipitation_sp, precipitation_grid_sp, precipitation.vfit)
  
  lz.ok@data$var1.pred <- exp(lz.ok@data$var1.pred)
  #plot(lz.ok['var1.pred'])

  ## CONVERT TO RASTER
  rasterDF <- raster(lz.ok)
  #export option
  writeRaster(rasterDF, paste0(out_location_dir, month_abr[[j]], '.tif'))
  
  #store variable in loop
  list_months[[paste0('OKprecipitation', month_abr[[j]])]] <- lz.ok
  
  #extract raster values to test dataset to measure performance
  r <- raster(lz.ok)

  Evaluation = raster::extract(r, test_sf, sp=T) #sp = T: keep all data
  #convert back to spatial dataframe 
  Evaluation <- st_as_sf(Evaluation)
  Evaluation$error = (Evaluation$PRCP - Evaluation$var1.pred)

  #export option
  layer  <- paste0('evaluation_', month_abr[[j]], '.shp')
  #sf::st_write(Evaluation, dsn=out_location_dir, layer=layer, driver = "ESRI Shapefile")
  
  #indicate process
  print("next month")
         
  #next month     
  i = i+1
  j = j+1
  }