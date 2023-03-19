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

## IMPORT GEODATA

#import csv of monthly precipitation (2017)
#import measurement stations of "precipitation"
prec <- read.csv('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/tifs/precipitation/MonthlyPrecipitation.csv', sep = ',')
#examine
#view(prec)

## DATA PROCESSING

#drop irrelevant columns (non-nummeric)
prec_pro <- subset(prec, select = -c(STATION, NAME, PRCP_ATTRIBUTES))
#view(prec_pro)

## == Create while loop with intention of creating precipitation tif per month == ##

#initialize lists that will be relevant for iterations in while loop
months = list("2017-01", "2017-02", "2017-03","2017-04","2017-05","2017-06",
              "2017-07","2017-08","2017-09","2017-10","2017-11","2017-12")

month_abr = list("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep",
                 "oct", "nov", "dec")

list_months = list()
i = 1
j = 1
while(i <= length(months)){
  #filter month 
  prec_month <- subset(prec_pro, DATE == months[[i]])
  
  #make spatial dataframe
  prec_month_sf <- st_as_sf(prec_month, coords = c("LONGITUDE", "LATITUDE"))
  st_crs(prec_month_sf) <- "+proj=longlat +datum=WGS84"
  #convert to metric projection system
  prec_month_3035 <- st_transform(prec_month_sf, 3035)

  #deleting na-values
  #examine data
  #view(prec_month_3035)
  #verify is na are apparent
  #is.na(prec_month_3035$PRCP)
  #remove na-values
  prec_month_3035 <- prec_month_3035[!is.na(prec_month_3035$PRCP),]
  #verify
  #view(prec_jan_3035)

  ## CREATE TRAIN- AND TEST DATASETS
  dataset = sort(sample(nrow(prec_month_3035), nrow(prec_month_3035)*.8)) #training = 80% (0.8)
  train <- prec_month_3035[dataset,]
  test <- prec_month_3035[-dataset,]
  
  #export option
  write.csv(train, paste0('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/tifs/precipitation/csv/train', month_abr[[j]], '.csv'),col.names = TRUE)
  write.csv(test, paste0('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/tifs/precipitation/csv/test', month_abr[[j]], '.csv'),col.names = TRUE)
  
  #export option - spatial
  #convert dataframes to spatial dataframes
  train_sf <- st_as_sf(train)
  test_sf <- st_as_sf(test)
  layer_train  <- paste0('train_', month_abr[[j]], '.shp')
  sf::st_write(train_sf, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/tifs/precipitation/Evaluation", layer=layer_train, driver = "ESRI Shapefile")
  layer_test  <- paste0('test_', month_abr[[j]], '.shp')
  sf::st_write(test_sf, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/tifs/precipitation/Evaluation", layer=layer_test, driver = "ESRI Shapefile")

  # view(train)
  # view(test)
  
  #define area of interest
  bbox <- st_bbox(train)

  prec_grid <- train %>% 
    st_bbox() %>%     # determines bounding box coordinates from prec_pro_3035
    st_as_sfc() %>%   # creates sfc object from bounding box
    st_make_grid(     # create grid 1000 x 1000 pixel size
      cellsize = c(1000, 1000), 
      what = "centers") %>%
    st_as_sf(crs=st_crs(train)) # convert to sf object

  # Convert prec samples to SpatialPointsDataFrame
  prec_sp <- as(train, "Spatial")

  # Convert prec grid to SpatialPixelsDataFrame, the raster/grid equivalent in 
  # in the sp world
  prec_grid_sp <- as(as(prec_grid, "Spatial"), "SpatialPixels")

  ## VARIOGRAM

  ## EXAMINE RANGE OPTIONS - CUTOFF

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
  prec.variogram <- variogram(log(PRCP) ~ 1, train)
  #examine
  #plot(prec.variogram)
  #fit variogram model (fixed). 
  prec.vfit <- fit.variogram(prec.variogram, vgm(psill=0.1, "Sph", range=150000, nugget=0.02))

  #ordinary kriging
  lz.ok <- krige(log(PRCP) ~1, prec_sp, prec_grid_sp, prec.vfit)
  
  lz.ok@data$var1.pred <- exp(lz.ok@data$var1.pred)
  #plot(lz.ok['var1.pred'])

  ## CONVERT TO RASTER
  rasterDF <- raster(lz.ok)
  
  writeRaster(rasterDF, paste0('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/tifs/precipitation/tifsMonths/', month_abr[[j]], '.tif'))
  
  #store variable in loop
  list_months[[paste0('OKprec', month_abr[[j]])]] <- lz.ok
  
  #extract raster values to test dataset to measure performance
  r <- raster(lz.ok)
  plot(r)
  Evaluation = raster::extract(r, test_sf, sp=T) #sp = T: keep all data
  #convert back to spatial dataframe 
  Evaluation <- st_as_sf(Evaluation)
  Evaluation$error = (Evaluation$PRCP - Evaluation$var1.pred)

  #export option
  layer  <- paste0('evaluation_', month_abr[[j]], '.shp')
  sf::st_write(Evaluation, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/tifs/precipitation/Evaluation", layer=layer, driver = "ESRI Shapefile")
  
  #indicate process
  print("next month")
         
  #next month     
  i = i+1
  j = j+1
  }