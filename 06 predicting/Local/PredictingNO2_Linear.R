## == Necessary packages == ##

# We will need some packages for (spatial) data processing
library(tidyverse) # wrangling tabular data and plotting
library(sf) # processing spatial vector data - the easy way
library(sp) # processing spatial vector data - the way gstat needs it
library(raster) # processing spatial raster data. !!!overwrites dplyr::select!!!
library(rgdal) #import shapefiles
library(rgeos) #contains gCentroid
library(tidyr) #geometry to apart long/lat
library(dismo) #for kfold
library(lme4) #for mixed models (random effects)
library(stats) #quantile
library(nlme) #mixed-effect model
library('parallel') 
# Packages for geostatistics
library(gstat)   # The most popular R-Package for Kriging 
library(automap) # Automatize some (or all) parts of the gstat-workflow 

## == DEFINE COORDINATE SYSTEMS == ##

#CRS with metric system is preferred (=3035).

crs <- CRS("+proj=longlat +datum=WGS84") # crs

## == import geodata == ##

data <- read.csv('C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/LocalModelData/PredictingDataset.csv', sep=',')
#replace NA with 0
data[is.na(data)] <- 0

data

#make imported csv spatial

#to sf
data_sf = st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
#change to planar crs
data_3035 <- st_transform(data_sf,crs=3035)

data_3035

#import grid where predictions will be projected on
grid100 = readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/Grid100_LocalPredictors_Amsterdam.gpkg')

#make spatial - obtain the geometry for each sample
grid100_sf <- st_as_sf(grid100)
#make key qith unique fid that will be used for joining all local datasets and related predictions
grid100_sf$key = seq(1,nrow(grid100_sf))

## == modelling == ##

# Fit a model on the training data - linear, use whole dataset (=132 observations)
model <- lm(Lopend_gemiddelde ~ 1 + nightlight_450 +  nightlight_4950  + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data = data_3035)
#predict on the grid
grid100_sf$predNO2_Lin <- predict(model, grid100_sf)

## == export option == ##
sf::st_write(grid100_sf, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/LocalModels/predictedNO2_Linear.gpkg", driver = "GPKG")
