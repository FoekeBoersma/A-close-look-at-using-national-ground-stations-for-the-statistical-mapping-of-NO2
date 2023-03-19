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

# Finally, some packages to make pretty plots
library(patchwork)
library(viridis)
library(tmap)
library(graphics) #for text


## == DEFINE COORDINATE SYSTEMS == ##

#CRS with metric system is preferred (=crs_32).

crs <- CRS("+proj=longlat +datum=WGS84") # crs
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs_32 <- CRS(sprintf(utmStr, 32))

## == import geodata == ##

data <- read.csv('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForModelling/LocalModels/Local_ModellingDataset_distance-processed.csv', sep=';')
#replace NA with 0
data[is.na(data)] <- 0

#make imported csv spatial

#to sf
data_sf = st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
#change to planar crs
data_32 <- st_transform(data_sf,crs=crs_32)

## == create column relating to spatial characterization (e.g. distance to road/population) == ##

#examine basic data statistics of variables that will be used for quantile filtering!
quantile(data_32$road_class_3_100)
quantile(data_32$population_1000)

#spatial character: group "urban"
data_32$spachar = ifelse(data_32$population_1000 > quantile(data_32$population_1000, 0.5) & ((data_32$road_class_2_100 > 0 | data_32$road_class_1_100 > 0) | data_32$road_class_3_100 > quantile(data_32$road_class_3_100, 0.5)), 1, 0)

#spatial character: group "low population"
data_32$spachar = ifelse(data_32$population_1000 < quantile(data_32$population_1000, 0.5) & ((data_32$road_class_2_100 > 0 | data_32$road_class_1_100 > 0) | data_32$road_class_3_100 > quantile(data_32$road_class_3_100, 0.5)), 2, data_32$spachar)

#spatial character: group "far from road"
data_32$spachar = ifelse((data_32$spachar == 1 | data_32$spachar == 2), data_32$spachar, 3)

#import grid where predictions will be projected on
grid100 = readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Amsterdam/Grid100_LocalPredictors_Amsterdam_inclSelVars.gpkg')

#make spatial - obtain the geometry for each sample
grid100_sf <- st_as_sf(grid100)
#make key qith unique fid that will be used for joining all local datasets and related predictions
grid100_sf$key = seq(1,nrow(grid100_sf))

#add a column to the 100m grid describing the spatial group where each sample belongs to

#assign to variable
population1000_05 = as.vector(quantile(data_32$population_1000, 0.5))
roadclass3_100_05 = as.vector(quantile(data_32$road_class_3_100, 0.5)) #as.vector necessary to only obtain value

#spatial character: group "urban" - higher than ,5 quantile of population 1000 - within 100m of road class 1 OR 2 OR higher than .5 quantile of road class 3 100.
grid100_sf$spachar = ifelse(grid100_sf$population_1000 > population1000_05 & ((grid100_sf$road_class_2_100 > 0 | grid100_sf$road_class_1_100 > 0) | grid100_sf$road_class_3_100 > roadclass3_100_05), 1, 0)

#spatial character: group "low population" - lower than ,5 quantile of population 1000 - within 100m of road class 1 OR 2 OR higher than .5 quantile of road class 3 100.
grid100_sf$spachar = ifelse(grid100_sf$population_1000 < population1000_05 & ((grid100_sf$road_class_2_100 > 0 | grid100_sf$road_class_1_100 > 0) | grid100_sf$road_class_3_100 < roadclass3_100_05), 2, grid100_sf$spachar)

#spatial character: group "far from road" - all other samples (i.e. further away than 100m from road class 1/2 OR lower than .5 quantile of road class 3 100)
grid100_sf$spachar = ifelse((grid100_sf$spachar == 1 | grid100_sf$spachar == 2), grid100_sf$spachar, 3)

## == modelling == ##

# Fit a model on the training data - mixed effects model, use whole dataset (=132 observations)
model <- lmer(Lopend_gemiddelde ~ 1 + nightlight_450 +  nightlight_4950  + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50 + (1|spachar), data = data_32)
#predict on the grid
grid100_sf$predNO2_MEM <- predict(model, grid100_sf)

## == export option == ##
sf::st_write(grid100_sf, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels/predictedNO2_MEM.gpkg", driver = "GPKG")
