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

#all variables are necessary for spatial grouping determination
data <- read.csv('C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/LocalModelData/ModellingDataset-Local.csv', sep=';')
#replace NA with 0
data[is.na(data)] <- 0

#make imported csv spatial

#to sf
data_sf = st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
#change to planar crs
data_3035 <- st_transform(data_sf,crs=3035)

#import grid where predictions will be projected on
grid100 = readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/Grid100_LocalPredictors_Amsterdam.gpkg')

#make spatial - obtain the geometry for each sample
grid100_sf <- st_as_sf(grid100)

## == create column relating to spatial characterization (e.g. distance to road/population) == ##

#add a column to the data_3035 describing the spatial group where each sample belongs to

#examine basic data statistics of variables that will be used for quantile filtering!
quantile(data_3035$road_class_3_100)
quantile(data_3035$population_1000)

#spatial character: group "urban" - higher than ,5 quantile of population 1000 - within 100m of road class 1 OR 2 OR higher than .5 quantile of road class 3 100.
data_3035$spachar = ifelse(data_3035$population_1000 > quantile(data_3035$population_1000, 0.5) & ((data_3035$road_class_2_100 > 0 | data_3035$road_class_1_100 > 0) | data_3035$road_class_3_100 > quantile(data_3035$road_class_3_100, 0.5)), 1, 0)

#spatial character: group "low population" - lower than ,5 quantile of population 1000 - within 100m of road class 1 OR 2 OR higher than .5 quantile of road class 3 100.
data_3035$spachar = ifelse(data_3035$population_1000 < quantile(data_3035$population_1000, 0.5) & ((data_3035$road_class_2_100 > 0 | data_3035$road_class_1_100 > 0) | data_3035$road_class_3_100 > quantile(data_3035$road_class_3_100, 0.5)), 2, data_3035$spachar)

#spatial character: group "far from road" - all other samples (i.e. further away than 100m from road class 1/2 OR lower than .5 quantile of road class 3 100)
data_3035$spachar = ifelse((data_3035$spachar == 1 | data_3035$spachar == 2), data_3035$spachar, 3)

#examine thresholds for each variable-criterium, per spatial group
print(quantile(data_3035$population_1000, 0.5))
print(quantile(data_3035$road_class_3_100, 0.5))

#add a column to the 100m grid describing the spatial group where each sample belongs to

#assign to variable
population1000_05 = as.vector(quantile(data_3035$population_1000, 0.5))
roadclass3_100_05 = as.vector(quantile(data_3035$road_class_3_100, 0.5)) #as.vector necessary to only obtain value

#spatial character: group "urban" - higher than ,5 quantile of population 1000 - within 100m of road class 1 OR 2 OR higher than .5 quantile of road class 3 100.
grid100_sf$spachar = ifelse(grid100_sf$population_1000 > population1000_05 & ((grid100_sf$road_class_2_100 > 0 | grid100_sf$road_class_1_100 > 0) | grid100_sf$road_class_3_100 > roadclass3_100_05), 1, 0)

#spatial character: group "low population" - lower than ,5 quantile of population 1000 - within 100m of road class 1 OR 2 OR higher than .5 quantile of road class 3 100.
grid100_sf$spachar = ifelse(grid100_sf$population_1000 < population1000_05 & ((grid100_sf$road_class_2_100 > 0 | grid100_sf$road_class_1_100 > 0) | grid100_sf$road_class_3_100 > roadclass3_100_05), 2, grid100_sf$spachar)

#spatial character: group "far from road" - all other samples (i.e. further away than 100m from road class 1/2 OR lower than .5 quantile of road class 3 100)
grid100_sf$spachar = ifelse((grid100_sf$spachar == 1 | grid100_sf$spachar == 2), grid100_sf$spachar, 3)

#make key qith unique fid that will be used for joining all subdata after data processing
grid100_sf$key = seq(1,nrow(grid100_sf))
#based on column spachar
Urb_grid100_sf <- grid100_sf[grid100_sf$spachar == 1, ]
Lowpop_grid100_sf <- grid100_sf[grid100_sf$spachar == 2, ]
FFR_grid100_sf <- grid100_sf[grid100_sf$spachar == 3, ]

#convert to sf
Urb_grid100_sf <- st_as_sf(Urb_grid100_sf)
Lowpop_grid100_sf <- st_as_sf(Lowpop_grid100_sf)
FFR_grid100_sf <- st_as_sf(FFR_grid100_sf)

### === linear modelling per spatial group === ###

#create different datasets, based on the spatial character
Urban <- data_3035[data_3035$spachar == 1, ]
Lowpopulation <- data_3035[data_3035$spachar == 2, ]
FarFromRoad <- data_3035[data_3035$spachar == 3, ]

#shp - grid
#sf::st_write(Urb_grid100_sf, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/LocalModels/Urban_100grid_1.gpkg", driver = "GPKG")
#sf::st_write(Lowpop_grid100_sf, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/LocalModels/Lowpop_grid100_2.gpkg", driver = "GPKG")
#sf::st_write(FFR_grid100_sf, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/LocalModels/Amsterdam/FFR_grid100_1.gpkg", driver = "GPKG")

## == Urban - linear modeling == ##

#train model based on urban samples (=56)
linear_urb= lm(Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data=Urban)
#predicting based on the trained model - predict on grid cells that have the spachar value "1" (urban grid)
Urb_grid100_sf$pred_urb <- predict(linear_urb, Urb_grid100_sf)
#convert NA to 0's
Urb_grid100_sf[is.na(Urb_grid100_sf)] <- 0
URB_select = Urb_grid100_sf[,c('key', 'pred_urb')]

## == Low population - linear modelling == ##

#train model based on low population samples (=46)
linear_lp= lm(Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data=Lowpopulation)
#predicting based on the trained model - predict on grid cells that have the spachar value "2" (low population grid)
Lowpop_grid100_sf$pred_lp <- predict(linear_lp, Lowpop_grid100_sf)
#convert NA to 0's
Lowpop_grid100_sf[is.na(Lowpop_grid100_sf)] <- 0
LP_select = Lowpop_grid100_sf[,c('key', 'pred_lp')]

## == Far From Road - Linear model == ##

#train model based on far from road samples (=30)
linear_ffr= lm(Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data=FarFromRoad)
#predicting based on the trained model - predict on grid cells that have the spachar value "3" (far from road grid)
FFR_grid100_sf$pred_ffr <- predict(linear_ffr, FFR_grid100_sf)
# predictions_urban <- predictions_urban %>% rename(predictions_urban = "var1.pred")
#convert NA to 0's
FFR_grid100_sf[is.na(FFR_grid100_sf)] <- 0
#select only relevant columns to join with grid100
FFR_select = FFR_grid100_sf[,c('key', 'pred_ffr')]

FFR_select

## == join different prediction dataset (derived from each spatial group) == ##

grid100 <- as.data.frame(grid100)
URB_select <- as.data.frame(URB_select)
LP_select <- as.data.frame(LP_select)
FFR_select <- as.data.frame(FFR_select)

merge = list(grid100_sf, URB_select, LP_select, FFR_select)
merge <- merge %>% reduce(full_join, by= 'key')
merge <- replace(merge, is.na(merge), 0)

#define final prediction column by adding up prediction columns per spatial group
merge$predNO2_LinSep <- merge$pred_urb + merge$pred_lp + merge$pred_ffr

colnames(merge)

merge <- merge[,c("nightlight_450" ,"nightlight_4950" ,"population_1000","population_3000" ,"road_class_1_5000","road_class_2_100" ,
"road_class_2_1000","road_class_1_100" ,"road_class_2_5000","road_class_3_100","road_class_3_300" ,"trafBuf50" ,"spachar", "key","pred_urb", "pred_lp",    
"pred_ffr","geometry.x","predNO2_LinSep")]

#make spatial
merge_sf <- st_as_sf(merge)

## == export option == ##
sf::st_write(merge_sf, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/LocalModels/predictedNO2_Linear_SeparatingSpatialGroups.gpkg", driver = "GPKG")
