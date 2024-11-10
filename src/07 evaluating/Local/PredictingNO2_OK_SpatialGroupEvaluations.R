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
library(Metrics)
library('parallel') 

# Packages for geostatistics
library(gstat)   # The most popular R-Package for Kriging 
library(automap) # Automatize some (or all) parts of the gstat-workflow 

# Finally, some packages to make pretty plots
library(patchwork)
library(viridis)
library(tmap)
library(graphics) #for text
# Connect to YAML file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir)) # One level up in directory
config07_path <- file.path(config_dir, "config_07.yml")

# Read YAML configuration file
config07 <- yaml::yaml.load_file(config07_path)

# Define the parent directory (move four levels up)
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))

# Paths for input data based on YAML configuration
modeling_dataset_local_dir <- normalizePath(file.path(parent_directory, config07$input_data$modeling_dataset_local), winslash = "/")



## == DEFINE COORDINATE SYSTEMS == ##

#CRS with metric system is preferred (=3035).

crs <- CRS("+proj=longlat +datum=WGS84") # crs

## == import geodata == ##

data <- read.csv(modeling_dataset_local_dir , sep=';')
#replace NA with 0
data[is.na(data)] <- 0

#convert to spatial points dataframe (gstat relies on sp package more than sf)
#first convert to sf

#to sf
data_sf = st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
#change to planar crs
data_3035 <- st_transform(data_sf,crs=3035)

## == create column relating to spatial characterization (e.g. distance to road/population) == ##

#examine basic data statistics of variables that will be used for quantile filtering!
quantile(data_3035$road_class_3_100)
quantile(data_3035$population_1000)

#spatial character: group "urban"
data_3035$spachar = ifelse(data_3035$population_1000 > quantile(data_3035$population_1000, 0.5) & ((data_3035$road_class_2_100 > 0 | data_3035$road_class_1_100 > 0) | data_3035$road_class_3_100 > quantile(data_3035$road_class_3_100, 0.5)), 1, 0)

#spatial character: group "low population"
data_3035$spachar = ifelse(data_3035$population_1000 < quantile(data_3035$population_1000, 0.5) & ((data_3035$road_class_2_100 > 0 | data_3035$road_class_1_100 > 0) | data_3035$road_class_3_100 > quantile(data_3035$road_class_3_100, 0.5)), 2, data_3035$spachar)

#spatial character: group "far from road"
data_3035$spachar = ifelse((data_3035$spachar == 1 | data_3035$spachar == 2), data_3035$spachar, 3)

#now to spatial points dataframe
data_sp <- as(data_3035, "Spatial")

## == Kriging - variogram setting == ##

#define x & y variables to coordinates
data_xy <- data.frame(x = data_sp$coords.x1, y = data_sp$coords.x2)
coordinates(data_xy) = ~x+y

#variogram

#perform autofit variogram, based on dependent variable 'Lopend_gemiddelde'
variogram_auto_lin = autofitVariogram(Lopend_gemiddelde ~ 1, data_sp)

plot(variogram_auto_lin)

autofit_params_lin <- variogram_auto_lin$var_model 

#examine suggested variogram paramater settings via print function
print(autofit_params_lin)

#manually insert variogram settings, based on autofit
m <- vgm(psill = 80.16775, "Ste", range = 98.67688)

## == models evaluations ==##

# leave-one-out cross validation - exclusion of predictors (i.e. ordinary kriging):
krige_ok <- krige.cv(Lopend_gemiddelde ~ 1, data_sp, m)

krige_ok$observed

#convert to dataframes - useful for evaluation purposes
data_df <- as.data.frame(data_sp)
krige <- as.data.frame(krige_ok)

#merge the kriging output with the initial dataframe - again, useful for evaluation purposes
data_merge <- merge(krige,data_df, by = c("coords.x1", "coords.x2"))

#narrow data 
data_filtered = data_merge[, c("Lopend_gemiddelde", "observed", "var1.pred", "residual", "zscore", "spachar", "coords.x1", "coords.x2")]

## == overall evaluation == ##

#define R2
#R SQUARED error metric -- Coefficient of Determination
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2}

#=================

#overall RMSE

RMSE_model = rmse(data_filtered$Lopend_gemiddelde, data_filtered$var1.pred)
print(paste0("RMSE TOTAL: ", RMSE_model))

#overall R2

R2_model = RSQUARE(data_filtered$Lopend_gemiddelde, data_filtered$var1.pred)
print(paste0("R2 TOTAL: ",R2_model))

#overall MAE

MAE_model = mae(data_filtered$Lopend_gemiddelde, data_filtered$var1.pred)
print(paste0("MAE TOTAL: ", MAE_model))

Filtered_urban <- data_filtered[data_filtered$spachar == 1, ]
Filtered_lowpop <- data_filtered[data_filtered$spachar == 2, ]
Filtered_ffr <- data_filtered[data_filtered$spachar == 3, ]

#model evaluations

#define R2
#R SQUARED error metric -- Coefficient of Determination
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2}

#URBAN
MAE_urb = mae(Filtered_urban$observed, Filtered_urban$var1.pred)
print(paste0("MAE URBAN: ", MAE_urb))
print(MAE_urb)
summary(MAE_urb)

R2_model_urb = RSQUARE(Filtered_urban$observed,Filtered_urban$var1.pred)
print(paste0("R2 URBAN: ",R2_model_urb))
summary(R2_model_urb)

RMSE_model_urb = rmse(Filtered_urban$observed, Filtered_urban$var1.pred)
print(paste0("RMSE URBAN: ", RMSE_model_urb))
summary(RMSE_model_urb)

#LOW POP
MAE_lowpop = mae(Filtered_lowpop$observed, Filtered_lowpop$var1.pred)
print(paste0("MAE LOWPOP: ", MAE_lowpop))
summary(MAE_lowpop)

R2_model_lowpop = RSQUARE(Filtered_lowpop$observed,Filtered_lowpop$var1.pred)
print(paste0("R2 LOWPOP: ",R2_model_lowpop))
summary(R2_model_lowpop)

RMSE_model_lowpop = rmse(Filtered_lowpop$observed, Filtered_lowpop$var1.pred)
print(paste0("RMSE LOWPOP: ", RMSE_model_lowpop))
summary(RMSE_model_lowpop)

#FFR
MAE_ffr = mae(Filtered_ffr$observed, Filtered_ffr$var1.pred)
print(paste0("MAE FFR: ", MAE_ffr))
summary(MAE_ffr)

R2_model_ffr = RSQUARE(Filtered_ffr$observed,Filtered_ffr$var1.pred)
print(paste0("R2 FFR: ",R2_model_ffr))
summary(R2_model_ffr)

RMSE_model_ffr = rmse(Filtered_ffr$observed, Filtered_ffr$var1.pred)
print(paste0("RMSE FFR: ", RMSE_model_ffr))
summary(RMSE_model_ffr)
