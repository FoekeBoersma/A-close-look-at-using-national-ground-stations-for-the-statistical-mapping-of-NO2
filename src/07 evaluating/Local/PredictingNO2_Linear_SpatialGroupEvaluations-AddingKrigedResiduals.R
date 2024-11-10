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
library(stats) #quantile
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
library(yaml)

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

data <- read.csv(modeling_dataset_local_dir, sep=';')
#replace NA with 0
data[is.na(data)] <- 0

#convert to spatial points dataframe (gstat relies on sp package more than sf)
#first convert to sf

#to sf
data_sf = st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
#change to planar crs
data_3035 <- st_transform(data_sf,crs=3035)

## == make grid - this is the AOI where the kriging will be projected on == ##

bbox <- st_bbox(data_3035)

grid <- data_3035 %>% 
  st_bbox() %>%     # determines bounding box coordinates from meuse
  st_as_sfc() %>%   # creates sfc object from bounding box
  st_make_grid(     # create grid 50 x 50 pixel size
    cellsize = c(100, 100), 
    what = "centers") %>%
  st_as_sf(crs=st_crs(data_3035)) # convert to sf object

# Convert grid to SpatialPixelsDataFrame, the raster/grid equivalent in 
# in the sp world
grid_sp <- as(as(grid, "Spatial"), "SpatialPixels")

## == create column relating to spatial characterization (e.g. urban/low population/far from road) == ##

#examine basic data statistics of variables that will be used for quantile filtering!
quantile(data_3035$road_class_3_100)
quantile(data_3035$population_1000)

#spatial character: group "urban" - higher than ,5 quantile of population 1000 - within 100m of road class 1 OR 2 OR higher than .5 quantile of road class 3 100.
data_3035$spachar = ifelse(data_3035$population_1000 > quantile(data_3035$population_1000, 0.5) & ((data_3035$road_class_2_100 > 0 | data_3035$road_class_1_100 > 0) | data_3035$road_class_3_100 > quantile(data_3035$road_class_3_100, 0.5)), 1, 0)

#spatial character: group "low population" - lower than ,5 quantile of population 1000 - within 100m of road class 1 OR 2 OR higher than .5 quantile of road class 3 100.
data_3035$spachar = ifelse(data_3035$population_1000 < quantile(data_3035$population_1000, 0.5) & ((data_3035$road_class_2_100 > 0 | data_3035$road_class_1_100 > 0) | data_3035$road_class_3_100 > quantile(data_3035$road_class_3_100, 0.5)), 2, data_3035$spachar)

#spatial character: group "far from road" - all other samples (i.e. further away than 100m from road class 1/2 OR lower than .5 quantile of road class 3 100)
data_3035$spachar = ifelse((data_3035$spachar == 1 | data_3035$spachar == 2), data_3035$spachar, 3)

## == multiple linear regression, input data is trained dataset == ##

#convert to dataframe
#now to spatial points dataframe
data_sp <- as(data_3035, "Spatial") #spatial points dataframe as input for spatial operations (e.g. kriging)
data_df <- as.data.frame(data_sp) #input dataset for modelling  part

# train model - whole dataset (wd) (132 observations)
model_train_wd = lm(Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50 , data=data_df)

# kriging on the residuals 
data_model_train_wd <- data.frame(x = data_df$coords.x1, y = data_df$coords.x2,resid=resid(model_train_wd))

#fitting the variogram (autofit)
coordinates(data_model_train_wd) = ~x+y
variogram_train = autofitVariogram(resid ~ 1, data_model_train_wd)

#create dataset including variogram parameters
autofit_params <- variogram_train$var_model 
#examine autofitted parameters which will be used for the variogram during the process of kriging.
print(autofit_params)

#manually insert parameters into variogram
m <- vgm(psill = 20.6688, "Sph", range =  276.6727)


## ==  LEAVE-ONE-OUT CROSS VALIDATION == ##

#define function which outputs a leave-one-out cross validation
leave_one_out_cv <- function(data_df) {
  # Create an empty vector to store the predictions
  predictions <- vector("numeric", length = nrow(data_df))
  
  # Loop through each observation in the dataset
  for (i in 1:nrow(data_df)) {
    
    # Split the data into training and test sets
    
    #train: all observations - one sample
    train_data <- data_sp[-i, ]
    
    #test: remainder - one sample
    test_data <- data_sp[i, ]
    
    #use the linear model again to train, this time on the train dataset (131 observations)
    model_train_train = lm(Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50 , data=train_data)
    
    #predict NO2 by the trained model (linear)
    test_data$predicted = predict(model_train_train, test_data)
    
    #kriging on the residuals
    train_data_sf <- data.frame(x = train_data$coords.x1, y = train_data$coords.x2, resid=resid(model_train_train))
    coordinates(train_data_sf) = ~x+y

    #set coordinate reference system
    crs(train_data_sf) <- "+init=epsg:3035"
    #perform kriging on residuals, use autofitted parameters that were manually defined outside this function and via the whole dataset
    lz.ok_train_resid <- krige(resid ~ 1, train_data_sf, grid_sp, m)

    #convert to raster - residual
    raster_train_resid <- raster(lz.ok_train_resid['var1.pred'])

    #spatially join predicted values by trained model with corresponding kriged residuals.
    predicted_model = raster::extract(raster_train_resid, test_data, sp=T) #sp = T: keep all data

    #add kriged residuals to the predicted values by the trained model
    predicted_model$PredAddedKrigedResi <- predicted_model$predicted + predicted_model$var1.pred


    predictions[i] <- predicted_model$PredAddedKrigedResi
  }
  
  # Return the vector of predictions
  return(predictions)
}

## == assign all leave-one-out cross-validation prediction's  to input dataframe == ##

#input dataset with 132 observations - 131 used for training; 1 for training for each of every 132 folds
predictions <- leave_one_out_cv(data_sp)

#examine values outputted by function
print(predictions)

#assign to dataframe - NO2 predictions, corrected by model's residual
data_df$PredAddedKrigedResi <- predictions

#filter out dataframe
data_filtered = data_df[, c("Lopend_gemiddelde", "PredAddedKrigedResi", "spachar", "coords.x1", "coords.x2")]

## == overall evaluation == ##

#define R2
#R SQUARED error metric -- Coefficient of Determination
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2}

#=================

#overall RMSE

RMSE_model = rmse(data_filtered$Lopend_gemiddelde, data_filtered$PredAddedKrigedResi)
print(paste0("RMSE TOTAL: ", RMSE_model))

#overall R2

R2_model = RSQUARE(data_filtered$Lopend_gemiddelde, data_filtered$PredAddedKrigedResi)
print(paste0("R2 TOTAL: ",R2_model))

#overall MAE

MAE_model = mae(data_filtered$Lopend_gemiddelde, data_filtered$PredAddedKrigedResi)
print(paste0("MAE TOTAL: ", MAE_model))

## == per spatial group == ##

#create different datasets, derived from the different spatial groups
Filtered_urban <- data_filtered[data_filtered$spachar == 1, ]
Filtered_lowpop <- data_filtered[data_filtered$spachar == 2, ]
Filtered_ffr <- data_filtered[data_filtered$spachar == 3, ]

#model evaluations

#URBAN
MAE_urb = mae(Filtered_urban$Lopend_gemiddelde, Filtered_urban$PredAddedKrigedResi)
print(paste0("MAE URBAN: ", MAE_urb))
print(MAE_urb)
summary(MAE_urb)

R2_model_urb = RSQUARE(Filtered_urban$Lopend_gemiddelde,Filtered_urban$PredAddedKrigedResi)
print(paste0("R2 URBAN: ",R2_model_urb))
summary(R2_model_urb)

RMSE_model_urb = rmse(Filtered_urban$Lopend_gemiddelde, Filtered_urban$PredAddedKrigedResi)
print(paste0("RMSE URBAN: ", RMSE_model_urb))
summary(RMSE_model_urb)

#LOW POP
MAE_lowpop = mae(Filtered_lowpop$Lopend_gemiddelde, Filtered_lowpop$PredAddedKrigedResi)
print(paste0("MAE LOWPOP: ", MAE_lowpop))
summary(MAE_lowpop)

R2_model_lowpop = RSQUARE(Filtered_lowpop$Lopend_gemiddelde,Filtered_lowpop$PredAddedKrigedResi)
print(paste0("R2 LOWPOP: ",R2_model_lowpop))
summary(R2_model_lowpop)

RMSE_model_lowpop = rmse(Filtered_lowpop$Lopend_gemiddelde, Filtered_lowpop$PredAddedKrigedResi)
print(paste0("RMSE LOWPOP: ", RMSE_model_lowpop))
summary(RMSE_model_lowpop)

#FFR
MAE_ffr = mae(Filtered_ffr$Lopend_gemiddelde, Filtered_ffr$PredAddedKrigedResi)
print(paste0("MAE FFR: ", MAE_ffr))
summary(MAE_ffr)

R2_model_ffr = RSQUARE(Filtered_ffr$Lopend_gemiddelde,Filtered_ffr$PredAddedKrigedResi)
print(paste0("R2 FFR: ",R2_model_ffr))
summary(R2_model_ffr)

RMSE_model_ffr = rmse(Filtered_ffr$Lopend_gemiddelde, Filtered_ffr$PredAddedKrigedResi)
print(paste0("RMSE FFR: ", RMSE_model_ffr))
summary(RMSE_model_ffr)
