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
#library(nlme) #mixed-effect model
library(Metrics)
library(conflicted)
library('parallel') 
install.packages("glmnet")
library(glmnet)
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

## == data examination purposes == ##

#examine thresholds for each variable-criterium, per spatial group
print(quantile(data_3035$population_1000, 0.5))
print(quantile(data_3035$road_class_3_100, 0.5))

#assign to variable
population1000_05 = as.vector(quantile(data_3035$population_1000, 0.5))
roadclass3_100_05 = as.vector(quantile(data_3035$road_class_3_100, 0.5)) #as.vector necessary to only obtain value

#make dataset, thereby selecting only variables that have been usef for filtering purposes relating to grouping (spatial character)
data_3035_sel = data_3035[,c("population_1000", "road_class_1_100", "road_class_2_100", "road_class_3_100", "spachar", "Lopend_gemiddelde")]

#examine
#view(data_3035_sel)

#get NO2 statistics per group
Urb <- data_3035_sel[data_3035_sel$spachar == 1, ]
Lowpop <- data_3035_sel[data_3035_sel$spachar == 2, ]
FFR <- data_3035_sel[data_3035_sel$spachar == 3, ]

#Examine observations per group (spatial character)
length(Urb$Lopend_gemiddelde)
length(Lowpop$Lopend_gemiddelde)
length(FFR$Lopend_gemiddelde)

#Examine basic statistics per group (spatial character) - concerning NO2
summary(Urb$Lopend_gemiddelde)
summary(Lowpop$Lopend_gemiddelde)
summary(FFR$Lopend_gemiddelde)

#now to spatial points dataframe
data_sp <- as(data_3035, "Spatial")

#examine data
spplot(data_sp, c("Lopend_gemiddelde"))

#create different datasets, based on the spatial character
Urban <- data_sp[data_sp$spachar == 1, ]
Lowpopulation <- data_sp[data_sp$spachar == 2, ]
FarFromRoad <- data_sp[data_sp$spachar == 3, ]

data_df <- as.data.frame(data_sp)

## == manually CV == ##
# Define the leave-one-out cross-validation function for Ridge regression
leave_one_out_cv_ridge <- function(data_df) {
  # Create an empty vector to store the predictions
  predictions <- vector("numeric", length = nrow(data_df))
  
  # Loop through each observation in the dataset
  for (i in 1:nrow(data_df)) {
    # Split the data into training and test sets
    train_data <- data_df[-i, ]
    test_data <- data_df[i, ]
    
    # Define the response variable and predictor variables
    y_train <- train_data$Lopend_gemiddelde
    x_train <- model.matrix(Lopend_gemiddelde ~ nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data = train_data)[, -1]
    x_test <- model.matrix(Lopend_gemiddelde ~ nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data = test_data)[, -1]
    
    # Perform k-fold cross-validation to find the optimal lambda value
    cv_model <- cv.glmnet(x_train, y_train, alpha = 0)
    best_lambda <- cv_model$lambda.min
    
    # Fit the Ridge regression model using the optimal lambda value
    ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda)
    
    # Make a prediction on the test data
    predictions[i] <- predict(ridge_model, s = best_lambda, newx = x_test)
  }
  
  # Return the vector of predictions
  return(predictions)
}
predictions <- leave_one_out_cv_ridge(data_df)
#store results of loop to variable including predictions
pred <- predictions

print(length(pred))

#observations
obs <- data_df$Lopend_gemiddelde

#merge observations and predictions
obs_pred <- do.call(rbind, Map(cbind, pred, obs))

#convert to dataframe. The variable name, model_lm, hints to the model used
model_lm <- as.data.frame(obs_pred)
#rename variables to 'pred' and 'obs'
model_lm <- model_lm %>% rename( 'pred' = 'V1',  'obs' = 'V2')

#assign unique identifier to dataset - useful for joining the prediction and observation dataset (model_lm) with the initial dataset (data_df)
model_lm$FID <- seq(1, nrow(model_lm), 1)
data_df$M_id <- seq(1, nrow(data_df), 1)

#join the aforementioned dataset to get the final dataset which is used for evaluations
merge_dataset <- merge(model_lm, data_df, by.x = 'FID', by.y = 'M_id')

## == overall evaluation == ##

#define R2
#R SQUARED error metric -- Coefficient of Determination
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2}

#=================

#overall RMSE

#manual
RMSE_model = rmse(merge_dataset$obs, merge_dataset$pred)
print(paste0("RMSE TOTAL: ", RMSE_model))

#overall R2

#manual
R2_model = RSQUARE(merge_dataset$obs,merge_dataset$pred)
print(paste0("R2 TOTAL: ",R2_model))

#overall MAE

#manual
MAE_model = mae(merge_dataset$obs, merge_dataset$pred)
print(paste0("MAE TOTAL: ", MAE_model))

## == narrow down dataset == ##

#used for evaluating manual cv
data_filtered <- merge_dataset[, c("Lopend_gemiddelde", "obs", "pred", "spachar", "coords.x1", "coords.x2")]

#create different datasets, based on spatial characteristics - used for evaluating prediction results per spatial group

#manual cv
Filtered_urban <- data_filtered[data_filtered$spachar == 1, ]
Filtered_lowpop <- data_filtered[data_filtered$spachar == 2, ]
Filtered_ffr <- data_filtered[data_filtered$spachar == 3, ]

## == model evaluations (accounting for spatial characteristics) == ##

#URBAN
MAE_urb = mae(Filtered_urban$obs, Filtered_urban$pred)
print(paste0("MAE URBAN: ", MAE_urb))
print(MAE_urb)
# summary(MAE_urb)

R2_model_urb = RSQUARE(Filtered_urban$obs,Filtered_urban$pred)
print(paste0("R2 URBAN: ",R2_model_urb))
# summary(R2_model_urb)

RMSE_model_urb = rmse(Filtered_urban$obs, Filtered_urban$pred)
print(paste0("RMSE URBAN: ", RMSE_model_urb))
# summary(RMSE_model_urb)

#LOW POP
MAE_lowpop = mae(Filtered_lowpop$obs, Filtered_lowpop$pred)
print(paste0("MAE LOWPOP: ", MAE_lowpop))
summary(MAE_lowpop)

R2_model_lowpop = RSQUARE(Filtered_lowpop$obs,Filtered_lowpop$pred)
print(paste0("R2 LOWPOP: ",R2_model_lowpop))
summary(R2_model_lowpop)

RMSE_model_lowpop = rmse(Filtered_lowpop$obs, Filtered_lowpop$pred)
print(paste0("RMSE LOWPOP: ", RMSE_model_lowpop))
summary(RMSE_model_lowpop)

#FFR
MAE_ffr = mae(Filtered_ffr$obs, Filtered_ffr$pred)
print(paste0("MAE FFR: ", MAE_ffr))

R2_model_ffr = RSQUARE(Filtered_ffr$obs,Filtered_ffr$pred)
print(paste0("R2 FFR: ",R2_model_ffr))

RMSE_model_ffr = rmse(Filtered_ffr$obs, Filtered_ffr$pred)
print(paste0("RMSE FFR: ", RMSE_model_ffr))
