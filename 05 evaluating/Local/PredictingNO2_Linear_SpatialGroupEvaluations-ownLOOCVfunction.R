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
library(caret)
#library("sjstats")

library(conflicted)

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

#convert to spatial points dataframe (gstat relies on sp package more than sf)
#first convert to sf

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

## == data examination purposes == ##

#examine thresholds for each variable-criterium, per spatial group
print(quantile(data_32$population_1000, 0.5))
print(quantile(data_32$road_class_3_100, 0.5))

#assign to variable
population1000_05 = as.vector(quantile(data_32$population_1000, 0.5))
roadclass3_100_05 = as.vector(quantile(data_32$road_class_3_100, 0.5)) #as.vector necessary to only obtain value

#make dataset, thereby selecting only variables that have been usef for filtering purposes relating to grouping (spatial character)
data_32_sel = data_32[,c("population_1000", "dist", "road_class_1_100", "road_class_2_100", "road_class_3_100", "spachar", "Lopend_gemiddelde")]

#examine
#view(data_32_sel)

#get NO2 statistics per group
Urb <- data_32_sel[data_32_sel$spachar == 1, ]
Lowpop <- data_32_sel[data_32_sel$spachar == 2, ]
FFR <- data_32_sel[data_32_sel$spachar == 3, ]

#Examine observations per group (spatial character)
length(Urb$Lopend_gemiddelde)
length(Lowpop$Lopend_gemiddelde)
length(FFR$Lopend_gemiddelde)

#Examine basic statistics per group (spatial character) - concerning NO2
summary(Urb$Lopend_gemiddelde)
summary(Lowpop$Lopend_gemiddelde)
summary(FFR$Lopend_gemiddelde)


#now to spatial points dataframe
data_sp <- as(data_32, "Spatial")


#examine data
spplot(data_sp, c("Lopend_gemiddelde"))


#create different datasets, based on the spatial character
Urban <- data_sp[data_sp$spachar == 1, ]
Lowpopulation <- data_sp[data_sp$spachar == 2, ]
FarFromRoad <- data_sp[data_sp$spachar == 3, ]

## == validation == ##

#performance::r2(lm(Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data = data_df))

## == caret == ##

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")

data_df <- as.data.frame(data_sp)

#fit a regression model and use LOOCV to evaluate performance
model_caret <- train(Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data = data_df, method = "lm", trControl = ctrl)
summary(model_caret)
print(model_caret)
# #store model results to dataframe
# pred_caret <- extractPrediction(list(NAME = model),   # extractPrediction want list(model)
#                           testX = dplyr::select(data_df))

# #assign unique identifier - useful for joining the results dataset with the initial dataset
# pred_caret$FID <- seq(1, nrow(pred_caret), 1)
# #join aforementioned datasets
# merge_dataset_caret <- merge(pred_caret, data_df, by.x = 'FID', by.y = 'M_id')

#pred_caret$pred - predictions
## == manually CV == ##

# #examine manual loocv via example
# test_sample = data_df[132,]
# train_sample = data_df[-132,]
# model_132<-lm(Lopend_gemiddelde ~ 1 + nightlight_450 +  nightlight_4950  + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data=train_sample)
# predict_132 = predict(model_132, newdata = test_sample)
# 
# predict_132


## ATTEMPT 1
# 
# #First initialize the output vector as an empty object outside the loop.
# fitted_value <- NULL
# for(i in 1:132){
#   #split into training dataset and validation sample
#   training<-data_df[-i,]
#   validation<-data_df[i,]
#   #train the model
#   model_lm<-lm(Lopend_gemiddelde ~ 1 + nightlight_450 +  nightlight_4950  + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data=training)
#   #when you fit the model, use the newdata argument to predict on a new row
#   #also, fitted_value needs the index [i], so the each loop doesn't overwrite the previous
#   fitted_value[i] <- predict(model_lm, newdata = validation)}

## ATTEMPT 2
leave_one_out_cv <- function(data_df) {
  # Create an empty vector to store the predictions
  predictions <- vector("numeric", length = nrow(data_df))
  
  # Loop through each observation in the dataset
  for (i in 1:nrow(data_df)) {
    # Split the data into training and test sets
    train_data <- data_df[-i, ]
    
    test_data <- data_df[i, ]
    
    # Fit a model on the training data
    model <- lm(Lopend_gemiddelde ~ 1 + nightlight_450 +  nightlight_4950  + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data = train_data)
    
    # Make a prediction on the test data
    predictions[i] <- predict(model, test_data)
  }
  
  # Return the vector of predictions
  return(predictions)
}

predictions <- leave_one_out_cv(data_df)

#store results of loop to variable including predictions
pred <- predictions

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

#join the aforementioned dataset to get the final dataset which is used for evaluations
merge_dataset <- merge(model_lm, data_df, by.x = 'FID', by.y = 'M_id')


## == overall evaluation == ##

#define R2
#R SQUARED error metric -- Coefficient of Determination
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2}

#=================

#overall RMSE

#caret
# RMSE_model_caret = rmse(merge_dataset_caret$obs, merge_dataset_caret$pred)
# print(paste0("RMSE TOTAL caret: ", RMSE_model_caret))
#manual
RMSE_model = rmse(merge_dataset$obs, merge_dataset$pred)
print(paste0("RMSE TOTAL: ", RMSE_model))

#overall R2

#caret
# R2_model_caret = RSQUARE(merge_dataset_caret$obs,merge_dataset_caret$pred)
# print(paste0("R2 TOTAL caret: ",R2_model_caret))
#manual
R2_model = RSQUARE(merge_dataset$obs,merge_dataset$pred)
print(paste0("R2 TOTAL: ",R2_model))

#overall MAE

#caret
# MAE_model_caret = mae(merge_dataset_caret$obs, merge_dataset_caret$pred)
# print(paste0("MAE TOTAL caret: ", MAE_model_caret))
#manual
MAE_model = mae(merge_dataset$obs, merge_dataset$pred)
print(paste0("MAE TOTAL: ", MAE_model))



## == narrow down dataset == ##

# #used for evaluating cv results via caret
# data_filtered_caret <- merge_dataset_caret[, c("Lopend_gemiddelde", "obs", "pred", "spachar", "coords.x1", "coords.x2")]
#used for evaluating manual cv
data_filtered <- merge_dataset[, c("Lopend_gemiddelde", "obs", "pred", "spachar", "coords.x1", "coords.x2")]

#create different datasets, based on spatial characteristics - used for evaluating prediction results per spatial group

# #caret cv
# Filtered_urban_caret <- data_filtered_caret[data_filtered_caret$spachar == 1, ]
# Filtered_lowpop_caret <- data_filtered_caret[data_filtered_caret$spachar == 2, ]
# Filtered_ffr_caret <- data_filtered_caret[data_filtered_caret$spachar == 3, ]
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


# MAE_urb_caret = mae(Filtered_urban_caret$obs, Filtered_urban_caret$pred)
# print(paste0("MAE URBAN caret: ", MAE_urb_caret))
# print(MAE_urb_caret)
# # summary(MAE_urb)
# 
# R2_model_urb_caret = RSQUARE(Filtered_urban_caret$obs,Filtered_urban_caret$pred)
# print(paste0("R2 URBAN caret: ",R2_model_urb_caret))
# # summary(R2_model_urb_caret)
# 
# RMSE_model_urb_caret = rmse(Filtered_urban_caret$obs, Filtered_urban_caret$pred)
# print(paste0("RMSE URBAN caret: ", RMSE_model_urb_caret))
# # summary(RMSE_model_urb_caret)






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

# #caret
# MAE_lowpop_caret = mae(Filtered_lowpop_caret$obs, Filtered_lowpop_caret$pred)
# print(paste0("MAE LOWPOP caret: ", MAE_lowpop_caret))
# 
# 
# R2_model_lowpop_caret = RSQUARE(Filtered_lowpop_caret$obs,Filtered_lowpop_caret$pred)
# print(paste0("R2 LOWPOP caret: ",R2_model_lowpop_caret))
# 
# 
# RMSE_model_lowpop_caret = rmse(Filtered_lowpop_caret$obs, Filtered_lowpop_caret$pred)
# print(paste0("RMSE LOWPOP caret: ", RMSE_model_lowpop_caret))
# summary(RMSE_model_lowpop)





#FFR
MAE_ffr = mae(Filtered_ffr$obs, Filtered_ffr$pred)
print(paste0("MAE FFR: ", MAE_ffr))


R2_model_ffr = RSQUARE(Filtered_ffr$obs,Filtered_ffr$pred)
print(paste0("R2 FFR: ",R2_model_ffr))


RMSE_model_ffr = rmse(Filtered_ffr$obs, Filtered_ffr$pred)
print(paste0("RMSE FFR: ", RMSE_model_ffr))

# #caret
# MAE_ffr_caret = mae(Filtered_ffr_caret$obs, Filtered_ffr_caret$pred)
# print(paste0("MAE FFR caret: ", MAE_ffr_caret))
# 
# 
# R2_model_ffr_caret = RSQUARE(Filtered_ffr_caret$obs,Filtered_ffr_caret$pred)
# print(paste0("R2 FFR caret: ",R2_model_ffr_caret))
# 
# 
# RMSE_model_ffr_caret = rmse(Filtered_ffr_caret$obs, Filtered_ffr_caret$pred)
# print(paste0("RMSE FFR caret: ", RMSE_model_ffr_caret))


