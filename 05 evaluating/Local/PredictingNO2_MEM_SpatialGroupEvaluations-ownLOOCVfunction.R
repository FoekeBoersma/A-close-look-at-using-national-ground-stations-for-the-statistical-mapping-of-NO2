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



## == models evaluations ==##
library(base)
data_df <- as.data.frame(data_sp)

print(data_df)


## ATTEMPT 2
leave_one_out_cv <- function(data_df) {
  # Create an empty vector to store the predictions
  predictions <- vector("numeric", length = nrow(data_df))
  
  # Loop through each observation in the dataset
  for (i in 1:nrow(data_df)) {
    # Split the data into training and test sets
    train_data <- data_df[-i, ]
    
    test_data <- data_df[i, ]
    
    # Fit a model on the training data - Mixed  Model Effects model (https://www.jaredknowles.com/journal/2013/11/25/getting-started-with-mixed-effect-models-in-r)
    model <- lmer(Lopend_gemiddelde ~ 1 + nightlight_450 +  nightlight_4950  + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50 + (1|spachar), data = train_data)
    
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
model_lmer <- as.data.frame(obs_pred)
#rename variables to 'pred' and 'obs'
model_lmer <- model_lmer %>% rename( 'pred' = 'V1',  'obs' = 'V2')

#assign unique identifier to dataset - useful for joining the prediction and observation dataset (model_lm) with the initial dataset (data_df)
model_lmer$FID <- seq(1, nrow(model_lmer), 1)

#join the aforementioned dataset to get the final dataset which is used for evaluations
merge_dataset_lmer <- merge(model_lmer, data_df, by.x = 'FID', by.y = 'M_id')


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
RMSE_model_lmer = rmse(merge_dataset_lmer$obs, merge_dataset_lmer$pred)
print(paste0("RMSE TOTAL: ", RMSE_model_lmer))

#overall R2

#caret
# R2_model_caret = RSQUARE(merge_dataset_caret$obs,merge_dataset_caret$pred)
# print(paste0("R2 TOTAL caret: ",R2_model_caret))
#manual
R2_model_lmer = RSQUARE(merge_dataset_lmer$obs,merge_dataset_lmer$pred)
print(paste0("R2 TOTAL: ",R2_model_lmer))

#overall MAE

#caret
# MAE_model_caret = mae(merge_dataset_caret$obs, merge_dataset_caret$pred)
# print(paste0("MAE TOTAL caret: ", MAE_model_caret))
#manual
MAE_model_lmer = mae(merge_dataset_lmer$obs, merge_dataset_lmer$pred)
print(paste0("MAE TOTAL: ", MAE_model_lmer))



## == MODEL PERFORMANCE PER SPATIAL GROUP == ##

#narrow down dataset
data_filtered_lmer <- merge_dataset_lmer[, c("Lopend_gemiddelde", "obs", "pred", "spachar", "coords.x1", "coords.x2")]

Filtered_urban_lmer <- data_filtered_lmer[data_filtered_lmer$spachar == 1, ]
Filtered_lowpop_lmer <- data_filtered_lmer[data_filtered_lmer$spachar == 2, ]
Filtered_ffr_lmer <- data_filtered_lmer[data_filtered_lmer$spachar == 3, ]


#model evaluations


#URBAN
MAE_urb_lmer = mae(Filtered_urban_lmer$obs, Filtered_urban_lmer$pred)
print(paste0("MAE URBAN MEM: ", MAE_urb_lmer))
print(MAE_urb_lmer)

R2_model_urb_lmer = RSQUARE(Filtered_urban_lmer$obs,Filtered_urban_lmer$pred)
print(paste0("R2 URBAN MEM: ",R2_model_urb_lmer))
summary(R2_model_urb_lmer)

RMSE_model_urb_lmer = rmse(Filtered_urban_lmer$obs, Filtered_urban_lmer$pred)
print(paste0("RMSE URBAN MEM: ", RMSE_model_urb_lmer))
summary(RMSE_model_urb_lmer)

#LOW POP
MAE_lowpop_lmer = mae(Filtered_lowpop_lmer$obs, Filtered_lowpop_lmer$pred)
print(paste0("MAE LOWPOP MEM: ", MAE_lowpop_lmer))
summary(MAE_lowpop_lmer)

R2_model_lowpop_lmer = RSQUARE(Filtered_lowpop_lmer$obs,Filtered_lowpop_lmer$pred)
print(paste0("R2 LOWPOP MEM: ",R2_model_lowpop_lmer))
summary(R2_model_lowpop_lmer)

RMSE_model_lowpop_lmer = rmse(Filtered_lowpop_lmer$obs, Filtered_lowpop_lmer$pred)
print(paste0("RMSE LOWPOP MEM: ", RMSE_model_lowpop_lmer))
summary(RMSE_model_lowpop_lmer)

#FFR
MAE_ffr_lmer = mae(Filtered_ffr_lmer$obs, Filtered_ffr_lmer$pred)
print(paste0("MAE FFR MEM: ", MAE_ffr_lmer))
summary(MAE_ffr_lmer)

R2_model_ffr_lmer = RSQUARE(Filtered_ffr_lmer$obs,Filtered_ffr_lmer$pred)
print(paste0("R2 FFR MEM: ",R2_model_ffr_lmer))
summary(R2_model_ffr_lmer)

RMSE_model_ffr_lmer = rmse(Filtered_ffr_lmer$obs, Filtered_ffr_lmer$pred)
print(paste0("RMSE FFR MEM: ", RMSE_model_ffr_lmer))
summary(RMSE_model_ffr_lmer)

