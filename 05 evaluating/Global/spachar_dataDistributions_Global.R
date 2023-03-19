library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(rgdal)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library("ggplot2")
library("GGally")
library("readxl")

# 
# global <- readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Amsterdam/Spachar/Amsterdam_NO2PredictionPerModel_spachar.gpkg')
# query <- readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels/Amsterdam_square_buffer30.gpkg')
# ams_query <- readOGR('"C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels/sp_query_Amsterdam.gpkg"')

#dataset tht defines threshold for globa, dataset
global_ms <- read.csv('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Amsterdam/AllPredictors_Amsterdam100m-processed.csv', sep=';')

global_ms

#global
global = readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Amsterdam/Amsterdam_NO2PredictionPerModel.shp')


#assign to variable
population1000_05 = as.vector(quantile(data_32$population_1000, 0.5))
roadclass3_100_05 = as.vector(quantile(data_32$road_class_3_100, 0.5)) #as.vector necessary to only obtain value


grid100$spachar = ifelse(grid100$population_1000 > population1000_05 & ((grid100$road_class_2_100 > 0 | grid100$road_class_1_100 > 0) | grid100$road_class_3_100 > roadclass3_100_05), 1, 0)

grid100$spachar = ifelse(grid100$population_1000 < population1000_05 & ((grid100$road_class_2_100 > 0 | grid100$road_class_1_100 > 0) | grid100$road_class_3_100 < roadclass3_100_05), 2, grid100$spachar)

grid100$spachar = ifelse((grid100$spachar == 1 | grid100$spachar == 2), grid100$spachar, 3)



library(spatialEco)
sp_query_Amsterdam <-spatial.select(query ,y = global,predicate = "contains")

global_df <- as.data.frame(global)
colnames(global_df)


#rename
global_df <- global_df %>% rename("nightlight_450" = "ngh_450",
                                      
                                      "nightlight_3150" = "ng_3150",
                                      "population_1000" = "pp_1000",
                                      "population_3000" = "pp_3000",
                             
                                      "road_class_2_25" = "r__2_25",

                                      "road_class_3_3000" = "r__3_3000",
                                      "road_class_3_300" = "rd__3_300",
                                      "trop_mean_filt" = "trp_mn_",
                                      "building_density_100m" = "BldD100",
                                      "trafBuf25" = "trfBf25",
                                      "trafBuf50" = "trfBf50")

global_df

predictors = list(global_df$nightlight_450, global_df$nightlight_3150,global_df$population_1000,
                  global_df$population_3000,global_df$road_class_2_25,global_df$road_class_3_3000,
                  global_df$road_class_3_300,global_df$trop_mean_filt,global_df$building_density_100m,
                  global_df$NDVI,global_df$trafBuf25,global_df$trafBuf50)


predictor_names = list("nightlight_450", "nightlight_3150","population_1000",
                       "population_3000", "road_class_2_25", "road_class_3_3000"
                       ,"road_class_3_300","trop_mean_filt", "building_density_100m",
                       "NDVI","trafBuf25",  "trafBuf50")


grid <- readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/grid100Amsterdam.shp')

# f <- function(data, name){
#   
#   print(name)
#   jpeg(file=paste0("C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/Amsterdam/", name, ".jpeg", sep=''))
#   boxplot(data~spachar,data=local, main="Predictor values per spatial group",
#           xlab="Spatial group", ylab=name,names = c("Urban",
#                                                               "Low population",
#                                                               "Far from road"))
#   dev.off()
#   
#   
#   
# }
# 
# apply(predictors, predictor_names, f)


global_df



for(i in 1:length(predictors)){
  
  print(predictor_names[i])
  # print(predictors[i])
  jpeg(file=paste0("C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/Amsterdam/global/GlobalSpacharDist_", predictor_names[i], ".jpeg", sep=''))
  boxplot(unlist(predictors[i])~spachar,data=local, col="lightsteelblue2", 
          xlab="Spatial group", ylab=predictor_names[i],names = c("Urban",
                                                                  "Low population",
                                                                  "Far from road"))
  dev.off()
  
  
}