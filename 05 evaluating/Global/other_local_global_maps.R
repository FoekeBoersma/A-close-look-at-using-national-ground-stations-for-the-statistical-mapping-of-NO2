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

local <- read_excel('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/Local_df.xlsx')



colnames(local)

library("writexl")


\noindent {\centering nightlight\_450,   nightlight\_4950,   population\_3000, 
  road\_class\_1\_5000, road\_class\_2\_1000, road\_class\_2\_5000,  road\_class\_3\_100,  road\_class\_3\_300,         trafbuf50\par} 

local


Urb <- local[local$spachar == 1, ]
Lowpop <- local[local$spachar == 2, ]
FFR <- local[local$spachar == 3, ]

predictors = list(local$nightlight_450, local$nightlight_4950,local$population_1000,
                  local$population_3000,local$road_class_1_5000,local$road_class_2_100,
                  local$road_class_2_1000,local$road_class_1_100,local$road_class_2_5000,
                  local$road_class_3_100,local$road_class_3_300,local$trafBuf50)


predictor_names = list("nightlight_450", "nightlight_4950","population_1000",
                  "population_3000","road_class_1_5000","road_class_2_100",
                  "road_class_2_1000","road_class_1_100","road_class_2_5000",
                  "road_class_3_100","road_class_3_300","trafBuf50")



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






for(i in 1:length(predictors)){
  
  print(predictor_names[i])
  # print(predictors[i])
  jpeg(file=paste0("C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/Amsterdam/LocalSpacharDist_", predictor_names[i], ".jpeg", sep=''))
  boxplot(unlist(predictors[i])~spachar,data=local, col="lightsteelblue2", 
          xlab="Spatial group", ylab=predictor_names[i],names = c("Urban",
                                                              "Low population",
                                                              "Far from road"))
  dev.off()
  
  
}


local_predictionColumns <- local[, c("spachar", "predNO2_Lin","predNO2_LinSep" , "predNO2_MEM","predNO2_UK","predNO2_UKSep","predicted_OK" )]

#to excel
write_xlsx(local_predictionColumns, "C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/local_predictionColumns.xlsx")
