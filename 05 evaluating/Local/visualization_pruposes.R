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


grid100 = readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels/all_models.gpkg')

#to datadrame
grid100_df <- as.data.frame(grid100)

# Add fill layer to nz shape
grid100 <- st_as_sf(grid100)


colnames(grid100_df)

View(grid100_df)

grid100_df <- grid100_df %>% rename("Li"  ="predNO2_Lin",
                                    "LiSpa"= "predNO2_LinSep",
                                    "MEM" = "predNO2_MEM",
                                    "Uk" = "predNO2_UK",
                                    "UkSpa" = "predNO2_UKSep",
                                    "Ok" = "predicted_OK",
                                    "RF" =  "p_NO2_RF",
                                    "Las" = "p_NO2_LA", 
                                    "Rid" = "p_NO2_RI", 
                                    "Lgb" = "p_NO2_LG",
                                    "Xgb" = "p_NO2_X"
  
)

colnames(grid100_df)

# 
# grid100_df_models <- grid100_df[,c("predNO2_Lin","predNO2_LinSep","predNO2_MEM","predNO2_UK","predNO2_UKSep",
#                                    "p_NO2_RF","p_NO2_LA", "p_NO2_RI", "p_NO2_LG", "p_NO2_X")]
# 
# grid100_df_models
# 




# ## == scatter plot == ##


grid100_df_models <- grid100_df[,c("RF","Lgb","Xgb","Las","Rid","Li","LiSpa","MEM","Uk","UkSpa","Ok","no2")]

local <- grid100_df[,c("Li","LiSpa","MEM","Uk","UkSpa","Ok")]
#export local models to csv
write.csv(local, 'C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/localmodels.csv')

#grid100_df_models <- grid100_df[,c("predNO2_Lin","predNO2_LinSep","predNO2_MEM","predNO2_UK","predNO2_UKSep",
                                   # "p_NO2_RF","p_NO2_LA", "p_NO2_RI", "p_NO2_LG", "p_NO2_X")]
#examine statistics per model
summary(grid100_df_models)


grid100_df_noNAs = na.omit(grid100_df_models)

# # use jpg() instead of svg(), if you want PDF output
#jpeg(file = "C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/allplot-includingNO2tif.jpeg",width=600, height=350)
pm <- ggpairs(grid100_df_noNAs[, c("RF","Lgb","Xgb","Las","Rid","Li","LiSpa","MEM","Uk","UkSpa","Ok","no2")], 
              upper = list(continuous = wrap(ggally_cor, title=""))) +
                       theme(axis.text.x = element_text(angle =90, hjust = 1)) 

ggsave("C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/allplot-includingNO2tif.jpeg", pm, width = 6, height = 6)



dev.off()


## == no outliers in scatter plot == ##

# #https://universeofdatascience.com/how-to-test-for-identifying-outliers-in-r/
# 
# ggplot(grid100_df) +
#   aes(x = "", y = predNO2_LinSep) +
#   geom_boxplot(fill = "#0c4c8a") +
#   theme_minimal()
# 
# # upper_bound <- quantile(grid100_df$predNO2_LinSep, 0.9985)
# # upper_bound
# # 
# # library(outliers)
# # chisq.out.test(grid100_df$predNO2_LinSep)
# # 
# grubbs.test(grid100_df$predNO2_LinSep)
# 
# library(EnvStats)
# rosnerTest(grid100_df$predNO2_LinSep)$all.stats
# # 
# # hoi <- grid100_df$predNO2_LinSep
# # grubbs.test(hoi)
# # m = max(hoi)
# # #remove max value
# # hoi1 <- hoi[-c(which.max(hoi))]
# # hoi2 <- hoi1[-c(which.max(hoi1))]
# # grubbs.test(hoi2)
# # # mod <- lm(predNO2_LinSep ~ ., data=grid100_df)
# # 
# # hoi <- as.data.frame(hoi)
# # hoi
# 
# 
# lin <- grid100_df$predNO2_LinSep
# lin <- as.data.frame(lin)
# # Top N highest values by group
# lin_desc <- lin%>%                                      
#   arrange(desc(lin))
# 
# #get rid off 100 highest values
# lin_no100s <- lin_desc[-c(1:100),] 
# rosnerTest(lin_no100s)$all.stats
# 
# 
# data_new2 <-hoi%>%                                      # Top N highest values by group
#   arrange(desc(hoi))
# 
# 
# data_new2
# 
# datar <- data_new2[-c(1:100),]
# # 
# # datar
# # 
# # grubbs.test(datar)
# # 
# Q1 <- quantile(grid100_df$predNO2_LinSep, .25)
# Q3 <- quantile(grid100_df$predNO2_LinSep, .75)
# IQR <- IQR(grid100_df$predNO2_LinSep)
# 
# Q3
# Q3_1_5 <- (Q3-Q1)*1.5
# Q3_outlier <- Q3 + Q3_1_5
# Q3_outlier
# 
# no_outliers <- subset(grid100_df, grid100_df$predNO2_LinSep > (Q1 - 1.5*IQR) & grid100_df$predNO2_LinSep < (Q3 + 1.5*IQR))
# # 
# # dim(grid100_df)
# # dim(no_outliers)
# # print(no_outliers)
# # 
# # no_outliers_noNAs = na.omit(no_outliers)
# 
# no2 <- no_outliers$no2
# dim(no_outliers_noNAs)
# # 
# ggpairs(no_outliers[, c("predNO2_LinSep", "no2")])
# 
# ggpairs(grid100_df[, c("predNO2_LinSep", "no2")])
# 
# 
# vars = c("predNO2_Lin","predNO2_LinSep","predNO2_MEM","predNO2_UK","predNO2_UKSep","predicted_OK",
#          "p_NO2_RF","p_NO2_LA", "p_NO2_RI", "p_NO2_LG", "p_NO2_X", "no2")


#create for loop that for every model filters rows and keep the ones with values between 0 and 85.

#relevant models
vars = c("RF","Lgb","Xgb","Las","Rid","Li","LiSpa","MEM","Uk","UkSpa","Ok", "no2")
#remove NA values
grid100_df_noNAs = na.omit(grid100_df)
#create empty list where processed data will be stored to.
processed_data <- list()
for(var in vars){
  #apply filtering
  between0_85 <- grid100_df_noNAs[grid100_df_noNAs[var] < 85 & grid100_df_noNAs[var] > 0, ]
  #keep track of progress in for loop via printing
  print(var)
  #give every processed dataset a unique name that hints to the model used in the fold.
  assign(paste0("between0_85", var, sep=""), between0_85[,c(var, "key")])
  #examine dimensions
  dim(paste0("between0_85", var, sep=""))
  #append processed data to list that is defined outside for loop.
  processed_data <- c(processed_data, paste0("between0_85", var, sep=""))
}
#examine
processed_data


between0_85_allmodels <- Reduce(function(x,y) merge(x, y, by = "key", all.x = TRUE, all.y = TRUE),
                         list(between0_85RF, between0_85Lgb,between0_85Xgb,between0_85Las,between0_85Rid,
                        between0_85Li, between0_85LiSpa, between0_85MEM, between0_85Uk, between0_85UkSpa, between0_85Ok, between0_85no2))

between0_85_allmodels





pm <- ggpairs(between0_85_allmodels[, c("RF","Lgb","Xgb","Las","Rid","Li","LiSpa","MEM","Uk","UkSpa","Ok","no2")], 
              upper = list(continuous = wrap(ggally_cor, title=""))) +
  theme(axis.text.x = element_text(angle =90, hjust = 1)) 

ggsave("C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/allplot-includingNO2tif-between0_85.jpeg", pm, width = 6, height = 6)



dev.off()



## == maps == ##

#create list with variables to visualize
vars = c("predNO2_Lin","predNO2_LinSep","predNO2_MEM","predNO2_UK","predNO2_UKSep","predicted_OK",
            "p_NO2_RF","p_NO2_LA", "p_NO2_RI", "p_NO2_LG", "p_NO2_X", "no2")


length(vars)
breaks = c(-100, 0, 15, 20, 25, 30, 35, 40, 45, 50, 100, 1000)
#manually define color palette
paleta1 <- c("grey", "palegreen4", "palegreen3","palegreen","greenyellow",  "yellow", "gold", "darkorange", "red", "darkred", "grey")


# loop through the shapefiles and create a map for each
for (i in 1:length(vars)) {
  vars[i]
  map <- tm_shape(grid100) + tm_fill(col = vars[i], breaks=breaks, palette=paleta1, legend.show = FALSE)
  hi = vars[i]
  tmap_save(map, width = 1000, height = 1000, units="px", filename = paste("C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/",hi,".jpg", sep=""))
}

# tm_shape(grid100) + tm_fill(col = "p_NO2_LG", breaks=breaks, palette=paleta1)
# 
# tm_shape(grid100) + tm_fill(col = "predNO2_Lin", breaks=breaks, palette=paleta1, legend.show = FALSE)

legende <- tm_shape(grid100) + tm_fill(col = "predNO2_Lin", title = "                        Predicted NO2", breaks=breaks, palette=paleta1, legend.is.portrait = FALSE) + tm_layout(legend.only = T, fontfamily = "serif")
tmap_save(legende, width = 1000, height = 1000, units="px", filename = "C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/Legende.jpg")



#local predictors
predictors = c("nightlight_450",    "nightlight_4950", "population_1000","population_3000","road_class_1_5000",
               "road_class_2_100","road_class_2_1000","road_class_1_100","road_class_2_5000" ,"road_class_3_100",  "road_class_3_300" 
               ,"trafBuf50")


# loop through the shapefiles and create a map for each
for (i in 1:length(predictors)) {
  predictors[i]
  map <- tm_shape(grid100) + tm_fill(col = predictors[i], legend.show = FALSE)
  hi = predictors[i]
  tmap_save(map, width = 1000, height = 1000, units="px", filename = paste("C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/local_predictors",hi,".jpg", sep=""))
}


colnames(grid100)

summary(grid100$road_class_2_5000)

breaks_rcl2_5000 = c(0, 10000,20000, 30000, 40000,50000, 60000, 70000, 80000)
paleta_rcl2_5000 <- c("palegreen4", "palegreen3","palegreen","greenyellow",  "yellow",  "darkorange", "red", "darkred")

rcl2_5000 = tm_shape(grid100) + tm_fill(col = "road_class_2_5000", breaks=breaks_rcl2_5000, palette=paleta_rcl2_5000, legend.show = FALSE)
legende_rcl2_5000 <- tm_shape(grid100) + tm_fill(col = "road_class_2_5000", title = "                        road_class_2_5000", breaks=breaks_rcl2_5000, palette=paleta_rcl2_5000, legend.is.portrait = FALSE) + tm_layout(legend.only = T, fontfamily = "serif")
tmap_save(rcl2_5000, width = 1000, height = 1000, units="px", filename = "C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/rcl2_5000.jpg")
tmap_save(legende_rcl2_5000, width = 1000, height = 1000, units="px", filename = "C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/Legende_rcl2_5000.jpg")
