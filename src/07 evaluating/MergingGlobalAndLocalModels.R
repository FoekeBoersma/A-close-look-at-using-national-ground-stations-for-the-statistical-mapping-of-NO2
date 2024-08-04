library(rgdal)
library(base)
library(dplyr)
library(purrr)
library(sf)
library(raster)
library("writexl")


#global
global = readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/Grid100/Amsterdam_NO2PredictionPerModel.gpkg')

global_df <- as.data.frame(global)

colnames(global_df)

global_RF <- global_df$predicted_NO2_RF
global_xgboost <- global_df$predicted_NO2_XGBoost
global_lightgbm <- global_df$predicted_NO2_LightGBM
global_lasso <- global_df$predicted_NO2_LASSO
global_ridge <- global_df$predicted_NO2_RIDGE

global_models <- list(global_RF, global_xgboost, global_lightgbm, global_lasso, global_ridge)

for(model in global_models){
  print(summary(model))
}

#local
linear = readOGR("C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/LocalModels/predictedNO2_Linear.gpkg")
linear_separated = readOGR("C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/LocalModels/predictedNO2_Linear_SeparatingSpatialGroups.gpkg")
MEM = readOGR("C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/LocalModels/predictedNO2_MEM.gpkg")
UK = readOGR("C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/LocalModels/predictedNO2_UK_formula.gpkg")
UK_separated = readOGR("C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/LocalModels/predictedNO2_UK_SeparatingSpatialGroups.gpkg")
OK = readOGR("C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/LocalModels/predictedNO2_OK.gpkg")

#make spatial - obtain the geometry for each sample
grid100_sf <- st_as_sf(linear)
geo <- grid100_sf[,c("key", "geometry")]

#to dataframe - for every local dataset
linear_df <- as.data.frame(linear)
linear_separated_df <- as.data.frame(linear_separated)
MEM_df <- as.data.frame(MEM)
UK_df <- as.data.frame(UK)
UK_separated_df <- as.data.frame(UK_separated)
OK_df <- as.data.frame(OK)


colnames(linear_df)

colnames(linear_separated_df)

colnames(MEM_df)

colnames(UK_df)

colnames(UK_separated_df)

colnames(OK_df)

#merge local dataframes to one dataframe, containing all predictions 
merge_local = list(linear_df, linear_separated_df, MEM_df, UK_df, UK_separated_df, OK_df)


merge_local <- merge_local %>% reduce(full_join, by= 'key')

merge_local


#rename
merge_local <- merge_local %>% rename("nightlight_450" = "nightlight_450.x",
                                      
                                      "nightlight_4950" = "nightlight_4950.x",
                                      "population_1000" = "population_1000.x",
                                      "population_3000" = "population_3000.x"
                                      ,"road_class_1_5000" = "road_class_1_5000.x",
                                      "road_class_2_100" = "road_class_2_100.x",
                                      "road_class_2_1000" = "road_class_2_1000.x",
                                      "road_class_1_100" = "road_class_1_100.x" ,
                                      "road_class_2_5000" = "road_class_2_5000.x",
                                      "road_class_3_100" = "road_class_3_100.x",
                                      "road_class_3_300" = "road_class_3_300.x",
                                      "trafBuf50" = "trafBuf50.x")

#filter to only relevant data
local <- merge_local[,c("nightlight_450" ,"nightlight_4950" ,"population_1000","population_3000" ,"road_class_1_5000","road_class_2_100" ,
                  "road_class_2_1000","road_class_1_100" ,"road_class_2_5000","road_class_3_100","road_class_3_300" ,"trafBuf50" ,"spachar", "key","predNO2_Lin",
                  "predNO2_LinSep" , "predNO2_MEM" , "predNO2_UK" , "predNO2_UKSep", 'predicted_OK' )]



local
local_sf <- merge(geo, local, by="key")

local_df <- as.data.frame(local)




#to excel
write_xlsx(local_df, "C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/AllModels/Local_df.xlsx")

## == export option == ##
#sf::st_write(local_sf, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/AllModels/local_predictions.gpkg", driver = "GPKG")

## == assign NO2 tif data to local models == ##

#OG_cen <- gCentroid(OverlayGrid,byid=TRUE)
local_cen <- st_centroid(local_sf,byid=TRUE)

#NO2tif
NO2tif <- ('C:/Users/foeke/OneDrive/Documenten/submitting paper/All scripts - paper/data/no2_Amsterdam/NO2.tif')
NO2tif=raster(NO2tif)

#plot(NO2tif)

#spatially join
local_and_no2tif = raster::extract(NO2tif, local_cen, sp=T) #sp = T: keep all data
#to dataframe
local_and_no2tif_df <- as.data.frame(local_and_no2tif)
#to grid
local_and_no2tif_grid <- merge(geo, local_and_no2tif_df, by="key")

View(local_and_no2tif_grid)

local_and_no2tif_grid = subset(local_and_no2tif_grid, select = -c(coords.x1,coords.x2) )

## == export option == ##
sf::st_write(local_and_no2tif_grid, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/AllModels/local_and_no2tif_grid.gpkg", driver = "GPKG")

global_sf <- st_as_sf(global)
all_models <- st_join(local_and_no2tif_grid, global_sf, largest = T, left = T)

## == export option == ##
sf::st_write(all_models, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/AllModels/all_models.gpkg", driver = "GPKG")
