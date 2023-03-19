library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(rgdal)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package

#Bayreuth
grid100_Bayreuth = readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Bayreuth/Bayreuth_NO2PredictionPerModel.gpkg')

#Hamburg
grid100_HH = readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Hamburg/Hamburg_NO2PredictionPerModel.gpkg')

#Utrecht
grid100_Utrecht = readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Utrecht/Utrecht_NO2PredictionPerModel.gpkg')

#to datadrame
grid100_Utrecht_df <- as.data.frame(grid100_Utrecht)

# Add fill layer to nz shape
grid100_Bayreuth <- st_as_sf(grid100_Bayreuth)
grid100_HH <- st_as_sf(grid100_HH)
grid100_Utrecht <- st_as_sf(grid100_Utrecht)

colnames(grid100_Utrecht_df)
#create list with variables to visualize
vars = c("predicted_NO2_RF",       "predicted_NO2_LASSO" ,  
         "predicted_NO2_RIDGE"  ,  "predicted_NO2_LightGBM" ,"predicted_NO2_XGBoost")

length(vars)
breaks = c(-100, 0, 15, 20, 25, 30, 35, 40, 45, 50, 100, 1000)
#manually define color palette
paleta1 <- c("grey", "palegreen4", "palegreen3","palegreen","greenyellow",  "yellow", "gold", "darkorange", "red", "darkred", "grey")

tm_shape(grid100_Bayreuth) + tm_fill(col = "predicted_NO2_RF", breaks=breaks, palette=paleta1, legend.show = FALSE, width = 1000, height = 500)



tmap_save(map,asp = 1, width = 1000, height = 1000, units="px", filename = "C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/Bayreuth/Bayreuth_TEST1.jpg")



# loop through the shapefiles and create a map for each - Bayreuth
for (i in 1:length(vars)) {
  vars[i]
  map <- tm_shape(grid100_Bayreuth) + tm_fill(col = vars[i], breaks=breaks, palette=paleta1, legend.show = FALSE)
  hi = vars[i]
  tmap_save(map, width = 1000, height = 600, units="px", filename = paste("C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/Bayreuth/Bayreuth_",hi,".jpg", sep=""))
}


## == zoomed in version == ##

## == intialize coordinates - area of interest (BAYREUTH) == ##
coor_1 = 11.4869
coor_3 = 11.6627
coor_2 = 49.9868
coor_4 = 49.8991

## == create polygon with coordinates == ##

# automatic option - create own polygon and use for spatial filtering

#filter traffic counting points within bounding box
x_coord = c(coor_1, coor_1, coor_3, coor_3)
y_coord = c(coor_4, coor_2, coor_2, coor_4)
xym <- cbind(x_coord, y_coord)

#transform into spatial polygon
p = Polygon(xym) #create polygon
ps = Polygons(list(p),1) #wrap into a Polygons object
poly = SpatialPolygons(list(ps)) #wrap into a SpatialPolygons object

proj4string(poly) = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs")

#export option
poly_sf <- st_as_sf(poly)
##  First project data into a planar coordinate system (here UTM zone 32)
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs_32 <- CRS(sprintf(utmStr, 32))
poly_32 <- st_transform(poly_sf, crs=crs_32)
#export option
sf::st_write(poly_32, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels/poly_32.gpkg", driver = "GPKG")


#spatial query

library(spatialEco)
sp_query <-spatial.select(poly_32,y = grid100_Bayreuth,predicate = "contains")

#manual option: import Bayreuth ZI
grid100_Bayreuth_ZI = readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Bayreuth/Bayreuth_NO2PredictionPerModel_ZI.shp')

grid100_Bayreuth_ZI <- st_as_sf(grid100_Bayreuth_ZI)


grid100_Bayreuth_ZI <- grid100_Bayreuth_ZI %>% rename(nightlight_450 = "nightlight",
                              nightlight_3150 = "nightlig_1",
                              population_1000 = "population",
                              population_3000 = "populati_1",
                              road_class_2_25 = "road_class",
                              road_class_3_3000 = "road_cla_1",
                              road_class_3_300 = "road_cla_2",
                              trop_mean_filt = "trop_mean_",
                              predicted_NO2_RF = "predicted_",
                              predicted_NO2_LASSO = "predicted1",
                              predicted_NO2_RIDGE = "predicte_1",
                              predicted_NO2_LightGBM = "predicte_2",
                              predicted_NO2_XGBoost = "predicte_3")

grid100_Bayreuth_ZI

sf::st_write(grid100_Bayreuth_ZI, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Bayreuth/Bayreuth_NO2PredictionPerModel_ZI.gpkg", driver = "GPKG")

grid100_Bayreuth_ZI = readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Bayreuth/Bayreuth_NO2PredictionPerModel_ZI.gpkg')



grid100_Bayreuth_ZI

# loop through the shapefiles and create a map for each - Hamburg
for (i in 1:length(vars)) {
  vars[i]
  map <- tm_shape(grid100_Bayreuth_ZI) + tm_fill(col = vars[i], breaks=breaks, palette=paleta1, legend.show = FALSE)
  hi = vars[i]
  tmap_save(map, width = 1000, height = 1000, units="px", filename = paste("C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/Bayreuth/Bayreuth_ZI_",hi,".jpg", sep=""))
}

#export option
# sf::st_write(sp_query, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels/sp_query.gpkg", driver = "GPKG")












# loop through the shapefiles and create a map for each - Hamburg
for (i in 1:length(vars)) {
  vars[i]
  map <- tm_shape(grid100_HH) + tm_fill(col = vars[i], breaks=breaks, palette=paleta1, legend.show = FALSE)
  hi = vars[i]
  tmap_save(map, width = 1000, height = 1000, units="px", filename = paste("C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/Hamburg/Hamburg_",hi,".jpg", sep=""))
}

# loop through the shapefiles and create a map for each - Utrecht
for (i in 1:length(vars)) {
  vars[i]
  map <- tm_shape(grid100_Utrecht) + tm_fill(col = vars[i], breaks=breaks, palette=paleta1, legend.show = FALSE)
  hi = vars[i]
  tmap_save(map, width = 1000, height = 1000, units="px", filename = paste("C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/Utrecht/Utrecht_",hi,".jpg", sep=""))
}

# tm_shape(grid100_Bayreuth) + tm_fill(col = "predicted_NO2_LASSO", breaks=breaks, palette=paleta1, legend.show = FALSE)
# 
# tm_shape(grid100) + tm_fill(col = "predicted_NO2_RF", breaks=breaks, palette=paleta1) + tm_layout(legend.only = T)
