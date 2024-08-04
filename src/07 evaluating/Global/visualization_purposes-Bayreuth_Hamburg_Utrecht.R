#import necessary packages
library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(rgdal)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(spatialEco)

## == import spatial datasets == ##

#Bayreuth
grid100_Bayreuth = readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/Grid100/Bayreuth_NO2PredictionPerModel.gpkg')

#Hamburg
grid100_HH = readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/Grid100/Hamburg_NO2PredictionPerModel.gpkg')

#Utrecht
grid100_Utrecht = readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/Grid100/Utrecht_NO2PredictionPerModel.gpkg')

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
palette <- c("grey", "palegreen4", "palegreen3","palegreen","greenyellow",  "yellow", "gold", "darkorange", "red", "darkred", "grey")


# loop through the shapefiles and create a map for each - Bayreuth
for (i in 1:length(vars)) {
  vars[i]
  map <- tm_shape(grid100_Bayreuth) + tm_fill(col = vars[i], breaks=breaks, palette=palette, legend.show = FALSE)
  model = vars[i]
  tmap_save(map, width = 1000, height = 600, units="px", filename = paste("C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/visualization/Bayreuth_",model,".jpg", sep=""))
}


## == zoomed in version (Bayreuth) == ##

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
##  First project data into a planar coordinate system (here 3035)
poly_3035 <- st_transform(poly_sf, crs=3035)
#export option
sf::st_write(poly_3035, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/visualization/poly_3035.gpkg", driver = "GPKG")


#spatial query035
sp_query <-spatial.select(poly_3035,y = grid100_Bayreuth,predicate = "contains")

sf::st_write(sp_query, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/visualization/Bayreuth/Bayreuth_ZI.gpkg", driver = "GPKG")

#manual option: import Bayreuth ZI (two Bayreuth versions are available: a zoomed out- and zoomed in-version where we use the latter now.
grid100_Bayreuth_ZI = readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/visualization/Bayreuth/Bayreuth_ZI.gpkg')


# loop through the shapefiles and create a map for each - Bayreuth
for (i in 1:length(vars)) {
  vars[i]
  map <- tm_shape(grid100_Bayreuth_ZI) + tm_fill(col = vars[i], breaks=breaks, palette=palette, legend.show = FALSE)
  model = vars[i]
  tmap_save(map, width = 1000, height = 1000, units="px", filename = paste("C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/visualization/Bayreuth/Bayreuth_ZI_",model,".jpg", sep=""))
}

# loop through the shapefiles and create a map for each - Hamburg
for (i in 1:length(vars)) {
  vars[i]
  map <- tm_shape(grid100_HH) + tm_fill(col = vars[i], breaks=breaks, palette=palette, legend.show = FALSE)
  model = vars[i]
  tmap_save(map, width = 1000, height = 1000, units="px", filename = paste("C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/visualization/Hamburg/Hamburg_",model,".jpg", sep=""))
}

# loop through the shapefiles and create a map for each - Utrecht
for (i in 1:length(vars)) {
  vars[i]
  map <- tm_shape(grid100_Utrecht) + tm_fill(col = vars[i], breaks=breaks, palette=palette, legend.show = FALSE)
  model = vars[i]
  tmap_save(map, width = 1000, height = 1000, units="px", filename = paste("C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/visualization/Utrecht/Utrecht_",model,".jpg", sep=""))
}

# tm_shape(grid100_Bayreuth) + tm_fill(col = "predicted_NO2_LASSO", breaks=breaks, palette=palette, legend.show = FALSE)
# 
# tm_shape(grid100) + tm_fill(col = "predicted_NO2_RF", breaks=breaks, palette=palette) + tm_layout(legend.only = T)
