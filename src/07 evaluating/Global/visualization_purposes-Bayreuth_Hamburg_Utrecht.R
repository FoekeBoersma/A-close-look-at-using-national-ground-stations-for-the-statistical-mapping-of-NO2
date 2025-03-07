#import necessary packages
library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(spatialEco)
library(yaml)

## == import spatial datasets == ##

# Connect to YAML file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir)) # One level up in directory
config07_path <- file.path(config_dir, "config_07.yml")

# Read YAML configuration file
config07 <- yaml::yaml.load_file(config07_path)

# Define the parent directory (move four levels up)
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
Bayreuth_NO2PredictionPerModel_dir <- normalizePath(file.path(parent_directory, config07$input_data$Bayreuth_NO2PredictionPerModel), winslash = "/")
Bayreuth_NO2PredictionPerModel_ZI_dir <- normalizePath(file.path(parent_directory, config07$input_data$Bayreuth_NO2PredictionPerModel_ZI), winslash = "/")
Hamburg_NO2PredictionPerModel_dir <- normalizePath(file.path(parent_directory, config07$input_data$Hamburg_NO2PredictionPerModel), winslash = "/")
Utrecht_NO2PredictionPerModel_dir <- normalizePath(file.path(parent_directory, config07$input_data$Utrecht_NO2PredictionPerModel), winslash = "/")

grid100_Bayreuth <- st_read(Bayreuth_NO2PredictionPerModel_dir)
grid100_HH <- st_read(Hamburg_NO2PredictionPerModel_dir)
grid100_Utrecht <- st_read(Utrecht_NO2PredictionPerModel_dir)

# Define output directory
out_location_dir <- normalizePath(file.path(parent_directory, config07$out_location), winslash = "/")


# create new map inside the output directory and update variable out_location_dir
out_location_dir <- file.path(out_location_dir, "maps_bayreuth_hh_utrecht")

# check if folder exists; if not, create it
if (!dir.exists(out_location_dir)) {
  dir.create(out_location_dir, recursive = TRUE)
}
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
# palette <- c("grey", "palegreen4", "palegreen3","palegreen","greenyellow",  "yellow", "gold", "darkorange", "red", "darkred", "grey")
palette <- c("#808080", "#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026" , "#808080")


# loop through the shapefiles and create a map for each - Bayreuth
for (i in seq_along(vars)) {
  vars[i]
  map <- tm_shape(grid100_Bayreuth) + tm_fill(col = vars[i], breaks=breaks, palette=palette, legend.show = FALSE, border.col = NA,
      lwd.scale = tm_scale())
  model = vars[i]
  filename <- file.path(out_location_dir, paste0("Bayreuth_", model, ".png"))
  tmap_save(map, width = 5000, height = 5000, units="px", filename = filename)
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
grid100_Bayreuth <- st_transform(grid100_Bayreuth, crs=3035)

#export option
sf::st_write(poly_3035, dsn=file.path(out_location_dir,"poly_3035.gpkg"), driver = "GPKG", append = FALSE)


#spatial query035
sp_query <-spatial.select(poly_3035,y = grid100_Bayreuth,predicate = "contains")

sf::st_write(sp_query, dsn = file.path(out_location_dir, "Bayreuth_ZI.gpkg"), driver = "GPKG", append = FALSE)


#manual option: import Bayreuth ZI (two Bayreuth versions are available: a zoomed out- and zoomed in-version where we use the latter now.
grid100_Bayreuth_ZI = st_read(Bayreuth_NO2PredictionPerModel_ZI_dir)


# loop through the shapefiles and create a map for each - Bayreuth
for (i in seq_along(vars)) {
  vars[i]
  map <- tm_shape(grid100_Bayreuth_ZI) + tm_fill(col = vars[i], breaks=breaks, palette=palette, legend.show = FALSE, border.col = NA,
      lwd.scale = tm_scale())
  model = vars[i]
  filename <- file.path(out_location_dir, paste0("Bayreuth_ZI_", model, ".png"))
  tmap_save(map, width = 5000, height = 5000, units="px", filename = filename)
}

# loop through the shapefiles and create a map for each - Hamburg
for (i in seq_along(vars)) {
  vars[i]
  map <- tm_shape(grid100_HH) + tm_fill(col = vars[i], breaks=breaks, palette=palette, legend.show = FALSE, border.col = NA,
      lwd.scale = tm_scale())
  model = vars[i]
  filename <- file.path(out_location_dir, paste0("Hamburg_", model, ".png"))
  tmap_save(map, width = 5000, height = 5000, units="px", filename = filename)
}

# loop through the shapefiles and create a map for each - Utrecht
for (i in seq_along(vars)) {
  vars[i]
  map <- tm_shape(grid100_Utrecht) + tm_fill(col = vars[i], breaks=breaks, palette=palette, legend.show = FALSE, border.col = NA,
      lwd.scale = tm_scale())
  model = vars[i]
  filename <- file.path(out_location_dir, paste0("Utrecht_", model, ".png"))
  tmap_save(map, width = 5000, height = 5000, units="px", filename = filename)
}

# tm_shape(grid100_Bayreuth) + tm_fill(col = "predicted_NO2_LASSO", breaks=breaks, palette=palette, legend.show = FALSE)
# 
# tm_shape(grid100) + tm_fill(col = "predicted_NO2_RF", breaks=breaks, palette=palette) + tm_layout(legend.only = T)
