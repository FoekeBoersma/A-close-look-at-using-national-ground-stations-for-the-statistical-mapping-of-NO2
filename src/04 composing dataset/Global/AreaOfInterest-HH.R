#necessary libraries
library(sf)
library(sfnetworks)
library(tidygraph)
library(tmap)
library(nngeo)
library(osmdata)
library(dplyr)
library(lwgeom)
library(raster)
library(rgdal)
library(utils)
library(ggplot2)
library(rgeos)
library(tidyverse)
library(leaflet) #mapping in OSM
library(bnspatial)
library(gstat)
library(geosphere) #geosphere::dist2Line
library(stars) #for st_rasterize
library(base) #sprintf

#connect to yaml file
current_dir <- rstudioapi::getActiveDocumentContext()$path
# Move one level up in the directory
config_dir <- dirname(dirname(current_dir))
# Construct the path to the YAML configuration file
config_path <- file.path(config_dir, "config_04.yml")
# Read the YAML configuration file
config <- yaml.load_file(config_path)

# Use dirname() to get the parent directory
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))

## == define output path == ##
out_location <- config_04$out_location
out_location_dir <- normalizePath(file.path(parent_directory, out_location ), winslash = "/")

## == intialize coordinates - area of interest == ##
coor_1 = 9.7668
coor_3 = 10.2228
coor_2 = 53.3948 
coor_4 = 53.7145

## == create polygon with coordinates == ##

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
class(poly_sf)
#sf::st_write(poly_sf, dsn=out_location_dir +'/polyHamburg.gpkg', driver = "GPKG")

## == make grid == ##

##  First project data into a planar coordinate system (here 3035 - Lambert Azimuthal Equal Area projection)
poly_3035 <- st_transform(poly_sf, crs=3035)

grid <- st_make_grid(poly_3035, cellsize=100)

#export option
sf::st_write(grid, dsn=out_location_dir +'/grid100Hamburg.gpkg', driver = "GPKG")
