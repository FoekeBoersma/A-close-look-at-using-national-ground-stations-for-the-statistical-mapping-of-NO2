# Load required libraries
library(sf)
library(osmdata)
library(dplyr)
library(tmap)
library(leaflet)
library(rgdal)
library(stars)

# Connect to YAML file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir))
config_path <- file.path(config_dir, "config_04.yml")
config <- yaml::yaml.load_file(config_path)

# Define output path
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
out_location_dir <- normalizePath(file.path(parent_directory, config$out_location), winslash = "/")

## == intialize coordinates - area of interest (Hamburg) == ##
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
polygon = Polygon(xym) #create polygon
polygons = Polygons(list(polygon),1) #wrap into a Polygons object
spatial_polygon = SpatialPolygons(list(polygons)) #wrap into a SpatialPolygons object
proj4string(spatial_polygon) = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs")

# Convert to sf object
polygon_sf <- st_as_sf(spatial_polygon)

## == make grid == ##

##  First project data into a planar coordinate system (here 3035 - Lambert Azimuthal Equal Area projection)
polygon_3035 <- st_transform(polygon_sf, crs=3035)
grid_Hamburg <- st_make_grid(polygon_3035, cellsize=100)

#export option
sf::st_write(grid_Hamburg, dsn=file.path(out_location_dir,'grid100Hamburg.gpkg'), driver = "GPKG")
