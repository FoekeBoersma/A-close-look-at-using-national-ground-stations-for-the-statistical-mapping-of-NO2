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

## == intialize coordinates - area of interest == ##
coor_1 = 11.2749
coor_3 = 11.8729
coor_2 = 50.2327
coor_4 = 49.6339

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
#sf::st_write(poly_sf, dsn='C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/polyBayreuth.gpkg', driver = "GPKG")

## == make grid == ##

##  First project data into a planar coordinate system (here 3035 - Lambert Azimuthal Equal Area projection)
poly_3035 <- st_transform(poly_sf, crs=3035)

grid <- st_make_grid(poly_3035, cellsize=100)

#export option
sf::st_write(grid, dsn='C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/grid100Bayreuth.gpkg', driver = "GPKG")

