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

plot(poly)

proj4string(poly) = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs")

#export option
poly_sf <- st_as_sf(poly)
class(poly_sf)
sf::st_write(poly_sf, dsn='C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Hamburg/AOI/polyHH.gpkg', driver = "GPKG")

## == make grid == ##

##  First project data into a planar coordinate system (here UTM zone 32)
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs_32 <- CRS(sprintf(utmStr, 32))
poly_32 <- st_transform(poly_sf, crs=crs_32)

grid <- st_make_grid(poly_32, cellsize=100)

#export option
sf::st_write(grid, dsn='C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Hamburg/AOI/grid100HH.gpkg', driver = "GPKG")
#convert back to wgs84
grid_84 <- st_transform(grid, crs=st_crs(poly_sf))
#export option
#sf::st_write(grid_84, dsn='C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Hamburg/AOI',layer='grid100Amsterdam84', driver = "ESRI Shapefile")
