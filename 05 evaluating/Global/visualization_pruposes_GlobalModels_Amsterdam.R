

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



#global
global = readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Amsterdam/Amsterdam_NO2PredictionPerModel.shp')



## == Amsterdam == ##

library(sp)


y =  52.370216
x = 4.852168

Amsterdam_point <- cbind(x, y)

print(Amsterdam_point)
Amsterdam_point <- as.data.frame(Amsterdam_point)
xy <- Amsterdam_point[,c(1,2)]

spdf <- SpatialPointsDataFrame(coords = xy, data = Amsterdam_point,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


Amsterdam_point <- st_as_sf(spdf)

##  First project data into a planar coordinate system (here UTM zone 32)
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"

crs_32 <- CRS(sprintf(utmStr, 32))

Amsterdam_point_32 <- st_transform(Amsterdam_point, crs=crs_32)

#Important: coordinates in m, adjust the units on function
# Helper funct
rect_around_point <- function(x,xsize,ysize){
  bbox <- st_bbox(x)
  bbox <- bbox +c(xsize/2,ysize/2,-xsize/2,-ysize/2)
  return(st_as_sfc(bbox))
}

Amsterdam_square_buffer <- rect_around_point(Amsterdam_point_32, 30000, 30000)




## == export option == ##
#sf::st_write(Amsterdam_square_buffer, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels/Amsterdam_square_buffer30.gpkg", driver = "GPKG")

#spatial query

library(spatialEco)
sp_query_Amsterdam <-spatial.select(Amsterdam_square_buffer ,y = global,predicate = "contains")



colnames(sp_query_Amsterdam)

sf::st_write(sp_query_Amsterdam, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels/sp_query_Amsterdam.gpkg", driver = "GPKG")



## == maps == ##

#create list with variables to visualize
vars = c("p_NO2_RF","p_NO2_LA", "p_NO2_RI", "p_NO2_LG", "p_NO2_X", "no2")


length(vars)
breaks = c(-100, 0, 15, 20, 25, 30, 35, 40, 45, 50, 100, 1000)
#manually define color palette
paleta1 <- c("grey", "palegreen4", "palegreen3","palegreen","greenyellow",  "yellow", "gold", "darkorange", "red", "darkred", "grey")


# loop through the shapefiles and create a map for each
for (i in 1:length(vars)) {
  vars[i]
  map <- tm_shape(sp_query_Amsterdam) + tm_fill(col = vars[i], breaks=breaks, palette=paleta1, legend.show = FALSE)
  hi = vars[i]
  tmap_save(map, width = 1000, height = 1000, units="px", filename = paste("C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/Amsterdam/Global_",hi,".jpg", sep=""))
}