#import necessary libraries
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
library(sp)
library(spatialEco)

## == import global dataset, projected onto Amsterdam == ##
global = readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/Grid100/Amsterdam_NO2PredictionPerModel.gpkg')

## == Amsterdam == ##

#coordinates relating to Amsterdam
y =  52.370216
x = 4.852168

#combine to make a point
Amsterdam_point <- cbind(x, y)
#converse to dataframe to make further data processing feasible
Amsterdam_point <- as.data.frame(Amsterdam_point)
#assign the coordinate values to the variabble xy
xy <- Amsterdam_point[,c(1,2)]

#make spatial - first converting it to a SpatialPointsDataFrame is feasible to make the data spatial
spdf <- SpatialPointsDataFrame(coords = xy, data = Amsterdam_point,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#to sf - necessary for further data operations
Amsterdam_point <- st_as_sf(spdf)

##  First project data into a planar coordinate system (here 3035)

#apply variable as argument to project onto Amsterdam point
Amsterdam_point_3035 <- st_transform(Amsterdam_point, crs=3035)

#Important: coordinates in m, adjust the units on function
rect_around_point <- function(x,xsize,ysize){
  bbox <- st_bbox(x)
  bbox <- bbox +c(xsize/2,ysize/2,-xsize/2,-ysize/2)
  return(st_as_sfc(bbox))
}

#create rectangle relating to Amsterdam area (2nd and 3rd argument specify extent)
Amsterdam_square_buffer <- rect_around_point(Amsterdam_point_3035, 30000, 30000)

## == export option == ##
#sf::st_write(Amsterdam_square_buffer, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/visualization/Amsterdam_square_buffer.gpkg", driver = "GPKG")

#spatial query - assign the data to the extent of Amsterdam which was defined above
sp_query_Amsterdam <-spatial.select(Amsterdam_square_buffer ,y = global,predicate = "contains")
#export option
#sf::st_write(sp_query_Amsterdam, dsn="C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/visualization/sp_query_Amsterdam.gpkg", driver = "GPKG")

## == maps == ##

sp_query_Amsterdam

#create list with variables to visualize
vars = c("predicted_NO2_RF", "predicted_NO2_LASSO", "predicted_NO2_RIDGE", "predicted_NO2_LightGBM", "predicted_NO2_XGBoost")
#specify groups of no2 values
breaks = c(-100, 0, 15, 20, 25, 30, 35, 40, 45, 50, 100, 1000)
#manually define color palette: color x equals no2 value group y.
palette_colors <- c("grey", "palegreen4", "palegreen3","palegreen","greenyellow",  "yellow", "gold", "darkorange", "red", "darkred", "grey")

# loop through the shapefiles and create a map for each, save it too
for (i in 1:length(vars)) {
  print(vars[i])
  map <- tm_shape(sp_query_Amsterdam) + tm_fill(col = vars[i], breaks=breaks, palette=palette_colors, legend.show = FALSE)
  model = vars[i]
  tmap_save(map, width = 1000, height = 1000, units="px", filename = paste("C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/visualization/Amsterdam/Global_",model,".jpg", sep=""))
}
