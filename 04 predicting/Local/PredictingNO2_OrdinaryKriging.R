## == Necessary packages == ##

# We will need some packages for (spatial) data processing
library(tidyverse) # wrangling tabular data and plotting
library(sf) # processing spatial vector data - the easy way
library(sp) # processing spatial vector data - the way gstat needs it
library(raster) # processing spatial raster data. !!!overwrites dplyr::select!!!
library(rgdal) #import shapefiles
library(rgeos) #contains gCentroid
library(tidyr) #geometry to apart long/lat
library(dismo) #for kfold
library(lme4) #for mixed models (random effects)
library(stats) #quantile
library(nlme) #mixed-effect model
library('parallel') 

# Packages for geostatistics
library(gstat)   # The most popular R-Package for Kriging 
library(automap) # Automatize some (or all) parts of the gstat-workflow 

# Finally, some packages to make pretty plots
library(patchwork)
library(viridis)
library(tmap)
library(graphics) #for text


## == DEFINE COORDINATE SYSTEMS == ##

#CRS with metric system is preferred (=crs_32).

crs <- CRS("+proj=longlat +datum=WGS84") # crs
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs_32 <- CRS(sprintf(utmStr, 32))

## == import geodata == ##

data <- read.csv('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForModelling/LocalModels/Local_ModellingDataset_distance-processed.csv', sep=';')
#replace NA with 0
data[is.na(data)] <- 0

#make imported csv spatial

#to sf
data_sf = st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
#change to planar crs
data_32 <- st_transform(data_sf,crs=crs_32)

#import grid where predictions will be projected on
grid100 = readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Amsterdam/Grid100_LocalPredictors_Amsterdam_inclSelVars.gpkg')

#make spatial - obtain the geometry for each sample
grid100_sf <- st_as_sf(grid100)
#make key qith unique fid that will be used for joining all local datasets and related predictions
grid100_sf$key = seq(1,nrow(grid100_sf))

## == modelling == ##

## == Kriging - variogram setting == ##

#now to spatial points dataframe
data_sp <- as(data_32, "Spatial")


# define grid for projection 

# = create grid where kriging needs to be projected on = #

grid100_centroids <- st_centroid(grid100_sf)

bbox <- st_bbox(grid100_centroids)


grid <- grid100_centroids %>% 
  st_bbox() %>%     # determines bounding box coordinates from meuse
  st_as_sfc() %>%   # creates sfc object from bounding box
  st_make_grid(     # create grid 100m x 100m pixel size
    cellsize = c(100, 100), 
    what = "corners") %>%
  st_as_sf(crs=st_crs(data_32)) # convert to sf object

# Convert grid to SpatialPixelsDataFrame, the raster/grid equivalent in 
# in the sp world
grid_sp <- as(as(grid, "Spatial"), "SpatialPixels")


#define x & y variables to coordinates
data_xy <- data.frame(x = data_sp$coords.x1, y = data_sp$coords.x2)
coordinates(data_xy) = ~x+y

#variogram

#perform autofit variogram, based on dependent variable 'Lopend_gemiddelde'
variogram_auto_lin = autofitVariogram(Lopend_gemiddelde ~ 1, data_sp)

plot(variogram_auto_lin)

autofit_params_lin <- variogram_auto_lin$var_model 

#examine suggested variogram paramater settings via print function
print(autofit_params_lin)

#manually insert variogram settings, based on autofit
#m <- vgm(psill = 80.16775, "Ste", range = 98.67688)
m <- vgm(psill = 80.16775, "Ste", range = 10000)

OK<-krige(Lopend_gemiddelde~1, 
          loc= data_sp,        # Data frame
          newdata=grid_sp,      # Prediction grid
          model = m)       # fitted varigram model


OK_sf <- st_as_sf(OK)

OK_sf <- OK_sf %>%rename(predicted_OK = var1.pred, 
                   variance_OK = var1.var)
print(OK_sf)

OK_grid <- st_join(grid100_sf, OK_sf)

## == export option == ##
sf::st_write(OK_grid, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels/predictedNO2_OK.gpkg", driver = "GPKG")
