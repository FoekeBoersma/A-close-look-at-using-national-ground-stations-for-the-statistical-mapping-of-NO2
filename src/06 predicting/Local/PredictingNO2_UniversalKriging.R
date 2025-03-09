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

## == Connect to YAML Configuration File == ##

current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir)) # One level up in directory
config_path <- file.path(config_dir, "config_06.yml")
config <- yaml::yaml.load_file(config_path)

# Define the parent directory (four levels up)
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
# Define output directory
out_location_dir <- normalizePath(file.path(parent_directory, config$out_location), winslash = "/")


## == DEFINE COORDINATE SYSTEMS == ##

#CRS with metric system is preferred (=crs_32).

crs <- CRS("+proj=longlat +datum=WGS84") # crs
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs_32 <- CRS(sprintf(utmStr, 32))

## == import geodata == ##

data <- read.csv(config$input_data$local_modeling_dataset, sep=';')
#replace NA with 0
data[is.na(data)] <- 0

#convert to spatial points dataframe (gstat relies on sp package more than sf)
#first convert to sf
#to sf
data_sf = st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
#change to planar crs
data_32 <- st_transform(data_sf,crs=crs_32)
#to SpatialPointsDataFrame
data_32 <- as(data_32, 'Spatial')

# define grid for projection 
# import AOI
grid100 = st_read(config$input_data$local_predictors_amsterdam)

#convert to sf to use function "rename"
grid100 <- st_as_sf(grid100)
#make key qith unique fid that will be used for joining all local datasets and related predictions
grid100$key = seq(1,nrow(grid100))
# data_32 is already in EPSG: 3035
grid100 <- st_transform(grid100, crs = st_crs(data_32))
#convert grid back to spatialPolygonsDataframe
grid100 <- as(grid100, 'Spatial')



linear= lm(Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data=data_32)

#to dataframe
data_linear <- data.frame(x = data_32$coords.x1, y = data_32$coords.x2, resid=resid(linear))

#variogram - automatically
coordinates(data_linear) = ~x+y
variogram_auto = autofitVariogram(resid ~ 1, data_linear)
plot(variogram_auto)
print(variogram_auto$var_model)
autofit_params <- variogram_auto$var_model #create dataset including variogram parameters

#manually insert parameters into variogram
m <- vgm(psill = 20.6688, "Sph", range =  276.6727)

# Universal kriging (parallel processing)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster (after loading all the necessary object to R environment: meuse, meuse.grid, m)
cl <- makeCluster(no_cores)

parts <- split(x = 1:length(grid100), f = 1:no_cores)

clusterExport(cl = cl, varlist = c("data_32", "grid100", "m", "parts"), envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))

crs(data_32)
crs(grid100)

system.time(parallelX <- parLapply(cl = cl, X = 1:no_cores, fun = function(x) krige(formula = Lopend_gemiddelde~1+ nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, locations = data_32, newdata = grid100[parts[[x]],], model = m)))

stopCluster(cl)

# Merge all the predictions    
# Merge all the predictions    
mergeParallelX <- maptools::spRbind(parallelX[[1]], parallelX[[2]])
mergeParallelX <- maptools::spRbind(mergeParallelX, parallelX[[3]])
mergeParallelX <- maptools::spRbind(mergeParallelX, parallelX[[4]])
mergeParallelX <- maptools::spRbind(mergeParallelX, parallelX[[5]])
mergeParallelX <- maptools::spRbind(mergeParallelX, parallelX[[6]])
mergeParallelX <- maptools::spRbind(mergeParallelX, parallelX[[7]])
mergeParallelX <- maptools::spRbind(mergeParallelX, parallelX[[8]])
mergeParallelX <- maptools::spRbind(mergeParallelX, parallelX[[9]])
mergeParallelX <- maptools::spRbind(mergeParallelX, parallelX[[10]])
mergeParallelX <- maptools::spRbind(mergeParallelX, parallelX[[11]])
mergeParallelX <- maptools::spRbind(mergeParallelX, parallelX[[12]])
mergeParallelX <- maptools::spRbind(mergeParallelX, parallelX[[13]])
mergeParallelX <- maptools::spRbind(mergeParallelX, parallelX[[14]])
mergeParallelX <- maptools::spRbind(mergeParallelX, parallelX[[15]])
# Create SpatialPixelsDataFrame from mergeParallelX
mergeParallelX <- SpatialPixelsDataFrame(points = mergeParallelX, data = mergeParallelX@data)

# Plot mergeParallelX    
spplot(mergeParallelX["var1.pred"], main = "universary kriging predictions")

raster_uk <- raster(mergeParallelX["var1.pred"])

#convert NA to 0's
raster_uk[is.na(raster_uk)] <- 0

#export option - to raster
writeRaster(raster_uk, file.path(out_location_dir, 'rasterUK.tif'))

## ==  Extract the raster values underlying the polygons == ##

grid100$ID <- seq(1:nrow(grid100))
grid_centroids <- gCentroid(grid100,byid=TRUE)

cen100_values_uk <- raster::extract(raster_uk, grid_centroids, sp=T)
#make spatial (grid) - needed for spatial join
grid100_sf <- st_as_sf(grid100)
#make spatial (centroids) - needed for spatial join
cen100_values_uk_sf <- st_as_sf(cen100_values_uk) 
#centroids to grid via spatial join
cen100_uk_values_predictedNO2<- st_join(grid100_sf, cen100_values_uk_sf, join=st_nearest_feature)

cen100_uk_values_predictedNO2 <- cen100_uk_values_predictedNO2 %>% rename(predNO2_UK = "var1.pred")

#convert NA to 0's
cen100_uk_values_predictedNO2[is.na(cen100_uk_values_predictedNO2)] <- 0

#export option
sf::st_write(cen100_uk_values_predictedNO2, dsn=file.path(out_location_dir,"predictedNO2_UK_formula.gpkg"), driver = "GPKG")

#export option - to csv
#write.csv(FFR_grid100_UK_values_FFR, '/LocalModels/PredictedNO2ByKriging/predictedNO2_UK_FFR.csv')



