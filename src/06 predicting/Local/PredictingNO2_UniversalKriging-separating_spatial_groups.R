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
library(yaml)

## == define file paths and import files == ##

current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir)) # One level up in directory
config_path <- file.path(config_dir, "config_06.yml")
config <- yaml::yaml.load_file(config_path)

# Define the parent directory (four levels up)
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
local_modeling_dataset_dir = normalizePath(file.path(parent_directory, config$input_data$local_modeling_dataset_distance_processed), winslash = "/")
grid100_amsterdam_dir = normalizePath(file.path(parent_directory, config$input_data$Grid100_LocalPredictors_Amsterdam_inclSelVars), winslash = "/")
# Define output directory
out_location_dir <- normalizePath(file.path(parent_directory, config$out_location), winslash = "/")

# Create the output directory for predictions
output_uksep_dir <- file.path(out_location_dir, "uksep")
if (!dir.exists(output_uksep_dir)) {
  dir.create(output_uksep_dir)
}

## == DEFINE COORDINATE SYSTEMS == ##

#CRS with metric system is preferred (=crs_32).
crs <- CRS("+proj=longlat +datum=WGS84") # crs
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs_32 <- CRS(sprintf(utmStr, 32))

## == import geodata == ##
data <- read.csv(local_modeling_dataset_dir, sep=';')
#replace NA with 0
data[is.na(data)] <- 0

#convert to spatial points dataframe (gstat relies on sp package more than sf)
#first convert to sf
data_sf = st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
data_32 <- st_transform(data_sf,crs=crs_32) #change to planar crs
#to SpatialPointsDataFrame
data_32 <- as(data_32, 'Spatial')

# define grid for projection (covariance)
# import AOI
grid100 = st_read(grid100_amsterdam_dir)
print(head(grid100))
grid100 <- st_as_sf(grid100)
#convert grid to spatialPolygonsDataframe
grid100 <- as(grid100, 'Spatial')

print(head(grid100))

## == create column relating to spatial characterization (e.g. distance to road/population) == ##

#examine basic data statistics of variables that will be used for quantile filtering!
quantile(data_32$road_class_3_100)
quantile(data_32$population_1000)

#spatial character: group "urban" - higher than ,5 quantile of population 1000 - within 100m of road class 1 OR 2 OR higher than .5 quantile of road class 3 100.
data_32$spachar = ifelse(data_32$population_1000 > quantile(data_32$population_1000, 0.5) & ((data_32$road_class_2_100 > 0 | data_32$road_class_1_100 > 0) | data_32$road_class_3_100 > quantile(data_32$road_class_3_100, 0.5)), 1, 0)
#spatial character: group "suburban" - lower than ,5 quantile of population 1000 - within 100m of road class 1 OR 2 OR higher than .5 quantile of road class 3 100.
data_32$spachar = ifelse(data_32$population_1000 < quantile(data_32$population_1000, 0.5) & ((data_32$road_class_2_100 > 0 | data_32$road_class_1_100 > 0) | data_32$road_class_3_100 > quantile(data_32$road_class_3_100, 0.5)), 2, data_32$spachar)
#spatial character: group "rural" - all other samples (i.e. further away than 100m from road class 1/2 OR lower than .5 quantile of road class 3 100)
data_32$spachar = ifelse((data_32$spachar == 1 | data_32$spachar == 2), data_32$spachar, 3)

#examine thresholds for each variable-criterium, per spatial group
print(quantile(data_32$population_1000, 0.5))
print(quantile(data_32$road_class_3_100, 0.5))

#assign to variable
population1000_05 = as.vector(quantile(data_32$population_1000, 0.5))
roadclass3_100_05 = as.vector(quantile(data_32$road_class_3_100, 0.5)) #as.vector necessary to only obtain value

# create unique number per spatial group
grid100$spachar = ifelse(grid100$population_1000 > population1000_05 & ((grid100$road_class_2_100 > 0 | grid100$road_class_1_100 > 0) | grid100$road_class_3_100 > roadclass3_100_05), 1, 0) #urban
grid100$spachar = ifelse(grid100$population_1000 < population1000_05 & ((grid100$road_class_2_100 > 0 | grid100$road_class_1_100 > 0) | grid100$road_class_3_100 > roadclass3_100_05), 2, grid100$spachar) #suburban
grid100$spachar = ifelse((grid100$spachar == 1 | grid100$spachar == 2), grid100$spachar, 3) #rural

## == make grids per spatial group == ##

#important is that the cells that do not correspond to the specific spatial group class, will get a "NA" value assigned.

#URBAN - assign predictor information to only 'urban' samples

#create unique key which will be used for later data processing
grid100$key = seq(1,nrow(grid100))
#assign grid100 to apart variable which will be used for data processing purposes - grid100 remains intact
Urb_grid100 <- grid100
#create unique key -this key will later be used to join the processed urban dataset with the initial urban dataset
Urb_grid100$key <- seq(1, nrow(Urb_grid100))
Urb_grid100_key <- Urb_grid100 #assign to variable for further data processing
#get rid of nearly all predictor information - keep "spachar" 
Urb_grid100 <- Urb_grid100[,-(1:13)]
#convert to dataframe for data processing purposes
Urb_grid100_key  <- as.data.frame(Urb_grid100_key )
#if the row does not have the value "1" related to column "spachar", give all other predictors the value NA
Urb_grid100_key[Urb_grid100_key$spachar != 1, 1:12] <- NA
#join all predictors information - only urban samples have predictors values 
Urb_grid100 <- merge(Urb_grid100, Urb_grid100_key, by.x = "key", by.y = "key")

# SUBURBAN - assign predictor information to only 'suburban' samples

#data processing - suburban (create grid whereby cells with suburban characteristics
#only contain predictors values)

#assign grid100 to apart variable which will be used for data processing purposes - grid100 remains intact
Suburban_grid100 <- grid100
#create unique key -this key will later be used to join the processed suburban dataset with the initial suburban dataset
Suburban_grid100$key <- seq(1, nrow(Suburban_grid100))
Suburban_grid100_key <- Suburban_grid100 #assign to variable for further data processing
#retain spatial polygon dataframe, only with key column. Other data will be processed
#and later joined via the common key
Suburban_grid100 <- Suburban_grid100[,-(1:13)]
#convert to dataframe for data processing purposes
Suburban_grid100_key  <- as.data.frame(Suburban_grid100_key )
#if the row does not have the value "2" (low pupulation) related to column "spachar", give all other predictors the value NA
Suburban_grid100_key[Suburban_grid100_key$spachar != 2, 1:12] <- NA
#join all predictors information - only urban samples have predictors values 
Suburban_grid100 <- merge(Suburban_grid100, Suburban_grid100_key, by.x = "key", by.y = "key")


# RURAL - assign predictor information to only 'rural' samples

#data processing - rural (create grid whereby cells with rural characteristics
#only contain values)

#assign grid100 to apart variable which will be used for data processing purposes - grid100 remains intact
rural_grid100 <- grid100
#create unique key -this key will later be used to join the processed rural dataset with the initial rural dataset
rural_grid100$key <- seq(1, nrow(rural_grid100))
rural_grid100_key <- rural_grid100 #assign to variable for further data processing
#retain spatial polygon dataframe, only with key column. Other data will be processed
#and later joined via the common key
rural_grid100 <- rural_grid100[,-(1:13)]
#convert to dataframe for data processing purposes
rural_grid100_key  <- as.data.frame(rural_grid100_key )
#if the row does not have the value "3" (low pupulation) related to column "spachar", give all other predictors the value NA
rural_grid100_key[rural_grid100_key$spachar != 3, 1:12] <- NA
#join all predictors information - only urban samples have predictors values 
rural_grid100 <- merge(rural_grid100, rural_grid100_key, by.x = "key", by.y = "key")


#convert to sf
Urb_grid100_sf <- st_as_sf(Urb_grid100)
Suburban_grid100_sf <- st_as_sf(Suburban_grid100)
rural_grid100_sf <- st_as_sf(rural_grid100)

# export options - spatial distributions
#shp - grid
# sf::st_write(Urb_grid100_sf, dsn=file.path(output_uksep_dir, "Urban_100grid.gpkg"), driver = "GPKG")
# sf::st_write(Suburban_grid100_sf, dsn=file.path(output_uksep_dir, "Suburban_grid100.gpkg"), driver = "GPKG")
# sf::st_write(rural_grid100, dsn=file.path(output_uksep_dir, Rural_grid100.gpkg"), driver = "GPKG")

### ===  kriging per spatial character === ###

data_sp <- data_32
#create different datasets, based on the spatial character - input for models
Urban <- data_sp[data_sp$spachar == 1, ]
Suburban <- data_sp[data_sp$spachar == 2, ]
Rural <- data_sp[data_sp$spachar == 3, ]

## == Urban - Kriging == ##
linear_urb= lm(Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data=Urban)
summary(resid(linear_urb))
#to dataframe
data_linear_urb <- data.frame(x = Urban$coords.x1, y = Urban$coords.x2, resid=resid(linear_urb))

#variogram - automatically
coordinates(data_linear_urb ) = ~x+y
variogram_auto_urban = autofitVariogram(resid ~ 1, data_linear_urb)
plot(variogram_auto_urban)
print(variogram_auto_urban$var_model)
autofit_params_urban <- variogram_auto_urban$var_model #create dataset including variogram parameters

# variogram_auto_urban_1 = variogram(Lopend_gemiddelde ~ 1, Urban, cutoff=1000)
# plot(variogram_auto_urban_1)

# = universal kriging = #

#source: https://gis.stackexchange.com/questions/237672/how-to-achieve-parallel-kriging-in-r-to-speed-up-the-process

#PARALLEL

Urban$Lopend_gemiddelde
# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster (after loading all the necessary object to R environment: meuse, meuse.grid, m)
cl <- makeCluster(no_cores)
parts <- split(x = 1:length(Urb_grid100), f = 1:no_cores)
clusterExport(cl = cl, varlist = c("Urban", "Urb_grid100", "autofit_params_urban", "parts"), envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
system.time(parallelX <- parLapply(cl = cl, X = 1:no_cores, fun = function(x) krige(formula = Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, locations = Urban, newdata = Urb_grid100[parts[[x]],], model = autofit_params_urban)))
stopCluster(cl)

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
mergeParallelX_Urban <- SpatialPixelsDataFrame(points = mergeParallelX, data = mergeParallelX@data)
# Plot mergeParallelX    
spplot(mergeParallelX_Urban["var1.pred"], main = "universal kriging predictions")
raster_uk_Urban <- raster(mergeParallelX_Urban["var1.pred"])
#examine
plot(raster_uk_Urban)
#convert NA to 0's
raster_uk_Urban[is.na(raster_uk_Urban)] <- 0

#export option - to raster
raster::crs(raster_uk_Urban) <- "+proj=utm +zone=32 +datum=NAD83 +units=m +no_defs +ellps=GRS80"
writeRaster(raster_uk_Urban, file.path(output_uksep_dir, 'rasterAmsterdam_UK_Urban.tif'))

# Extract the raster values underlying the polygons
Urb_grid100$ID <- seq(1:nrow(Urb_grid100))
grid_centroids <- gCentroid(Urb_grid100,byid=TRUE)
cen100_UK_values <- raster::extract(raster_uk_Urban, grid_centroids, sp=T)
Urb_grid100_sf <- st_as_sf(Urb_grid100)
cen100_UK_values_sf <- st_as_sf(cen100_UK_values) 
Urb_grid100_UK_values_Urban <- st_join(Urb_grid100_sf, cen100_UK_values_sf, join=st_nearest_feature)
Urb_grid100_UK_values_Urban <- Urb_grid100_UK_values_Urban %>% rename(predictedUK_Urban = "var1.pred")
#convert NA to 0's
Urb_grid100_UK_values_Urban[is.na(Urb_grid100_UK_values_Urban)] <- 0
#sf::st_write(Urb_grid100_UK_values_Urban, dsn=file.path(output_uksep_dir,"Urb_grid100_rasterUK_Amsterdam_Urban.gpkg", driver = "GPKG"))
#write.csv(Urb_grid100_UK_values_Urban, file.path(output_uksep_dir, 'predictedNO2_UK_Urban.csv'))

## == suburban - Kriging == ##

linear_suburban= lm(Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data=Suburban)
summary(resid(linear_suburban))
#to dataframe
data_linear_suburban <- data.frame(x = Suburban$coords.x1, y = Suburban$coords.x2, resid=resid(linear_suburban))
#variogram - automatically
coordinates(data_linear_suburban ) = ~x+y
variogram_auto_suburban = autofitVariogram(resid ~ 1, data_linear_suburban)
plot(variogram_auto_suburban)
print(variogram_auto_suburban$var_model)
autofit_params_suburban <- variogram_auto_suburban$var_model #create dataset including variogram parameters

# = universal kriging = #

#PARALLEL

Suburban$Lopend_gemiddelde
# Calculate the number of cores
no_cores_lp <- detectCores() - 1
# Initiate cluster (after loading all the necessary object to R environment: meuse, meuse.grid, m)
cl <- makeCluster(no_cores_lp)
parts_lp <- split(x = 1:length(Suburban_grid100), f = 1:no_cores_lp)
clusterExport(cl = cl, varlist = c("Suburban", "Suburban_grid100", "autofit_params_suburban", "parts_lp"), envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
system.time(parallelX <- parLapply(cl = cl, X = 1:no_cores_lp, fun = function(x) krige(formula = Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, locations = Suburban, newdata = Suburban_grid100[parts_lp[[x]],], model = autofit_params_suburban)))
stopCluster(cl)

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
mergeParallelX_suburban <- SpatialPixelsDataFrame(points = mergeParallelX, data = mergeParallelX@data)

# Plot mergeParallelX    
spplot(mergeParallelX_suburban["var1.pred"], main = "universal kriging predictions")
raster_uk_suburban <- raster(mergeParallelX_suburban["var1.pred"])
#examine
plot(raster_uk_suburban)
#convert NA to 0's
raster_uk_suburban[is.na(raster_uk_suburban)] <- 0
#export option - to raster
raster::crs(raster_uk_suburban) <- "+proj=utm +zone=32 +datum=NAD83 +units=m +no_defs +ellps=GRS80"
writeRaster(raster_uk_suburban, file.path(output_uksep_dir, 'rasterAmsterdam_UK_suburban.tif'))

# Extract the raster values underlying the polygons
Suburban_grid100$ID <- seq(1:nrow(Suburban_grid100))
grid_centroids <- gCentroid(Suburban_grid100,byid=TRUE)
cen100_UK_values_lp <- raster::extract(raster_uk_suburban, grid_centroids, sp=T)
Suburban_grid100_sf <- st_as_sf(Suburban_grid100)
cen100_UK_values_lp_sf <- st_as_sf(cen100_UK_values_lp) 
Suburban_grid100_UK_values_suburban <- st_join(Suburban_grid100_sf, cen100_UK_values_lp_sf, join=st_nearest_feature)
Suburban_grid100_UK_values_suburban <- Suburban_grid100_UK_values_suburban %>% rename(predictedUK_suburban = "var1.pred")

#convert NA to 0's
Suburban_grid100_UK_values_suburban[is.na(Suburban_grid100_UK_values_suburban)] <- 0
#sf::st_write(Suburban_grid100_UK_values_suburban, dsn=file.path(output_uksep_dir, "Suburban_grid100_rasterUK_Amsterdam_suburban.gpkg", driver = "GPKG"))
#write.csv(Suburban_grid100_UK_values_suburban, file.path(output_uksep_dir, 'predictedNO2_UK_suburban.csv'))

## == rural - Kriging == ##

#specify model
linear_rural= lm(Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data=Rural)
summary(resid(linear_rural))
#to dataframe
data_linear_rural <- data.frame(x = Rural$coords.x1, y = Rural$coords.x2, resid=resid(linear_rural))
#variogram - automatically
coordinates(data_linear_rural ) = ~x+y
variogram_auto_rural = autofitVariogram(resid ~ 1, data_linear_rural)
plot(variogram_auto_rural)
print(variogram_auto_rural$var_model)
autofit_params_rural <- variogram_auto_rural$var_model #create dataset including variogram parameters

# = universal kriging = #

#PARALLEL
# Rural <- Rural %>% select(c("Lopend_gemiddelde","nightlight_450","nightlight_4950","population_3000","road_class_1_5000","road_class_2_1000","road_class_2_5000","road_class_3_100","road_class_3_300","trafBuf50"))
# 
# Rural_sf <- st_as_sf(Rural)
# sf::st_write(Rural_sf, dsn=file.path(output_uksep_dir, "Rural.gpkg", driver = "GPKG"))

Rural$Lopend_gemiddelde
# Calculate the number of cores
no_cores_rural <- detectCores() - 1
# Initiate cluster (after loading all the necessary object to R environment: meuse, meuse.grid, m)
cl <- makeCluster(no_cores_rural)
parts_rural <- split(x = 1:length(rural_grid100), f = 1:no_cores_rural)
clusterExport(cl = cl, varlist = c("Rural", "rural_grid100", "autofit_params_rural", "parts_rural"), envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))
system.time(parallelX <- parLapply(cl = cl, X = 1:no_cores_rural, fun = function(x) krige(formula = Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, locations = Rural, newdata = rural_grid100[parts_rural[[x]],], model = autofit_params_rural)))
stopCluster(cl)

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
mergeParallelX_rural <- SpatialPixelsDataFrame(points = mergeParallelX, data = mergeParallelX@data)
# Plot mergeParallelX    
spplot(mergeParallelX_rural["var1.pred"], main = "universal kriging predictions")
#to raster
raster_uk_rural <- raster(mergeParallelX_rural["var1.pred"])
#examine
plot(raster_uk_rural)
#convert NA to 0's
raster_uk_rural[is.na(raster_uk_rural)] <- 0

#export option - to raster
raster::crs(raster_uk_rural) <- "+proj=utm +zone=32 +datum=NAD83 +units=m +no_defs +ellps=GRS80"
writeRaster(raster_uk_rural, file.path(output_uksep_dir, 'rasterAmsterdam_UK_rural.tif'))

# Extract the raster values underlying the polygons
rural_grid100$ID <- seq(1:nrow(rural_grid100)) #create key for data processing purposes (data will be joined based on this key)
grid_centroids <- gCentroid(rural_grid100,byid=TRUE) #convert to centroids - needed as input for extract (raster) function
cen100_UK_values_rural <- raster::extract(raster_uk_rural, grid_centroids, sp=T) #extract raster prediction patterns, based on rural samples, to 100m grid (centroids)
#make spatial (grid) - needed for spatial join
rural_grid100_sf <- st_as_sf(rural_grid100)
#make spatial (centroids) - needed for spatial join
cen100_UK_values_rural_sf <- st_as_sf(cen100_UK_values_rural) 
#centroids to grid via spatial join
rural_grid100_UK_values_rural <- st_join(rural_grid100_sf, cen100_UK_values_rural_sf, join=st_nearest_feature)
#rename column
rural_grid100_UK_values_rural <- rural_grid100_UK_values_rural %>% rename(predictedUK_rural = "var1.pred")
#convert NA to 0's
rural_grid100_UK_values_rural[is.na(rural_grid100_UK_values_rural)] <- 0

#sf::st_write(rural_grid100_UK_values_rural, dsn=file.path(output_uksep_dir, "Grid100_rasterUK_Amsterdam_rural.gpkg", driver = "GPKG"))

#export option - to csv
#write.csv(rural_grid100_UK_values_rural, file.path(output_uksep_dir, 'predictedNO2_UK_rural.csv'))

## == overlaying kriging outputs per layer == ##

#define the rasters per spatial character
pred_urb = raster(mergeParallelX_Urban['var1.pred']) #urban
pred_suburban = raster(mergeParallelX_suburban['var1.pred']) #low population
pred_rural = raster(mergeParallelX_rural['var1.pred']) #rural

#examine one of the spatial character's rasters
plot(pred_urb)
plot(pred_suburban)
plot(pred_rural)

#overlaying the rasters per spatial character, thereby getting the mean per raster cell (100m)
overlay_mean = mean(pred_urb, pred_suburban, pred_rural, na.rm=TRUE)
#examine
print(overlay_mean)
plot(overlay_mean)

#export option - to raster
raster::crs(overlay_mean) <- "+proj=utm +zone=32 +datum=NAD83 +units=m +no_defs +ellps=GRS80"

writeRaster(overlay_mean, file.path(output_uksep_dir, 'overlay_SpatialGroups1.tif'))

# Extract the raster values underlying the polygons
grid100$ID <- seq(1:nrow(grid100))
grid_centroids <- gCentroid(grid100,byid=TRUE)
cen100_UK_values_overlay <- raster::extract(overlay_mean, grid_centroids, sp=T)
grid100_sf <- st_as_sf(grid100)
cen100_UK_values_overlay_sf <- st_as_sf(cen100_UK_values_overlay) 
cen100_UK_values_overlay_join <- st_join(grid100_sf, cen100_UK_values_overlay_sf, join=st_nearest_feature)
#rename prediction column
cen100_UK_values_overlay_join <- cen100_UK_values_overlay_join %>% rename(predNO2_UKSep = "layer")
#convert NA to 0's
cen100_UK_values_overlay_join[is.na(cen100_UK_values_overlay_join)] <- 0

# = export data
sf::st_write(cen100_UK_values_overlay_join, dsn=file.path(output_uksep_dir, "predictedNO2_UK_SeparatingSpatialGroups.gpkg"), driver = "GPKG")
#export option - to csv
#write.csv(rural_grid100_UK_values_rural, file.path(output_uksep_dir, 'predictedNO2_UK_rural.csv'))
