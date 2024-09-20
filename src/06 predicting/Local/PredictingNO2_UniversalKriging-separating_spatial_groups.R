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

#CRS with metric system is preferred (=3035).
crs <- CRS("+proj=longlat +datum=WGS84") # crs

## == import geodata == ##

data <- read.csv('/data/LocalModelData/ModellingDataset-Local.csv', sep=';')
#replace NA with 0
data[is.na(data)] <- 0

data = data %>% rename('Lopend_gemiddelde' = 'mean_value_NO2')

#convert to spatial points dataframe (gstat relies on sp package more than sf)
#first convert to sf
#to sf
data_sf = st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
#change to planar crs
data_3035 <- st_transform(data_sf,crs=3035)
#to SpatialPointsDataFrame
data_3035 <- as(data_3035, 'Spatial')


# define grid for projection 
# import AOI
grid100 = readOGR('/TooBigData/Grid100_LocalPredictors_Amsterdam.gpkg')

#convert to sf to use function "rename"
grid100 <- st_as_sf(grid100)

#convert grid back to spatialPolygonsDataframe
grid100 <- as(grid100, 'Spatial')

## == create column relating to spatial characterization (e.g. distance to road/population) == ##

#examine basic data statistics of variables that will be used for quantile filtering!
quantile(data_3035$road_class_3_100)
quantile(data_3035$population_1000)

#spatial character: group "urban" - higher than ,5 quantile of population 1000 - within 100m of road class 1 OR 2 OR higher than .5 quantile of road class 3 100.
data_3035$spachar = ifelse(data_3035$population_1000 > quantile(data_3035$population_1000, 0.5) & ((data_3035$road_class_2_100 > 0 | data_3035$road_class_1_100 > 0) | data_3035$road_class_3_100 > quantile(data_3035$road_class_3_100, 0.5)), 1, 0)

#spatial character: group "low population" - lower than ,5 quantile of population 1000 - within 100m of road class 1 OR 2 OR higher than .5 quantile of road class 3 100.
data_3035$spachar = ifelse(data_3035$population_1000 < quantile(data_3035$population_1000, 0.5) & ((data_3035$road_class_2_100 > 0 | data_3035$road_class_1_100 > 0) | data_3035$road_class_3_100 > quantile(data_3035$road_class_3_100, 0.5)), 2, data_3035$spachar)

#spatial character: group "far from road" - all other samples (i.e. further away than 100m from road class 1/2 OR lower than .5 quantile of road class 3 100)
data_3035$spachar = ifelse((data_3035$spachar == 1 | data_3035$spachar == 2), data_3035$spachar, 3)

#examine thresholds for each variable-criterium, per spatial group
print(quantile(data_3035$population_1000, 0.5))
print(quantile(data_3035$road_class_3_100, 0.5))

#assign to variable
population1000_05 = as.vector(quantile(data_3035$population_1000, 0.5))
roadclass3_100_05 = as.vector(quantile(data_3035$road_class_3_100, 0.5)) #as.vector necessary to only obtain value


grid100$spachar = ifelse(grid100$population_1000 > population1000_05 & ((grid100$road_class_2_100 > 0 | grid100$road_class_1_100 > 0) | grid100$road_class_3_100 > roadclass3_100_05), 1, 0)

grid100$spachar = ifelse(grid100$population_1000 < population1000_05 & ((grid100$road_class_2_100 > 0 | grid100$road_class_1_100 > 0) | grid100$road_class_3_100 > roadclass3_100_05), 2, grid100$spachar)

grid100$spachar = ifelse((grid100$spachar == 1 | grid100$spachar == 2), grid100$spachar, 3)


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

# LOW POPULATION - assign predictor information to only 'low population' samples

#data processing - low population (create grid whereby cells with low population characteristics
#only contain predictors values)

#assign grid100 to apart variable which will be used for data processing purposes - grid100 remains intact
Lowpop_grid100 <- grid100
#create unique key -this key will later be used to join the processed low population dataset with the initial low population dataset
Lowpop_grid100$key <- seq(1, nrow(Lowpop_grid100))
Lowpop_grid100_key <- Lowpop_grid100 #assign to variable for further data processing
#retain spatial polygon dataframe, only with key column. Other data will be processed
#and later joined via the common key
Lowpop_grid100 <- Lowpop_grid100[,-(1:13)]
#convert to dataframe for data processing purposes
Lowpop_grid100_key  <- as.data.frame(Lowpop_grid100_key )
#if the row does not have the value "2" (low pupulation) related to column "spachar", give all other predictors the value NA
Lowpop_grid100_key[Lowpop_grid100_key$spachar != 2, 1:12] <- NA
#join all predictors information - only urban samples have predictors values 
Lowpop_grid100 <- merge(Lowpop_grid100, Lowpop_grid100_key, by.x = "key", by.y = "key")


# FAR FROM ROAD - assign predictor information to only 'far from road' samples

#data processing - far from road (create grid whereby cells with far from road characteristics
#only contain values)

#assign grid100 to apart variable which will be used for data processing purposes - grid100 remains intact
FFR_grid100 <- grid100
#create unique key -this key will later be used to join the processed far from road dataset with the initial far from road dataset
FFR_grid100$key <- seq(1, nrow(FFR_grid100))
FFR_grid100_key <- FFR_grid100 #assign to variable for further data processing
#retain spatial polygon dataframe, only with key column. Other data will be processed
#and later joined via the common key
FFR_grid100 <- FFR_grid100[,-(1:13)]
#convert to dataframe for data processing purposes
FFR_grid100_key  <- as.data.frame(FFR_grid100_key )
#if the row does not have the value "3" (low pupulation) related to column "spachar", give all other predictors the value NA
FFR_grid100_key[FFR_grid100_key$spachar != 3, 1:12] <- NA
#join all predictors information - only urban samples have predictors values 
FFR_grid100 <- merge(FFR_grid100, FFR_grid100_key, by.x = "key", by.y = "key")


#convert to sf
Urb_grid100_sf <- st_as_sf(Urb_grid100)
Lowpop_grid100_sf <- st_as_sf(Lowpop_grid100)
FFR_grid100_sf <- st_as_sf(FFR_grid100)

# export options - spatial distributions
#shp - grid
# sf::st_write(Urb_grid100_sf, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Amsterdam/Urban_100grid.gpkg", driver = "GPKG")
# sf::st_write(Lowpop_grid100_sf, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Amsterdam/Lowpop_grid100.gpkg", driver = "GPKG")
# sf::st_write(FFR_grid100, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/Grid100/Amsterdam/FFR_grid100.gpkg", driver = "GPKG")

### ===  kriging per spatial character === ###

data_sp <- data_3035

#create different datasets, based on the spatial character - input for models
Urban <- data_sp[data_sp$spachar == 1, ]
Lowpopulation <- data_sp[data_sp$spachar == 2, ]
FarFromRoad <- data_sp[data_sp$spachar == 3, ]


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

# 
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
#writeRaster(raster_uk_Urban, paste0('/LocalModels/PredictedNO2ByKriging/rasterAmsterdam_UK_Urban.tif'))

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

#sf::st_write(Urb_grid100_UK_values_Urban, dsn="/LocalModels/Urb_grid100_rasterUK_Amsterdam_Urban.gpkg", driver = "GPKG")

#export option - to csv
#write.csv(Urb_grid100_UK_values_Urban, '/LocalModels/PredictedNO2ByKriging/predictedNO2_UK_Urban.csv')


## == Low Population - Kriging == ##


linear_lowpop= lm(Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data=Lowpopulation)
summary(resid(linear_lowpop))

#to dataframe

data_linear_lowpop <- data.frame(x = Lowpopulation$coords.x1, y = Lowpopulation$coords.x2, resid=resid(linear_lowpop))


#variogram - automatically
coordinates(data_linear_lowpop ) = ~x+y
variogram_auto_lowpop = autofitVariogram(resid ~ 1, data_linear_lowpop)
plot(variogram_auto_lowpop)
print(variogram_auto_lowpop$var_model)
autofit_params_lowpop <- variogram_auto_lowpop$var_model #create dataset including variogram parameters



# = universal kriging = #

#PARALLEL

Lowpopulation$Lopend_gemiddelde

# Calculate the number of cores
no_cores_lp <- detectCores() - 1

# Initiate cluster (after loading all the necessary object to R environment: meuse, meuse.grid, m)
cl <- makeCluster(no_cores_lp)

parts_lp <- split(x = 1:length(Lowpop_grid100), f = 1:no_cores_lp)

clusterExport(cl = cl, varlist = c("Lowpopulation", "Lowpop_grid100", "autofit_params_lowpop", "parts_lp"), envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))

system.time(parallelX <- parLapply(cl = cl, X = 1:no_cores_lp, fun = function(x) krige(formula = Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, locations = Lowpopulation, newdata = Lowpop_grid100[parts_lp[[x]],], model = autofit_params_lowpop)))

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
mergeParallelX_Lowpop <- SpatialPixelsDataFrame(points = mergeParallelX, data = mergeParallelX@data)

# Plot mergeParallelX    
spplot(mergeParallelX_Lowpop["var1.pred"], main = "universal kriging predictions")

raster_uk_Lowpop <- raster(mergeParallelX_Lowpop["var1.pred"])
#examine
plot(raster_uk_Lowpop)


#convert NA to 0's
raster_uk_Lowpop[is.na(raster_uk_Lowpop)] <- 0



#export option - to raster
#writeRaster(raster_uk_Lowpop, paste0('/LocalModels/PredictedNO2ByKriging/rasterAmsterdam_UK_Lowpop.tif'))

# Extract the raster values underlying the polygons

Lowpop_grid100$ID <- seq(1:nrow(Lowpop_grid100))
grid_centroids <- gCentroid(Lowpop_grid100,byid=TRUE)

cen100_UK_values_lp <- raster::extract(raster_uk_Lowpop, grid_centroids, sp=T)

Lowpop_grid100_sf <- st_as_sf(Lowpop_grid100)
cen100_UK_values_lp_sf <- st_as_sf(cen100_UK_values_lp) 
Lowpop_grid100_UK_values_Lowpop <- st_join(Lowpop_grid100_sf, cen100_UK_values_lp_sf, join=st_nearest_feature)


Lowpop_grid100_UK_values_Lowpop <- Lowpop_grid100_UK_values_Lowpop %>% rename(predictedUK_Lowpop = "var1.pred")

#convert NA to 0's
Lowpop_grid100_UK_values_Lowpop[is.na(Lowpop_grid100_UK_values_Lowpop)] <- 0

#sf::st_write(Lowpop_grid100_UK_values_Lowpop, dsn="/LocalModels/Lowpop_grid100_rasterUK_Amsterdam_Lowpop.gpkg", driver = "GPKG")

#export option - to csv
#write.csv(Lowpop_grid100_UK_values_Lowpop, '/LocalModels/PredictedNO2ByKriging/predictedNO2_UK_Lowpop.csv')





## == Far From Road - Kriging == ##


#specify model
linear_ffr= lm(Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, data=FarFromRoad)
summary(resid(linear_ffr))

#to dataframe
data_linear_ffr <- data.frame(x = FarFromRoad$coords.x1, y = FarFromRoad$coords.x2, resid=resid(linear_ffr))

#variogram - automatically
coordinates(data_linear_ffr ) = ~x+y
variogram_auto_ffr = autofitVariogram(resid ~ 1, data_linear_ffr)
plot(variogram_auto_ffr)
print(variogram_auto_ffr$var_model)
autofit_params_ffr <- variogram_auto_ffr$var_model #create dataset including variogram parameters






# = universal kriging = #

#PARALLEL


FarFromRoad <- FarFromRoad %>% select(c("Lopend_gemiddelde","nightlight_450","nightlight_4950","population_3000","road_class_1_5000","road_class_2_1000","road_class_2_5000","road_class_3_100","road_class_3_300","trafBuf50"))
# 
# FarFromRoad_sf <- st_as_sf(FarFromRoad)
# sf::st_write(FarFromRoad_sf, dsn="/LocalModels/FarFromRoad.gpkg", driver = "GPKG")

FarFromRoad$Lopend_gemiddelde

# Calculate the number of cores
no_cores_ffr <- detectCores() - 1

# Initiate cluster (after loading all the necessary object to R environment: meuse, meuse.grid, m)
cl <- makeCluster(no_cores_ffr)

parts_ffr <- split(x = 1:length(FFR_grid100), f = 1:no_cores_ffr)

clusterExport(cl = cl, varlist = c("FarFromRoad", "FFR_grid100", "autofit_params_ffr", "parts_ffr"), envir = .GlobalEnv)
clusterEvalQ(cl = cl, expr = c(library('sp'), library('gstat')))

system.time(parallelX <- parLapply(cl = cl, X = 1:no_cores_ffr, fun = function(x) krige(formula = Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 + road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + road_class_3_100 + road_class_3_300 + trafBuf50, locations = FarFromRoad, newdata = FFR_grid100[parts_ffr[[x]],], model = autofit_params_ffr)))


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
mergeParallelX_FFR <- SpatialPixelsDataFrame(points = mergeParallelX, data = mergeParallelX@data)
# Plot mergeParallelX    
spplot(mergeParallelX_FFR["var1.pred"], main = "universal kriging predictions")
#to raster
raster_uk_FFR <- raster(mergeParallelX_FFR["var1.pred"])
#examine
plot(raster_uk_FFR)
#convert NA to 0's
raster_uk_FFR[is.na(raster_uk_FFR)] <- 0


#export option - to raster
#writeRaster(raster_uk_FFR, paste0('/LocalModels/PredictedNO2ByKriging/rasterAmsterdam_UK_FFR.tif'))

# Extract the raster values underlying the polygons

FFR_grid100$ID <- seq(1:nrow(FFR_grid100)) #create key for data processing purposes (data will be joined based on this key)
grid_centroids <- gCentroid(FFR_grid100,byid=TRUE) #convert to centroids - needed as input for extract (raster) function

cen100_UK_values_ffr <- raster::extract(raster_uk_FFR, grid_centroids, sp=T) #extract raster prediction patterns, based on ffr samples, to 100m grid (centroids)
#make spatial (grid) - needed for spatial join
FFR_grid100_sf <- st_as_sf(FFR_grid100)
#make spatial (centroids) - needed for spatial join
cen100_UK_values_ffr_sf <- st_as_sf(cen100_UK_values_ffr) 
#centroids to grid via spatial join
FFR_grid100_UK_values_FFR <- st_join(FFR_grid100_sf, cen100_UK_values_ffr_sf, join=st_nearest_feature)
#rename column
FFR_grid100_UK_values_FFR <- FFR_grid100_UK_values_FFR %>% rename(predictedUK_FFR = "var1.pred")
#convert NA to 0's
FFR_grid100_UK_values_FFR[is.na(FFR_grid100_UK_values_FFR)] <- 0

#sf::st_write(FFR_grid100_UK_values_FFR, dsn="/LocalModels/Grid100_rasterUK_Amsterdam_FFR.gpkg", driver = "GPKG")

#export option - to csv
#write.csv(FFR_grid100_UK_values_FFR, '/LocalModels/PredictedNO2ByKriging/predictedNO2_UK_FFR.csv')




## == overlaying kriging outputs per layer == ##

#define the rasters per spatial character
pred_urb = raster(mergeParallelX_Urban['var1.pred']) #urban
pred_lowpop = raster(mergeParallelX_Lowpop['var1.pred']) #low population
pred_ffr = raster(mergeParallelX_FFR['var1.pred']) #far from road

#examine one of the spatial character's rasters
plot(pred_urb)
plot(pred_lowpop)
plot(pred_ffr)

#overlaying the rasters per spatial character, thereby getting the mean per raster cell (100m)
overlay_mean = mean(pred_urb, pred_lowpop, pred_ffr, na.rm=TRUE)
#examine
print(overlay_mean)
plot(overlay_mean)

#export option - to raster
#writeRaster(overlay_mean, paste0('/TooBigData/LocalModels/test/overlay_SpatialGroups.tif'))



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

sf::st_write(cen100_UK_values_overlay_join, dsn="/TooBigData/LocalModels/test_3035/predictedNO2_UK_SeparatingSpatialGroups.gpkg", driver = "GPKG")

#export option - to csv
#write.csv(FFR_grid100_UK_values_FFR, '/LocalModels/PredictedNO2ByKriging/predictedNO2_UK_FFR.csv')






