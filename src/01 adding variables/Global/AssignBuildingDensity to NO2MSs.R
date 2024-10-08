require(rgdal)
library(raster)
require(sf)
library(ggplot2)
library(raster)
library(rgeos)
library(tidyverse)
library(sp) # spatial operations
library(leaflet) # mapping in OSM
library(terra) # rasterizeg
library(stars) # necessary for st_rasterize
library(yaml)
library(rstudioapi)

## == import building density dataset == ##

#connect to yaml file
current_dir <- rstudioapi::getActiveDocumentContext()$path
# Move one level up in the directory
config_dir <- dirname(dirname(current_dir))
# Construct the path to the YAML configuration file
config_path <- file.path(config_dir, "config.yml")
# Read the YAML configuration file
config <- yaml.load_file(config_path)

# Use dirname() to get the parent directory
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))

# location of shapefile buildingsNL
buildingsNL_relative <- config$global$buildings
# Construct the full path to the NDVI map directory based on the config file location
buildings_map_dir <- normalizePath(file.path(parent_directory, buildingsNL_relative), winslash = "/")

## == import no2 dataset == ##
no2_dataset <- config$global$no2
no2_map_dir <- normalizePath(file.path(parent_directory, no2_dataset ), winslash = "/")
# create spatial dataframe from no2 measurement stations dataset
NO2_stations_sf <- st_as_sf(NO2_stations, coords = c("Longitude", "Latitude"))
# assign ooordinate reference system (crs)
st_crs(NO2_stations_sf) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
NO2_stations_sf$M_id <- seq(1, 482) # assign unique identifier which will be used for coupling data later in the process
NO2_stations_sf <- NO2_stations_sf %>% dplyr::select(FID, geometry, M_id) # filter columns

## == define output path == ##
out_location <- config$out_location
out_location_dir <- normalizePath(file.path(parent_directory, out_location ), winslash = "/")

## == CREATE BUFFERS AROUND NO2 MEASUREMENT STATIONS == ##
##  First project data into a planar coordinate system
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
NO2_stations_utm <- st_transform(NO2_stations_sf, crs = 3035)

# create different buffers

# initialize buffer parameters
bufs <- list(100, 500, 1000)
# create for loop, thereby creating a different buffer per iteration.
buffer_vars <- list()
# loop
for (i in bufs) {
  buf_i <- st_buffer(NO2_stations_utm, i)
  assign(paste("buf", i, "m", sep = ""), buf_i)

  # assign coordinate sustem
  buf_i <- st_as_sf(buf_i)
  buf_i$area <- st_area(buf_i)
  # store variable in loop
  buffer_vars[[paste("buf", i, "m", sep = "")]] <- buf_i
  # export option
  layer <- paste("buf", i, "m", sep = "")
  print(layer)

  # sf::st_write(buf_i, dsn=out_location,layer=layer, driver = "ESRI Shapefile")
}

# examine list where variables of previous loop were stored to.
summary(buffer_vars)

# transform crs of building dataset to that of buffers to perform spatial calculations
buildings_data <- st_read(buildings_map_dir)
buildings_sf <- st_as_sf(buildings_data)

# initialize parameters used in for loop
j <- 1
i <- 1
merge_list <- list()
bldden_ms_list <- list()
measurement_stations <- as.data.frame(NO2_stations_utm)

# while loop consists of several spatial operations:
#' clip' for just selecting buildings in buffers
#' intersect' for creating ID for features in corresponding buffer
#' dissolve' to merge features into one, based on common ID (M_id)
#' merge' for calculating building density per buffer

while (j <= length(buffer_vars)) {
  # set to same crs to perform spatial operations
  buffer_vars[[j]] <- st_transform(buffer_vars[[j]], crs = st_crs(buildings_sf ))
  # clip - building dataset will be assigned to buffers
  clip <- buildings_sf [buffer_vars[[j]], ]
  layer_clip <- paste0("clip", bufs[[i]], ".shp")
  print(layer_clip)
  # make clip spatial to export as shapefile
  clip <- st_as_sf(clip)
  # export option
  # sf::st_write(clip, dsn=out_location, layer=layer_clip, driver = "ESRI Shapefile")

  ## == SELECT ROADS WITHIN BUFFER X == ##
  intersect_j <- st_intersection(clip, buffer_vars[[j]], sp = TRUE)
  # compute area (m2) for each polygon in dataset
  intersect_j$area <- st_area(intersect_j)
  layer_intersect <- paste0("intersect", bufs[[i]], ".shp")
  print(layer_intersect)
  # sf::st_write(intersect_j, dsn=out_location, layer=layer_intersect, driver = "ESRI Shapefile")

  # dissolve by common ID - polygons with similar buffer ID will be merged
  # the area of these polygons will be aggregated via "SUM"
  dissolve <- intersect_j %>%
    group_by(M_id) %>%
    summarize(BuiltArea = sum(area))
  layer_dissolve <- paste0("dissolve", bufs[[i]], ".shp")
  # export option
  # sf::st_write(dissolve, dsn=out_location, layer=layer_dissolve, driver = "ESRI Shapefile")

  # merge datasets via left_join
  # make normal dataframe to perform left_join
  bufdata <- as.data.frame(buffer_vars[[j]])
  # assign info based on common M_id
  merge <- left_join(bufdata, dissolve, by = "M_id")

  # create new column "building_density" dividing building surface per buffer by
  # total buffer surface
  merge[[paste("BldDen", bufs[[i]], sep = "")]] <- merge$BuiltArea / merge$area
  # drop irrelevant columns
  merge <- subset(merge, select = -c(FID, geometry.y))
  # replace NA by 0
  merge[is.na(merge)] <- 0
  # store variable in loop
  merge_list[[paste0("merge", bufs[[i]])]] <- merge
  # assign to variable which can be used as input argument for data export
  layer_mer <- paste0("merge", bufs[[i]], ".shp")
  # export option
  # sf::st_write(merge, dsn=out_location, layer=layer_mer, driver = "ESRI Shapefile")

  # next buffer dataset
  j <- j + 1
  i <- i + 1
}

## == merge all data frames together and assign to measurement stations == ##

# make dataframe spatial
measurement_stations <- st_as_sf(measurement_stations)
measurement_stations <- st_transform(measurement_stations, crs = 3035)

# join datasets in list 'merge_list' with each other, based on common M_id
bldden_per_buf <- merge_list %>% reduce(full_join, by = "M_id")
# assign to measurement stations, again via common M_id
ms_bldden_per_buf <- left_join(measurement_stations, bldden_per_buf, by = "M_id")

# deselect irrelevant columns
ms_bldden_per_buf <- ms_bldden_per_buf %>% dplyr::select(M_id, BldDen100, BldDen500, BldDen1000)

print(ms_bldden_per_buf )

# export option
sf::st_write(ms_bldden_per_buf, dsn = out_location_dir, layer = "BldDen_ms_StudyArea_Global", driver = "ESRI Shapefile")
