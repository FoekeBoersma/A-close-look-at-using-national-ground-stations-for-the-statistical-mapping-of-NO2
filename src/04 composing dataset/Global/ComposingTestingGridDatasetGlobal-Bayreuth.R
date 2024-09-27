# Load necessary libraries
library(sf)
library(raster)
library(ggplot2)
library(rgeos)
library(tidyverse)
library(sp)
library(leaflet)
library(terra)
library(stars)
library(dplyr)
library(yaml)
library(rgdal)
library(nngeo)     # For finding nearest features

# Connect to YAML configuration file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir))
config_path <- file.path(config_dir, "config_04.yml")
config <- yaml::yaml.load_file(config_path)

# Define output path
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
out_location_dir <- normalizePath(file.path(parent_directory, config$out_location), winslash = "/")

# IMPORT GEODATA
# Import area of interest at 100m resolution
bayreuth100m_grid_dir <- normalizePath(file.path(parent_directory, config$input_data$bayreuth100m_grid), winslash = "/")
grid <- readOGR(bayreuth100m_grid_dir)

# Data processing
grid_sf <- st_as_sf(grid)  # Convert grid to sf object
grid_3035 <- st_transform(grid_sf, crs = 3035)  # Project to EPSG: 3035
grid_centroids <- gCentroid(grid, byid = TRUE)  # Create centroids for each grid cell
grid_centroids_sf <- st_as_sf(grid_centroids)  # Convert to sf object
grid_centroids_sf$cenID <- seq(1, nrow(grid_centroids_sf))  # Assign unique ID
grid_centroids_df <- as.data.frame(grid_centroids_sf)
grid_centroids_3035 <- st_transform(grid_centroids_sf, crs = 3035)  # Project to planar coordinate system

# PREDICTORS 5 (BAYREUTH) GLOBAL DATASET
# Import all files in a folder as a list
tifs5_dir <- normalizePath(file.path(parent_directory, config$tifs$tifs5_bayreuth), winslash = "/")
rlist <- list.files(path = tifs5_dir, pattern = '.tif$', ignore.case = TRUE, full.names = FALSE)



for (i in rlist) {
  # Construct the full file path
  full_file_path <- file.path(tifs5_dir, i)  # Combine directory and file name

  # Extract the name without extension
  # var_name <- tools::file_path_sans_ext(basename(i))
  var_name <- gsub("^Bayreuth_", "", tools::file_path_sans_ext(basename(i)))
  # Load the raster and assign it to a variable dynamically
  assign(var_name, raster(full_file_path))  # Use the full file path
}

# EXTRACT TO CENTROIDS (Best 10/12 excluding trafBuf100 and bldDen100)
# Initialize lists for predictors
predictors <- list(population_3000, road_class_3_3000, population_1000, nightlight_450,
                   road_class_2_25, nightlight_3150, road_class_3_300, trop_mean_filt)
prednames <- c("population_3000", "road_class_3_3000", "population_1000", "nightlight_450",
               "road_class_2_25", "nightlight_3150", "road_class_3_300", "trop_mean_filt")

# Ensure all data have the same CRS
grid_centroids_sf <- st_transform(grid_centroids_sf, crs = st_crs(predictors[[1]]))

centroid_predictors <- list()
for (i in seq_along(predictors)) {
  # Extract raster values for the centroids
  centroid_pred <- raster::extract(predictors[[i]], grid_centroids_sf, sp = TRUE)
  
  # Convert to dataframe
  centroid_pred_df <- as.data.frame(centroid_pred)
  # Remove the "Bayreuth_" prefix from the column names
  names(centroid_pred_df) <- gsub("^Bayreuth_", "", names(centroid_pred_df))
  centroid_predictors[[prednames[[i]]]] <- centroid_pred_df
}

# Merge all predictors
centroids_5predictors <- centroid_predictors %>% reduce(full_join, by = 'cenID')

# CLEAN DATA 
centroids_5predictors <- centroids_5predictors %>%
  dplyr::select(cenID,
                population_3000, 
                road_class_3_3000,
                population_1000, 
                nightlight_450,
                road_class_2_25, 
                nightlight_3150,
                road_class_3_300, 
                trop_mean_filt)

# BUILDING DENSITY
grid_bldden_100m_path <- file.path(config$input_data$grid_bldden_bayreuth)
dis <- readOGR(grid_bldden_100m_path )
dis_BldDen <- as.data.frame(dis)

## == data processing == ##

mergeBldDen = left_join(grid_centroids_df, dis_BldDen, by = "cenID")
mergeBldDen <- mergeBldDen %>% dplyr::select(cenID, bld_100)

# TRAFFIC DATA
traffic_volume_study_area_dir <- normalizePath(file.path(parent_directory, config$input_data$traffic_volume_study_area), winslash = "/")
traffic <- readOGR(traffic_volume_study_area_dir)
traffic_sf <- st_as_sf(traffic) %>% st_transform(crs = st_crs(grid_centroids_3035))

# Calculate average traffic for each buffer
bufs <- c(25, 50)
buffer_vars <- lapply(bufs, function(i) {
  buf_i <- st_buffer(grid_centroids_3035, i)
  st_as_sf(buf_i)
})

merge_list <- list()
for (i in seq_along(buffer_vars)) {
  clip_traffic <- traffic_sf[buffer_vars[[i]], ]
  inter_traffic <- st_intersection(clip_traffic, buffer_vars[[i]], sp = TRUE)
  inter_traffic$road_length <- st_length(inter_traffic)
  inter_traffic$trafroad <- inter_traffic$road_length * inter_traffic$AvrgHrT
  dis_traffic <- inter_traffic %>% group_by(cenID) %>% summarize(
    TotalLength = sum(road_length),
    AccTraffic = sum(trafroad)
  )
  dis_traffic[[paste0('trafBuf', bufs[i])]] <- dis_traffic$AccTraffic / dis_traffic$TotalLength
  merge_list[[paste0('merge', bufs[i])]] <- left_join(grid_centroids_df, dis_traffic, by = "cenID")
}

traffic_per_buf <- merge_list %>% reduce(full_join, by = 'cenID') %>%
  dplyr::select(cenID, trafBuf25, trafBuf50)

# NDVI EXTRACTION
ndvi_tif_dir <- normalizePath(file.path(parent_directory, config$input_data$ndvi_map), winslash = "/")
ndvi_files <- list.files(ndvi_tif_dir, pattern = '.tif$', full.names = TRUE)
ndvi_stack <- stack(ndvi_files)

# Extract NDVI values for centroids
grid_centroids_sf <- st_transform(grid_centroids_sf, crs = st_crs(ndvi_stack))
points_NDVI <- raster::extract(ndvi_stack, grid_centroids_sf, sp = TRUE)
points_NDVI <- as.data.frame(points_NDVI) %>% rename(NDVI = mod13q1) %>%
  dplyr::select(cenID, NDVI)

# MERGE ALL DATASETS
datasets <- list(traffic_per_buf, mergeBldDen, centroids_5predictors, points_NDVI) %>%
  reduce(full_join, by = 'cenID') %>%
  dplyr::select(cenID, nightlight_450, nightlight_3150, population_1000, population_3000,
                road_class_2_25, road_class_3_3000, road_class_3_300, trop_mean_filt, bld_100, NDVI, trafBuf25, trafBuf50)

# JOIN SPATIAL DATA
# Perform a left join based on cenID
Cen100_GlobalPredictors <- grid_centroids_sf %>%
  left_join(datasets, by = "cenID")
Cen100_GlobalPredictors[is.na(Cen100_GlobalPredictors)] <- 0

# Add Longitude and Latitude
Cen100_GlobalPredictors_wgs <- st_transform(Cen100_GlobalPredictors, crs = 4326) %>%
  mutate(Longitude = unlist(map(geometry, 1)),
         Latitude = unlist(map(geometry, 2)))
 
# EXPORT OPTIONS
sf::st_write(Cen100_GlobalPredictors, dsn = file.path(out_location_dir, config$out_files$bayreuth_100mgrid), driver = "GPKG")
write.csv(Cen100_GlobalPredictors_wgs %>% dplyr::select(-geometry), 
          file.path(out_location_dir, config$out_files$bayreuth_100mgrid_csv))