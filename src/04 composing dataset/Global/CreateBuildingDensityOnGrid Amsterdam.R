# Load necessary libraries
library(sf)
library(raster)
library(ggplot2) 
library(tidyverse)
library(sp)
library(terra)
library(dplyr)
library(yaml)
library(nngeo)     # For finding nearest features

# Connect to YAML configuration file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir))
config_path <- file.path(config_dir, "config_04.yml")
config <- yaml::yaml.load_file(config_path)

# Define output path
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
out_location_dir <- normalizePath(file.path(parent_directory, config$out_location), winslash = "/")

## == import geodata == ## 
# Import area of interest at 100m resolution
amsterdam100m_grid_dir <- normalizePath(file.path(parent_directory, config$input_data$amsterdam100m_grid), winslash = "/")
grid <- st_read(amsterdam100m_grid_dir)

## ==  Data processing == ##
grid_sf <- st_as_sf(grid)  # Convert grid to sf object
grid_3035 <- st_transform(grid_sf, crs = 3035)  # Project to EPSG: 3035 (useful for geoprocessing)
grid_centroids_sf <- st_centroid(grid_sf)
grid_centroids_sf$cenID <- seq(1, nrow(grid_centroids_sf))  # Assign unique ID
grid_centroids_df <- as.data.frame(grid_centroids_sf)
grid_centroids_3035 <- st_transform(grid_centroids_sf, crs = 3035)  # Project to planar coordinate system

# PREDICTORS 5 (amsterdam) GLOBAL DATASET
# Import all files in a folder as a list
tifs5_dir <- normalizePath(file.path(parent_directory, config$tifs$tifs5_amsterdam), winslash = "/")
rlist <- list.files(path = tifs5_dir, pattern = '.tif$', ignore.case = TRUE, full.names = FALSE)

for (i in rlist) {
  # Construct the full file path
  full_file_path <- file.path(tifs5_dir, i)  # Combine directory and file name

  # Extract the name without extension
  var_name <- gsub("^Amsterdam_", "", tools::file_path_sans_ext(basename(i)))
  # Load the raster and assign it to a variable dynamically
  assign(var_name, raster(full_file_path))  # Use the full file path
}

## ==  EXTRACT TO CENTROIDS (Best 10/12 excluding trafBuf100 and bldDen100) == ##
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
  # Remove the "amsterdam_" prefix from the column names
  names(centroid_pred_df) <- gsub("^Amsterdam_", "", names(centroid_pred_df))
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

## == BUILDING DENSITY (SPATIAL JOIN) == ##
grid_bldden_100m_path <- file.path(config$input_data$grid_bldden_amsterdam)
dis <- st_read(grid_bldden_100m_path)

# Ensure both layers have the same CRS
dis <- st_transform(dis, crs = st_crs(grid_centroids_sf))

# Perform spatial join using the nearest feature (st_nearest_feature)
basic_join_result <- st_join(grid_centroids_sf, dis, join = st_nearest_feature)

print(head(basic_join_result))

# Clean up the result by keeping only necessary columns and renaming them
mergeBldDen <- basic_join_result %>%
  dplyr::select(cenID.x, bld_100) %>%  # Keep cenID.x and bld_100 columns
  rename(cenID = cenID.x, BldDen100 = bld_100)  # Rename cenID.x to cenID and bld_100 to bldDen100

mergeBldDen <- as.data.frame((mergeBldDen))
mergeBldDen <- subset(mergeBldDen, select = -c(geometry))

print(head(mergeBldDen))
## == TRAFFIC DATA == ##
traffic_volume_study_area_dir <- normalizePath(file.path(parent_directory, config$input_data$traffic_volume_study_area), winslash = "/")
traffic <- st_read(traffic_volume_study_area_dir)
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

plot(st_geometry(grid_centroids_3035), col = 'blue', pch = 20)
plot(st_geometry(traffic_sf), col = 'red', pch = 20, add = TRUE)

## == NDVI EXTRACTION == ##
ndvi_tif_dir <- normalizePath(file.path(parent_directory, config$input_data$ndvi_map), winslash = "/")
ndvi_files <- list.files(ndvi_tif_dir, pattern = '.tif$', full.names = TRUE)
ndvi_stack <- stack(ndvi_files)

# Extract NDVI values for centroids
grid_centroids_sf <- st_transform(grid_centroids_sf, crs = st_crs(ndvi_stack))
points_NDVI <- raster::extract(ndvi_stack, grid_centroids_sf, sp = TRUE)
points_NDVI <- as.data.frame(points_NDVI) %>% rename(NDVI = mod13q1) %>%
  dplyr::select(cenID, NDVI)

## == MERGE ALL DATASETS == ##
datasets <- list(traffic_per_buf, mergeBldDen, centroids_5predictors, points_NDVI) %>% 
  reduce(full_join, by = 'cenID') %>%
  dplyr::select(cenID, nightlight_450, nightlight_3150, population_1000, population_3000,
                road_class_2_25, road_class_3_3000, road_class_3_300, trop_mean_filt, 
                BldDen100, NDVI, trafBuf25, trafBuf50)

# JOIN SPATIAL DATA
# Perform a left join based on cenID
Cen100_GlobalPredictors <- grid_centroids_sf %>%
  left_join(datasets, by = "cenID")
Cen100_GlobalPredictors[is.na(Cen100_GlobalPredictors)] <- 0

# Add Longitude and Latitude
Cen100_GlobalPredictors_wgs <- st_transform(Cen100_GlobalPredictors, crs = 4326) %>%
  mutate(coords = st_coordinates(.)) %>%
  mutate(Longitude = coords[, 1], Latitude = coords[, 2]) %>%
  dplyr::select(-coords)  # Remove intermediate column

print(colnames(Cen100_GlobalPredictors))
print(head(Cen100_GlobalPredictors_wgs))
Cen100_GlobalPredictors <- Cen100_GlobalPredictors[, !names(Cen100_GlobalPredictors) %in% "fid"]
# EXPORT OPTIONS
# optional: point feature dataset (representing the centroids of 100m x100m grid cells)
sf::st_write(Cen100_GlobalPredictors, dsn = file.path(out_location_dir, "Cen100_GlobalPredictors_amsterdam.gpkg"), driver = "GPKG", overwrite=TRUE)

## == export to grid that will be used for assigning model predictions == ##
print(colnames(Cen100_GlobalPredictors))
print(head(Cen100_GlobalPredictors))
# Ensure both datasets are in the same CRS
Cen100_GlobalPredictors <- st_transform(Cen100_GlobalPredictors, crs = st_crs(grid_sf))

# Perform spatial join, keeping only the attributes from Cen100_GlobalPredictors
Grid100_GlobalPredictors_amsterdam <- st_join(grid_sf, Cen100_GlobalPredictors, left = FALSE)
Grid100_GlobalPredictors_amsterdam <- Grid100_GlobalPredictors_amsterdam %>% select(-fid)

# Save the output as a GeoPackage
st_write(Grid100_GlobalPredictors_amsterdam, 
         dsn = file.path(out_location_dir, "Grid100_GlobalPredictors-amsterdam1.gpkg"), 
         driver = "GPKG", 
         overwrite = TRUE)

# export to csv

# Transform Cen100_GlobalPredictors to WGS84 (EPSG:4326) to ensure correct notation
Cen100_GlobalPredictors_wgs <- st_transform(Cen100_GlobalPredictors, crs = 4326)

# Extract Longitude and Latitude
Cen100_GlobalPredictors_wgs <- Cen100_GlobalPredictors_wgs %>%
  mutate(coords = st_coordinates(.)) %>%
  mutate(Longitude = coords[, 1], Latitude = coords[, 2]) %>%
  dplyr::select(-coords)  # Remove intermediate column


Grid100_GlobalPredictors_amsterdam <- as.data.frame(Cen100_GlobalPredictors_wgs)
# If 'geometry' column is still present, remove it before writing to CSV
if ("geometry" %in% names(Grid100_GlobalPredictors_amsterdam)) {
  Grid100_GlobalPredictors_amsterdam <- dplyr::select(Grid100_GlobalPredictors_amsterdam, -geometry)
}

# Remove the 'cenID' column
Grid100_GlobalPredictors_amsterdam <- dplyr::select(Grid100_GlobalPredictors_amsterdam, -cenID)

# Check if Longitude and Latitude are missing, if so, extract from spatial object again
if(!"Longitude" %in% colnames(Grid100_GlobalPredictors_amsterdam) | !"Latitude" %in% colnames(Grid100_GlobalPredictors_amsterdam)) {
  # Add back Longitude and Latitude from the spatial object
  Grid100_GlobalPredictors_amsterdam$Longitude <- st_coordinates(Cen100_GlobalPredictors)[,1]
  Grid100_GlobalPredictors_amsterdam$Latitude <- st_coordinates(Cen100_GlobalPredictors)[,2]
}

# Grid100_GlobalPredictors_amsterdam <- Grid100_GlobalPredictors_amsterdam %>%
#   rename(BldDen100 = bld_100)
print(colnames(Grid100_GlobalPredictors_amsterdam))

# Write to CSV
write.csv(Grid100_GlobalPredictors_amsterdam, 
          file.path(out_location_dir, "grid100_GlobalPredictors-amsterdam.csv"), 
          row.names = FALSE)
