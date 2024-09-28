## == Necessary Packages == ##

# Load required packages using lapply for cleaner syntax
required_packages <- c("tidyverse", "sf", "sp", "raster", "rgdal", "rgeos", 
                       "tidyr", "dismo", "lme4", "stats", "nlme", 
                       "parallel", "gstat", "automap", "yaml")

# Load or install missing packages
lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
})

## == Connect to YAML Configuration File == ##

# Get the current script's directory
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir))  # One level up in directory
config_path <- file.path(config_dir, "config_06.yml")

# Load the YAML configuration file
config <- yaml::yaml.load_file(config_path)

# Define parent directory (four levels up)
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
# Define output directory
out_location_dir <- normalizePath(file.path(parent_directory, config$out_location), winslash = "/")

## == Define Coordinate Systems == ##

# Define CRS with metric system (EPSG: 3035)
crs_wgs84 <- CRS("+proj=longlat +datum=WGS84")
crs_3035 <- CRS("+init=EPSG:3035")

## == Import Geodata == ##

# Load data and replace missing values with 0
data <- read.csv(config$input_data$local_modeling_dataset, sep = ';')
data[is.na(data)] <- 0

# Convert the CSV data into an sf object (spatial data) and transform to planar CRS
data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
data_3035 <- st_transform(data_sf, crs = 3035)

## == Create Column for Spatial Characterization == ##

# Examine basic data statistics for quantile filtering
quantile(data_3035$road_class_3_100)
quantile(data_3035$population_1000)

# Spatial character: group "urban"
data_3035$spachar <- ifelse(
  data_3035$population_1000 > quantile(data_3035$population_1000, 0.5) & 
    ((data_3035$road_class_2_100 > 0 | data_3035$road_class_1_100 > 0) | 
      data_3035$road_class_3_100 > quantile(data_3035$road_class_3_100, 0.5)), 
  1, 0)

# Spatial character: group "low population"
data_3035$spachar <- ifelse(
  data_3035$population_1000 < quantile(data_3035$population_1000, 0.5) & 
    ((data_3035$road_class_2_100 > 0 | data_3035$road_class_1_100 > 0) | 
      data_3035$road_class_3_100 > quantile(data_3035$road_class_3_100, 0.5)), 
  2, data_3035$spachar)

# Spatial character: group "far from road"
data_3035$spachar <- ifelse(
  data_3035$spachar %in% c(1, 2), 
  data_3035$spachar, 3)

## == Import Grid for Predictions == ##

# Import the grid where predictions will be projected
grid100 <- readOGR(config$input_data$local_predictors_amsterdam)

# Convert the grid to an sf object and create a unique key
grid100_sf <- st_as_sf(grid100)
grid100_sf$key <- seq(1, nrow(grid100_sf))

## == Spatial Characterization of the Grid == ##

# Extract quantiles for population and road class
population1000_05 <- quantile(data_3035$population_1000, 0.5)
roadclass3_100_05 <- quantile(data_3035$road_class_3_100, 0.5)

# Spatial character: group "urban"
grid100_sf$spachar <- ifelse(
  grid100_sf$population_1000 > population1000_05 & 
    ((grid100_sf$road_class_2_100 > 0 | grid100_sf$road_class_1_100 > 0) | 
      grid100_sf$road_class_3_100 > roadclass3_100_05), 
  1, 0)

# Spatial character: group "low population"
grid100_sf$spachar <- ifelse(
  grid100_sf$population_1000 < population1000_05 & 
    ((grid100_sf$road_class_2_100 > 0 | grid100_sf$road_class_1_100 > 0) | 
      grid100_sf$road_class_3_100 < roadclass3_100_05), 
  2, grid100_sf$spachar)

# Spatial character: group "far from road"
grid100_sf$spachar <- ifelse(
  grid100_sf$spachar %in% c(1, 2), 
  grid100_sf$spachar, 3)

## == Modelling == ##

# Fit a mixed-effects model on the training data (132 observations)
model <- lmer(
  Lopend_gemiddelde ~ nightlight_450 + nightlight_4950 + population_3000 +
  road_class_1_5000 + road_class_2_1000 + road_class_2_5000 +
  road_class_3_100 + road_class_3_300 + trafBuf50 + (1 | spachar), 
  data = data_3035)

# Predict NO2 levels on the grid
grid100_sf$predNO2_MEM <- predict(model, grid100_sf)

## == Export Option == ##

# Export the prediction results to a GeoPackage file
output_file <- file.path(out_location_dir, "predictedNO2_MEM.gpkg")
sf::st_write(grid100_sf, dsn = output_file, driver = "GPKG")
