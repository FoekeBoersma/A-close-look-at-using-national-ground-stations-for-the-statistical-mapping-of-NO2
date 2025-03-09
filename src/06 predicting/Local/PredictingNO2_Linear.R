## == Necessary Packages == ##

# Load required packages using lapply for cleaner syntax
required_packages <- c("tidyverse", "sf", "sp", "raster", "rgdal", "rgeos", "tidyr", 
                       "dismo", "lme4", "stats", "nlme", "parallel", "gstat", 
                       "automap", "yaml")

# Install missing packages if needed and load them
lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
})

## == Connect to YAML Configuration File == ##

# Get the current script's directory
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir))  # Move one level up in directory
config_path <- file.path(config_dir, "config_06.yml")

# Load YAML configuration file
config <- yaml::yaml.load_file(config_path)

# Define parent directory (move four levels up)
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
# Define output directory
out_location_dir <- normalizePath(file.path(parent_directory, config$out_location), winslash = "/")

## == Define Coordinate Systems == ##

# Set CRS to WGS84 and define a metric projection (EPSG:3035 for Europe)
crs_wgs84 <- CRS("+proj=longlat +datum=WGS84")  # WGS84 CRS (long-lat)
crs_3035 <- CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")  # Planar CRS (EPSG:3035)

## == Import Geodata == ##

# Load the dataset and replace missing values with 0
data <- read.csv(config$input_data$local_dataset, sep = ',')
data[is.na(data)] <- 0  # Replace NA with 0

# Convert the CSV data into an sf object and transform to a planar CRS (3035)
data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
data_3035 <- st_transform(data_sf, crs = 3035)

# Import the prediction grid for Amsterdam
grid100 <- st_read(config$input_data$local_predictors_amsterdam)

# Convert the grid into an sf object and add a unique key for joining datasets
grid100_sf <- st_as_sf(grid100)
grid100_sf$key <- seq(1, nrow(grid100_sf))

## == Modelling == ##

# Fit a linear model on the full dataset (132 observations)
model <- lm(Lopend_gemiddelde ~ nightlight_450 + nightlight_4950 + population_3000 +
            road_class_1_5000 + road_class_2_1000 + road_class_2_5000 +
            road_class_3_100 + road_class_3_300 + trafBuf50, data = data_3035)

# Predict NO2 levels on the grid
grid100_sf$predNO2_Lin <- predict(model, grid100_sf)

## == Export Predicted Data == ##

# Export the prediction results to a GeoPackage file
output_file <- file.path(out_location_dir, "predictedNO2_Linear.gpkg")
sf::st_write(grid100_sf, dsn = output_file, driver = "GPKG")

