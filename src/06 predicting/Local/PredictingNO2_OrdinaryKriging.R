## == Necessary Packages == ##

# Define required packages
required_packages <- c("tidyverse", "sf", "sp", "raster", "rgdal", "rgeos", 
                       "tidyr", "dismo", "lme4", "stats", "nlme", 
                       "parallel", "gstat", "automap", "yaml")

# Load or install missing packages using lapply
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

# Load the YAML configuration file
config <- yaml::yaml.load_file(config_path)

# Define parent directory (four levels up)
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
# Define output directory
out_location_dir <- normalizePath(file.path(parent_directory, config$out_location), winslash = "/")

## == Define Coordinate Systems == ##

# Define CRS with metric system (ETRS89 / ETRS-LAEA = EPSG:3035)
crs <- CRS("+proj=longlat +datum=WGS84")  # For initial use

## == Import Geodata == ##

# Load data from CSV file specified in the YAML configuration
data <- read.csv(config$input_data$local_dataset, sep = ',')

# Replace NA values with 0
data[is.na(data)] <- 0

# Convert the imported CSV to a spatial (sf) object
data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)

# Reproject data to the planar CRS (EPSG:3035)
data_3035 <- st_transform(data_sf, crs = 3035)

# Import grid where predictions will be projected
grid100 <- st_read(config$input_data$local_predictors_amsterdam)

# Convert the imported grid to an sf object
grid100_sf <- st_as_sf(grid100)

# Create a unique key for each grid cell for future joining
grid100_sf$key <- seq(1, nrow(grid100_sf))

## == Modelling == ##

### == Kriging - Variogram Setup == ###

# Convert the spatial data (sf) back to a Spatial object
data_sp <- as(data_3035, "Spatial")

# Create centroids from the grid geometry to define the projection grid
grid100_centroids <- st_centroid(grid100_sf)

# Get the bounding box of the grid centroids
bbox <- st_bbox(grid100_centroids)

# Generate a 100m x 100m grid for kriging projections
grid <- grid100_centroids %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_make_grid(cellsize = c(100, 100), what = "corners") %>%
  st_as_sf(crs = st_crs(data_3035))

# Convert the grid to a SpatialPixelsDataFrame for kriging
grid_sp <- as(as(grid, "Spatial"), "SpatialPixels")

# Extract x and y coordinates from the spatial object
data_xy <- data.frame(x = data_sp$coords.x1, y = data_sp$coords.x2)
coordinates(data_xy) <- ~x + y

# Fit variogram using the autofit function, based on dependent variable 'Lopend_gemiddelde'
variogram_auto_lin <- autofitVariogram(Lopend_gemiddelde ~ 1, data_sp)

# Plot the fitted variogram
plot(variogram_auto_lin)

# Extract suggested variogram parameter settings
autofit_params_lin <- variogram_auto_lin$var_model
print(autofit_params_lin)

# Manually define the variogram model based on autofit parameters
m <- vgm(psill = 80.16775, model = "Ste", range = 10000)

# Perform Ordinary Kriging (OK) using the fitted variogram model
OK <- krige(Lopend_gemiddelde ~ 1, 
            loc = data_sp,      # Data to interpolate
            newdata = grid_sp,  # Prediction grid
            model = m)          # Fitted variogram model

# Convert the kriging results to an sf object
OK_sf <- st_as_sf(OK)

# Rename the kriging result columns for clarity
OK_sf <- OK_sf %>%
  rename(predicted_OK = var1.pred, variance_OK = var1.var)

print(OK_sf)

# Join the kriging predictions back to the grid geometry
OK_grid <- st_join(grid100_sf, OK_sf)

## == Export Output == ##

# Export the kriging results to a GeoPackage (GPKG) format
sf::st_write(OK_grid, dsn = file.path(out_location_dir, "predictedNO2_OK.gpkg"), driver = "GPKG")
