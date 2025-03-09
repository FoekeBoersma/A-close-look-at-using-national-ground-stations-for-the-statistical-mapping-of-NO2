## == Necessary packages == ##

# Load required packages using lapply for cleaner syntax
packages <- c("tidyverse", "sf", "sp", "raster", "rgdal", "rgeos", "tidyr", 
              "dismo", "lme4", "stats", "nlme", "parallel", "gstat", 
              "automap", "yaml")

lapply(packages, library, character.only = TRUE)

## == Connect to YAML Configuration File == ##

current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir)) # One level up in directory
config_path <- file.path(config_dir, "config_06.yml")
config <- yaml::yaml.load_file(config_path)

# Define the parent directory (four levels up)
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
# Define output directory
out_location_dir <- normalizePath(file.path(parent_directory, config$out_location), winslash = "/")

## == Define Coordinate Systems == ##

# CRS with metric system is preferred (EPSG: 3035)
crs <- CRS("+proj=longlat +datum=WGS84")

## == Import Geodata == ##

data <- read.csv(config$input_data$local_modeling_dataset, sep = ';')
data[is.na(data)] <- 0  # Replace NA values with 0

# Convert to spatial data frame (sf)
data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
data_3035 <- st_transform(data_sf, crs = 3035)  # Transform to planar CRS (3035)

# Import grid for prediction
grid100 <- st_read(config$input_data$local_predictors_amsterdam)
grid100_sf <- st_as_sf(grid100)

## == Spatial Characterization == ##

# Calculate quantiles for grouping
population1000_05 <- quantile(data_3035$population_1000, 0.5)
roadclass3_100_05 <- quantile(data_3035$road_class_3_100, 0.5)

# Use case_when for clearer logic in spatial group assignment
data_3035 <- data_3035 %>%
  mutate(spachar = case_when(
    population_1000 > population1000_05 & 
      ((road_class_2_100 > 0 | road_class_1_100 > 0) | road_class_3_100 > roadclass3_100_05) ~ 1,
    population_1000 < population1000_05 & 
      ((road_class_2_100 > 0 | road_class_1_100 > 0) | road_class_3_100 > roadclass3_100_05) ~ 2,
    TRUE ~ 3
  ))

# Apply the same grouping logic to grid data
grid100_sf <- grid100_sf %>%
  mutate(spachar = case_when(
    population_1000 > population1000_05 & 
      ((road_class_2_100 > 0 | road_class_1_100 > 0) | road_class_3_100 > roadclass3_100_05) ~ 1,
    population_1000 < population1000_05 & 
      ((road_class_2_100 > 0 | road_class_1_100 > 0) | road_class_3_100 > roadclass3_100_05) ~ 2,
    TRUE ~ 3
  ))

## == Key Generation for Grid == ##

# Add unique key to each row
grid100_sf$key <- seq(1, nrow(grid100_sf))

# Split into subsets based on spatial group
Urb_grid100_sf <- filter(grid100_sf, spachar == 1)
Lowpop_grid100_sf <- filter(grid100_sf, spachar == 2)
FFR_grid100_sf <- filter(grid100_sf, spachar == 3)

# Convert to sf objects
Urb_grid100_sf <- st_as_sf(Urb_grid100_sf)
Lowpop_grid100_sf <- st_as_sf(Lowpop_grid100_sf)
FFR_grid100_sf <- st_as_sf(FFR_grid100_sf)

## == Linear Modeling Per Spatial Group == ##

# Subset data based on spatial character
Urban <- filter(data_3035, spachar == 1)
Lowpopulation <- filter(data_3035, spachar == 2)
FarFromRoad <- filter(data_3035, spachar == 3)

# Common formula for all models
model_formula <- Lopend_gemiddelde ~ 1 + nightlight_450 + nightlight_4950 + population_3000 +
                 road_class_1_5000 + road_class_2_1000 + road_class_2_5000 + 
                 road_class_3_100 + road_class_3_300 + trafBuf50

# Train linear models
linear_urb <- lm(model_formula, data = Urban)
linear_lp <- lm(model_formula, data = Lowpopulation)
linear_ffr <- lm(model_formula, data = FarFromRoad)

# Predict and handle missing values
Urb_grid100_sf$pred_urb <- predict(linear_urb, Urb_grid100_sf)
Lowpop_grid100_sf$pred_lp <- predict(linear_lp, Lowpop_grid100_sf)
FFR_grid100_sf$pred_ffr <- predict(linear_ffr, FFR_grid100_sf)

# Convert NA values to 0
Urb_grid100_sf[is.na(Urb_grid100_sf)] <- 0
Lowpop_grid100_sf[is.na(Lowpop_grid100_sf)] <- 0
FFR_grid100_sf[is.na(FFR_grid100_sf)] <- 0

# Select necessary columns for joining
URB_select <- Urb_grid100_sf %>% dplyr::select(key, pred_urb)
LP_select <- Lowpop_grid100_sf %>% dplyr::select(key, pred_lp)
FFR_select <- FFR_grid100_sf %>% dplyr::select(key, pred_ffr)

## == Join Prediction Datasets == ##

# Convert to data frame for merging
grid100 <- as.data.frame(grid100_sf)
URB_select <- as.data.frame(URB_select)
LP_select <- as.data.frame(LP_select)
FFR_select <- as.data.frame(FFR_select)

# Merge prediction datasets
merge <- list(grid100_sf, URB_select, LP_select, FFR_select) %>%
  reduce(full_join, by = 'key') %>%
  replace(is.na(.), 0)

# Final prediction column
merge$predNO2_LinSep <- merge$pred_urb + merge$pred_lp + merge$pred_ffr

# Select relevant columns
merge <- merge %>%
  dplyr::select(nightlight_450, nightlight_4950, population_1000, population_3000, road_class_1_5000,
         road_class_2_100, road_class_2_1000, road_class_1_100, road_class_2_5000,
         road_class_3_100, road_class_3_300, trafBuf50, spachar, key, pred_urb, pred_lp, 
         pred_ffr, geometry.x, predNO2_LinSep)

# Convert to sf object
merge_sf <- st_as_sf(merge)

## == Export == ##

sf::st_write(merge_sf, dsn = file.path(out_location_dir, "predictedNO2_Linear_SeparatingSpatialGroups.gpkg"), driver = "GPKG")
