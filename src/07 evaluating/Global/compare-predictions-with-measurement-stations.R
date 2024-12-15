# Import necessary libraries
library(sf)
library(dplyr)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2)
library(GGally)
library(sp)
library(viridis)
library(yaml)
library(extrafont)
library(ggspatial)
library(rgdal)

# == Import data == #
# Connect to YAML file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir)) # One level up in directory
config07_path <- file.path(config_dir, "config_07.yml")
# Read YAML configuration file
config07 <- yaml::yaml.load_file(config07_path)
# Define the parent directory (move four levels up)
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
# Paths for input data based on YAML configuration
all_models_dir <- normalizePath(file.path(parent_directory, config07$input_data$all_models), winslash = "/")
bayreuth_predictions_dir <- normalizePath(file.path(parent_directory, config07$input_data$Bayreuth_NO2PredictionPerModel_ZI), winslash = "/")
hamburg_predictions_shape_dir <- normalizePath(file.path(parent_directory, config07$input_data$Hamburg_NO2PredictionPerModel), winslash = "/")
utrecht_predictions_shape_dir <- normalizePath(file.path(parent_directory, config07$input_data$Utrecht_NO2PredictionPerModel), winslash = "/")
amsterdam_shape_dir <- normalizePath(file.path(parent_directory, config07$input_data$amsterdam_shape), winslash = "/")
bayreuth_shape_dir <- normalizePath(file.path(parent_directory, config07$input_data$bayreuth_shape), winslash = "/")
hamburg_shape_dir <- normalizePath(file.path(parent_directory, config07$input_data$hamburg_shape), winslash = "/")
utrecht_shape_dir <- normalizePath(file.path(parent_directory, config07$input_data$utrecht_shape), winslash = "/")
global_ms_dir <- normalizePath(file.path(parent_directory, config07$input_data$modeling_dataset_global), winslash = "/")
# Define output directory
out_location_dir <- normalizePath(file.path(parent_directory, config07$out_location), winslash = "/")

# Read data
grid100 <- readOGR(all_models_dir)
grid100_bayreuth <- readOGR(bayreuth_predictions_dir)
grid100_hamburg <- readOGR(hamburg_predictions_shape_dir)
grid100_utrecht <- readOGR(utrecht_predictions_shape_dir)
global_ms <- read.csv(global_ms_dir, sep = ';')
amsterdam_shape <- readOGR(amsterdam_shape_dir)
bayreuth_shape <- readOGR(bayreuth_shape_dir)
hamburg_shape <- readOGR(hamburg_shape_dir)
utrecht_shape <- readOGR(utrecht_shape_dir)

# == Data Processing == #
global_ms_sf <- st_as_sf(global_ms, coords = c("Longitude", "Latitude"), crs = 4326)  # Set CRS to WGS 84 (EPSG:4326)
global_ms_sf <- global_ms_sf %>% select(FID, mean_value_NO2)  # Keep only FID and mean_value_NO2

# Convert both grid100 and global_ms_dir to sf objects
grid100 <- st_as_sf(grid100)
grid100_bayreuth <- st_as_sf(grid100_bayreuth)
grid100_hamburg <- st_as_sf(grid100_hamburg)
grid100_utrecht <- st_as_sf(grid100_utrecht)
amsterdam_shape <- st_as_sf(amsterdam_shape)
bayreuth_shape <- st_as_sf(bayreuth_shape)
hamburg_shape <- st_as_sf(hamburg_shape)
utrecht_shape <- st_as_sf(utrecht_shape)

# Adjust column names
names(grid100) <- gsub("\\.", "_", names(grid100))

# Ensure all spatial data are in the same CRS (EPSG:3035)
target_crs <- 3035  # European CRS (for example, ETRS89 / LAEA Europe)

# Transform the data to the target CRS
global_ms_sf <- st_transform(global_ms_sf, crs = target_crs)
grid100 <- st_transform(grid100, crs = target_crs)
grid100_bayreuth <- st_transform(grid100_bayreuth, crs = target_crs)
grid100_hamburg <- st_transform(grid100_hamburg, crs = target_crs)
grid100_utrecht <- st_transform(grid100_utrecht, crs = target_crs)
amsterdam_shape <- st_transform(amsterdam_shape, crs = target_crs)
bayreuth_shape <- st_transform(bayreuth_shape, crs = target_crs)
hamburg_shape <- st_transform(hamburg_shape, crs = target_crs)
utrecht_shape <- st_transform(utrecht_shape, crs = target_crs)

# == Join Datasets == #
# Check if there is any intersection for each city
city_grids_shapes <- list(
  Amsterdam = list(grid = grid100, shape = amsterdam_shape),
  Bayreuth = list(grid = grid100_bayreuth, shape = bayreuth_shape),
  Hamburg = list(grid = grid100_hamburg, shape = hamburg_shape),
  Utrecht = list(grid = grid100_utrecht, shape = utrecht_shape)
)

# Loop through each city and process data
for (city_name in names(city_grids_shapes)) {
  grid_layer <- city_grids_shapes[[city_name]]$grid   # Get the grid layer for the city
  shape_layer <- city_grids_shapes[[city_name]]$shape # Get the shape layer for the city
  
  # Convert both grid_layer and shape_layer to 'sf' if they're not already
  grid_layer_sf <- st_as_sf(grid_layer)
  shape_layer_sf <- st_as_sf(shape_layer)

  # Check and ensure both grid and shape layers are in the same CRS (already done above)
  if (st_crs(grid_layer) != st_crs(shape_layer)) {
    cat("Warning: CRS mismatch in", city_name, "correcting...\n")
    shape_layer <- st_transform(shape_layer, crs = st_crs(grid_layer))
  }
  
  # Check if there is any intersection between grid and shape
  overlap <- st_intersects(grid_layer_sf, shape_layer_sf, sparse = FALSE)
  cat("City:", city_name, "- Number of overlaps:", sum(overlap), "\n")
  
  # Join datasets
  global_ms_with_grid_info <- st_join(global_ms_sf, grid_layer_sf)
  
  # Filter the dataset to keep rows where at least one field is not NULL
  global_ms_with_grid_info_no_na <- global_ms_with_grid_info %>% filter(!is.na(predicted_NO2_RF))  # Assuming 'mean_value' is the relevant field in global_ms_sf
  
  # Calculate differences between model predictions and benchmark (NO2)
  global_ms_with_grid_info_differences <- global_ms_with_grid_info_no_na %>% mutate(
    diff_RF = predicted_NO2_RF - mean_value_NO2,
    diff_LASSO = predicted_NO2_LASSO - mean_value_NO2,
    diff_RIDGE = predicted_NO2_RIDGE - mean_value_NO2,
    diff_LightGBM = predicted_NO2_LightGBM - mean_value_NO2,
    diff_XGBoost = predicted_NO2_XGBoost - mean_value_NO2
  )
  
  # Create the output directory for JPEGs
  output_jpg_dir <- file.path(out_location_dir, "global_differences_jpgs")
  if (!dir.exists(output_jpg_dir)) {
    dir.create(output_jpg_dir)
  }
  
  # List of difference columns to visualize
  diff_columns <- c("diff_RF", "diff_LASSO", "diff_RIDGE", "diff_LightGBM", "diff_XGBoost")
  
  # Loop through each column and create a plot
  for (col in diff_columns) {

    stats <- global_ms_with_grid_info_differences[[col]]
    max_val <- max(stats, na.rm = TRUE)
    min_val <- min(stats, na.rm = TRUE)
    mean_val <- mean(stats, na.rm = TRUE)
    median_val <- median(stats, na.rm = TRUE)
    
    # Create a subtitle with the stats
    subtitle_text <- paste(
      "Min:", round(min_val, 2), 
      "| Max:", round(max_val, 2), 
      "| Mean:", round(mean_val, 2), 
      "| Median:", round(median_val, 2)
    )
    # Create the plot
    p <- ggplot(global_ms_with_grid_info_differences) +
      # Plot points from the difference dataset, coloring by the current difference column
      geom_sf(aes_string(fill = col), size = 3, shape = 21, color = "black", stroke = 0.5) +  # Black outline

      # Overlay the city's boundary
      geom_sf(data = shape_layer_sf, fill = NA, color = "black", size = 1.5) +  # Customize line color and size
      # Color scale for differences
      scale_fill_gradient2(
        low = "red",        # Color for negative values
        mid = "white",      # Color for zero (optional)
        high = "blue",      # Color for positive values
        midpoint = 0,       # Set midpoint to 0 (for balancing the red and blue colors)
        name = paste("Difference (", col, ")", sep = "")  # Color scale label with dynamic column name
      ) + 
      theme_minimal() +  # Minimal theme
      theme(legend.position = "right",  # Adjust legend position
            plot.title = element_text(hjust = 0.5)) +  # Center the title
      labs(
      title = paste("Differences in Model Prediction vs. global observations (n =", nrow(global_ms_with_grid_info_differences), ", City:", city_name, ")", sep = ""),
      subtitle = subtitle_text  # Add stats as subtitle
    )

    # Save the plot as a JPEG image
    output_jpg_path <- file.path(output_jpg_dir, paste(col, "_plot_with_", city_name, ".jpg", sep = ""))
    ggsave(output_jpg_path, plot = p, width = 10, height = 8, dpi = 300)  # Save at 300 dpi for high quality
  }
}