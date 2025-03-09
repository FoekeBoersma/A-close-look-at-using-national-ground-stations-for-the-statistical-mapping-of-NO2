## == import libraries == ##
library(sf)
library(tidyverse)
library(ggplot2)
library(yaml)
library(rstudioapi)

## == import data == ##
# Connect to YAML file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(dirname(current_dir)) # One level up in directory
config07_path <- file.path(config_dir, "config_07.yml")

# Read YAML configuration file
config07 <- yaml::yaml.load_file(config07_path)

# Define the parent directory (move four levels up)
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))

# Paths for input data based on YAML configuration
Amsterdam_NO2PredictionPerModel_dir <- normalizePath(file.path(parent_directory, config07$input_data$Amsterdam_NO2PredictionPerModel), winslash = "/")
Bayreuth_NO2PredictionPerModel_dir <- normalizePath(file.path(parent_directory, config07$input_data$Bayreuth_NO2PredictionPerModel), winslash = "/")
Hamburg_NO2PredictionPerModel_dir <- normalizePath(file.path(parent_directory, config07$input_data$Hamburg_NO2PredictionPerModel), winslash = "/")
Utrecht_NO2PredictionPerModel_dir <- normalizePath(file.path(parent_directory, config07$input_data$Utrecht_NO2PredictionPerModel), winslash = "/")

Amsterdam_shape_dir <- normalizePath(file.path(parent_directory, config07$input_data$amsterdam_shape), winslash = "/")
Bayreuth_shape_dir <- normalizePath(file.path(parent_directory, config07$input_data$bayreuth_shape), winslash = "/")
Hamburg_shape_dir <- normalizePath(file.path(parent_directory, config07$input_data$hamburg_shape), winslash = "/")
Utrecht_shape_dir <- normalizePath(file.path(parent_directory, config07$input_data$utrecht_shape), winslash = "/")


global_ms_dir <- normalizePath(file.path(parent_directory, config07$input_data$modeling_dataset_global), winslash = "/")

# Define output directory
out_location_dir <- normalizePath(file.path(parent_directory, config07$out_location), winslash = "/")

# create new map inside the output directory and update variable out_location_dir
out_location_dir <- file.path(out_location_dir, "compare_preds_with_mss")

# check if folder exists; if not, create it
if (!dir.exists(out_location_dir)) {
  dir.create(out_location_dir, recursive = TRUE)
}

# Define cities and corresponding YAML keys
cities <- list(
  "Amsterdam" = list("data" = "Amsterdam_NO2PredictionPerModel", "shape" = "amsterdam_shape"),
  "Bayreuth" = list("data" = "Bayreuth_NO2PredictionPerModel", "shape" = "bayreuth_shape"),
  "Hamburg" = list("data" = "Hamburg_NO2PredictionPerModel", "shape" = "hamburg_shape"),
  "Utrecht" = list("data" = "Utrecht_NO2PredictionPerModel", "shape" = "utrecht_shape")
)

# Function to process each city's data
process_city <- function(city_name, no2_data_path, shape_path) {
  print(paste("Processing:", city_name))

  # Load datasets
  global_ms <- read.csv(global_ms_dir, sep = ';')
  city_shape <- st_read(shape_path)
  city_data <- st_read(no2_data_path)  # Import NO₂ dataset for the city

  ## == data processing == ##
  global_ms_sf <- st_as_sf(global_ms, coords = c("Longitude", "Latitude"), crs = 4326)  # Set CRS to WGS 84 (EPSG:4326)

  # Keep only mean NO2 column
  global_ms_sf <- global_ms_sf %>% select(FID, mean_value_NO2)

  # Convert to sf objects
  city_shape <- st_as_sf(city_shape)
  city_data <- st_as_sf(city_data)

  # Standardize column names
  names(city_data) <- gsub("\\.", "_", names(city_data))

  # Transform CRS to match (EPSG:3035)
  global_ms_sf <- st_transform(global_ms_sf, crs = 3035)
  city_shape <- st_transform(city_shape, crs = 3035)
  city_data <- st_transform(city_data, crs = 3035)

  # Check bounding boxes
  bbox_city_data <- st_bbox(city_data)
  bbox_global_ms <- st_bbox(global_ms_sf)
  print(paste(city_name, "Bounding Box:"))
  print(bbox_city_data)
  print(bbox_global_ms)

  ## == join datasets == ##
  overlap <- st_intersects(city_data, global_ms_sf, sparse = FALSE)
  cat("Number of overlaps for", city_name, ":", sum(overlap), "\n")

  global_ms_with_city_info <- st_join(global_ms_sf, city_data)

  # Filter out NA values
  global_ms_with_city_info_no_na <- global_ms_with_city_info %>% filter(!is.na(predicted_NO2_RF))

  # Calculate differences
  global_ms_with_city_info_differences <- global_ms_with_city_info_no_na %>%
    mutate(
      diff_RF = predicted_NO2_RF - mean_value_NO2,
      diff_LASSO = predicted_NO2_LASSO - mean_value_NO2,
      diff_RIDGE = predicted_NO2_RIDGE - mean_value_NO2,
      diff_LightGBM = predicted_NO2_LightGBM - mean_value_NO2,
      diff_XGBoost = predicted_NO2_XGBoost - mean_value_NO2
    )

  # Create output directory for JPEGs
  output_jpg_dir <- file.path(out_location_dir, paste0(city_name, "_global_differences_jpgs_march2025"))
  if (!dir.exists(output_jpg_dir)) {
    dir.create(output_jpg_dir)
  }

  ## == visualization purposes == ##
  diff_columns <- c("diff_RF", "diff_LASSO", "diff_RIDGE", "diff_LightGBM", "diff_XGBoost")
  

  for (col in diff_columns) {
    # Calculate statistics
    stats <- global_ms_with_city_info_differences %>%
      summarize(
        min = min(.data[[col]], na.rm = TRUE),
        max = max(.data[[col]], na.rm = TRUE),
        mean = mean(.data[[col]], na.rm = TRUE),
        median = median(.data[[col]], na.rm = TRUE)
      )
    p <- ggplot(global_ms_with_city_info_differences) +
      geom_sf(aes_string(fill = col), color = "black", shape = 21, size = 3, stroke = 1) +
      geom_sf(data = city_shape, fill = NA, color = "black", size = 1.5) +
      scale_fill_gradient2(
        low = "red",
        mid = "white",
        high = "blue",
        midpoint = 0,
        name = paste("Difference (", col, ")", sep = "")
      ) +
      theme_minimal() +
      theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +
      labs(title = paste("Differences in Model Prediction vs. global observations in", city_name, "(n =", nrow(global_ms_with_city_info_differences), ")"),
      caption = paste0(
        "Min: ", round(stats$min, 2), 
        " | Max: ", round(stats$max, 2), 
        " | Mean: ", round(stats$mean, 2), 
        " | Median: ", round(stats$median, 2)
      ))
    output_jpg_path <- file.path(output_jpg_dir, paste0(city_name, "_", col, "_boundary.jpg"))
    ggsave(output_jpg_path, plot = p, width = 10, height = 8, dpi = 300)
  }
}

# Process each city
process_city("Amsterdam", Amsterdam_NO2PredictionPerModel_dir, Amsterdam_shape_dir)
process_city("Bayreuth", Bayreuth_NO2PredictionPerModel_dir, Bayreuth_shape_dir)
process_city("Hamburg", Hamburg_NO2PredictionPerModel_dir, Hamburg_shape_dir)
process_city("Utrecht", Utrecht_NO2PredictionPerModel_dir, Utrecht_shape_dir)

print("Processing completed for all cities.")
