# Check if necessary packages are installed, and install them if not
# packages_needed <- c("")

# for (pkg in packages_needed) {
#   if (!requireNamespace(pkg, quietly = TRUE)) {
#     install.packages(pkg)
#   }
# }

#import necessary libraries
library(sf)
library(dplyr)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library("ggplot2")
library("GGally")
library(sp)
library(viridis)
library(yaml)
library(extrafont)
library(ggspatial)

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
all_models_dir <- normalizePath(file.path(parent_directory, config07$input_data$all_models), winslash = "/")
amsterdam_shape_dir<- normalizePath(file.path(parent_directory, config07$input_data$amsterdam_shape), winslash = "/")
local_ms_dir<- normalizePath(file.path(parent_directory, config07$input_data$modeling_dataset_local), winslash = "/")
# Define output directory
out_location_dir <- normalizePath(file.path(parent_directory, config07$out_location), winslash = "/")

grid100 = st_read(all_models_dir)
local_ms = read.csv(local_ms_dir, sep=';')
amsterdam_shape <- st_read(amsterdam_shape_dir)

## == data processing == ##
local_ms_sf <- st_as_sf(local_ms, coords = c("Longitude", "Latitude"), crs = 4326)  # Set CRS to WGS 84 (EPSG:4326)

colnames(local_ms_sf)
colnames(grid100)
# keep only mean value NO2 column
local_ms_sf <- local_ms_sf %>%
  select(Lopend_gemiddelde) %>% # Keep only FID and wnd_day_value
  rename(mean_value_NO2 = Lopend_gemiddelde)

# Convert both grid100 and local_ms_dir to sf objects
grid100 <- st_as_sf(grid100)
amsterdam_shape <- st_as_sf(amsterdam_shape)
colnames(grid100)
# export option
# sf::st_write(local_ms_sf, dsn=file.path(out_location_dir,"local_ms_sf.gpkg"), driver = "GPKG")

#adjust column names
names(grid100) <- gsub("\\.", "_", names(grid100))

#export option
# sf::st_write(grid100, dsn=file.path(out_location_dir,"grid100.gpkg"), driver = "GPKG")

#set to same crs
local_ms_sf <- st_transform(local_ms_sf, crs = 3035)
grid100 <- st_transform(grid100, crs = 3035)
amsterdam_shape <- st_transform(amsterdam_shape, crs = 3035)

# Check bounding boxes
bbox_grid100 <- st_bbox(grid100)
bbox_local_ms <- st_bbox(local_ms_sf)
print(bbox_grid100)
print(bbox_local_ms)

## == join datasets == ##

# Check if there is any intersection
overlap <- st_intersects(grid100, local_ms_sf, sparse = FALSE)
cat("Number of overlaps:", sum(overlap), "\n")
# join datasets
local_ms_with_grid_info <- st_join(local_ms_sf, grid100)

# Filter the dataset to keep rows where at least one field is not NULL
local_ms_with_grid_info_no_na <- local_ms_with_grid_info %>%
  filter(!is.na(predicted_NO2_RF)) %>% # Assuming 'mean_val' is the relevant field in local_ms_sf
  rename(predNO2_OK = predicted_OK)

colnames(local_ms_with_grid_info_no_na)

# Calculate differences between model predictions and benchmark (NO2)
local_ms_with_grid_info_differences <- local_ms_with_grid_info_no_na %>%
  mutate(
    diff_Lin = predNO2_Lin - mean_value_NO2,
    diff_LinSep = predNO2_LinSep - mean_value_NO2,
    diff_MEM = predNO2_MEM - mean_value_NO2,
    diff_UK = predNO2_UK - mean_value_NO2,
    diff_UKSep = predNO2_UKSep - mean_value_NO2,
    diff_OK = predNO2_OK - mean_value_NO2
  )
# export option

# output_path <- file.path(out_location_dir, "local_ms_with_grid_info_differences.gpkg")
# sf::st_write(local_ms_with_grid_info_differences, dsn = output_path, driver = "GPKG")

## == visualization purposes == ##

# Create the output directory for JPEGs
output_jpg_dir <- file.path(out_location_dir, "local_differences_jpgs-march2025")
if (!dir.exists(output_jpg_dir)) {
  dir.create(output_jpg_dir)
}


# List of difference columns to visualize
diff_columns <- c("diff_Lin", "diff_LinSep", "diff_MEM", "diff_UK", "diff_UKSep", "diff_OK")

# Loop through each column and create a plot with Amsterdam boundary
for (col in diff_columns) {
  # Create the plot
  p <- ggplot(local_ms_with_grid_info_differences) +
    # Add the basemap (OpenStreetMap)
    
    # Plot points from the difference dataset, coloring by the current difference column
    geom_sf(aes_string(color = col), size = 3) +  
    
    # Overlay the Amsterdam boundary (amsterdam_shape)
    geom_sf(data = amsterdam_shape, fill = NA, color = "black", size = 1.5) +  # Customize line color and size
    
    # Color scale for differences
    scale_color_gradient2(
      low = "red",        # Color for negative values
      mid = "white",      # Color for zero (optional)
      high = "blue",      # Color for positive values
      midpoint = 0,       # Set midpoint to 0 (for balancing the red and blue colors)
      name = paste("Difference (", col, ")", sep = "")  # Color scale label with dynamic column name
    ) + 
    theme_minimal() +  # Minimal theme
    theme(legend.position = "right",  # Adjust legend position
          plot.title = element_text(hjust = 0.5)) +  # Center the title
    labs(title = paste("Differences in Model Prediction vs. local observations (n =", nrow(local_ms_with_grid_info_differences), ")", sep = ""))  # Dynamic title

  # Save the plot as a JPEG image
  output_jpg_path <- file.path(output_jpg_dir, paste(col, "_plot_with_amsterdam.jpg", sep = ""))
  ggsave(output_jpg_path, plot = p, width = 10, height = 8, dpi = 300)  # Save at 300 dpi for high quality
}

