#import necessary libraries
library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
# library(rgdal)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library("ggplot2")
library("GGally")
library(sp)
library(spatialEco)
library(extrafont)
library(viridis)
library(yaml)
library(RColorBrewer)

# Connect to YAML file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(current_dir) # One level up in directory
config07_path <- file.path(config_dir, "config_07.yml")
# Read YAML configuration file
config07 <- yaml::yaml.load_file(config07_path)

# Define the parent directory (move four levels up)
parent_directory <-dirname(dirname(dirname(current_dir)))
# Paths for input data based on YAML configuration
all_models_dir <- normalizePath(file.path(parent_directory, config07$input_data$all_models), winslash = "/")
# Define output directory
out_location_dir <- normalizePath(file.path(parent_directory, config07$out_location), winslash = "/")
grid100 = st_read(all_models_dir)

# create new map inside the output directory and update variable out_location_dir
out_location_dir <- file.path(out_location_dir, "map_differences_no2_benchmark_kerckhoffs")

# check if folder exists; if not, create it
if (!dir.exists(out_location_dir)) {
  dir.create(out_location_dir, recursive = TRUE)
}

## == (geo)processing == ##

if ("Mixed_NO2" %in% colnames(grid100)) {
  colnames(grid100)[colnames(grid100) == "Mixed_NO2"] <- "no2"
}

print(colnames(grid100))

# ## == maps == ##
#coordinates relating to Amsterdam
y =  52.370216
x = 4.852168

#combine to make a point
Amsterdam_point <- cbind(x, y)
#converse to dataframe to make further data processing feasible
Amsterdam_point <- as.data.frame(Amsterdam_point)
#assign the coordinate values to the variabble xy
xy <- Amsterdam_point[,c(1,2)]

#make spatial - first converting it to a SpatialPointsDataFrame is feasible to make the data spatial
spdf <- SpatialPointsDataFrame(coords = xy, data = Amsterdam_point,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#to sf - necessary for further data operations
Amsterdam_point <- st_as_sf(spdf)

##  First project data into a planar coordinate system (here 3035)

#apply variable as argument to project onto Amsterdam point
Amsterdam_point_3035 <- st_transform(Amsterdam_point, crs=3035)
grid100 <- st_as_sf(grid100)
grid100 <- st_transform(grid100, crs=3035)
print(colnames(grid100))
#Important: coordinates in m, adjust the units on function
rect_around_point <- function(x,xsize,ysize){
  bbox <- st_bbox(x)
  bbox <- bbox +c(xsize/2,ysize/2,-xsize/2,-ysize/2)
  return(st_as_sfc(bbox))
}

#create rectangle relating to Amsterdam area (2nd and 3rd argument specify extent)
Amsterdam_square_buffer <- rect_around_point(Amsterdam_point_3035, 30000, 30000)

## == export option == ##
# sf::st_write(Amsterdam_square_buffer, dsn=file.path(out_location_dir,"Amsterdam_square_buffer2.gpkg"), driver = "GPKG")


grid100 <- st_transform(grid100, crs = st_crs(Amsterdam_square_buffer))
print(st_bbox(Amsterdam_square_buffer))
print(st_bbox(grid100))

sp_query_Amsterdam <-spatial.select(Amsterdam_square_buffer ,y = grid100,predicate = "intersect")

## == maps (global) == ##

# Calculate differences between model predictions and benchmark (no2)
sp_query_Amsterdam <- sp_query_Amsterdam %>%
  mutate(
    diff_RF = predicted_NO2_RF - no2,
    diff_LASSO = predicted_NO2_LASSO - no2,
    diff_RIDGE = predicted_NO2_RIDGE - no2,
    diff_LightGBM = predicted_NO2_LightGBM - no2,
    diff_XGBoost = predicted_NO2_XGBoost - no2
  )

## == local models visualization == ##
sp_query_Amsterdam <- sp_query_Amsterdam %>%
  mutate(
    diff_lin = predNO2_Lin - no2,
    diff_linsep = predNO2_LinSep - no2,
    diff_mem = predNO2_MEM - no2,
    diff_uk = predNO2_UK - no2,
    diff_uksep = predNO2_UKSep - no2,
    diff_ok = predicted_OK - no2
  )

# sf::st_write(sp_query_Amsterdam, dsn=file.path(out_location_dir,"sp_query_AmsterdamTESTING332.gpkg"), driver = "GPKG")

# Identify columns with the prefix "diff_"
diff_columns <- grep("^diff_", names(sp_query_Amsterdam), value = TRUE)

## == maps (global + local) == ##

# Function to create the map
create_map <- function(data, model_diff, breaks, palette_colors) {
  # Check if the column exists in the data
  if (model_diff %in% names(data)) {
    # Create the map
    tm_shape(data) +
      tm_polygons(fill = model_diff, lwd=0, lwd.free = NA, palette=palette_colors, breaks=breaks, border.col = NA,
      lwd.scale = tm_scale()) + tm_layout(legend.show=FALSE)

  } else {
    # Print an error message if the column is not found
    stop(paste("Column", model_diff, "not found in the data"))
  }
}

# install.packages("dplyr")
library(dplyr)
# Replace NA values in the difference columns
sp_query_Amsterdam_map <- sp_query_Amsterdam %>%
  mutate(across(starts_with("diff_"), 
                ~ ifelse(is.na(.), -9999, .)))

print(head(sp_query_Amsterdam_map))
# Define breaks
breaks <- c(-10000, -10, 0, 0.5, 1, 2, 3, 5, 10, 20, 50, 1000000)
breaks <- sort(unique(breaks))  # Ensure no duplicates and sorted
diverging_palette <- c("grey", colorRampPalette(brewer.pal(11, "PiYG"))(length(breaks) - 3), "grey")

# Apply the function to each model
diff_vars <- c("diff_RF", "diff_LASSO", "diff_RIDGE", "diff_LightGBM", "diff_XGBoost", 
"diff_lin", "diff_linsep", "diff_mem", "diff_uk", "diff_uksep", "diff_ok")
fill_scale <- tm_scale(values = breaks)

print(head(sp_query_Amsterdam))












# Define breaks
breaks = c(-10000, -100, -50, -20, -10, -5, 0, 5, 10, 20, 50, 100, 1000000)
# Install and load the RColorBrewer package if not already installed
if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  install.packages("RColorBrewer")
}
library(RColorBrewer)
# Add grey color for extreme values
diverging_palette <- c("grey", colorRampPalette(brewer.pal(11, "PiYG"))(length(breaks) - 3), "grey")



for (model_diff in diff_vars) {
  print(model_diff)
  tryCatch({
    map_diff <- create_map(sp_query_Amsterdam_map, model_diff, breaks, diverging_palette)  # Remove borders
    
    # Fix: Close the parentheses properly
    tmap_save(map_diff, 
              width = 8000, 
              height = 8000, 
              units = "px", 
              filename = file.path(out_location_dir, paste0("Amsterdam_", model_diff, ".png")))
    
  }, error = function(e) {
    cat("Error with", model_diff, ":", e$message, "\n")  # Fix: Corrected newline character
  })
}

# legend

create_legend <- function(breaks, palette_colors) {
  tm_shape(sp_query_Amsterdam_map) +
    tm_fill(
      col = "diff_RF",  # use dummy variable
      breaks = breaks,  # Explicitly set breaks here
      palette = palette_colors,  # Use correct color palette
      auto.palette.mapping = FALSE,  # Disable automatic color scaling
      legend.show = TRUE,
      legend.title = "NO2 Mobile Map Differences"
    ) +
    tm_layout(legend.only = TRUE, legend.position = c("center", "center"))
}
# Generate the legend map
legend_map <- create_legend(breaks, diverging_palette)
tmap_save(legend_map, 
          width = 500, 
          height = 500, 
          units = "px", 
          filename = file.path(out_location_dir,"Amsterdam_diff_legend.jpg"))



## == residual statistics == ##

# Function to calculate residual statistics
calculate_residual_stats <- function(data, diff_columns) {
  stats_list <- list()
  
  for (col in diff_columns) {
    if (col %in% names(data)) {
      stats <- data %>%
        summarize(
          mean_residual = mean(.data[[col]], na.rm = TRUE),
          median_residual = median(.data[[col]], na.rm = TRUE),
          sd_residual = sd(.data[[col]], na.rm = TRUE),
          min_residual = min(.data[[col]], na.rm = TRUE),
          max_residual = max(.data[[col]], na.rm = TRUE),
          n = n()
        )
      stats_list[[col]] <- stats
    } else {
      warning(paste("Column", col, "not found in the data"))
    }
  }
  
  return(stats_list)
}

# Function to create formatted summary text
create_summary_text <- function(stats_df) {
  summary_text <- paste(
    "Residual statistics for the difference between model predictions and actual NO2 values are as follows:/n/n",
    paste(
      sapply(1:nrow(stats_df), function(i) {
        row <- stats_df[i, ]
        paste0(
          row["Model"], ": Mean = ", round(row["Mean"], 2),
          ", Median = ", round(row["Median"], 2),
          ", SD = ", round(row["SD"], 2),
          ", Min = ", round(row["Min"], 2),
          ", Max = ", round(row["Max"], 2)
        )
      }),
      collapse = "/n"
    )
  )
  
  return(summary_text)
}

# Calculate statistics for the difference columns
diff_columns <- grep("^diff_", names(sp_query_Amsterdam), value = TRUE)
residual_stats <- calculate_residual_stats(sp_query_Amsterdam, diff_columns)

# Convert the list of statistics to a data frame
stats_df <- do.call(rbind, lapply(names(residual_stats), function(name) {
  stats <- residual_stats[[name]]
  data.frame(
    Model = name,
    Mean = stats$mean_residual,
    Median = stats$median_residual,
    SD = stats$sd_residual,
    Min = stats$min_residual,
    Max = stats$max_residual,
    N = stats$n,
    stringsAsFactors = FALSE
  )
}))

# Create and save the summary text
summary_text <- create_summary_text(stats_df)
output_file_path <- file.path(out_location_dir,"residual_summary.txt")
writeLines(summary_text, con = output_file_path)

## = shapefile export option



# Remove duplicate columns
sp_query_Amsterdam <- sp_query_Amsterdam %>%
  select(all_of(names(sp_query_Amsterdam)[!duplicated(names(sp_query_Amsterdam))]))
# Verify the changes

print(names(sp_query_Amsterdam))


sf::st_write(sp_query_Amsterdam, dsn=file.path(out_location_dir,"all_models_diff_no2.gpkg"), driver = "GPKG")




# Function to calculate residual statistics
calculate_residual_stats <- function(data, diff_columns) {
  stats_list <- list()
  
  for (col in diff_columns) {
    if (col %in% names(data)) {
      stats <- data %>%
        summarize(
          mean_residual = mean(.data[[col]], na.rm = TRUE),
          median_residual = median(.data[[col]], na.rm = TRUE),
          sd_residual = sd(.data[[col]], na.rm = TRUE),
          min_residual = min(.data[[col]], na.rm = TRUE),
          max_residual = max(.data[[col]], na.rm = TRUE),
          n = n()
        )
      stats_list[[col]] <- stats
    } else {
      warning(paste("Column", col, "not found in the data"))
    }
  }
  
  return(stats_list)
}

# Function to create formatted summary text
create_summary_text <- function(stats_df) {
  summary_text <- paste(
    "Residual statistics for the difference between model predictions and actual NO2 values are as follows:/n/n",
    paste(
      sapply(1:nrow(stats_df), function(i) {
        row <- stats_df[i, ]
        paste0(
          row["Model"], ": Mean = ", round(row["Mean"], 2),
          ", Median = ", round(row["Median"], 2),
          ", SD = ", round(row["SD"], 2),
          ", Min = ", round(row["Min"], 2),
          ", Max = ", round(row["Max"], 2)
        )
      }),
      collapse = "/n"
    )
  )
  
  return(summary_text)
}

# Verify cases with NA values in 'no2' column before filtering
print("Cases with NA values in 'no2' before filtering:")
print(sp_query_Amsterdam[is.na(sp_query_Amsterdam$no2), ])

# Filter out rows where 'no2' column is NA
sp_query_Amsterdam <- sp_query_Amsterdam %>%
  filter(!is.na(no2))

# Verify cases with NA values in 'no2' column after filtering
print("Cases with NA values in 'no2' after filtering:")
print(sp_query_Amsterdam[is.na(sp_query_Amsterdam$no2), ])

# Calculate statistics for the difference columns
diff_columns <- grep("^diff_", names(sp_query_Amsterdam), value = TRUE)
residual_stats <- calculate_residual_stats(sp_query_Amsterdam, diff_columns)

# Convert the list of statistics to a data frame
stats_df <- do.call(rbind, lapply(names(residual_stats), function(name) {
  stats <- residual_stats[[name]]
  data.frame(
    Model = name,
    Mean = stats$mean_residual,
    Median = stats$median_residual,
    SD = stats$sd_residual,
    Min = stats$min_residual,
    Max = stats$max_residual,
    N = stats$n,
    stringsAsFactors = FALSE
  )
}))

# Create and save the summary text
summary_text <- create_summary_text(stats_df)
output_file_path <- file.path(out_location_dir,"residual_summary_nonan.txt")
writeLines(summary_text, con = output_file_path)