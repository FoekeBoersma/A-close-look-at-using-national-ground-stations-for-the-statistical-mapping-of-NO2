#import necessary libraries
library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(rgdal)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library("ggplot2")
library("GGally")
library(sp)
library(spatialEco)
library(extrafont)
library(viridis)
library(yaml)

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
# Define output directory
out_location_dir <- normalizePath(file.path(parent_directory, config07$out_location), winslash = "/")



grid100 = readOGR(all_models_dir)


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
#sf::st_write(Amsterdam_square_buffer, dsn=file.path(out_location_dir,"Amsterdam_square_buffer.gpkg"), driver = "GPKG")




sp_query_Amsterdam <-spatial.select(Amsterdam_square_buffer ,y = grid100,predicate = "intersect")

## == maps == ##

# sp_query_Amsterdam <- sp_query_Amsterdam %>% rename(
#     predicted_NO2_RF = p_NO2_RF,
#     predicted_NO2_LASSO = p_NO2_LA,
#     predicted_NO2_RIDGE = p_NO2_RI,
#     predicted_NO2_LightGBM = p_NO2_LG,
#     predicted_NO2_XGBoost = p_NO2_X
#   )

#create list with variables to visualize
vars = c("predicted_NO2_RF", "predicted_NO2_LASSO", "predicted_NO2_RIDGE", "predicted_NO2_LightGBM", "predicted_NO2_XGBoost")
#specify groups of no2 values
# breaks = c(-100, 0, 15, 20, 25, 30, 35, 40, 45, 50, 100, 1000)

# # Use the viridis palette for colorblind-friendly colors
# palette_colors <- c("#808080", "#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026" , "#808080" )

# breaks = c(-10000, -10, 0, 0.5, 1, 2, 3, 5, 10, 50, 1000000)
# palette_colors = c("#808080", "#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026", "#ffffff")




# global_no2_sf <- global_no2_sf %>%
#   mutate(no2 = ifelse(is.na(no2), -9999, no2))


# Calculate differences between model predictions and benchmark (no2)
sp_query_Amsterdam <- sp_query_Amsterdam %>%
  mutate(
    diff_RF = predicted_NO2_RF - no2,
    diff_LASSO = predicted_NO2_LASSO - no2,
    diff_RIDGE = predicted_NO2_RIDGE - no2,
    diff_LightGBM = predicted_NO2_LightGBM - no2,
    diff_XGBoost = predicted_NO2_XGBoost - no2
  )


# # Replace NA values in the difference columns
# global_no2 <- global_no2 %>%
#   mutate(across(starts_with("diff_"), 
#                 ~ ifelse(is.na(.), -9999, .)))




# diff_vars <- c("diff_RF", "diff_LASSO", "diff_RIDGE", "diff_LightGBM", "diff_XGBoost")
# breaks = c(-10000, -10, 0, 0.5, 1, 2, 3, 5, 10, 10, 50, 1000000)
# loop through the shapefiles and create a map for differences of each model compared to the benchmark, save it too
# for (i in 1:length(diff_vars)) {
#   print(diff_vars[i])
#   map_diff <- tm_shape(global_no2) + tm_fill(col = diff_vars[i],breaks=breaks, palette=palette_colors, legend.show = TRUE)
#   model_diff <- diff_vars[i]
#   tmap_save(map_diff, width = 1000, height = 1000, units = "px", filename =file.path(out_location_dir,paste("Amsterdam_diff_", model_diff, ".jpg", sep = ""))

# }

print(names(sp_query_Amsterdam))


summary(sp_query_Amsterdam$diff_LightGBM)

# = local models
sp_query_Amsterdam <- sp_query_Amsterdam %>%
  mutate(
    diff_lin = predNO2_Lin - no2,
    diff_linsep = predNO2_LinSep - no2,
    diff_mem = predNO2_MEM - no2,
    diff_uk = predNO2_UK - no2,
    diff_uksep = predNO2_UKSep - no2,
    diff_ok = predicted_OK - no2
  )

# Identify columns with the prefix "diff_"
diff_columns <- grep("^diff_", names(sp_query_Amsterdam), value = TRUE)


# Define breaks
breaks = c(-10000, -100, -50, -20, -10, -5, 0, 5, 10, 20, 50, 100, 1000000)
# Install and load the RColorBrewer package if not already installed
if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  install.packages("RColorBrewer")
}
library(RColorBrewer)

# Add grey color for extreme values
diverging_palette <- c("grey", colorRampPalette(brewer.pal(11, "PiYG"))(length(breaks) - 3), "grey")


# Function to create the map
create_map <- function(data, model_diff, breaks, palette_colors) {
  # Check if the column exists in the data
  if (model_diff %in% names(data)) {
    # Create the map
    tm_shape(data) +
      tm_fill(
        col = model_diff,
        breaks = breaks,
        palette = palette_colors,
        auto.palette.mapping = FALSE,  # Ensuring manual palette mapping
        legend.show = FALSE,
        legend.title = ifelse(any(data[[model_diff]] < 0), 
                              paste("no2 mobile map >", model_diff), 
                              paste("no2 mobile map <", model_diff))
      )
  } else {
    # Print an error message if the column is not found
    stop(paste("Column", model_diff, "not found in the data"))
  }
}


# Apply the function to each model
diff_vars <- c("diff_RF", "diff_LASSO", "diff_RIDGE", "diff_LightGBM", "diff_XGBoost", 
"diff_lin", "diff_linsep", "diff_mem", "diff_uk", "diff_uksep", "diff_ok")
for (model_diff in diff_vars) {
  print(model_diff)
  tryCatch({
    map_diff <- create_map(sp_query_Amsterdam, model_diff, breaks, diverging_palette)
    tmap_save(map_diff, 
              width = 1000, 
              height = 1000, 
              units = "px", 
              filename = paste(file.path(out_location_dir,paste0("Amsterdam_", model_diff, ".jpg", sep = ""))
  }, error = function(e) {
    cat("Error with", model_diff, ":", e$message, "/n")
  })
}

# Create a dummy map for the legend
create_legend <- function(breaks, palette_colors) {
  tm_shape(sp_query_Amsterdam) +
    tm_fill(
      col = "diff_RF",  # Use any column for dummy
      breaks = breaks,
      palette = palette_colors,
      auto.palette.mapping = FALSE,  # Ensuring manual palette mapping
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
          filename = file.path(out_location_dir,"Amsterdam_diff_legend.jpg")


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
    "Residual statistics for the difference between model predictions and actual NO2 values are as follows:\n\n",
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
      collapse = "\n"
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
output_file_path <- file.path(output_file_path,"residual_summary.txt")
writeLines(summary_text, con = output_file_path)

## = shapefile export option


# names(global_no2) <- gsub("[^A-Za-z0-9_]", "_", names(global_no2))  # Replace invalid characters
# names(global_no2) <- substr(names(global_no2), 1, 10)  # Truncate names to 10 characters
# global_no2_sf <- st_make_valid(global_no2)
# # Remove duplicate columns
# global_no2_sf <- global_no2_sf %>%
#   select(all_of(names(global_no2_sf)[!duplicated(names(global_no2_sf))]))
# # Verify the changes

# print(names(global_no2_sf))


# print(names(global_no2_sf))

# sf::st_write(global_no2, dsn=file.path(out_location_dir,"global_no2"), driver = "ESRI shapefile")




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
    "Residual statistics for the difference between model predictions and actual NO2 values are as follows:\n\n",
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
      collapse = "\n"
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