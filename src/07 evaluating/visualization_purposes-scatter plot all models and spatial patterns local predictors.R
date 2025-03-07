library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library("ggplot2")
library("GGally")
library(yaml)

## == importing data == ## 

# Connect to YAML file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(current_dir) # One level up in directory
config07_path <- file.path(config_dir, "config_07.yml")
print(config07_path)
# Read YAML configuration file
config07 <- yaml::yaml.load_file(config07_path)

# Define the parent directory (move three levels up)
parent_directory <- dirname(dirname(dirname(current_dir)))

# Paths for input data based on YAML configuration
all_models_dir <- normalizePath(file.path(parent_directory, config07$input_data$all_models), winslash = "/")
# Define output directory
grid100 = st_read(all_models_dir)

# Define output directory
out_location_dir <- normalizePath(file.path(parent_directory, config07$out_location), winslash = "/")

# Create a new folder 'all_models_output' inside the output directory
all_models_output_dir <- file.path(out_location_dir, "scatter plot and spatial patterns local pred")

# Check if the folder exists; if not, create it
if (!dir.exists(all_models_output_dir)) {
  dir.create(all_models_output_dir, recursive = TRUE)
}

# Update output location directory
out_location_dir <- all_models_output_dir

## == data processing == ##

#to datadrame
grid100_df <- as.data.frame(grid100)
# Add fill layer to nz shape
grid100 <- st_as_sf(grid100)


# Define the new column names
new_names <- c("Li" = "predNO2_Lin",
               "LiSpa" = "predNO2_LinSep",
               "MEM" = "predNO2_MEM",
               "Uk" = "predNO2_UK",
               "UkSpa" = "predNO2_UKSep",
               "Ok" = "predicted_OK",
               "RF" = "predicted_NO2_RF",
               "Las" = "predicted_NO2_LASSO",
               "Rid" = "predicted_NO2_RIDGE",
               "Lgb" = "predicted_NO2_LightGBM",
               "Xgb" = "predicted_NO2_XGBoost")



# Check if renaming is needed
if (!all(names(new_names) %in% names(grid100_df))) {
  grid100_df <- grid100_df %>% rename(!!!new_names)
}
# ## == scatter plot == ##

# Create the output directory for JPEGs
output_jpg_dir <- file.path(out_location_dir, "allplots")
if (!dir.exists(output_jpg_dir)) {
  dir.create(output_jpg_dir)
}

# Create the output directory for JPEGs
output_local_predictors_dir <- file.path(out_location_dir, "local_predictors")
if (!dir.exists(output_local_predictors_dir)) {
  dir.create(output_local_predictors_dir)
}
if ("Mixed_NO2" %in% colnames(grid100_df)) {
  colnames(grid100_df)[colnames(grid100_df) == "Mixed_NO2"] <- "no2"
}
print(colnames(grid100_df))

grid100_df_models <- grid100_df[,c("RF","Xgb","Las","Rid","Li","LiSpa","MEM","Uk","UkSpa","Ok","no2")]
local <- grid100_df[,c("Li","LiSpa","MEM","Uk","UkSpa","Ok")]
#export local models to csv
write.csv(local, file.path(out_location_dir, 'localmodel_predictions.csv'))

#examine statistics per model
summary(grid100_df_models)
grid100_df_noNAs = na.omit(grid100_df_models)
print(nrow(grid100_df_noNAs))

# # use jpg() instead of svg(), if you want PDF output
pm <- ggpairs(grid100_df_noNAs[, c("RF","Xgb","Las","Rid","Li","LiSpa","MEM","Uk","UkSpa","Ok","no2")], 
              upper = list(continuous = GGally::wrap(ggally_cor))) +
                       theme(axis.text.x = element_text(angle =90, hjust = 1)) 
ggsave(file.path(output_jpg_dir, "allplot-includingNO2tif-excluding_LightGBM.jpeg"), pm, width = 6, height = 6)


# if (dev.cur() > 1) dev.off()  # Only close if a device is open

## == adjusted ranges/correcting for outliers == ##

#create for loop that for every model filters rows and keep the ones with values between 0 and 85.

#create a data frame to store the results
omission_summary <- data.frame(Variable = character(), NAs = numeric(), OutOfRange = numeric(), TotalOmitted = numeric())

#relevant models
vars = c("RF","Lgb","Xgb","Las","Rid","Li","LiSpa","MEM","Uk","UkSpa","Ok", "no2")
#remove NA values
grid100_df_noNAs = na.omit(grid100_df)
#create empty list where processed data will be stored to.
processed_data <- list()
for(var in vars){
  #apply filtering
  between0_85 <- grid100_df_noNAs[grid100_df_noNAs[var] < 85 & grid100_df_noNAs[var] > 0, ]
  #keep track of progress in for loop via printing
  print(var)
  #give every processed dataset a unique name that hints to the model used in the fold.
  assign(paste0("between0_85", var, sep=""), between0_85[,c(var, "key")])
  #examine dimensions
  dim(paste0("between0_85", var, sep=""))
  #append processed data to list that is defined outside for loop.
  processed_data <- c(processed_data, paste0("between0_85", var, sep=""))

    # Count NA values
  n_na <- sum(is.na(grid100_df[[var]]))
  
  # Count values outside the range (0, 85)
  out_of_range <- sum(grid100_df[[var]] <= 0 | grid100_df[[var]] >= 85, na.rm = TRUE)
  
  # Total omitted (NA + out of range)
  total_omitted <- n_na + out_of_range
  
  # Append to summary
  omission_summary <- rbind(omission_summary, 
                             data.frame(Variable = var, 
                                        NAs = n_na, 
                                        OutOfRange = out_of_range, 
                                        TotalOmitted = total_omitted))
  
  # For printing progress in console
  cat(paste("Variable:", var, "NAs:", n_na, "Out of Range:", out_of_range, "Total Omitted:", total_omitted, "\n"))
}

# Save summary as CSV (optional)
write.csv(omission_summary, file.path(out_location_dir, "omission_summary.csv"), row.names = FALSE)

# Add detailed information to a text file
txt_file_path <- file.path(out_location_dir, "omission_summary_details.txt")

print(txt_file_path)

# Open a connection to the file
txt_file <- file(txt_file_path, open = "wt")

# Write a header
writeLines("Omission Summary Details", txt_file)
writeLines(paste0("Total number of samples: ", nrow(grid100_df_noNAs)), txt_file)
writeLines("\nVariable-specific omission counts:", txt_file)

# Loop over each variable and add detailed omission counts
for (var in vars) {
  # Samples not meeting criteria
  not_meeting_criteria <- sum(!(grid100_df_noNAs[[var]] > 0 & grid100_df_noNAs[[var]] < 85), na.rm = TRUE)
  
  # Write details to the file
  writeLines(paste0(
    "Variable: ", var, 
    "\n - Not Meeting Criteria (<0 or >=85): ", not_meeting_criteria, 
    "\n - NAs: ", sum(is.na(grid100_df[[var]])), 
    "\n"
  ), txt_file)
}

# Close the connection
close(txt_file)

between0_85_allmodels <- Reduce(function(x,y) merge(x, y, by = "key", all.x = TRUE, all.y = TRUE),
                         list(between0_85RF, between0_85Lgb,between0_85Xgb,between0_85Las,between0_85Rid,
                        between0_85Li, between0_85LiSpa, between0_85MEM, between0_85Uk, between0_85UkSpa, between0_85Ok, between0_85no2))

pm_between <- ggpairs(between0_85_allmodels[, c("RF","Xgb","Las","Rid","Li","LiSpa","MEM","Uk","UkSpa","Ok","no2")], 
              upper = list(continuous = GGally::wrap(ggally_cor))) +
                       theme(axis.text.x = element_text(angle =90, hjust = 1)) 

ggsave(file.path(output_jpg_dir, "allplot-includingNO2tif-between0_85.jpeg"), pm_between, width = 6, height = 6)

## == maps == ##

#create list with variables to visualize
vars = c("predNO2_Lin","predNO2_LinSep","predNO2_MEM","predNO2_UK","predNO2_UKSep","predicted_OK",
         "predicted_NO2_RF","predicted_NO2_LASSO", "predicted_NO2_RIDGE","predicted_NO2_LightGBM", "predicted_NO2_XGBoost")


length(vars)
breaks = c(-100, 0, 15, 20, 25, 30, 35, 40, 45, 50, 100, 1000)
#manually define color palette
palette <- c("grey", "palegreen4", "palegreen3","palegreen","greenyellow",  "yellow", "gold", "darkorange", "red", "darkred", "grey")


# loop through the shapefiles and create a map for each
for (i in 1:seq_along(vars)) {
  vars[i]
  map <- tm_shape(grid100) + tm_polygons(fill =vars[i], breaks=breaks, palette=palette, legend.show = FALSE)
  model = vars[i]
  tmap_save(map, width = 3000, height = 3000, units="px", filename = file.path(out_location_dir, paste0(model, ".png")))

}

#legend visualization
legende <- tm_shape(grid100) + tm_fill(col = "predNO2_Lin", title = "                        Predicted NO2", breaks=breaks, palette=palette, legend.is.portrait = FALSE) + tm_layout(legend.only = T, fontfamily = "serif")
tmap_save(legende, width = 1000, height = 1000, units="px", filename = file.path(out_location_dir, "Legende.png"))

## == local predictors == ##
predictors = c("nightlight_450",    "nightlight_4950", "population_1000","population_3000","road_class_1_5000",
               "road_class_2_100","road_class_2_1000","road_class_1_100","road_class_2_5000" ,"road_class_3_100",  "road_class_3_300" 
               ,"trafBuf50")


# Loop through each predictor and create a map if the column exists
for (i in seq_along(predictors)) {
  predictor <- predictors[i]
  
  # Check if the predictor column exists in grid100
  if (predictor %in% names(grid100)) {
    map <- tm_shape(grid100) + tm_fill(col = predictor, legend.show = FALSE)
    
    # Construct the file path for saving
    filename <- file.path(output_local_predictors_dir, paste0("local_predictors_", predictor, ".jpg"))
    
    # Save the map
    tmap_save(map, width = 1000, height = 1000, units = "px", filename = filename)
    
    # Optional: Print a message to confirm successful save
    print(paste("Saved map for predictor:", predictor))
  } else {
    # Print a warning if the predictor column is missing
    warning(paste("Column", predictor, "not found in grid100"))
  }
}

summary(grid100$road_class_2_5000)

breaks_rcl2_5000 = c(0, 10000,20000, 30000, 40000,50000, 60000, 70000, 80000)
palette_rcl2_5000 <- c("palegreen4", "palegreen3","palegreen","greenyellow",  "yellow",  "darkorange", "red", "darkred")

rcl2_5000 = tm_shape(grid100) + tm_polygons(fill = "road_class_2_5000", breaks=breaks_rcl2_5000, palette=palette_rcl2_5000, legend.show = FALSE)
legende_rcl2_5000 <- tm_shape(grid100) + tm_fill(col = "road_class_2_5000", title = "                        road_class_2_5000", breaks=breaks_rcl2_5000, palette=palette_rcl2_5000, legend.is.portrait = FALSE) + tm_layout(legend.only = T, fontfamily = "serif")
tmap_save(rcl2_5000, width = 3000, height = 3000, units="px", filename = file.path(output_local_predictors_dir,"rcl2_5000.png"))
tmap_save(legende_rcl2_5000, width = 1000, height = 1000, units="px", filename = file.path(output_local_predictors_dir,"Legende_rcl2_5000.png"))
