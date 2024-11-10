library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(rgdal)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library("ggplot2")
library("GGally")
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

#to datadrame
grid100_df <- as.data.frame(grid100)

# Add fill layer to nz shape
grid100 <- st_as_sf(grid100)


colnames(grid100_df)

View(grid100_df)

grid100_df <- grid100_df %>% rename("Li"  ="predNO2_Lin",
                                    "LiSpa"= "predNO2_LinSep",
                                    "MEM" = "predNO2_MEM",
                                    "Uk" = "predNO2_UK",
                                    "UkSpa" = "predNO2_UKSep",
                                    "Ok" = "predicted_OK",
                                    "RF" =  "predicted_NO2_RF",
                                    "Las" = "predicted_NO2_LASSO", 
                                    "Rid" = "predicted_NO2_RIDGE", 
                                    "Lgb" = "predicted_NO2_LightGBM",
                                    "Xgb" = "predicted_NO2_XGBoost"
  
)

colnames(grid100_df)


# ## == scatter plot == ##


grid100_df_models <- grid100_df[,c("RF","Lgb","Xgb","Las","Rid","Li","LiSpa","MEM","Uk","UkSpa","Ok","no2")]

local <- grid100_df[,c("Li","LiSpa","MEM","Uk","UkSpa","Ok")]
#export local models to csv
write.csv(local, file.path(out_location_dir, 'localmodel_predictions.csv'))

#examine statistics per model
summary(grid100_df_models)

grid100_df_noNAs = na.omit(grid100_df_models)

# # use jpg() instead of svg(), if you want PDF output
pm <- ggpairs(grid100_df_noNAs[, c("RF","Lgb","Xgb","Las","Rid","Li","LiSpa","MEM","Uk","UkSpa","Ok","no2")], 
              upper = list(continuous = GGally::wrap(ggally_cor))) +
                       theme(axis.text.x = element_text(angle =90, hjust = 1)) 
ggsave(file.path(out_location_dir, "allplot-includingNO2tif.jpeg"), pm, width = 6, height = 6)


if (dev.cur() > 1) dev.off()  # Only close if a device is open

## == adjusted raanges/correcting for outliers == ##

#create for loop that for every model filters rows and keep the ones with values between 0 and 85.

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
}
#examine
processed_data


between0_85_allmodels <- Reduce(function(x,y) merge(x, y, by = "key", all.x = TRUE, all.y = TRUE),
                         list(between0_85RF, between0_85Lgb,between0_85Xgb,between0_85Las,between0_85Rid,
                        between0_85Li, between0_85LiSpa, between0_85MEM, between0_85Uk, between0_85UkSpa, between0_85Ok, between0_85no2))


pm <- ggpairs(between0_85_allmodels[, c("RF","Lgb","Xgb","Las","Rid","Li","LiSpa","MEM","Uk","UkSpa","Ok","no2")], 
              upper = list(continuous = GGally::wrap(ggally_cor))) +
  theme(axis.text.x = element_text(angle =90, hjust = 1)) 

ggsave(file.path(out_location_dir, "allplot-includingNO2tif-between0_85.jpeg"), pm, width = 6, height = 6)



if (dev.cur() > 1) dev.off()  # Only close if a device is open

colnames(grid100)

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
  map <- tm_shape(grid100) + tm_fill(col = vars[i], breaks=breaks, palette=palette, legend.show = FALSE)
  model = vars[i]
  tmap_save(map, width = 1000, height = 1000, units="px", filename = file.path(out_location_dir, paste0(model, ".jpg")))

}

#legend visualization
legende <- tm_shape(grid100) + tm_fill(col = "predNO2_Lin", title = "                        Predicted NO2", breaks=breaks, palette=palette, legend.is.portrait = FALSE) + tm_layout(legend.only = T, fontfamily = "serif")
tmap_save(legende, width = 1000, height = 1000, units="px", filename = file.path(out_location_dir, "Legende.jpg"))

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
    filename <- file.path(out_location_dir, paste0("local_predictors_", predictor, ".jpg"))
    
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

rcl2_5000 = tm_shape(grid100) + tm_fill(col = "road_class_2_5000", breaks=breaks_rcl2_5000, palette=palette_rcl2_5000, legend.show = FALSE)
legende_rcl2_5000 <- tm_shape(grid100) + tm_fill(col = "road_class_2_5000", title = "                        road_class_2_5000", breaks=breaks_rcl2_5000, palette=palette_rcl2_5000, legend.is.portrait = FALSE) + tm_layout(legend.only = T, fontfamily = "serif")
tmap_save(rcl2_5000, width = 1000, height = 1000, units="px", filename = file.path(out_location_dir,"rcl2_5000.jpg"))
tmap_save(legende_rcl2_5000, width = 1000, height = 1000, units="px", filename = file.path(out_location_dir,"Legende_rcl2_5000.jpg"))
