library(base)
library(dplyr)
library(purrr)
library(sf)
library(raster)
# library("writexl")
library(yaml)

# Connect to YAML file
current_dir <- rstudioapi::getActiveDocumentContext()$path
config_dir <- dirname(current_dir) # One level up in directory
config07_path <- file.path(config_dir, "config_07.yml")

# Read YAML configuration file
config07 <- yaml::yaml.load_file(config07_path)
# Define the parent directory (move four levels up)
parent_directory <- dirname(dirname(dirname(current_dir)))
# Paths for input data based on YAML configuration
Amsterdam_NO2PredictionPerModel_dir <- normalizePath(file.path(parent_directory, config07$input_data$Amsterdam_NO2PredictionPerModel), winslash = "/")
# Import global dataset as an sf object, projected onto Amsterdam (if shapefile format is used)
global <- st_read(Amsterdam_NO2PredictionPerModel_dir)

## == (geo)processing == ##
rename_columns <- FALSE  # Set to FALSE if renaming is not needed / i.e. not a shapefile

if (rename_columns) {
  global <- global %>%
    rename(
      predicted_NO2_RF = p_NO2_RF,
      predicted_NO2_LASSO = p_NO2_LA,
      predicted_NO2_RIDGE = p_NO2_RI,
      predicted_NO2_LightGBM = p_NO2_LG,
      predicted_NO2_XGBoost = p_NO2_X
    )
}

# Define output directory
out_location_dir <- normalizePath(file.path(parent_directory, config07$out_location), winslash = "/")

# Create a new folder 'all_models_output' inside the output directory
all_models_output_dir <- file.path(out_location_dir, "all_models_output")

# Check if the folder exists; if not, create it
if (!dir.exists(all_models_output_dir)) {
  dir.create(all_models_output_dir, recursive = TRUE)
}

# Update output location directory
out_location_dir <- all_models_output_dir

global_df <- as.data.frame(global)

colnames(global_df)

global_RF <- global_df$predicted_NO2_RF
global_xgboost <- global_df$predicted_NO2_XGBoost
global_lightgbm <- global_df$predicted_NO2_LightGBM
global_lasso <- global_df$predicted_NO2_LASSO
global_ridge <- global_df$predicted_NO2_RIDGE

global_models <- list(global_RF, global_xgboost, global_lightgbm, global_lasso, global_ridge)

for(model in global_models){
  print(summary(model))
}

#local
predictedNO2_Linear_dir <- normalizePath(file.path(parent_directory, config07$input_data$predictedNO2_Linear), winslash = "/")
predictedNO2_Linear_SeparatingSpatialGroups_dir <- normalizePath(file.path(parent_directory, config07$input_data$predictedNO2_Linear_SeparatingSpatialGroups), winslash = "/")
predictedNO2_MEM_dir <- normalizePath(file.path(parent_directory, config07$input_data$predictedNO2_MEM), winslash = "/")
predictedNO2_UK_formula_dir <- normalizePath(file.path(parent_directory, config07$input_data$predictedNO2_UK_formula), winslash = "/")
predictedNO2_UK_SeparatingSpatialGroups_dir <- normalizePath(file.path(parent_directory, config07$input_data$predictedNO2_UK_SeparatingSpatialGroups), winslash = "/")
predictedNO2_OK_dir <- normalizePath(file.path(parent_directory, config07$input_data$predictedNO2_OK), winslash = "/")


linear = st_read(predictedNO2_Linear_dir)
linear_separated = st_read(predictedNO2_Linear_SeparatingSpatialGroups_dir)
MEM = st_read(predictedNO2_MEM_dir)
UK = st_read(predictedNO2_UK_formula_dir)
UK_separated = st_read(predictedNO2_UK_SeparatingSpatialGroups_dir)
OK = st_read(predictedNO2_OK_dir)

#make spatial - obtain the geometry for each sample
grid100_sf <- st_as_sf(linear)
print(colnames(grid100_sf))
geo <- grid100_sf[,c("key", "geom")]

#to dataframe - for every local dataset
linear_df <- as.data.frame(linear)
linear_separated_df <- as.data.frame(linear_separated)
MEM_df <- as.data.frame(MEM)
UK_df <- as.data.frame(UK)
UK_separated_df <- as.data.frame(UK_separated)
OK_df <- as.data.frame(OK)

#merge local dataframes to one dataframe, containing all predictions 
merge_local = list(linear_df, linear_separated_df, MEM_df, UK_df, UK_separated_df, OK_df)
merge_local <- merge_local %>% reduce(full_join, by= 'key')

#rename
merge_local <- merge_local %>% rename("nightlight_450" = "nightlight_450.x",
                                      
                                      "nightlight_4950" = "nightlight_4950.x",
                                      "population_1000" = "population_1000.x",
                                      "population_3000" = "population_3000.x"
                                      ,"road_class_1_5000" = "road_class_1_5000.x",
                                      "road_class_2_100" = "road_class_2_100.x",
                                      "road_class_2_1000" = "road_class_2_1000.x",
                                      "road_class_1_100" = "road_class_1_100.x" ,
                                      "road_class_2_5000" = "road_class_2_5000.x",
                                      "road_class_3_100" = "road_class_3_100.x",
                                      "road_class_3_300" = "road_class_3_300.x",
                                      "trafBuf50" = "trafBuf50.x")

#filter to only relevant data
local <- merge_local[,c("nightlight_450" ,"nightlight_4950" ,"population_1000","population_3000" ,"road_class_1_5000","road_class_2_100" ,
                  "road_class_2_1000","road_class_1_100" ,"road_class_2_5000","road_class_3_100","road_class_3_300" ,"trafBuf50" ,"spachar", "key","predNO2_Lin",
                  "predNO2_LinSep" , "predNO2_MEM" , "predNO2_UK" , "predNO2_UKSep", 'predicted_OK' )]

local_sf <- merge(geo, local, by="key")
local_df <- as.data.frame(local)

#to excel
# write_xlsx(local_df, file.path(out_location_dir,"Local_df.xlsx"))

## == export option == ##
#sf::st_write(local_sf, dsn=file.path(out_location_dir,"local_predictions.gpkg"), driver = "GPKG")

## == assign NO2 tif data to local models == ##

#OG_cen <- gCentroid(OverlayGrid,byid=TRUE)
local_cen <- st_centroid(local_sf,byid=TRUE)

#NO2tif

no2tif_dir <- normalizePath(file.path(parent_directory, config07$input_data$no2tif), winslash = "/")
NO2tif <- (no2tif_dir)
NO2tif=raster(NO2tif)

#plot(NO2tif)

#spatially join
local_and_no2tif = raster::extract(NO2tif, local_cen, sp=T) #sp = T: keep all data
#to dataframe
local_and_no2tif_df <- as.data.frame(local_and_no2tif)
#to grid
local_and_no2tif_grid <- merge(geo, local_and_no2tif_df, by="key")
local_and_no2tif_grid = subset(local_and_no2tif_grid, select = -c(coords.x1,coords.x2) )

## == export option == ##
sf::st_write(local_and_no2tif_grid, dsn=file.path(out_location_dir,"local_and_no2tif_grid.gpkg"), driver = "GPKG")
global_sf <- st_as_sf(global)
# global_sf <- global_sf[, c("predicted_NO2_RF", "predicted_NO2_LASSO", "predicted_NO2_RIDGE","predicted_NO2_LightGBM", "predicted_NO2_XGBoost")]


all_models <- st_join(local_and_no2tif_grid, global_sf, largest = T, left = T)
print(colnames(all_models))

# Extract non-geometry columns
non_geom_cols <- setdiff(colnames(all_models), "geom")

# Identify columns ending with .x or .y
x_cols <- grep("\\.x$", non_geom_cols, value = TRUE)
y_cols <- grep("\\.y$", non_geom_cols, value = TRUE)

# Get base column names (without .x or .y)
base_names <- unique(gsub("\\.x$|\\.y$", "", c(x_cols, y_cols)))

# Select columns to keep (excluding .y versions)
columns_to_keep <- setdiff(non_geom_cols, y_cols)

# Rename .x columns to their base names
new_col_names <- setNames(columns_to_keep, gsub("\\.x$", "", columns_to_keep))

# Preserve geometry column separately
geom_col <- st_geometry(all_models)



# Convert `sf` object to `data.frame` and clean up columns
all_models_cleaned <- all_models %>%
  st_drop_geometry() %>%  # Remove spatial attributes temporarily
  dplyr::select(all_of(columns_to_keep)) %>%  # Keep only required columns
  dplyr::rename(!!!new_col_names)  # Rename .x columns

# Convert back to `sf` object
all_models <- st_as_sf(all_models_cleaned, geometry = geom_col)

sf::st_write(all_models, dsn=file.path(out_location_dir,"all_models.gpkg"), driver = "GPKG")
write_xlsx(all_models, file.path(out_location_dir,"all_models.xlsx"))