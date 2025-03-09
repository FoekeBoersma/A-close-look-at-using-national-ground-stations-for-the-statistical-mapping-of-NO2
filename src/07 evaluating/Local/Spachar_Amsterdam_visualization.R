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

#import dataset
grid100 = st_read(all_models_dir)

#define color palette
palette = c("red", "yellow", "green")

#visualize using tmap
map <- tm_shape(grid100) + 
  tm_polygons("spachar", 
              palette = palette ,
              border.lwd = NA,
              legend.show = FALSE,
              borders = NULL
              
                )
#save figure
tmap_save(map, width = 1000, height = 1000, units="px", filename = file.path(out_location_dir, "spachar.jpg"))

#map figure apart as this figure is applicable to multiple figures
legende_spachar <- tm_shape(grid100) + tm_fill(col = "spachar", title = "                        Spatial group",  palette=palette, legend.is.portrait = FALSE) + tm_layout(legend.only = T, fontfamily = "serif")
tmap_save(legende_spachar, width = 1000, height = 1000, units="px", filename = file.path(out_location_dir,"Legende_spachar.jpg"))
