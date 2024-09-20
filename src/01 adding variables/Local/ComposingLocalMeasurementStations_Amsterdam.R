# Install necessary packages
library(magrittr)
library(dplyr)
library(sf)
library(tidyverse)  # for separating x, y-values into different columns
library(readr)
library(yaml)

## == Connect with YAML file == ##

# Get the current script directory
current_dir <- rstudioapi::getActiveDocumentContext()$path

# Move up one level in the directory
config_dir <- dirname(dirname(current_dir))

# Path to YAML config file
config_path <- file.path(config_dir, "config.yml")

# Read the YAML configuration file
config <- yaml.load_file(config_path)

# Define the parent directory
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))

## == Load datasets using YAML file paths == ##

# Reference to the correct dataset paths from YAML
no2_locaties_dataset <- config$local$no2_locaties
no2_locaties_map_dir <- normalizePath(file.path(parent_directory, no2_locaties_dataset), winslash = "/")

# Load the NO2 location dataset
loc <- read.csv(file = no2_locaties_map_dir, sep = ";")

# Load the NO2 4-week dataset
no2_4weken_dataset <- config$local$no2_4weken
no2_4weken_map_dir <- normalizePath(file.path(parent_directory, no2_4weken_dataset), winslash = "/")
no2ams <- read.csv(file = no2_4weken_map_dir, sep = ";")

## == Define output path == ##
out_location <- config$out_location
out_location_dir <- normalizePath(file.path(parent_directory, out_location), winslash = "/")



## == Data Processing == ##

# Filter NO2 data for 2017 and remove irrelevant columns
no2ams <- no2ams %>%
  filter(Jaar == 2017) %>%
  filter(Lopend_gemiddelde > 0.2) %>%
  select(-WKT_LNG_LAT, -WKT_LAT_LNG, -LAT, -LNG)

# Merge NO2 data with location data by code
no2ams <- merge(no2ams, loc, by.x = "CodeW", by.y = "Code")

# Convert the dataset to a spatial dataframe (sf) with CRS WGS84
no2ams <- no2ams %>%
  st_as_sf(coords = c("LNG", "LAT"), crs = 4326)

## == Separate x, y-values into separate columns == ##

# Extract latitude and longitude from geometry column
separated_coord <- no2ams %>%
  mutate(lat = unlist(map(no2ams$geometry, 2)),
         long = unlist(map(no2ams$geometry, 1)))

# Drop irrelevant columns
cols.dont.want <- c("WKT_LNG_LAT", "WKT_LAT_LNG", "X.y")
verdata <- separated_coord[, !names(separated_coord) %in% cols.dont.want, drop = FALSE]

# Drop irrelevant columns - part II
verdata <- subset(verdata, select = -c(`ï..OBJECTNUMMER.y`, `ï..OBJECTNUMMER.x`, geometry, X.x, Afstand_tot_gevel_m, Type_meetpunt, Meting_op_wettelijke_toetsafstand, Type_meting, Opmerkingen))

# Show remaining column names (for debugging)
colnames(verdata)

# Remove the geometry column
verdata <- st_set_geometry(verdata, NULL)

# Drop rows with more than 2 "0"-values in relevant columns
verdata <- subset(verdata, rowSums(verdata == 0.0) < 3)

# Export the processed dataset to the output directory
write.csv(verdata, file.path(out_location_dir, "LocalMeasurementStations.csv"))

