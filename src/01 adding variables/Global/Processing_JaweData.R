#necessary libraries
library(sf)
library(sfnetworks)
library(tidygraph)
library(tmap)
library(nngeo)
library(osmdata)
library(dplyr)
library(lwgeom)
library(raster)
library(rgdal)
library(utils)
library(ggplot2)
library(rgeos)
library(tidyverse)
library(leaflet) #mapping in OSM
library(bnspatial)
library(gstat)
library(geosphere) #geosphere::dist2Line
library(stars) #for st_rasterize
library(yaml)

## == connect with yaml file == ##

#connect to yaml file
current_dir <- rstudioapi::getActiveDocumentContext()$path
# Move one level up in the directory
config_dir <- dirname(dirname(current_dir))
# Construct the path to the YAML configuration file
config_path <- file.path(config_dir, "config.yml")
# Read the YAML configuration file
config <- yaml.load_file(config_path)

## == define output path == ##
out_location <- config$out_location
out_location_dir <- normalizePath(file.path(parent_directory, out_location ), winslash = "/")

## == TRAFFIC DATA == ## (point features)

# Use dirname() to get the parent directory
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
jawe2017 <- config$global$jawe2017
jawe2017_relative <- normalizePath(file.path(parent_directory, jawe2017), winslash = "/")

#Germany traffic data
trafGER <- read.csv(jawe2017_relative, sep = ';')

#select only relevant columns
trafGER <- trafGER %>% dplyr::select(Str_Kl, Betriebs_km, DTV_Kfz_MobisSo_Q, DTV_Kfz_W_Q, DTV_Kfz_U_Q, DTV_Kfz_S_Q, DTV_SV_WU_MobisFr_Q,
                                     Koor_WGS84_E, Koor_WGS84_N)
#rename certain columns
trafGER <- trafGER %>% rename(AverageDailyTraffic =  DTV_Kfz_MobisSo_Q,
                              WeekdayTraffic = DTV_Kfz_W_Q,
                              HolidayWeekdayTraffic = DTV_Kfz_U_Q,
)


#PROCESSING GEODATA
#get rid of NA values
trafGER_nona <- na.omit(trafGER)

trafGER_nona$lat = as.numeric(gsub(",","",trafGER_nona$Koor_WGS84_N,fixed=TRUE))
trafGER_nona$long = as.numeric(gsub(",","",trafGER_nona$Koor_WGS84_E,fixed=TRUE))

#convert to coordinates - latitude
trafGER_nona$lat <- sub("^(\\d{2})", "\\1,", trafGER_nona$lat)

#convert to coordinates - longitude
#some rows have 10 characters 
ten <- subset(trafGER_nona, nchar(long) == 10)
ten$long <- sub("^(\\d{2})", "\\1,", ten$long)
#some rows have 9 characters
nine <- subset(trafGER_nona, nchar(long) == 9)
nine$long <- sub("^(\\d{1})", "\\1,", nine$long)

#merge datasets of nine and ten characters
total_long <- rbind(nine, ten)

#processing coordinates - convert from comma to point
total_na <- na.omit(total_long)
total_na$long <- gsub(",", ".", total_na$long)
total_na$lat <- gsub(",", ".", total_na$lat)

#nummeric format is needed
total_na$long <- as.numeric(total_na$long)
total_na$lat <- as.numeric(total_na$lat)

#change name
Jawe_processed = total_na

#calculate Average Hourly Traffic
Jawe_processed$AverageDailyTraffic <- Jawe_processed$AverageDailyTraffic <- as.numeric(gsub("\\.","",Jawe_processed$AverageDailyTraffic))

Jawe_processed$AverageHourlyTraffic <- Jawe_processed$AverageDailyTraffic /24

#minimize columns 
Jawe_processed <- Jawe_processed[,c("AverageHourlyTraffic", "long", "lat")]

coords <- Jawe_processed[ , c("long", "lat")]
crs <- CRS("+proj=longlat +datum=WGS84") # crs

# make the SpatialPointsDataFrame object
spdf <- SpatialPointsDataFrame(coords      = coords,
                               data        = Jawe_processed, 
                               proj4string = crs)

## == export options == ##

#to sf format
Jawe_processed_sf <- st_as_sf(spdf)

crs(Jawe_processed_sf)

#export to spatial data, this case shapefile. for geopackage, use 'gpkg' after dataname.
sf::st_write(Jawe_processed_sf, dsn=out_location_dir,layer='Jawe_processed', driver = "ESRI Shapefile")

#csv
write.csv(Jawe_processed, out_location_dir + 'Jawe_processed.csv')
