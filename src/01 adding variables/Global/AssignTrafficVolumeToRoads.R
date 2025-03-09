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
library(base) #sprintf
library(sfheaders) #converting to multistring
library(yaml)

## == DEFINE COORDINATE SYSTEMS == ##
crs <- CRS("+proj=longlat +datum=WGS84") # crs

#connect to yaml file
current_dir <- rstudioapi::getActiveDocumentContext()$path
# Move one level up in the directory
config_dir <- dirname(dirname(current_dir))
# Construct the path to the YAML configuration file
config_path <- file.path(config_dir, "config.yml")
# Read the YAML configuration file
config <- yaml.load_file(config_path)

# Use dirname() to get the parent directory
parent_directory <- dirname(dirname(dirname(dirname(current_dir))))
roads_Germany <- config$global$roads_Germany
roads_Germany_relative <- normalizePath(file.path(parent_directory, roads_Germany ), winslash = "/")

## == define output path == ##
out_location <- config$out_location
out_location_dir <- normalizePath(file.path(parent_directory, out_location ), winslash = "/")

### === GERMANY === ###

#import motorways and primary roads in Germany

#NOTE: secondary roads are excluded due to the limited traffic counting stations that are
#situated at secondary roads. Therefore, the results for the secondary roads are prone
#to high uncertainty.

#import motorways and primary roads in Germany
roads_Germany <- st_read(roads_Germany_relative)

#make spatial via sf package
roads_Germany_sf <- st_as_sf(roads_Germany)

#decrease dataset size and only keep relevant columns
roads_Germany_sf <- roads_Germany_sf %>% dplyr::select(type, geometry) #type = 'Motorway' or 'Primary'

#assign coordinate reference system, in first instance use WGS84 so that transformation to local crs goes without problems
roads_Germany_sf <- st_transform(roads_Germany_sf, crs=crs)
#transform to local crs for planar projection
roads_Germany_3035 <- st_transform(roads_Germany_sf, crs = 3035)

# #optionally, export
#sf::st_write(oads_Germany_3035, dsn=out_location_dir, layer=roads_Germany.shp, driver = "ESRI Shapefile")

#decrease dataset size and only keep relevant columns
roads_Germany_3035 <- roads_Germany_3035 %>% dplyr::select(type, geometry) #type = 'Motorway' or 'Primary'

## == TRAFFIC DATA == ## (point features)
jawe_relative <- config$global$jawe
#Germany traffic data (Jawe) - Average daily traffic (Mo-So), All traffic type, measured over 2017.
Jawe <- st_read(jawe_relative)

#convert to sf
Jawe_sf <- st_as_sf(Jawe, coords = c("long", "lat"))
Jawe_sf
Jawe_sf <- Jawe_sf %>% rename("AverageHourlyTraffic" = "AvrgHrT")

#export option
#sf::st_write(spdf, dsn=out_location_dir,layer='Jawe_TrafficVolume', driver = "ESRI Shapefile")

# ## == potentially: fix geometry issues related to road shapefiles == ##
# 
# #create new column for filtering purposes
# roads$filter <- ifelse(roads$type=='primary', 0,1 ) 
# 
# #as 'motorway' is "sfc_GEOMETRYCOLLECTION" "sfc", conversion to multistring is necessary.
# motorway <- roads[roads$filter == 1, ]
# #convert from geometry collection to only line
# motorway_line <- st_collection_extract(motorway, "LINESTRING")
# 
# #also create seperate dataset for primary roads
# primary <- roads[roads$filter == 0, ]
# #fix geometry issues
# primary = st_make_valid(primary)
# 
# #merge datasets
# roads_merge <- rbind(primary, motorway_line)


## == INITIALIZE LOOP AND NECESSARY ELEMENTS == ##

i=1 #set first element that will be used 
distance_points = 1000  #points every x meters
bufsize = 10 #in meters - necessary to perform spatial selection as point-to-line operations do not yield reliable results
result_variables = list() #store loop results to list
road_types = c("motorway", "primary") #road types considered

#FOR LOOP
for(i in road_types)
{
  #select specific road type
  road_type <- roads_Germany_sf[roads_Germany_sf$type == i, ]
  #verify
  print(i)
  ## == create extra points == ##
  
  # #convert to class "SpatialLines"
  spatial_lines_roadtype <- sf:::as_Spatial(road_type)

  proj4string(spatial_lines_roadtype) <- CRS("+proj=longlat +datum=WGS84")
  ##  First project data into a planar coordinate system (3035)
  roads_UTM <- spTransform(spatial_lines_roadtype, CRS = "+init=epsg:3035")

  #insert extra points
  numberOfPoints  <-  gLength(roads_UTM) / distance_points
  extra_points <- spsample(roads_UTM, n = numberOfPoints, type = "regular")
  st_extra_points <- st_as_sf(extra_points) #convert to spatial dataframe
  st_extra_points <- st_transform(st_extra_points, crs=3035)
  
  #export option
  layer_extra_points <- paste('ExtraPointsGermany3035', i, sep = " ") 
  print(layer_extra_points)
  
  sf::st_write(st_extra_points, dsn=out_location_dir,layer=layer_extra_points, driver = "ESRI Shapefile")
  
  ## == SELECT RELEVANT POINTS PER ROAD TYPE == ##
  
  #from the traffic counting stations in the area of interest, select only
  #the ones that intersect with motorways
  
  spatial_lines_roadtype <- spTransform(spatial_lines_roadtype, "+init=epsg:3035") #converting to this spatial format is necessary for specific data operations
  buffer_roadtype <- buffer(spatial_lines_roadtype, width = bufsize) #initialized buffer parameter is relevant here.
  
  sf_buffer_roadtype <- st_as_sf(buffer_roadtype) #convert to spatial dataframe to perform spatial operations
  sf_buffer_roadtype <- st_transform(sf_buffer_roadtype, crs=3035)
  sf_points_Jawe <- st_as_sf(Jawe_sf)  #convert to spatial dataframe to perform spatial operations
  sf_points_Jawe <- st_transform(sf_points_Jawe, crs=3035)
  
  #export option
  layer_sf_buffer_roadtype<- paste('sf_buffer_roadtype', i, sep = " ") 
  print(layer_sf_buffer_roadtype)
  
  
  sf::st_write(sf_buffer_roadtype, dsn=out_location_dir, layer = layer_sf_buffer_roadtype, driver = "ESRI Shapefile")
  
  Jawe_roadtype <- sf_points_Jawe[sf_buffer_roadtype,] #select points that intersect with specific roadtype
  
  layer_Jawe_roadtype<- paste('Jawe_roadtype', i, sep = " ") 
  print(layer_Jawe_roadtype)
  
  sf::st_write(Jawe_roadtype, dsn=out_location_dir,layer=layer_Jawe_roadtype, driver = "ESRI Shapefile")

  ## == ASSIGN TRAFFIC COUNTING STATION VALUES TO EXTRA CREATED POINTS == ##
  
  #to get  better coverage, and potentially less skewed results, more point values contain traffic counting station values. Assigning the original traffic counting station
  #values to the newly created points will be done in this part of code, depending in the road type(hence "Jawe_roadtype").

  #identify nearest point feature to point feature
  nearest_points <- st_nearest_feature(st_extra_points, Jawe_roadtype)
  
  #assign traffic values to line dataset, based on nearest distance
  trafficvalues_points_roadtype = cbind(st_extra_points, st_drop_geometry(Jawe_roadtype)[nearest_points,])
  
  layer_trafficvalues_points_roadtype<- paste('trafficvalues_points_roadtype', i, sep = " ") 
  print(layer_trafficvalues_points_roadtype)
  sf::st_write(trafficvalues_points_roadtype, dsn=out_location_dir,layer=layer_trafficvalues_points_roadtype, driver = "ESRI Shapefile")
  
  ## == ASSIGN POINT VALUES TO LINES (ROADS) == ##
  
  #convert relevant variables to spatial dataframes - the now many points containing traffic volume values will be assigned to the closest road segment
  sf_line_roadtype <- st_as_sf(road_type)
  sf_line_roadtype <- st_transform(sf_line_roadtype, crs=3035)
  sf_traffic_values_points_rt <- st_as_sf(trafficvalues_points_roadtype)

  #identify nearest point feature to line feature
  nearest_poi_toline <- st_nearest_feature(sf_line_roadtype, sf_traffic_values_points_rt)
  
  #assign traffic values to line dataset, based on nearest distance
  linepoints_join = cbind(sf_line_roadtype, st_drop_geometry(sf_traffic_values_points_rt)[nearest_poi_toline,])
  
  #store variable in loop
  result_variables[[i]] <- linepoints_join
  
  #export option
  layer_trafficVolumeGermany_roadtype <- paste('TrafficVolumeGermany3035', i, sep = " ")
  print(layer_trafficVolumeGermany_roadtype)
  
  sf::st_write(linepoints_join, dsn=out_location_dir,layer=layer_trafficVolumeGermany_roadtype, driver = "ESRI Shapefile")
  
}


summary(result_variables) #examine if assigning to list went well.

#combine the dataframes in the list into one dataset
TrafficVolume_RoadsGermany <- rbind(result_variables[[1]], result_variables[[2]])

#export option
sf::st_write(TrafficVolume_RoadsGermany, dsn=out_location_dir,layer='TrafficVolume_RoadsGermany', driver = "ESRI Shapefile")


### === NETHERLANDS === ###

## == IMPORT ROADS NETHERLANDS (MOTORWAY AND PRIMARY) == ##

motorway_primary_NL <- config$global$motorway_primary_NL
motorway_primary_NL_relative <- normalizePath(file.path(parent_directory, motorway_primary_NL ), winslash = "/")


roadsNL <- st_read(dsn = motorway_primary_NL_relative)

## == IMPORT TRAFFIC COUNTING STATIONS == ##
## == A: VALUES 
snelheid_export_RotterdamDenHaag <- config$global$snelheid_export_RotterdamDenHaag
snelheid_export_RotterdamDenHaag_relative <- normalizePath(file.path(parent_directory,snelheid_export_RotterdamDenHaag), winslash = "/")
tc_stations_RotDH <- read.csv(file = snelheid_export_RotterdamDenHaag_relative, sep= ",")


snelheid_export_zuid <- config$global$snelheid_export_zuid
snelheid_export_zuid_relative <- normalizePath(file.path(parent_directory, snelheid_export_zuid), winslash = "/")
tc_stations_zuid <- read.csv(file = snelheid_export_zuid_relative, sep = ',')

snelheid_export_noord <- config$global$snelheid_export_noord
snelheid_export_noord_relative <- normalizePath(file.path(parent_directory, snelheid_export_noord), winslash = "/")
tc_stations_noord <- read.csv(file = snelheid_export_noord_relative, sep = ',')

snelheid_export_AmsterdamWest <- config$global$snelheid_export_AmsterdamWest
snelheid_export_AmsterdamWest_relative <- normalizePath(file.path(parent_directory, snelheid_export_AmsterdamWest ), winslash = "/")
tc_stations_AmsWest <- read.csv(file = snelheid_export_AmsterdamWest_relative, sep = ',')

snelheid_export_AmsterdamOost <- config$global$snelheid_export_AmsterdamOost
snelheid_export_AmsterdamOost_relative <- normalizePath(file.path(parent_directory, snelheid_export_AmsterdamOost), winslash = "/")
tc_stations_AmsOost <- read.csv(file = snelheid_export_AmsterdamOost_relative, sep = ',')


snelheid_export_AmsterdamNoord <- config$global$snelheid_export_AmsterdamNoord
snelheid_export_AmsterdamNoord_relative <- normalizePath(file.path(parent_directory, snelheid_export_AmsterdamNoord), winslash = "/")
tc_stations_AmsNoord <- read.csv(file = snelheid_export_AmsterdamNoord_relative, sep = ',')

snelheid_export_overig <- config$global$snelheid_export_overig
snelheid_export_overig_relative <- normalizePath(file.path(parent_directory, snelheid_export_overig), winslash = "/")
tc_stations_overig <- read.csv(file = snelheid_export_overig_relative, sep = ',')

#combine the different road parts of the Netherlands into one dataset via row bind.
traffic_counting_stations <- rbind(tc_stations_RotDH, tc_stations_zuid, tc_stations_noord,
                     tc_stations_AmsWest, tc_stations_AmsOost, 
                     tc_stations_AmsNoord, tc_stations_overig)

#export option
write.csv(traffic_counting_stations,out_location_dir+"tc_stations.csv")

#filter to "anyVehicle"
tc_stations_any_vehicle <- traffic_counting_stations[traffic_counting_stations$voertuigcategorie == 'anyVehicle',]

#merge lanes into 1 point, representing the total value of the different lanes. The gem intensiteit values, measured in average hourly traffic, now are per month, per station
tc_stations_any_vehicle_merged <- tc_stations_any_vehicle %>% group_by(id_meetlocatie, start_meetperiode, eind_meetperiode )%>% summarise(gem_intensiteit = sum(gem_intensiteit))

# #aggregate the traffic data based on the counting station's id to create average hourly traffic, measured over a year (2017)
tc_stations_avght <- tc_stations_any_vehicle_merged %>%
  group_by(id_meetlocatie) %>%
  summarize(AverageHourlyTraffic = sum(gem_intensiteit) / 12)


## == B: COORDINATES COUNTING STATIONS
Telpunten_WGS84<- config$global$Telpunten_WGS84
Telpunten_WGS84_relative <- normalizePath(file.path(parent_directory, Telpunten_WGS84), winslash = "/")
location_tc_stations <- st_read(dsn = Telpunten_WGS84_relative)
location_tc_stations <- as.data.frame(location_tc_stations)
location_tc_stations <- location_tc_stations %>% rename(id_meetlocatie = dgl_loc)

#transform to spatial dataframe
tc_stations_xy <- st_as_sf(location_tc_stations, coords=c("POINT_X", "POINT_Y"))
st_crs(tc_stations_xy) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"

sf::st_write(tc_stations_xy, dsn=out_location_dir, layer="telpunten", driver = "ESRI Shapefile")

## == combine traffic data with coords == ##

#assign xy coords to traffic counting stations
tc_stations_xy <- merge(tc_stations_avght,location_tc_stations,by.tc_stations_avght="id_meetlocatie", by.location_tc_stations = "id_meetlocatie")
#transform to spatial dataframe
tc_stations_xy <- st_as_sf(tc_stations_xy, coords=c("POINT_X", "POINT_Y"))
st_crs(tc_stations_xy) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"

#export option
sf::st_write(tc_stations_xy, dsn=out_location_dir, layer="tc_stations_NL", driver = "ESRI Shapefile")

tc_stations_xy <- st_as_sf(tc_stations_xy, coords = c("crds_x1", "crds_x2"))

## == Assign traffic volume to Motorway and Primary roads == ##

#initialize parameters for for loop
road_types = c('motorway', 'primary')
distance_points = 1000  #points every x meters
bufsize = 10 #in meters - necessary to perform spatial selection as point-to-line operations do not yield reliable results
result_variables = list()

class(road_type)

for(i in road_types)
{
  road_type <- roadsNL[roadsNL$type == i, ]
  print(i)
  ## == create extra points == ##

  ##  First project data into a planar coordinate system (here 3035)
  roads_UTM <- spTransform(road_type, CRS = "+init=epsg:3035")
  
  #insert extra points
  numberOfPoints  <-  gLength(roads_UTM) / distance_points #every x meters
  extra_points <- spsample(roads_UTM, n = numberOfPoints, type = "regular")
  st_extra_points <- st_as_sf(extra_points) #convert to spatial dataframe
  
  #convert back to geographic coordination system
  crs_gcs <- 4326 #define desired gcs
  st_extra_points <- st_transform(st_extra_points, crs = crs_gcs) #apply on relevant layer
  
  #export option
  layer_extra_points <- paste('ExtraPointsNetherlands3035',  i, sep = " ")
  print(layer_extra_points)
  
  sf::st_write(st_extra_points, dsn=out_location_dir,layer=layer_extra_points, driver = "ESRI Shapefile")
  
  ## == SELECT RELEVANT POINTS PER ROAD TYPE - OPTION A (VIA SPATIAL SELECTION) == ##
  
  #from the traffic counting stations in the area of interest, select only
  #the ones that intersect with specified road type
  
  #as calculations are performed, planar coordination system is required.
  
  spatial_lines_roadtype <- spTransform(road_type, "+init=epsg:3035")
  buffer_roadtype <- buffer(spatial_lines_roadtype, width = bufsize) #initialized buffer parameter is applied here
  
  sf_buffer_roadtype <- st_as_sf(buffer_roadtype) #convert to spatial dataframe to perform spatial operations
  sf_points_NDW <- st_as_sf(tc_stations_xy)  #convert to spatial dataframe to perform spatial operations
  sf_buffer_roadtype <- st_transform(sf_buffer_roadtype, crs = crs_gcs) #apply on relevant layer
  
  sf::sf_use_s2(FALSE) #for resolving spherical geometry failures
  NDW_roadtype <- sf_points_NDW[sf_buffer_roadtype,] #select points that intersect with specific roadtype
  
  ## == ASSIGN TRAFFIC COUNTING STATION VALUES TO EXTRA CREATED POINTS == ##
  
  #to get  better coverage, and potentially less skewed results, more point values contain traffic counting station values. Assigning the original traffic counting station
  #values to the newly created points will be done in this part of code, depending in the road type(hence "NDW_roadtype").
  
  #identify nearest point feature to point feature
  nearest_points <- st_nearest_feature(st_extra_points, NDW_roadtype)
  
  #assign traffic values to line dataset, based on nearest distance
  trafficvalues_points_roadtype = cbind(st_extra_points, st_drop_geometry(NDW_roadtype)[nearest_points,])
  
  
  ## == ASSIGN POINT VALUES TO LINES (ROADS) == ##
  
  #convert relevant variables to spatial dataframes - the now many points containing traffic volume values will be assigned to the closest road segment
  sf_line_roadtype <- st_as_sf(road_type) 
  sf_traffic_values_points_rt <- st_as_sf(trafficvalues_points_roadtype) 
  
  #identify nearest point feature to line feature
  nearest_poi_to_line <- st_nearest_feature(sf_line_roadtype, sf_traffic_values_points_rt)
  
  #assign traffic values to line dataset, based on nearest distance
  linepoints_join = cbind(sf_line_roadtype, st_drop_geometry(sf_traffic_values_points_rt)[nearest_poi_to_line,])
  linepoints_join <- st_transform(linepoints_join, crs = 3035) #convert to planar coordinates
  #store variable in loop
  result_variables[[i]] <- linepoints_join
  
  #export option
  layer_trafficVolumeNetherlands_roadtype <- paste('TrafficVolumeNetherlands3035', i, sep = " ")
  print(layer_trafficVolumeNetherlands_roadtype)
  
  sf::st_write(linepoints_join, dsn=out_location_dir,layer=layer_trafficVolumeNetherlands_roadtype, driver = "ESRI Shapefile")
  
  
}

#combine the dataframes in the list into one dataset
TrafficVolume_RoadsNL <- rbind(result_variables[[1]], result_variables[[2]])

#export option
sf::st_write(TrafficVolume_RoadsNL, dsn=out_location_dir,layer='TrafficVolume_RoadsNL', driver = "ESRI Shapefile")

## == Combine Traffic Volume on German roads with Traffic Volume on Dutch Roads via merge operation == ##
TrafficVolume_RoadsNL

TrafficVolume_RoadsGermany <- as.data.frame(TrafficVolume_RoadsGermany)
TrafficVolume_RoadsGermany <- TrafficVolume_RoadsGermany[, c('type', 'AverageHourlyTraffic', 'geometry')]
TrafficVolume_RoadsNL <- as.data.frame(TrafficVolume_RoadsNL)
TrafficVolume_RoadsNL <- TrafficVolume_RoadsNL[, c('type', 'AverageHourlyTraffic', 'geometry')]
TrafficVolume_StudyArea = rbind(TrafficVolume_RoadsGermany, TrafficVolume_RoadsNL) #merge roads from Germany with Netherlands
#make spatial again since geometry info is still apparent
TrafficVolume_StudyArea <- st_as_sf(TrafficVolume_StudyArea) 
#export option
sf::st_write(TrafficVolume_StudyArea, dsn=out_location_dir,layer='TrafficVolume_StudyArea', driver = "ESRI Shapefile")
