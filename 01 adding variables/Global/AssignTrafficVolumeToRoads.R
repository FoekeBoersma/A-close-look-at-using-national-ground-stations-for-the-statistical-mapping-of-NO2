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

## == DEFINE COORDINATE SYSTEMS == ##
crs <- CRS("+proj=longlat +datum=WGS84") # crs
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs_32 <- CRS(sprintf(utmStr, 32))


### === GERMANY === ###

#import motorways and primary roads in Germany

#NOTE: secondary roads are excluded due to the limited traffic counting stations that are
#situated at secondary roads. Therefore, the results for the secondary roads are prone
#to high uncertainty.

roads_Germany <- readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/Germany/Motorway_Primary_Germany.shp')
#make spatial
roads_Germany_sf <- st_as_sf(roads_Germany)


#decrease dataset size and only keep relevant columns
roads_Germany_sf <- roads_Germany_sf %>% dplyr::select(type, geometry) #type = 'Motorway' or 'Primary'

## == TRAFFIC DATA == ## (point features)

#Germany traffic data (Jawe) - Average daily traffic (Mo-So), All traffic type, measured over 2017.

Jawe <- readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Jawe_processed.shp')
#convert to sf
Jawe_sf <- st_as_sf(Jawe)
#rename
Jawe_sf <- Jawe_sf %>% rename(AverageDailyTraffic = AvrgHrT)

#export option
#sf::st_write(spdf, dsn='C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject21/Germany/complete',layer='Jawe_TrafficVolume', driver = "ESRI Shapefile")



# # = potentially: fix geometry issues related to road shapefiles = #
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


## INITIALIZE LOOP AND NECESSARY ELEMENTS
i=1 #set first element that will be used 
dis_points = 1000  #points every x meters
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
  ##  First project data into a planar coordinate system (here UTM zone 32)
  roads_UTM <- spTransform(spatial_lines_roadtype, CRS = crs_32)
  
  #insert extra points
  numOfPoints  <-  gLength(roads_UTM) / dis_points
  extra_points <- spsample(roads_UTM, n = numOfPoints, type = "regular")
  st_extra_points <- st_as_sf(extra_points) #convert to spatial dataframe
  
  #convert back to geographic coordination system
  crs_gcs <- 4326 #define desired gcs
  st_extra_points <- st_transform(st_extra_points, crs = crs_gcs) #apply on relevant layer
  
  #export option
  layer_ep <- paste('ExtraPoints', 'motorway', sep = " ")
  print(layer_ep)
  
  #sf::st_write(st_extra_points, dsn='C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Germany',layer=layer_ep, driver = "ESRI Shapefile")
  
  #SELECT RELEVANT POINTS PER ROAD TYPE - OPTION A (VIA SPATIAL SELECTION)
  
  #from the traffic counting stations in the area of interest, select only
  #the ones that intersect with motorways
  
  line <- spTransform(spatial_lines_roadtype, crs_32)
  buf <- buffer(line, width = bufsize) #initialized buffer parameter is relevant here.
  
  sf_buf <- st_as_sf(buf) #convert to spatial dataframe to perform spatial operations
  sf_pts <- st_as_sf(pts_in)  #convert to spatial dataframe to perform spatial operations
  sf_buf <- st_transform(sf_buf, crs = crs) #apply on relevant layer
  
  Jewe_roadtype <- sf_pts[sf_buf,] #select points that intersect with specific roadtype
  
  ## == ASSIGN TRAFFIC COUNTING STATION VALUES TO EXTRA CREATED POINTS == ##
  
  #to get  better coverage, and potentially less skewed results, more point values contain traffic counting station values. Assigning the original traffic counting station
  #values to the newly created points will be done in this part of code, depending in the road type(hence "Jewe_roadtype").
  
  #identify nearest point feature to point feature
  nearest_points <- st_nearest_feature(st_extra_points, Jewe_roadtype)
  
  #assign traffic values to line dataset, based on nearest distance
  trafficval_points_roadtype = cbind(st_extra_points, st_drop_geometry(Jewe_roadtype)[nearest_points,])
  
  
  ## == ASSIGN POINT VALUES TO LINES (ROADS) == ##
  
  #convert relevant variables to spatial dataframes - the now many points containing traffic volume values will be assigned to the closest road segment
  sf_line_roadtype <- st_as_sf(road_type) 
  sf_tv_points_rt <- st_as_sf(trafficval_points_roadtype) 
  
  #identify nearest point feature to line feature
  nearest_poi_toline <- st_nearest_feature(sf_line_roadtype, sf_tv_points_rt)
  
  #assign traffic values to line dataset, based on nearest distance
  linepoi_join = cbind(sf_line_roadtype, st_drop_geometry(sf_tv_points_rt)[nearest_poi_toline,])
  
  #store variable in loop
  result_variables[[i]] <- linepoi_join
  
  #export option
  layer <- paste('TrafficVolume', i, sep = " ")
  print(layer)
  
  sf::st_write(linepoi_join, dsn='C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Germany',layer=layer, driver = "ESRI Shapefile")
  
}

summary(result_variables) #examine if assigning to list went well.

#combine the dataframes in the list into one dataset
roads_combined_GER <- rbind(result_variables[[1]], result_variables[[2]])
#view(linepoi_join) #option to examine

#export option
sf::st_write(roads_combined_GER, dsn='C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Germany',layer='TrafficVolume_RoadsGermany', driver = "ESRI Shapefile")



### === NETHERLANDS === ###





## == IMPORT ROADS NETHERLANDS (MOTORWAY AND PRIMARY) == ##
roadsNL <- readOGR(dsn = "C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/NL/motorway_primary_NL.shp")

## == IMPORT TRAFFIC COUNTING STATIONS == ##
## == A: VALUES 
tc_stations_RotDH <- read.csv(file = 'C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/NL/ndw/intensiteit-snelheid-export-RotterdamDenHaag.csv', sep= ",")
tc_stations_zuid <- read.csv(file = 'C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/NL/ndw/intensiteit-snelheid-export-zuid.csv', sep = ',')
tc_stations_noord <- read.csv(file = 'C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/NL/ndw/intensiteit-snelheid-export-noord.csv', sep = ',')
tc_stations_AmsWest <- read.csv(file = 'C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/NL/ndw/intensiteit-snelheid-export-Amsterdam-West.csv', sep = ',')
tc_stations_AmsOost <- read.csv(file = 'C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/NL/ndw/intensiteit-snelheid-export-Amsterdam-Oost.csv', sep = ',')
tc_stations_AmsNoord <- read.csv(file = 'C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/NL/ndw/intensiteit-snelheid-export-Amsterdam-Noord.csv', sep = ',')
tc_stations_overig <- read.csv(file = 'C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/NL/ndw/intensiteit-snelheid-export-overig.csv', sep = ',')

tc_stations <- rbind(tc_stations_RotDH, tc_stations_zuid, tc_stations_noord,
                     tc_stations_AmsWest, tc_stations_AmsOost, 
                     tc_stations_AmsNoord, tc_stations_overig)

#export option
#write.csv(tc_stations_join,"C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject21/NL/raw/tc_stations.csv")

#filter to "anyVehicle"
tc_stations_av <- tc_stations[tc_stations$voertuigcategorie == 'anyVehicle',]
view(tc_stations_av)

#merge lanes into 1 point, representing the total value of the different lanes. The gem intensiteit values, measured in average hourly traffic, now are per month, per station
tc_stations_av_1 <- tc_stations_av %>% group_by(id_meetlocatie, start_meetperiode, eind_meetperiode )%>% summarise(gem_intensiteit = sum(gem_intensiteit))
#verify
view(tc_stations_av_1)


# #aggregate the traffic data based on the counting station's id to create average hourly traffic, measured over a year (2017)
tc_stations_avght <- tc_stations_av_1 %>%
  group_by(id_meetlocatie) %>%
  summarize(AverageHourlyTraffic = sum(gem_intensiteit) / 12)

view(tc_stations_avght)

## == B: COORDINATES COUNTING STATIONS
loc_tc_stations <- readOGR(dsn = "C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/NL/Telpunten_WGS84.shp")
loc_tc_stations <- as.data.frame(loc_tc_stations)
loc_tc_stations <- loc_tc_stations %>% rename(id_meetlocatie = dgl_loc)

#transform to spatial dataframe
tc_stations_xy <- st_as_sf(loc_tc_stations, coords=c("POINT_X", "POINT_Y"))
st_crs(tc_stations_xy) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"

# sf::st_write(tc_stations_xy, dsn="C:/Users/foeke/OneDrive/Documenten/ArcGIS/Projects/MyProject21/NL/raw", layer="telpunten", driver = "ESRI Shapefile")

#assign xy coors to traffic counting stations
tc_stations_xy <- merge(tc_stations_avght,loc_tc_stations,by.tc_stations_avght="id_meetlocatie", by.loc_tc_stations = "id_meetlocatie")
#transform to spatial dataframe
tc_stations_xy <- st_as_sf(tc_stations_xy, coords=c("POINT_X", "POINT_Y"))
st_crs(tc_stations_xy) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"

#export option
sf::st_write(tc_stations_xy, dsn="C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Netherlands", layer="tc_stations_NL", driver = "ESRI Shapefile")



## == Assign traffic volume to Motorway and Primary roads == ##

#initialize parameters for for loop
road_types = c('motorway', 'primary') #road types considered
dis_points = 1000  #points every x meters
bufsize = 10 #in meters - necessary to perform spatial selection as point-to-line operations do not yield reliable results
result_variables = list() #road types considered

#loop
for(i in road_types)
{
  road_type <- roadsNL[roadsNL$type == i, ]
  print(i)
  ## == create extra points == ##
  
  
  # # #convert to class "SpatialLines"
  # spatial_lines_roadtype <- sf:::as_Spatial(road_type)
  
  
  ##  First project data into a planar coordinate system (here UTM zone 32)
  roads_UTM <- spTransform(road_type, CRS = crs_32)
  
  #insert extra points
  numOfPoints  <-  gLength(roads_UTM) / dis_points #every x meters
  extra_points <- spsample(roads_UTM, n = numOfPoints, type = "regular")
  st_extra_points <- st_as_sf(extra_points) #convert to spatial dataframe
  
  #convert back to geographic coordination system
  crs_gcs <- 4326 #define desired gcs
  st_extra_points <- st_transform(st_extra_points, crs = crs_gcs) #apply on relevant layer
  
  #export option
  layer_ep <- paste('ExtraPoints',  i, sep = " ")
  print(layer_ep)
  
  sf::st_write(st_extra_points, dsn='C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Netherlands',layer=layer_ep, driver = "ESRI Shapefile")
  
  #SELECT RELEVANT POINTS PER ROAD TYPE - OPTION A (VIA SPATIAL SELECTION)
  
  #from the traffic counting stations in the area of interest, select only
  #the ones that intersect with specified road type
  
  #as calculations are performed, planar coordination system is required.
  
  line <- spTransform(road_type, crs_32)
  buf <- buffer(line, width = bufsize) #initialized buffer parameter is applied here
  
  sf_buf <- st_as_sf(buf) #convert to spatial dataframe to perform spatial operations
  sf_pts <- st_as_sf(tc_stations_xy)  #convert to spatial dataframe to perform spatial operations
  sf_buf <- st_transform(sf_buf, crs = crs_gcs) #apply on relevant layer
  
  NDW_roadtype <- sf_pts[sf_buf,] #select points that intersect with specific roadtype
  
  ## == ASSIGN TRAFFIC COUNTING STATION VALUES TO EXTRA CREATED POINTS == ##
  
  #to get  better coverage, and potentially less skewed results, more point values contain traffic counting station values. Assigning the original traffic counting station
  #values to the newly created points will be done in this part of code, depending in the road type(hence "Jewe_roadtype").
  
  #identify nearest point feature to point feature
  nearest_points <- st_nearest_feature(st_extra_points, NDW_roadtype)
  
  #assign traffic values to line dataset, based on nearest distance
  trafficval_points_roadtype = cbind(st_extra_points, st_drop_geometry(NDW_roadtype)[nearest_points,])
  
  
  ## == ASSIGN POINT VALUES TO LINES (ROADS) == ##
  
  #convert relevant variables to spatial dataframes - the now many points containing traffic volume values will be assigned to the closest road segment
  sf_line_roadtype <- st_as_sf(road_type) 
  sf_tv_points_rt <- st_as_sf(trafficval_points_roadtype) 
  
  #identify nearest point feature to line feature
  nearest_poi_to_line <- st_nearest_feature(sf_line_roadtype, sf_tv_points_rt)
  
  #assign traffic values to line dataset, based on nearest distance
  linepoi_join = cbind(sf_line_roadtype, st_drop_geometry(sf_tv_points_rt)[nearest_poi_to_line,])
  
  #store variable in loop
  result_variables[[i]] <- linepoi_join
  
  #export option
  layer <- paste('TrafficVolume', i, sep = " ")
  print(layer)
  
  sf::st_write(linepoi_join, dsn='C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Netherlands',layer=layer, driver = "ESRI Shapefile")
  
  
  
}

#combine the dataframes in the list into one dataset
roads_combined_NL <- rbind(result_variables[[1]], result_variables[[2]])
#view(roads_combined) #option to examine

#export option
sf::st_write(roads_combined_NL, dsn='C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22/Netherlands',layer='TrafficVolume_RoadsNL', driver = "ESRI Shapefile")


## == Combine Traffic Volume on German roads with Traffic Volume on Dutch Roads via merge operation == ##
TrafficVolume_StudyArea = merge(TrafficVolume_RoadsGER, TrafficVolume_RoadsNL)
#export option
sf::st_write(roads_combined_NL, dsn='C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Traffic_May2022/ARCGIS/MyProject22',layer='TrafficVolume_StudyArea', driver = "ESRI Shapefile")
