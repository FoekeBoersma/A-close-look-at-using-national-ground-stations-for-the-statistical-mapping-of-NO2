#install necessary packages
library(magrittr)
library(dplyr)
library(sf)
library(tidyverse) #for separating x,y-values into different columns
#import dataset
library(readr)

## == import geodata == ##
#coordinates
loc <-read.csv('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/NO2_4weken/NO2_LOCATIES.csv', sep=';')
#no2 values
no2ams <- read.csv('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/NO2_4weken/NO2_4WEKEN.csv', sep=';')
#examine
print(no2ams)

## == data processing == ##

#filter dataset: select only 2017 and whereby Lopend Gemiddelde (mean_NO2) is larger than value 0.2. Moreover: drop irrelevant columns
no2ams = no2ams%>%filter(no2ams$Jaar==2017)%>%filter(Lopend_gemiddelde>0.2)%>%select(-WKT_LNG_LAT, -WKT_LAT_LNG, -LAT, -LNG)

no2ams= merge(no2ams,loc,by.x = "CodeW", by.y = "Code") # get coordinates
#make spatial dataframe (sf) - change to crs wgs84
no2ams=no2ams%>%st_as_sf(coords = c("LNG", "LAT"), crs = 4326) 
#examine
head(no2ams)

## == separate x,y-values into separate columns == ##

#extract latitude- and longitude-columns from geometry-column.
separated_coord <- no2ams %>%
  mutate(lat = unlist(map(no2ams$geometry,2)),
         long = unlist(map(no2ams$geometry,1)))
#verify
View(separated_coord)

#drop irrelevant columns in dataframe
cols.dont.want <- c("WKT_LNG_LAT", "WKT_LAT_LNG", "X.y")
verdata <- separated_coord[, ! names(separated_coord) %in% cols.dont.want, drop = F]
#verify
head(verdata)
View(verdata)

#drop irrelevant columns - part II
colnames(verdata)
verdata = subset(verdata, select = -c(ï..OBJECTNUMMER.y, ï..OBJECTNUMMER.x, geometry, X.x, Afstand_tot_gevel_m, Type_meetpunt, Meting_op_wettelijke_toetsafstand, Type_meting, Opmerkingen))
#show remainder of column names
colnames(verdata)


#remove geometry column
verdata <- st_set_geometry(verdata, NULL)
#verify
View(verdata)

#drop rows which contain more than 2 "0"-values in related columns
verdata <- subset(verdata, rowSums(verdata == 0.0) < 3)
#verify
View(verdata)

#export as dataset for verification
write.csv(verdata, "C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForModelling/LocalModels/LocalMeasurementStations.csv")
