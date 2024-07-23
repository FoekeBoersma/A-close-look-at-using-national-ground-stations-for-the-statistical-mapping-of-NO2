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

#import dataset
grid100 = readOGR('C:/Users/foeke/OneDrive/Documenten/submitting paper/TooBigData/AllModels/all_models.gpkg')

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
tmap_save(map, width = 1000, height = 1000, units="px", filename = "C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/output07/spachar.jpg")

#map figure apart as this figure is applicable to multiple figures
legende_spachar <- tm_shape(grid100) + tm_fill(col = "spachar", title = "                        Spatial group",  palette=palette, legend.is.portrait = FALSE) + tm_layout(legend.only = T, fontfamily = "serif")
tmap_save(legende_spachar, width = 1000, height = 1000, units="px", filename = "C:/Users/foeke/OneDrive/Documenten/submitting paper/testing_script_outputs/output07/Legende_spachar.jpg")
