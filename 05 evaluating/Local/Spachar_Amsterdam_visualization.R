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


grid100 = readOGR('C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels/all_models.gpkg')
# grid100_df <- as.data.frame(grid100)
# colnames(grid100_df)
# 
# grid100_df$spachar
# 
# grid100_sf <- st_as_sf(grid100_df)

palette = c("red", "yellow", "green")

map <- tm_shape(grid100) + 
  tm_polygons("spachar", 
              palette = palette ,
              border.lwd = NA,
              legend.show = FALSE,
              borders = NULL
              
                )

tmap_save(map, width = 1000, height = 1000, units="px", filename = "C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/spachar.jpg")

legende_spachar <- tm_shape(grid100) + tm_fill(col = "spachar", title = "                        Spatial group",  palette=palette, legend.is.portrait = FALSE) + tm_layout(legend.only = T, fontfamily = "serif")
tmap_save(legende_spachar, width = 1000, height = 1000, units="px", filename = "C:/Users/foeke/OneDrive/Documenten/april onwards/2022/Initial dataset/ForPredicting/LocalModels - Predicting/MAPS FINAL SCRIPTS - LOCAL/Legende_spachar.jpg")
