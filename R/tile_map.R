# Plot Tiles overview
{library(sf)
library(tmap)
library(terra)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)}

# Data prep ---------------------------------------------------------------

final_tiles <- st_read("R/data/final_tile_selection.gpkg")


tile_label <- c("55HEV" = "Australia", "20MMD" = "Brazil", "33NTG" = "Cameroon", "32UQU" = "Germany", 
                "35VML" = "Finland", "49NHC" = "Malaysia", "49UCP" = "Mongolia", 
                "34UFD" = "Poland", "32TMT" = "Switzerland", "10TES" = "USA East", "17SNB" = "USA West")


final_tiles <- final_tiles %>%
  mutate(Location = tile_label[Name])



# Maps --------------------------------------------------------------------

# tm_basemap("OpenTopoMap") +
# tm_basemap("Esri.WorldImagery")+
# tm_basemap("OpenStreetMap")+
tm_basemap("Esri.WorldGrayCanvas") +
  tm_shape(final_tiles) +
  tm_borders(col = "red", lwd = 2) +
  tm_labels("Location", size = 0.8, col = "black", options = opt_tm_labels(point.label.gap = 0.6)) # lables



bbox <- st_bbox(final_tiles)
# Expand by 5% on all sides
expand_factor <- 0.05
bbox_expanded <- bbox + c(-expand_factor, -expand_factor, expand_factor, expand_factor) * (bbox[c("xmax","ymax","xmax","ymax")] - bbox[c("xmin","ymin","xmin","ymin")])

tm_basemap("Esri.WorldGrayCanvas") +
  tm_shape(final_tiles, bbox = bbox_expanded) +
  tm_borders(col = "red", lwd = 2) +
  tm_labels("Location", size = 0.8, col = "black", options = opt_tm_labels(point.label.gap = 0.61)) # lables
