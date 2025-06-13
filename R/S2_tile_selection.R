
# View Tiles by continent -------------------------------------------------
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

sentinel_kml <- st_read("/home/emilio/global-canopy-height-model/workbench/data/S2A_OPER_GIP_TILPAR_MPC__20151209T095117_V20150622T000000_21000101T000000_B00.kml", quiet = TRUE)

# Get continents shapefile using rnaturalearth
continents <- ne_countries(scale = "medium", returnclass = "sf") %>%
  group_by(continent) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

# Adjust CRS to match
sentinel_kml <- st_transform(sentinel_kml, st_crs(continents))

# Select continent
unique(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")$continent)
target_continent <- "Europe"
continent_geom <- filter(continents, continent == target_continent)

# Filter tiles by continent
tiles_continent <- sentinel_kml[st_intersects(sentinel_kml, continent_geom, sparse = FALSE), ]

tiles_continent <- st_zm(tiles_continent, drop = TRUE, what = "ZM")         # Drop Z/M dimensions
tiles_continent <- st_collection_extract(tiles_continent, "POLYGON")          # Explode geometry collections into individual parts
tiles_continent <- st_cast(tiles_continent, "MULTIPOLYGON")     

# Plot
tmap_mode("view")
tm_basemap("OpenTopoMap") +
  tm_shape(tiles_continent) +
  tm_borders(col = "red", lwd = 2) +
  tm_text("Name", size = 0.8, col = "black") # lables




# Select Tiles ------------------------------------------------------------
library(sf)
library(leaflet)
library(mapview)
library(mapedit)
library(terra)
library(tmap)

tiles <- st_read("/home/emilio/global-canopy-height-model/workbench/data/S2A_OPER_GIP_TILPAR_MPC__20151209T095117_V20150622T000000_21000101T000000_B00.kml", quiet = TRUE)
# tiles <- sentinel_kml
tiles <- st_zm(tiles, drop = TRUE, what = "ZM")
tiles <- st_collection_extract(tiles, "POLYGON")
tiles <- st_cast(tiles, "MULTIPOLYGON")

selected_tiles <- selectFeatures(tiles)

# st_write(selected_tiles, "R/data/proposed_tiles.gpkg")
# st_write(selected_tiles, "R/data/selected_tiles.geojson")




# Merge with Munich and final proposal ------------------------------------

ms_tiles <- st_read("R/data/proposed_tiles.gpkg")
mu_tiles <- st_read("R/data/S2_Tiles_filtered.geojson")

mu_names <- mu_tiles$Name
ms_names <- ms_tiles$Name
tile_names <- c(mu_names, ms_names)
merged <- tiles[tiles$Name %in% tile_names, ]

tile_names[duplicated(tile_names)]

# st_write(merged, "R/data/combined_tiles.geojson")

# tmap_mode("view")
# tm_basemap("OpenTopoMap") +
tm_basemap("Esri.WorldGrayCanvas") +
  tm_shape(merged[1]) +
  tm_borders(col = "red", lwd = 2) +
  tm_text("Name", size = 0.8, col = "black") # lables



proposed <- st_read("R/data/combined_tiles.geojson")
# biomes <- st_read("/home/emilio/data_storage/Biomes.gpkg")

tmap_mode("view")
tm_shape(proposed) +
  tm_borders(lwd = 4, col = "red") +          # thick red borders
  tm_text("Name", size = 1, just = "center") + # names centered inside tiles
  tm_layout(legend.show = FALSE) +             # no legend
  tm_basemap("Esri.WorldImagery")

# tm_basemap("OpenStreetMap")


### Create Overview table
library(dplyr)
library(elevatr)

centroids <- st_centroid(merged$geometry)
latitudes <- st_coordinates(centroids)[, "Y"]  # Extract latitude (Y coordinate) from centroids

centroids <- st_centroid(merged)  # keep the sf structure
countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
centroids_with_country <- st_join(centroids, countries[c("admin", "continent")])

elevation_data <- get_elev_point(centroids, src = "aws")  # src could be "aws" or "epqs"
elevations <- elevation_data$elevation # Extract elevation values

result_df <- data.frame(
  Continent = centroids_with_country$continent,
  Latitude = round(latitudes, 1),
  Name = merged$Name,
  Country = centroids_with_country$admin,
  Centeroid_Elevation = elevations
)

# Create info of proposal group
all_names <- merged$Name
source_info <- sapply(all_names, function(n) {
  in_mu <- n %in% mu_names
  in_ms <- n %in% ms_names
  
  if (in_mu & in_ms) {
    "BOTH"
  } else if (in_mu) {
    "MU"
  } else if (in_ms) {
    "MS"
  } else {
    NA  # or "UNKNOWN" if you want to flag something unexpected
  }
})

result_df$Source <- source_info # add to data frame

result_df_sorted <- result_df %>%
  arrange(Continent)

print(result_df_sorted)

# write.csv(result_df_sorted, "R/data/combined_tiles.csv", row.names = FALSE)



