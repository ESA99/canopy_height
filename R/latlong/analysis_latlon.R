# Lat Lon Analysis
library(terra)
library(stringr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(sf)
library(tmap)


# Data --------------------------------------------------------------------

d <- list.dirs("/data/ESA99/lat_lon_results/", full.names = TRUE, recursive = FALSE)
d

tif_files <- list.files(d[11], pattern = "\\.tif$", full.names = TRUE)
# tif_files <- files[!grepl("\\.tif\\.aux", files)] # Exclude .tif.aux.xml 

# Read them all into a list of SpatRaster objects
rasters <- lapply(tif_files, rast)

# Optionally stack them into a single SpatRaster (multi-layer)
raster_stack <- rast(rasters)



# Extract coordinates -----------------------------------------------------

layer_names <- names(raster_stack)

# Extract numeric latitude and longitude
lat_abs <- as.numeric(str_extract(layer_names, "(?<=Lat:)[-0-9]+"))
lon_abs <- as.numeric(str_extract(layer_names, "(?<=Lon:)[-0-9]+"))

coord_extract_wgs84 <- function(raster){
  
  ext <- ext(raster_stack)
  top_left <- c(x = ext[1], y = ext[4])
  
  xy <- matrix(c(x = ext[1], y = ext[4]), ncol=2)
  colnames(xy) <- c("x","y")
  
  pts <- vect(xy, crs=crs(raster_stack))
  latlon <- project(pts, "EPSG:4326")
  
  ext <- ext(latlon)
  top_left <- round(c(x = ext[1], y = ext[4]), digits = 1)
  
  return(top_left)
}
b <- coord_extract_wgs84(raster_stack)

lat_abs[is.na(lat_abs)] <- as.numeric(b[2])
lon_abs[is.na(lon_abs)] <- as.numeric(b[1])

coord_info <- tibble(
  layer = layer_names,
  lat_abs = lat_abs,
  lon_abs = lon_abs
)


# Analysis ----------------------------------------------------------------

# A simple summary for a single tile to compare it globally
# Men height and SD for comparison to itself
# Diff to original calculations should be performed later too!
summary_df <- tibble(
  layer = names(raster_stack),
  mean_pred = global(raster_stack, "mean", na.rm = TRUE)[,1],
  sd_pred   = global(raster_stack, "sd", na.rm = TRUE)[,1]
) %>%
  left_join(coord_info, by = "layer")

summary_sf <- st_as_sf(summary_df, coords = c("lon_abs", "lat_abs"), crs = 4326)

original_point <- summary_df %>% filter(grepl("_original", layer))

# Maps --------------------------------------------------------------------

# World map with SD magnitude
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80") +
  geom_point(data = summary_df,
             aes(x = lon_abs, y = lat_abs, color = mean_pred, size = sd_pred)) +
  geom_point(data = original_point, aes(x = lon_abs, y = lat_abs),
             color = "#01DC03",   shape = 0,    size = 7, stroke = 2) +
  scale_color_viridis_c(option = "C") +
  scale_size_continuous(range = c(2, 6)) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude",
       color = "Mean prediction",
       size = "Std. dev.",
       title = "Prediction sensitivity by global position | Tile: Australia")


# # Map of mean prediction height for a single tile at all 58 locations
# ggplot(summary_df, aes(x = lon_abs, y = lat_abs, color = mean_pred)) +
#   geom_point(size = 4) +
#   scale_color_viridis_c(option = "C") +
#   coord_equal() +
#   theme_minimal() +
#   labs(x = "Longitude (°)", y = "Latitude (°)",
#        color = "Mean prediction",
#        title = "Prediction sensitivity by global position")



# Plots -----------------------------------------------------------

# Are there latitudinal or longitudinal trends in the mean prediction height averaged over all points?

# Latitudinal trend (average over longitude)
lat_trend <- summary_df %>%
  group_by(lat_abs) %>%
  summarise(mean_pred = mean(mean_pred, na.rm = TRUE))

ggplot(lat_trend, aes(x = lat_abs, y = mean_pred)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  theme_minimal() +
  labs(x = "Latitude (°)", y = "Mean prediction", title = "Latitudinal gradient")

# Longitudinal trend (average over latitude)
lon_trend <- summary_df %>%
  group_by(lon_abs) %>%
  summarise(mean_pred = mean(mean_pred, na.rm = TRUE))

ggplot(lon_trend, aes(x = lon_abs, y = mean_pred)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  theme_minimal() +
  labs(x = "Longitude (°)", y = "Mean prediction", title = "Longitudinal gradient")







