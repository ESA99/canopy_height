# Lat Lon Analysis of a single tile
# Read and combine all prediction tifs into a raster stack, extract coordinates from names
# Calculate mean heigth of the prediciton
library(terra)
library(stringr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(sf)
library(tmap)

###############################################################################
            ##### ATTENTION: ONLY MEAN HEIGHT CALCULATED HERE #####            
###############################################################################


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

# Get coordinates of original location
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



# FUNCTION Single Tile processing -----------------------------------------

base_dir <- "/data/ESA99/lat_lon_results/"
tile_dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)

latlon_singleTile_process <- function(tile_dir) {
  
  tile_name <- basename(tile_dir)
  message("Processing tile: ", tile_name)
  
  # Get .tif files
  tif_files <- list.files(tile_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(tif_files) == 0) return(NULL)
  
  # Read all rasters
  rasters <- lapply(tif_files, rast)
  raster_stack <- rast(rasters)
  
  # Layer names come from filenames (remove extensions)
  layer_names <- basename(tools::file_path_sans_ext(tif_files))
  names(raster_stack) <- layer_names
  
  # Extract lat/lon info from filenames
  lat_vals <- as.numeric(str_extract(layer_names, "(?<=Lat:)-?\\d+"))
  lon_vals <- as.numeric(str_extract(layer_names, "(?<=Lon:)-?\\d+"))
  
  # Get original raster coords if NA
  coord_extract_wgs84 <- function(raster_stack){
    ext <- ext(raster_stack)
    xy <- matrix(c(x = ext[1], y = ext[4]), ncol = 2, dimnames = list(NULL, c("x","y")))
    pts <- vect(xy, crs = crs(raster_stack))
    latlon <- project(pts, "EPSG:4326")
    round(c(x = ext(latlon)[1], y = ext(latlon)[4]), 1)
  }
  
  b <- coord_extract_wgs84(raster_stack)
  lat_vals[is.na(lat_vals)] <- as.numeric(b[2])
  lon_vals[is.na(lon_vals)] <- as.numeric(b[1])
  
  # Summaries for each layer
  summary_df <- tibble(
    tile = tile_name,
    layer = layer_names,
    mean_pred = global(raster_stack, "mean", na.rm = TRUE)[,1],
    sd_pred = global(raster_stack, "sd", na.rm = TRUE)[,1],
    lat_abs = lat_vals,
    lon_abs = lon_vals
  )
  
  return(summary_df)
}

all_summaries <- lapply(tile_dirs, latlon_singleTile_process)
summary_global <- bind_rows(all_summaries)

# write.csv(summary_global, "/data/ESA99/global_summary.csv", row.names = FALSE)

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


ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80") +
  geom_point(data = summary_global,
             aes(x = lon_abs, y = lat_abs, color = mean_pred, size = sd_pred)) +
  geom_point(data = subset(summary_global, grepl("_original", layer)),
             aes(x = lon_abs, y = lat_abs),
             color = "#01DC03", shape = 0, size = 3, stroke = 1.5) +
  scale_color_viridis_c(option = "C") +
  scale_size_continuous(range = c(1, 6)) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Global prediction sensitivity by location",
    subtitle = paste("Tiles processed:", length(tile_dirs)),
    x = "Longitude", y = "Latitude",
    color = "Mean prediction",
    size = "Std. dev."
  )


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







