# Full Data analysis LatLon

library(terra)
library(stringr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(sf)
library(tmap)

# ---- Settings ----
base_dir <- "/data/ESA99/lat_lon_results/"

# Get list of all tile folders
tile_dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)

# Function to process a single tile folder
process_tile <- function(tile_dir) {
  
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

# ---- Run for all tiles ----
all_summaries <- lapply(tile_dirs, process_tile)
summary_global <- bind_rows(all_summaries)

# Optional: save as CSV
# write.csv(summary_global, "/data/ESA99/global_summary.csv", row.names = FALSE)

# ---- Plot global overview ----
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80") +
  geom_point(data = summary_global,
             aes(x = lon_abs, y = lat_abs, color = mean_pred, size = sd_pred)) +
  # geom_point(data = subset(summary_global, grepl("_original", layer)),
  #            aes(x = lon_abs, y = lat_abs),
  #            color = "#01DC03", shape = 0, size = 3, stroke = 1.5) +
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
