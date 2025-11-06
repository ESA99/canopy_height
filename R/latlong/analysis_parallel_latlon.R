#### Parallel calculation of Lat Lon results ####

# Full Data analysis Lat-Lon
packages <- c("terra", "sf", "tmap", "ggplot2", "dplyr", "stringr", "tibble", "purrr", "progressr", "furrr",
              "ggspatial", "rnaturalearth", "rnaturalearthdata")
lapply(packages, library, character.only = TRUE)


base_dir <- "/data/ESA99/lat_lon_results/"
tile_dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)

extract_latlon <- function(names_vec, raster_stack) {
  lat_vals <- as.numeric(str_extract(names_vec, "(?<=Lat:)-?\\d+"))
  lon_vals <- as.numeric(str_extract(names_vec, "(?<=Lon:)-?\\d+"))
  
  # If missing (original tif), estimate using top-left corner in WGS84
  coord_extract_wgs84 <- function(raster_stack) {
    ext <- ext(raster_stack)
    xy <- matrix(c(x = ext[1], y = ext[4]), ncol = 2, dimnames = list(NULL, c("x","y")))
    pts <- vect(xy, crs = crs(raster_stack))
    latlon <- project(pts, "EPSG:4326")
    round(c(x = ext(latlon)[1], y = ext(latlon)[4]), 1)
  }
  
  b <- coord_extract_wgs84(raster_stack)
  lat_vals[is.na(lat_vals)] <- as.numeric(b[2])
  lon_vals[is.na(lon_vals)] <- as.numeric(b[1])
  
  tibble(LAT = lat_vals, LON = lon_vals)
}


# A) Parallel processing -----------------------------------------------------

# Set up parallel plan (use all but 10 cores)
plan(multicore, workers = parallel::detectCores() - 30) # Correct way for LINUX not multisession (windows)

process_single_tile <- function(tile_dir) {
  tile_name <- basename(tile_dir)
  message("Processing tile: ", tile_name)
  
  tifs <- list.files(tile_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(tifs) == 0) return(NULL)
  
  rasters <- lapply(tifs, rast)
  raster_stack <- rast(rasters)
  layer_names <- basename(tools::file_path_sans_ext(tifs))
  names(raster_stack) <- layer_names
  
  coord_info <- extract_latlon(layer_names, raster_stack)
  
  orig_idx <- which(str_detect(layer_names, "_original"))
  if (length(orig_idx) != 1) { 
    warning("No unique original raster found for tile: ", tile_name) 
    return(NULL)
  }
  
  original_raster <- raster_stack[[orig_idx]]
  
  results <- vector("list", length(tifs))
  counter <- 1
  
  pb <- txtProgressBar(min = 0, max = length(tifs), style = 3)
  
    for (t in seq_along(tifs)) {
      # if (t == orig_idx) next # length -1 if enabled
      
      lyr_name <- layer_names[t]
      current_raster <- raster_stack[[t]]
      
      difference <- current_raster - original_raster
      eps <- 1e-6
      difference_percent <- ((current_raster - original_raster) /
                               ((abs(current_raster) + abs(original_raster)) / 2 + eps)) * 100
      
      avg_diff <- global(difference, fun = "mean", na.rm = TRUE)[[1]]
      avg_abs_diff <- global(abs(difference), fun = "mean", na.rm = TRUE)[[1]]
      correlation <- cor(values(current_raster), values(original_raster),
                         method = "pearson", use = "complete.obs") |> as.numeric()
      std_dev <- global(difference, fun = "sd", na.rm = TRUE)[[1]]
      avg_percent_diff <- global(difference_percent, fun = "mean", na.rm = TRUE)[[1]]
      avg_abs_percent_diff <- global(abs(difference_percent), fun = "mean", na.rm = TRUE)[[1]]
      
      results[[counter]] <- tibble(
        tile = tile_name,
        name = lyr_name,
        original = ifelse(t == orig_idx, TRUE, FALSE),
        lat = coord_info$LAT[t],
        lon = coord_info$LON[t],
        orig_lat = coord_info$LAT[orig_idx],
        orig_lon = coord_info$LON[orig_idx],
        average_difference = avg_diff,
        avg_abs_diff = avg_abs_diff,
        avg_differece_percent = avg_percent_diff,
        avg_abs_diff_perc = avg_abs_percent_diff,
        correlation = correlation,
        std_dev = std_dev
      )
      
      counter <- counter + 1
      setTxtProgressBar(pb, t)
    }
  
  close(pb)
  bind_rows(results)
}

start_time <- Sys.time()
summary_global <- future_map_dfr(tile_dirs, process_single_tile, .progress = TRUE)
end_time <- Sys.time()

cat("Total computation time: ", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")


# B) Parallel computing with global progress -------------------------------------

handlers(global = TRUE)
handlers("txtprogressbar") # Or "progress" for nicer console bar

plan(multisession, workers = parallel::detectCores() - 10)

process_single_tile <- function(tile_dir) {
  tile_name <- basename(tile_dir)
  tifs <- list.files(tile_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(tifs) == 0) return(NULL)
  
  rasters <- lapply(tifs, rast)
  raster_stack <- rast(rasters)
  layer_names <- basename(tools::file_path_sans_ext(tifs))
  names(raster_stack) <- layer_names
  
  coord_info <- extract_latlon(layer_names, raster_stack)
  orig_idx <- which(str_detect(layer_names, "_original"))
  if (length(orig_idx) != 1) return(NULL)
  
  original_raster <- raster_stack[[orig_idx]]
  
  results <- vector("list", length(tifs))
  
  # Get progressor passed in from outer scope
  p <- progressor(along = seq_along(tifs))
  
  counter <- 1
  for (t in seq_along(tifs)) {
    # if (t == orig_idx) next
    current_raster <- raster_stack[[t]]
    difference <- current_raster - original_raster
    eps <- 1e-6
    difference_percent <- ((current_raster - original_raster) /
                             ((abs(current_raster) + abs(original_raster)) / 2 + eps)) * 100
    
    avg_diff <- global(difference, fun = "mean", na.rm = TRUE)[[1]]
    avg_abs_diff <- global(abs(difference), fun = "mean", na.rm = TRUE)[[1]]
    correlation <- cor(values(current_raster), values(original_raster),
                       method = "pearson", use = "complete.obs") |> as.numeric()
    std_dev <- global(difference, fun = "sd", na.rm = TRUE)[[1]]
    avg_percent_diff <- global(difference_percent, fun = "mean", na.rm = TRUE)[[1]]
    avg_abs_percent_diff <- global(abs(difference_percent), fun = "mean", na.rm = TRUE)[[1]]
    
    results[[counter]] <- tibble(
      tile = tile_name,
      name = layer_names[t],
      original = ifelse(t == orig_idx, TRUE, FALSE),
      lat = coord_info$LAT[t],
      lon = coord_info$LON[t],
      orig_lat = coord_info$LAT[orig_idx],
      orig_lon = coord_info$LON[orig_idx],
      average_difference = avg_diff,
      avg_abs_diff = avg_abs_diff,
      avg_differece_percent = avg_percent_diff,
      avg_abs_diff_perc = avg_abs_percent_diff,
      correlation = correlation,
      std_dev = std_dev
    )
    
    counter <- counter + 1
    p() # increment global progress
  }
  bind_rows(results)
}

# Wrap the whole call in progressr::with_progress
with_progress({
  p <- progressor(steps = length(tile_dirs))
  summary_global <- future_map_dfr(tile_dirs, function(tile_dir) {
    res <- process_single_tile(tile_dir)
    p()
    res
  })
})



# c) pbmcapply parallel computing -----------------------------------------
library(pbmcapply)
# Function: Process one tile 
process_tile <- function(tile_dir) {
  tile_name <- basename(tile_dir)
  message("Processing tile: ", tile_name)
  
  tifs <- list.files(tile_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(tifs) == 0) return(NULL)
  
  # Load all rasters
  rasters <- lapply(tifs, rast)
  raster_stack <- rast(rasters)
  layer_names <- basename(tools::file_path_sans_ext(tifs))
  names(raster_stack) <- layer_names
  
  # Get Lat/Lon Info
  coord_info <- extract_latlon(layer_names, raster_stack)
  
  # Identify original raster
  orig_idx <- which(str_detect(layer_names, "_original"))
  if (length(orig_idx) != 1) {
    warning("No unique original raster found for tile: ", tile_name)
    return(NULL)
  }
  original_raster <- raster_stack[[orig_idx]]
  
  # Iterate through all rasters in this tile
  results <- vector("list", length(tifs))
  
  for (t in seq_along(tifs)) {
    lyr_name <- layer_names[t]
    current_raster <- raster_stack[[t]]
    
    # Pixel-wise difference
    difference <- current_raster - original_raster
    eps <- 1e-6
    difference_percent <- ((current_raster - original_raster) /
                             ((abs(current_raster) + abs(original_raster)) / 2 + eps)) * 100
    
    # Statistics
    avg_diff <- global(difference, fun = "mean", na.rm = TRUE)[[1]]
    avg_abs_diff <- global(abs(difference), fun = "mean", na.rm = TRUE)[[1]]
    correlation <- cor(values(current_raster), values(original_raster), 
                       method = "pearson", use = "complete.obs") |> as.numeric()
    std_dev <- global(difference, fun = "sd", na.rm = TRUE)[[1]]
    avg_percent_diff <- global(difference_percent, fun = "mean", na.rm = TRUE)[[1]]
    avg_abs_percent_diff <- global(abs(difference_percent), fun = "mean", na.rm = TRUE)[[1]]
    
    results[[t]] <- tibble(
      tile = tile_name,
      name = lyr_name,
      original = (t == orig_idx),
      lat = coord_info$LAT[t],
      lon = coord_info$LON[t],
      orig_lat = coord_info$LAT[orig_idx],
      orig_lon = coord_info$LON[orig_idx],
      average_difference = avg_diff,
      avg_abs_diff = avg_abs_diff,
      avg_differece_percent = avg_percent_diff,
      avg_abs_diff_perc = avg_abs_percent_diff,
      correlation = correlation,
      std_dev = std_dev
    )
  }
  
  bind_rows(results)
}

# Parallel deployment
start_time <- Sys.time()

# Use all available cores minus one
num_cores <- parallel::detectCores() - 10
summary_global_list <- pbmclapply(tile_dirs, process_tile, mc.cores = num_cores)
summary_global <- bind_rows(summary_global_list)

end_time <- Sys.time()
cat("Total computation time: ", 
    round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")



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
