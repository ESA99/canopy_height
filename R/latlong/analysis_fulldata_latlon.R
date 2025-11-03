# DIFF to original mean prediction

library(terra)
library(stringr)
library(dplyr)
library(sf)
library(tibble)
library(rnaturalearth)
library(ggplot2)
library(purrr)

# Set up parallel plan (use all but one core)
library(furrr)
plan(multisession, workers = parallel::detectCores() - 10)

# ---- Settings ----
base_dir <- "/data/ESA99/lat_lon_results/"
tile_dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)

# Function: get lat/lon from filenames
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

# Function: For all tiles analyse all predictions
# Calculates pixel wise difference and then performs statistics -> mean, sd, relative diff ...
global_process <- function(directories){
  
  results <- list()
  
    for (i in seq_along(directories)) {
      tile_name <- basename(directories[i])
      message("Processing tile: ", tile_name)
      
      # Get paths for tile i
      tifs <- list.files(directories[i], pattern = "\\.tif$", full.names = TRUE)
      if (length(tifs) == 0) next
      
      # Read all rasters
      rasters <- lapply(tifs, rast)
      raster_stack <- rast(rasters)
      
      # Get and set names
      layer_names <- basename(tools::file_path_sans_ext(tifs))
      names(raster_stack) <- layer_names
      
      # Get Lat Lon Info
      coord_info <- extract_latlon(layer_names, raster_stack)
      
      # Identify and get original raster
      orig_idx <- which(str_detect(layer_names, "_original"))
      if (length(orig_idx) != 1) { warning("No unique original raster found for tile: ", tile_name) 
        next }
      original_raster <- raster_stack[[orig_idx]]
      
      
      
      pb <- txtProgressBar(min = 0, max = length(tifs)-1, style = 3)
        for (t in seq_along(tifs)) {
          
          lyr_name <- layer_names[t]
          current_raster <- raster_stack[[t]]
          
          # Skip original itself
          if (t == orig_idx) next
          
          # DIFFERECE Calculation (Pixel wise)
          difference <- current_raster - original_raster
          # Difference raster in percent (SMAPE formulation)
          eps <- 1e-6
          difference_percent <- ((current_raster - original_raster) /
                                   ((abs(current_raster) + abs(original_raster)) / 2 + eps)) * 100
          # Statistics
          avg_diff <- global(difference, fun = "mean", na.rm = TRUE)[[1]]
          avg_abs_diff <- global(abs(difference), fun = "mean", na.rm = TRUE)[[1]]
          correlation <- cor(values(current_raster), values(original_raster), method = "pearson", use = "complete.obs") |> as.numeric()
          std_dev <- global(difference, fun = "sd", na.rm = TRUE)[[1]]
          
          avg_percent_diff <- global(difference_percent, fun = "mean", na.rm = TRUE)[[1]]
          avg_abs_percent_diff <- global(abs(difference_percent), fun = "mean", na.rm = TRUE)[[1]]
          
          
          # Store Results
          results[[length(results) + 1]] <- tibble(
            tile = tile_name,
            name = lyr_name,
            lat = coord_info$LAT[t], # LAT of prediction
            lon = coord_info$LON[t], # LON of prediciton
            orig_lat = coord_info$LAT[orig_idx], # Original LatLon of the tile
            orig_lon = coord_info$LON[orig_idx],
            average_difference =    avg_diff,
            avg_abs_diff =           avg_abs_diff,
            avg_differece_percent = avg_percent_diff,
            avg_abs_diff_perc =      avg_abs_percent_diff,
            correlation = correlation , # between moved pred and original pred
            std_dev = std_dev # SD inside the prediction raster
          )
          
          setTxtProgressBar(pb, t)
        } # END of tif loop
      close(pb)
      
    }# END of tile loop
  df <- bind_rows(results)
  return(df)
# END of Function
} 

start_time <- Sys.time()
summary_global <- global_process(tile_dirs)
end_time <- Sys.time()
cat("Total computation time: ", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")

head(summary_global)

# summary_global <- future_map_dfr(tile_dirs, process_tile, .progress = TRUE)


# Optional: Save combined table
# write.csv(summary_global, "/data/ESA99/lat_lon_results/global_summary_with_differences.csv", row.names = FALSE)
# write.csv(summary_global, "results/latlon_global_summary_with_differences.csv", row.names = FALSE)


# Parallel processing -----------------------------------------------------

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
  
  results <- vector("list", length(tifs) - 1)
  counter <- 1
  
  pb <- txtProgressBar(min = 0, max = length(tifs) - 1, style = 3)
  
  for (t in seq_along(tifs)) {
    if (t == orig_idx) next
    
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

library(furrr)
plan(multicore, workers = parallel::detectCores() - 10)

start_time <- Sys.time()
summary_global <- future_map_dfr(tile_dirs, process_single_tile, .progress = TRUE)
end_time <- Sys.time()

cat("Total computation time: ", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")




# ----- Visualize -----
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80") +
  geom_point(data = summary_global,
             aes(x = lon, y = lat, color = avg_differece_percent, size = abs(std_dev))) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                        name = "Relative Difference\n to original in %") +
  scale_size_continuous(range = c(1, 6), name = "SD") +
  # geom_point(data = subset(summary_global, str_detect(layer, "_original")),
  #            aes(x = LON, y = LAT),
  #            color = "#01DC03", shape = 0, size = 3, stroke = 1.5) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Global prediction deviation from original models",
    # subtitle = "Difference in mean predictions (red = higher than original, blue = lower)",
    x = "Longitude", y = "Latitude"
  )


### PARALLEL COMPUTING WITH PROGRESS ####

library(furrr)
library(progressr)
library(terra)
library(stringr)
library(dplyr)
library(tibble)

global_process_parallel <- function(directories) {
  plan(multisession, workers = parallel::detectCores() - 1)
  handlers(global = TRUE)
  handlers("progress")
  
  with_progress({
    p <- progressor(steps = length(directories))
    
    results <- future_map(directories, function(dir) {
      tile_name <- basename(dir)
      p(sprintf("Processing tile: %s", tile_name))
      
      tifs <- list.files(dir, pattern = "\\.tif$", full.names = TRUE)
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
      tile_results <- vector("list", length(tifs))
      
      pb <- txtProgressBar(min = 0, max = length(tifs), style = 3)
      for (t in seq_along(tifs)) {
        if (t == orig_idx) next
        current_raster <- raster_stack[[t]]
        lyr_name <- layer_names[t]
        
        # Compute differences
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
        
        tile_results[[t]] <- tibble(
          tile = tile_name,
          name = lyr_name,
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
        
        setTxtProgressBar(pb, t)
      }
      close(pb)
      
      bind_rows(tile_results)
    }, .options = furrr_options(seed = TRUE))
  }) -> results
  
  bind_rows(results)
}

start_time <- Sys.time()
summary_global <- global_process_parallel(tile_dirs)
end_time <- Sys.time()
cat("Total time:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")


# OLD FUNCTION ------------------------------------------------------------

# Function: Calculate statistics for all tiles
process_tile <- function(tile_dir) {
  tile_name <- basename(tile_dir)
  message("Processing tile: ", tile_name)
  
  tif_files <- list.files(tile_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(tif_files) == 0) return(NULL)
  
  rasters <- lapply(tif_files, rast)
  raster_stack <- rast(rasters)
  
  layer_names <- basename(tools::file_path_sans_ext(tif_files))
  names(raster_stack) <- layer_names
  
  coord_info <- extract_latlon(layer_names, raster_stack)
  
  # Identify the original raster
  orig_idx <- which(str_detect(layer_names, "_original"))
  if (length(orig_idx) != 1) {
    warning("No unique original raster found for tile: ", tile_name)
    return(NULL)
  }
  
  original_raster <- raster_stack[[orig_idx]]
  
  results <- list()
  
  for (i in seq_along(layer_names)) {
    
    cat(paste0(tile_name,":"), i, "out of", length(layer_names),"\n")
    
    lyr_name <- layer_names[i]
    current_raster <- raster_stack[[i]]
    
    # Skip original itself
    if (i == orig_idx) next
    
    # Calculate pixel-wise difference
    difference <- current_raster - original_raster
    
    # Mean and SD of the *difference raster*
    avg_diff <- global(difference, fun = "mean", na.rm = TRUE)[[1]]
    avg_abs_diff <- global(abs(difference), fun = "mean", na.rm = TRUE)[[1]]
    std_dev <- global(difference, fun = "sd", na.rm = TRUE)[[1]]
    
    mean_pred_height <- global(current_raster, "mean", na.rm = TRUE)[,1]
    mean_sd_pred <- global(raster_stack, "sd", na.rm = TRUE)[,1]
    
    # Optional: correlation between rasters
    correlation <- cor(values(current_raster), values(original_raster),
                       method = "pearson", use = "complete.obs")
    
    # Store results
    results[[i]] <- tibble(
      tile = tile_name,
      layer = lyr_name,
      LAT = coord_info$LAT[i], # LAT of prediction
      LON = coord_info$LON[i], # LON of prediciton
      mean_pred = mean_pred_height, # Mean Height of the prediciton
      sd_pred = mean_sd_pred, # Mean SD
      mean_diff = avg_diff,
      abs_mean_diff = avg_abs_diff,
      sd_diff = std_dev,
      correlation = correlation,
      orig_lat = coord_info$LAT[orig_idx], # Original LatLon of the tile
      orig_lon = coord_info$LON[orig_idx]
    )
  }
  
  # Combine into one table for this tile
  df <- bind_rows(results)
  
}

# Run for all tiles/rasters
# summary_global <- map_dfr(tile_dirs, process_tile)

# Parallel computing
# summary_global <- future_map_dfr(tile_dirs, process_tile, .progress = TRUE)

# TIMING
start_time <- Sys.time()
summary_global <- future_map_dfr(tile_dirs, process_tile, .progress = TRUE)
end_time <- Sys.time()
cat("Total computation time: ", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")

