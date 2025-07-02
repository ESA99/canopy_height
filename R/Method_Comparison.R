library(terra)
library(ggplot2)
library(gridExtra)

compare_tile_by_prefix <- function(base_dir, tile_id) {
  # Define subfolders
  model_dirs <- list(
    Ensemble = file.path(base_dir, "Ensemble"),
    Original = file.path(base_dir, "Original"),
    Single   = file.path(base_dir, "Single")
  )
  
  # Helper: find the first .tif that starts with tile_id
  find_tile_file <- function(folder) {
    files <- list.files(folder, pattern = paste0("^", tile_id, ".*\\.tif$"), full.names = TRUE)
    if (length(files) == 0) stop(paste("No file found for tile", tile_id, "in", folder))
    files[1]
  }
  
  # Locate files
  pathA <- find_tile_file(model_dirs$Ensemble)
  pathB <- find_tile_file(model_dirs$Original)
  pathC <- find_tile_file(model_dirs$Single)
  
  # Load rasters
  r1 <- rast(pathA)
  r2 <- rast(pathB)
  r3 <- rast(pathC)
  
  # Align resolution and extent
  # r2 <- resample(r2, r1)
  # r3 <- resample(r3, r1)
  
  # Compute differences
  diff_ab <- r1 - r2
  diff_ac <- r1 - r3
  diff_bc <- r2 - r3
  
  # Summary stats
  rmse <- function(x, y) sqrt(mean((x - y)^2, na.rm = TRUE))
  stats <- data.frame(
    Comparison = c("Ensemble - Original", "Ensemble - Single", "Original - Single"),
    Mean_Diff = c(global(diff_ab, "mean", na.rm = TRUE)[1,1],
                  global(diff_ac, "mean", na.rm = TRUE)[1,1],
                  global(diff_bc, "mean", na.rm = TRUE)[1,1]),
    SD_Diff = c(global(diff_ab, "sd", na.rm = TRUE)[1,1],
                global(diff_ac, "sd", na.rm = TRUE)[1,1],
                global(diff_bc, "sd", na.rm = TRUE)[1,1]),
    RMSE = c(
      rmse(values(r1), values(r2)),
      rmse(values(r1), values(r3)),
      rmse(values(r2), values(r3))
    )
  )
  
  # Histograms
  # Extract differences and remove NA
  vals_ab <- values(diff_ab)
  vals_ab <- vals_ab[!is.na(vals_ab)]
  
  vals_ac <- values(diff_ac)
  vals_ac <- vals_ac[!is.na(vals_ac)]
  
  vals_bc <- values(diff_bc)
  vals_bc <- vals_bc[!is.na(vals_bc)]
  
 
  
  # Because lengths may differ if NA removed, plot separately
  hist1 <- ggplot(data.frame(diff = vals_ab), aes(x = diff)) +
    geom_histogram(binwidth = 0.25, fill = "tomato", alpha = 0.7) +
    labs(title = paste(tile_id, "Ensemble - Original"), x = "Height Difference (m)")
  
  hist2 <- ggplot(data.frame(diff = vals_ac), aes(x = diff)) +
    geom_histogram(binwidth = 0.25, fill = "skyblue", alpha = 0.7) +
    labs(title = paste(tile_id, "Ensemble - Single"), x = "Height Difference (m)")
  
  hist3 <- ggplot(data.frame(diff = vals_bc), aes(x = diff)) +
    geom_histogram(binwidth = 0.25, fill = "seagreen", alpha = 0.7) +
    labs(title = paste(tile_id, "Original - Single"), x = "Height Difference (m)")
  
  grid.arrange(hist1, hist2, hist3, ncol = 1)
  
  
  return(list(
    Tile = tile_id,
    Stats = stats,
    Diff_Ensemble_Original = diff_ab,
    Diff_Ensemble_Single = diff_ac,
    Diff_Original_Single = diff_bc
  ))
}


### Deploy

# Base directory containing Ensemble, Original, Single
base_dir <- "/home/emilio/data_storage/2_COMPARISON"

# For one tile
res <- compare_tile_by_prefix(base_dir, "33NTG")
print(res$Stats)

# FULL COMPARISON
tile_ids <- c("33NTG", "49NHC", "49UCP", "55HEV")  # etc.

results <- lapply(tile_ids, function(tid) {
  compare_tile_by_prefix(base_dir, tid)
})

# Extract stats from all results
all_stats <- do.call(rbind, lapply(results, function(r) {
  cbind(Tile = r$Tile, r$Stats)
}))
print(all_stats)

write_csv(all_stats, "/home/emilio/data_storage/2_COMPARISON/stats.csv")
