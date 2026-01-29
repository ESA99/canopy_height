library(terra)
library(ggplot2)
library(gridExtra)


# Comparison of Lang et al. vs. My Ensemble vs. Single Scene prediction ------------------

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


# Base directory containing Ensemble, Original, Single
base_dir <- "/home/emilio/data_storage/2_COMPARISON"

# For one tile
res <- compare_tile_by_prefix(base_dir, "33NTG")
print(res$Stats)

### FULL COMPARISON ###
tile_ids <- c("33NTG", "49NHC", "49UCP", "55HEV")  # etc.

results <- lapply(tile_ids, function(tid) {
  compare_tile_by_prefix(base_dir, tid)
})

# Extract stats from all results
all_stats <- do.call(rbind, lapply(results, function(r) {
  cbind(Tile = r$Tile, r$Stats)
}))
print(all_stats)

# write.csv(all_stats, "/home/emilio/data_storage/2_COMPARISON/stats.csv")




# Compare local prediction to Lang et al. results -------------------------
# Idea: Check the quality and verify accurate prediction, compared to the original results presented by Lang et al.
# This ensures deployment of the model works correctly


#### Preparation of rasters for comparison ####

p6 <- rast("/home/emilio/data_storage/2_COMPARISON/Switzerland/Lang_Original_Pred/ETH_GlobalCanopyHeight_10m_2020_N45E006_Map.tif")
p9 <- rast("/home/emilio/data_storage/2_COMPARISON/Switzerland/Lang_Original_Pred/ETH_GlobalCanopyHeight_10m_2020_N45E009_Map.tif")

pred <- terra::merge(p6, p9)

tmt32 <- rast("/home/emilio/data_storage/2_COMPARISON/Switzerland/32TMT_original_ESA.tif")

crs(tmt32)
crs(pred)

pred_reprojected <- project(pred, "epsg:32632", method = "near")

raster_cropped <- terra::crop(pred_reprojected, tmt32, snap="out")
# Resample raster to match tmt32 grid exactly
raster_aligned <- terra::resample(raster_cropped, tmt32, method="near")
raster_masked <- terra::mask(raster_aligned, tmt32)
# raster_masked <- terra::mask(raster_cropped, tmt32)

### Faster alternative (if it works)
# pred_reprojected <- project(pred, tmt32, method="near")
# raster_masked <- terra::mask(pred_reprojected, tmt32)

plot(tmt32)
plot(raster_masked)

ext(tmt32)
ext(raster_masked)


# writeRaster(raster_masked, "/home/emilio/data_storage/2_COMPARISON/Switzerland/pred_langnico.tif")
# writeRaster(tmt32, "/home/emilio/data_storage/2_COMPARISON/Switzerland/pred_esa.tif")



#### Comparison of ESA and langnico results in Switzerland ####

langnico <- rast("/home/emilio/data_storage/2_COMPARISON/Switzerland/pred_langnico.tif")
esa <- rast("/home/emilio/data_storage/2_COMPARISON/Switzerland/pred_esa.tif")
# langnico <- raster_masked
# esa <- tmt32

## Difference
diff <- langnico - esa
plot(diff)
# writeRaster(diff, "/home/emilio/data_storage/2_COMPARISON/Switzerland/pred_diff_lang_esa.tif")
# diff <- rast("/home/emilio/data_storage/2_COMPARISON/Switzerland/pred_diff_lang_esa.tif")

diff_percent <- ((langnico - esa) / esa) * 100
plot(diff_percent)


# Statistical
stats <- global(diff, fun=c("min","max","mean","sd"), na.rm=TRUE)
print(stats)

diff_values <- values(diff, na.rm=TRUE)

median_diff <- median(diff_values)
quantiles_diff <- quantile(diff_values, probs=c(0.25, 0.5, 0.75))

cat("Median difference:", median_diff, "\n")
cat("25% and 75% quantiles:", quantiles_diff[1], quantiles_diff[3], "\n")




# Full difference raster
plot(diff, main="Difference Raster (Lang Pred - ESA)")

# Highlight large differences, e.g., >2 meters
threshold <- 2
diff_significant <- diff
diff_significant[abs(diff_significant) <= threshold] <- NA
plot(diff_significant, main="Significant Differences (>2 m)")


# Percentage over threashold
total_cells <- sum(!is.na(values(diff)))
cells_exceeding <- sum(!is.na(values(diff_significant)))
percent_exceeding <- (cells_exceeding / total_cells) * 100
cat("Percentage of raster with difference >", threshold, "m:", round(percent_exceeding, 2), "%\n")



