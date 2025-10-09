# Comparison Switzerland

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
