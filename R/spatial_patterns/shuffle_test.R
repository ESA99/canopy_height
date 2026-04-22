library(terra)

# Create empty raster (100x100)
test <- rast(nrows = 100, ncols = 100, xmin = 0, xmax = 100, ymin = 0, ymax = 100)

# Create diagonal gradient
# Values increase from bottom-left to top-right
vals <- outer(1:100, 1:100, function(x, y) x + y)

# Assign values to raster
values(test) <- as.vector(vals)

# Plot
plot(test)

writeRaster(test, "R/spatial_patterns/gradient.tif", overwrite=TRUE)




################################################################################

original <- rast(env_vars[1])
modified <- rast("R/spatial_patterns/modified_raster.tif")

plot(original)
plot(modified)

diff <- original - modified

plot(diff)

mean(values(original))
mean(values(modified))
