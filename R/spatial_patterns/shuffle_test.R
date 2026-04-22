library(terra)
# 
# # Create empty raster
# r <- rast(nrows = 100, ncols = 100)
# 
# # Fill with gradient (left → right)
# values(r) <- rep(1:100, each = 100)
# 
# plot(r)
# 

### DIAGONAL
r <- rast(nrows = 100, ncols = 100)

# Create coordinate grid
x <- rep(1:100, each = 100)
y <- rep(1:100, times = 100)

# Diagonal gradient
values(r) <- x + y

plot(r)

# ### RADIAL
# r <- rast(nrows = 100, ncols = 100)
# 
# x <- rep(1:100, each = 100)
# y <- rep(1:100, times = 100)
# 
# # Center coordinates
# cx <- 50
# cy <- 50
# 
# # Distance from center
# values(r) <- sqrt((x - cx)^2 + (y - cy)^2)
# 
# plot(r)

# writeRaster(r, "gradient.tif", overwrite=TRUE)