
library(terra)
library(sf)


# Raster path only for test reasons -> in function later: object
# Also: no saving? return x but don't export
  # saving optional? -> output path -> remove from standard function
# pass on percentage -> Watch out for overlapping names with modify Bands!!! -> shuffle_percentage
# Include rasterio and os in imports of files!
  # Include in installation of environment files too!!
# Import function to script from     gchm/utils/transforms.py     in     gchm/deploy.py

env_vars <- c(
 RASTER_PATH = "./R/spatial_patterns/T10TES_20200417T185921_B02_10m.jp2",
 PERCENTAGE = 10
)

cat("Deploy python test script.\n")
withr::with_envvar(env_vars, {
  system2("python", "./R/spatial_patterns/test.py")
})


