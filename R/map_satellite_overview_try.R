# Satellite overview plots of all locations
# Did not satisfy yet because of colour stretch and takes forever
library(terra)

base_dir <- file.path(file.path("/home/emilio/canopy_height", "deploy_example/sentinel2/2020/"))
dirs <- list.dirs(base_dir)[-1]


out_dir <- "/home/emilio/data_storage/true_colour_plots"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (d in dirs) {
  
  message("Processing: ", d)
  
  # 1. Get first Sentinel-2 zip
  zip_file <- list.files(d, pattern = "\\.zip$", full.names = TRUE)
  if (length(zip_file) == 0) {
    warning("No zip files in ", d)
    next
  }
  zip_file <- zip_file[1]
  
  # 2. List files inside zip
  zip_list <- unzip(zip_file, list = TRUE)$Name
  
  # 3. Extract RGB bands
  b4 <- zip_list[grepl("B04_10m.jp2$", zip_list)]
  b3 <- zip_list[grepl("B03_10m.jp2$", zip_list)]
  b2 <- zip_list[grepl("B02_10m.jp2$", zip_list)]
  
  if (length(b4) == 0 | length(b3) == 0 | length(b2) == 0) {
    warning("Missing RGB bands in ", zip_file)
    next
  }
  
  # 4. Read bands directly from zip
  r <- rast(paste0("/vsizip/", zip_file, "/", b4[1]))
  g <- rast(paste0("/vsizip/", zip_file, "/", b3[1]))
  b <- rast(paste0("/vsizip/", zip_file, "/", b2[1]))
  
  rgb <- c(r, g, b)
  
  # 5. Stretch for visualization
  rgb <- stretch(rgb, minq = 0.01, maxq = 0.99)
  
  # 6. Output filename
  tile <- basename(d)
  out_file <- file.path(out_dir, paste0(tile, "_true_colour.png"))
  
  # 7. Plot and export
  png(out_file, width = 2000, height = 1500, res = 200)
  plotRGB(rgb, r = 1, g = 2, b = 3, axes = FALSE, main = tile)
  dev.off()
}


rgb_small <- aggregate(rgb, fact = 10)
plotRGB(rgb_small, r = 1, g = 2, b = 3, scale = 255, axes = FALSE)



writeRaster(
  rgb,
  "/home/emilio/data_storage/true_colour_plots/rgb_full.tif",
  overwrite = TRUE,
  wopt = list(
    datatype = "INT1U",   # 0–255
    gdal = c("COMPRESS=DEFLATE")
  )
)
rgb_vis <- aggregate(rgb, fact = 10, fun = mean)
png("rgb_preview.png", width = 2000, height = 2000, res = 200)
plotRGB(rgb_vis, r = 1, g = 2, b = 3, scale = 255, axes = FALSE)
dev.off()

rgb_stars <- read_stars("/home/emilio/data_storage/true_colour_plots/rgb_full.tif")
plot(rgb_stars, rgb = 1:3)