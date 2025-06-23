library(terra)
library(sf)
library(dplyr)
library(tmap)

### Selected tiles
selected_tiles <- st_read("R/data/final_tile_selection.gpkg")

tmap_mode("view")
tm_shape(selected_tiles) +
  tm_borders(lwd = 4, col = "red") +          # thick red borders
  tm_text("Name", size = 1, just = "center") + # names centered inside tiles
  tm_layout(legend.show = FALSE) +             # no legend
  tm_basemap("Esri.WorldImagery")


# Setup & Inputs ----------------------------------------------------------

tiles_path <- "R/data/final_tile_selection.gpkg"
worldcover_dir <- "/home/emilio/canopy_height/deploy_example/ESAworldcover/2020/new/originalZIP/WORLDCOVER/"
output_dir <- "/home/emilio/canopy_height/deploy_example/ESAworldcover/2020/try"


# Sentinel-2 tile EPSG table
sentinel_epsg <- data.frame(
  tile_code = c("33NTG", "50TPT", "49NHC", "32TMT", "32UQU", 
                "34UFD", "35VML", "10TES", "17SNB", "55HEV", "20MMD"),
  epsg_code = c(32633, 32650, 32649, 32632, 32632, 
                32634, 32635, 32610, 32617, 32655, 32620),
  WC_N = c("E012", "N45E117", "N00", "", "", "N51", "N6", "N45W123", "N36W", "S39", "S03"),
  stringsAsFactors = FALSE
)

## Worldcover downloads 
files <- list.files(worldcover_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
wc_files <- grep("MAP\\.tif$", files, ignore.case = TRUE, value = TRUE)


# Merge WC tifs -----------------------------------------------------------

for (i in sentinel_epsg$tile_code) {

  TILENUM <- i

  if (sentinel_epsg[sentinel_epsg$tile_code == TILENUM,]$WC_N == "") {
    cat("Skipping", i)
  } else{
  

cat("Processing Tile", TILENUM)

wc_select <- grep(sentinel_epsg[sentinel_epsg$tile_code == TILENUM,]$WC_N, wc_files, ignore.case = TRUE, value = TRUE)

raster_list <- lapply(wc_select, rast)
orig_coltab <- coltab(raster_list[[1]])
# merged_raster <- do.call(merge, raster_list)
merged_raster <- Reduce(function(x, y) merge(x, y), raster_list)

s2tile <- selected_tiles[selected_tiles$Name == TILENUM,]

# s2_reprojected <- st_transform(s2tile, crs = sentinel_epsg[sentinel_epsg$tile_code == TILENUM,]$epsg_code)
# s2_reprojected <- project(s2tile, paste0("epsg:", as.character(sentinel_epsg[sentinel_epsg$tile_code == TILENUM,]$epsg_code)))
# raster_reprojected <- project(merged_raster, paste0("epsg:", as.character(sentinel_epsg[sentinel_epsg$tile_code == TILENUM,]$epsg_code)))
raster_reprojected <- project(
  merged_raster,
  paste0("epsg:", as.character(sentinel_epsg[sentinel_epsg$tile_code == TILENUM,]$epsg_code)),
  method = "near"
)



# raster_cropped <- crop(raster_reprojected, s2_reprojected)
# raster_masked <- mask(raster_cropped, s2_reprojected)
# raster_cropped <- crop(raster_reprojected, vect(s2_reprojected), snap = "out")
# raster_masked  <- mask(raster_cropped, vect(s2_reprojected))
raster_masked <- raster_reprojected

coltab(raster_masked) <- orig_coltab

out_file <- file.path(output_dir, paste0("WC_",TILENUM,".tif") )
writeRaster(raster_masked, out_file, overwrite = TRUE)
cat(i,"DONE\n")

  }

}



# Crop to extent of S2 ----------------------------------------------------

crop_raster_with_s2 <- function(raster_dir, s2_dir, out_dir, out_basename, filetype = "tif"){
  
  rasters <- list.files(raster_dir, full.names = T)
  s2_tile <- list.files(s2_dir, full.names = T)
  
  if(length(rasters) != length(s2_tile) ){ stop("Number of raster inputs do not match!\n") }
  
  for (i in 1:length(rasters)) {
    
    wc_rast <- terra::rast(rasters[i])
    s2_rast <- terra::rast(s2_tile[i])
    
    colour_table <- terra::coltab(wc_rast)
    cat("Colourtable extracted.\n")
    
    wc_trans <- terra::project(wc_rast, crs(s2_rast))
    cat("CRS adjusted.\n")
    
    s2_vect <- terra::as.polygons(s2_rast, dissolve = TRUE)
    cat("S2 raster converted to polygon. Cropping...\n")
    wc_crop <- terra::crop(wc_trans, s2_vect, mask = TRUE)
    # wc_crop <- terra::crop(wc_trans, s2_rast, mask = T)
    cat("Crop successfull. Apply colourtable and extract tilename.\n")
    terra::coltab(wc_crop) <- colour_table
    
    tile_name <- tools::file_path_sans_ext(basename(rasters[i]))
    # tile_name <- sub("^WC_", "", tile_name)
    tile_name <- sub(".*?(\\d{2}[A-Za-z]{3}).*", "\\1", tile_name)
    
    # Save final worldcover raster
    out_file <- file.path(out_dir, paste0(out_basename, tile_name, ".", filetype))
    cat("Writing to:", out_file, "\n")
    terra::writeRaster(wc_crop, out_file, overwrite = TRUE)
    cat("Done with tile:", tile_name, "\n")
    
    
  }
  
  cat("\n ******* All rasters processed successfully. *******\n")
  
}

wc_raster_dir <- "/home/emilio/canopy_height/deploy_example/ESAworldcover/2020/new/WC_merged_tiles"
s2_tiles_dir <- "/home/emilio/canopy_height/deploy_example/ESAworldcover/2020/new/WC_cropped_masked/crop_S2_extents"
output_dir <- "/home/emilio/canopy_height/deploy_example/ESAworldcover/2020/new/WC_cropped_masked/"
out_basename <- "ESA_WorldCover_10m_2020_v100_"

crop_raster_with_s2(wc_raster_dir, s2_tiles_dir, output_dir, out_basename)
dandelion::


# wc_merged <- list.files("/home/emilio/canopy_height/deploy_example/ESAworldcover/2020/new/WC_merged_tiles", full.names = T)
# s2_tile <- list.files("/home/emilio/canopy_height/deploy_example/ESAworldcover/2020/new/WC_cropped_masked/crop_S2_extents", full.names = T)



# wc_rast <- rast(wc_merged[1])
# s2_rast <- rast(s2_tile[1])
# 
# colour_table <- coltab(wc_rast)
# 
# wc_trans <- terra::project(wc_rast, crs(s2_rast))
# 
# s2_vect <- as.polygons(s2_rast, dissolve = TRUE)
# wc_crop <- terra::crop(wc_trans, s2_vect, mask = TRUE)
# # wc_crop <- terra::crop(wc_trans, s2_rast, mask = T)
# 
# coltab(wc_crop) <- colour_table
# 
# tile_name <- tools::file_path_sans_ext(basename(wc_merged[1]))
# tile_name <- sub("^WC_", "", tile_name)
# pattern_match <- sub(".*?(\\d{2}[A-Za-z]{3}).*", "\\1", tile_name)
# 
# # Save final worldcover raster
# out_file <- file.path(output_dir, paste0("ESA_WorldCover_10m_2020_v100_", tile_name, ".tif"))
# cat("Writing to:", out_file, "\n")
# writeRaster(wc_crop, out_file, overwrite = TRUE)
# cat("Done with tile:", tile_name, "\n")
# 
# cat("\n All tiles processed successfully.\n")



### As Function
# crop_raster_with_s2 <- function(raster_dir, s2_dir, out_dir, out_basename, filetype = "tif"){
#   
#   rasters <- list.files(raster_dir, full.names = T)
#   s2_tile <- list.files(s2_dir, full.names = T)
#   
#   if(length(rasters) != length(s2_tile) ){ stop("Number of raster inputs do not match!\n") }
#   
#   for (i in 1:length(rasters)) {
#     
#     wc_rast <- rast(rasters[i])
#     s2_rast <- rast(s2_tile[i])
#     
#     colour_table <- coltab(wc_rast)
#     cat("Colourtable extracted.\n")
#     
#     wc_trans <- terra::project(wc_rast, crs(s2_rast))
#     cat("CRS adjusted.\n")
#     
#     s2_vect <- as.polygons(s2_rast, dissolve = TRUE)
#     cat("S2 raster converted to polygon. Cropping...\n")
#     wc_crop <- terra::crop(wc_trans, s2_vect, mask = TRUE)
#     # wc_crop <- terra::crop(wc_trans, s2_rast, mask = T)
#     cat("Crop successfull. Apply colourtable and extract tilename.\n")
#     coltab(wc_crop) <- colour_table
#     
#     tile_name <- tools::file_path_sans_ext(basename(wc_merged[i]))
#     # tile_name <- sub("^WC_", "", tile_name)
#     tile_name <- sub(".*?(\\d{2}[A-Za-z]{3}).*", "\\1", tile_name)
#     
#     # Save final worldcover raster
#     out_file <- file.path(out_dir, paste0(out_basename, tile_name, ".", filetype))
#     cat("Writing to:", out_file, "\n")
#     writeRaster(wc_crop, out_file, overwrite = TRUE)
#     cat("Done with tile:", tile_name, "\n")
#     
#     
#   }
#   
#   cat("\n ******* All rasters processed successfully. *******\n")
# 
# }
# 
