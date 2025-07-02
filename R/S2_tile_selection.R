library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(tmap)
library(leaflet)
library(mapview)
library(mapedit)
library(terra)


# View Tiles by continent -------------------------------------------------

sentinel_kml <- st_read("/home/emilio/global-canopy-height-model/workbench/data/S2A_OPER_GIP_TILPAR_MPC__20151209T095117_V20150622T000000_21000101T000000_B00.kml", quiet = TRUE)

# Get continents shapefile using rnaturalearth
continents <- ne_countries(scale = "medium", returnclass = "sf") %>%
  group_by(continent) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

# Adjust CRS to match
sentinel_kml <- st_transform(sentinel_kml, st_crs(continents))

# Select continent
unique(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")$continent)
target_continent <- "Asia"
continent_geom <- filter(continents, continent == target_continent)

# Filter tiles by continent
tiles_continent <- sentinel_kml[st_intersects(sentinel_kml, continent_geom, sparse = FALSE), ]

tiles_continent <- st_zm(tiles_continent, drop = TRUE, what = "ZM")         # Drop Z/M dimensions
tiles_continent <- st_collection_extract(tiles_continent, "POLYGON")          # Explode geometry collections into individual parts
tiles_continent <- st_cast(tiles_continent, "MULTIPOLYGON")     

# Plot
tmap_mode("view")
tm_basemap("OpenTopoMap") +
  tm_shape(tiles_continent) +
  tm_borders(col = "red", lwd = 2) +
  tm_text("Name", size = 0.8, col = "black") # lables




# Select Tiles ------------------------------------------------------------


tiles <- st_read("/home/emilio/global-canopy-height-model/workbench/data/S2A_OPER_GIP_TILPAR_MPC__20151209T095117_V20150622T000000_21000101T000000_B00.kml", quiet = TRUE)
# tiles <- sentinel_kml
tiles <- st_zm(tiles, drop = TRUE, what = "ZM")
tiles <- st_collection_extract(tiles, "POLYGON")
tiles <- st_cast(tiles, "MULTIPOLYGON")

# SELECT TILES
selected_tiles <- selectFeatures(tiles, label = tiles$Name)


tmap_mode("view")
tm_shape(selected_tiles) +
  tm_borders(lwd = 4, col = "red") +          # thick red borders
  tm_text("Name", size = 1, just = "center") + # names centered inside tiles
  tm_layout(legend.show = FALSE) +             # no legend
  tm_basemap("Esri.WorldImagery")

# st_write(selected_tiles, "R/data/final_tile_selection.gpkg")
# st_write(selected_tiles, "R/data/selected_tiles.geojson")



# Overview Table ----------------------------------------------------------

# Create Overview table
library(elevatr)
merged <- selected_tiles


centroids <- st_centroid(merged$geom) # geometry column my be renamed and therefore the correct option has to be choosen
centroids <- st_centroid(merged$geometry)
latitudes <- st_coordinates(centroids)[, "Y"]  # Extract latitude (Y coordinate) from centroids

centroids <- st_centroid(merged)  # keep the sf structure
countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
centroids_with_country <- st_join(centroids, countries[c("admin", "continent")])

elevation_data <- get_elev_point(centroids, src = "aws")  # src could be "aws" or "epqs"
elevations <- elevation_data$elevation # Extract elevation values

result_df <- data.frame(
  Continent = centroids_with_country$continent,
  Latitude = round(latitudes, 1),
  Name = merged$Name,
  Country = centroids_with_country$admin,
  Centeroid_Elevation = elevations
)

# Create info of proposal group
# all_names <- merged$Name
# source_info <- sapply(all_names, function(n) {
#   in_mu <- n %in% mu_names
#   in_ms <- n %in% ms_names
#   
#   if (in_mu & in_ms) {
#     "BOTH"
#   } else if (in_mu) {
#     "MU"
#   } else if (in_ms) {
#     "MS"
#   } else {
#     NA  # or "UNKNOWN" if you want to flag something unexpected
#   }
# })
# 
# result_df$Source <- source_info # add to data frame

result_df_sorted <- result_df %>%
  arrange(Continent)

result_df_sorted$Source <- c("PAPER", "PAPER", "Munich", "PAPER", "Munich", "Munich", "Münster", "PAPER", "BOTH", "Münster", "Münster")

print(result_df_sorted)

# write.csv(result_df_sorted, "R/data/final_tile_selection.csv", row.names = F)



# Merge with Munich and final proposal ------------------------------------

ms_tiles <- st_read("R/data/proposed_tiles.gpkg")
mu_tiles <- st_read("R/data/S2_Tiles_filtered.geojson")

mu_names <- mu_tiles$Name
ms_names <- ms_tiles$Name
tile_names <- c(mu_names, ms_names)
merged <- tiles[tiles$Name %in% tile_names, ]

tile_names[duplicated(tile_names)]

# st_write(merged, "R/data/combined_tiles.geojson")

# tmap_mode("view")
# tm_basemap("OpenTopoMap") +
tm_basemap("Esri.WorldGrayCanvas") +
  tm_shape(merged[1]) +
  tm_borders(col = "red", lwd = 2) +
  tm_text("Name", size = 0.8, col = "black") # lables



proposed <- st_read("R/data/combined_tiles.geojson")
# biomes <- st_read("/home/emilio/data_storage/Biomes.gpkg")

tmap_mode("view")
tm_shape(proposed) +
  tm_borders(lwd = 4, col = "red") +          # thick red borders
  tm_text("Name", size = 1, just = "center") + # names centered inside tiles
  tm_layout(legend.show = FALSE) +             # no legend
  tm_basemap("Esri.WorldImagery")

# tm_basemap("OpenStreetMap")




# NEW WORLDCOVER ----------------------------------------------------------
selected_tiles <- st_read("R/data/final_tile_selection.gpkg")

# Load all worldcover files (assuming GeoTIFFs)
files <- list.files("/home/emilio/canopy_height/deploy_example/ESAworldcover/2020/new/WORLDCOVER/", pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
wc_files <- grep("MAP\\.tif$", files, ignore.case = TRUE, value = TRUE)


### Automatically crop correct tifs to tile and merge if needed

library(terra)
library(sf)

# --- Inputs ---
tiles_path <- "R/data/final_tile_selection.gpkg"
tif_files <- list.files("/home/emilio/canopy_height/deploy_example/ESAworldcover/2020/new", 
                        full.names = TRUE, pattern = "tif$")
output_dir <- "/home/emilio/canopy_height/deploy_example/ESAworldcover/2020/new/cropped/"

# Create output directory if not exists
if (!dir.exists(output_dir)) dir.create(output_dir)

# --- Load tiles and CRS ---
{
tiles <- st_read(tiles_path)
tiles_crs <- st_crs(tiles)$wkt

cat("Loaded", nrow(tiles), "tiles.\n")

# --- Preload raster extents and CRSs ---
raster_meta <- lapply(tif_files, function(f) {
  r <- rast(f)
  list(path = f, extent = ext(r), crs = crs(r), nlyr = nlyr(r), coltab = coltab(r))
})

cat("Loaded metadata for", length(raster_meta), "rasters.\n")

# --- Process each tile ---
for (i in seq_len(nrow(tiles))) {
  tile <- tiles[i, ]
  tile_name <- tile$Name
  
  cat("\n=== Processing tile:", tile_name, "(", i, "/", nrow(tiles), ") ===\n")
  
  tile_vect <- vect(tile)
  tile_ext <- ext(tile_vect)
  
  # Identify overlapping rasters using extent intersection (fast)
  candidate_rasters <- Filter(function(meta) {
    !is.null(intersect(tile_ext, meta$extent))
  }, raster_meta)
  
  if (length(candidate_rasters) == 0) {
    cat("  No overlapping raster extents.\n")
    next
  }
  
  cat("  Found", length(candidate_rasters), "potential overlapping rasters.\n")
  
  # Confirm overlap with projection match
  overlapping <- list()
  for (meta in candidate_rasters) {
    cat("    Checking raster:", basename(meta$path), "\n")
    r <- rast(meta$path)
    r_proj <- project(r, tiles_crs)
    
    tile_proj <- project(tile_vect, crs(r_proj))
    
    if (!is.null(intersect(ext(r_proj), ext(tile_proj)))) {
      overlapping[[length(overlapping) + 1]] <- list(rast = r_proj, coltab = meta$coltab)
      cat("      -> Overlaps!\n")
    } else {
      cat("      -> Does NOT overlap.\n")
    }
  }
  
  if (length(overlapping) == 0) {
    cat("  No actual overlapping rasters found after reprojection.\n")
    next
  }
  
  # Merge rasters if necessary
  if (length(overlapping) > 1) {
    cat("  Merging", length(overlapping), "rasters...\n")
    merged <- do.call(mosaic, lapply(overlapping, `[[`, "rast"))
  } else {
    merged <- overlapping[[1]]$rast
  }
  
  # Crop & mask to tile
  cat("  Cropping and masking...\n")
  cropped <- crop(merged, tile_vect)
  masked <- mask(cropped, tile_vect)
  
  # Apply color table (only 1st layer assumed)
  coltab(masked) <- overlapping[[1]]$coltab
  
  # Save
  out_file <- file.path(output_dir, paste0("ESA_WorldCover_10m_2020_v100_",tile_name,".tif") )
  writeRaster(masked, out_file, overwrite = TRUE)
  
  cat("  ✔ Saved to:", out_file, "\n")
}

cat("\n✅ All tiles processed.\n")
}

# Reproject Worldcover tiles ----------------------------------------------
library(terra)
library(dplyr)
library(stringr)

sentinel_epsg <- data.frame(
  tile_code = c("33NTG", "50TPT", "49NHC", "32TMT", "32UQU", 
                "34UFD", "35VML", "10TES", "17SNB", "55HEV", "20MMD"),
  utm_zone = c("33N", "50N", "49N", "32N", "32U", 
               "34U", "35V", "10T", "17S", "55H", "20M"),
  epsg_code = c(32633, 32650, 32649, 32632, 32632, 
                32634, 32635, 32610, 32617, 32655, 32620)
)

file_list <- list.files("/home/emilio/canopy_height/deploy_example/ESAworldcover/2020/new/cropped/",
                        full.names = T)

# 3. Extract tile names from file names
get_tile <- function(path) {
  # Extract tile code at the end, e.g., "10TES" before ".tif"
  match <- regmatches(path, regexpr("[0-9]{2}[A-Z]{3}(?=\\.tif$)", path, perl = TRUE))
  if (length(match) == 0) return(NA)
  return(match)
}

file_info <- data.frame(
  file = file_list,
  tile_code = sapply(file_list, get_tile),
  stringsAsFactors = FALSE
)

# 4. Join with EPSG info
file_info <- left_join(file_info, sentinel_epsg, by = "tile_code") %>%
  filter(!is.na(epsg_code))  # Keep only those with matched EPSG

# 5. Load and reproject rasters
# reprojected_list <- lapply(1:nrow(file_info), function(i) {
#   cat("Processing tif", i, "out of", nrow(file_info),"\n")
#   file_path <- file_info$file[i]
#   epsg <- file_info$epsg_code[i]
#   
#   cat("Rast and reproject...\n")
#   r <- rast(file_path)
#   r_proj <- project(r, paste0("EPSG:", epsg))
#   cat("Completed, continuing with writing.\n")
#   
#   # Save or return as needed:
#   writeRaster(r_proj, paste0("deploy_example/ESAworldcover/2020/new/1_Final/reprojected_", basename(file_path)))
#   # return(r_proj)
# })

reprojected_list <- lapply(1:nrow(file_info), function(i) {
  cat("Processing tif", i, "out of", nrow(file_info), "\n")
  file_path <- file_info$file[i]
  epsg <- file_info$epsg_code[i]
  
  cat("Reading raster...\n")
  r <- rast(file_path)
  
  # Preserve categories and color table if present
  categories <- cats(r)
  coltab <- coltab(r)
  
  cat("Reprojecting to EPSG:", epsg, "...\n")
  r_proj <- project(r, paste0("EPSG:", epsg), method = "near")  # 'near' for categorical data
  
  # Restore categories and color table
  if (!is.null(categories[[1]])) cats(r_proj) <- categories
  if (!is.null(coltab)) coltab(r_proj) <- coltab
  
  out_file <- paste0("deploy_example/ESAworldcover/2020/new/1_Final/", basename(file_path))
  cat("Writing reprojected raster to:\n", out_file, "\n\n")
  writeRaster(r_proj, out_file, overwrite = TRUE)
  
  # No return
  invisible(NULL)
})
