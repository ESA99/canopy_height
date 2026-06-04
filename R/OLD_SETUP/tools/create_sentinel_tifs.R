# List all subfolders
subfolders <- list.files("/home/emilio/canopy_height/deploy_example/sentinel2/2020/", full.names = T)

output_dir <- "/data/ESA99/sentinel_scenes/"

# Function to process a single subfolder
process_sentinel_folder <- function(folder) {
  
  # List all zip files in the folder
  zip_files <- list.files(folder, pattern = "\\.zip$", full.names = TRUE)
  
  # Create an output folder for tifs
  out_folder <- file.path(output_dir, "tif_10m")
  if (!dir.exists(out_folder)) dir.create(out_folder)
  
  for (zip_file in zip_files) {
    
    # Temporary folder to unzip
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    
    # Unzip the sentinel file
    unzip(zip_file, exdir = tmp_dir)
    
    # Sentinel 2 structure: look for 10m bands
    # Usually in: tmp_dir/SENTINEL2_L1C/GRANULE/.../IMG_DATA/R10m
    ten_m_files <- list.files(tmp_dir, pattern = "B0[2348]_10m.jp2$", recursive = TRUE, full.names = TRUE)
    
    if (length(ten_m_files) == 0) {
      message("No 10m bands found in ", zip_file)
      next
    }
    
    # Stack the bands
    s <- rast(ten_m_files)
    
    # Output filename (same as zip, but tif)
    # out_file <- file.path(out_folder, paste0(tools::file_path_sans_ext(basename(zip_file)), "_10m.tif"))
    matches <- regmatches(basename(zip_file), regexec("MSIL2A_(\\d{8})T\\d{6}_.*_T([0-9A-Z]{5})_", basename(zip_file)))[[1]]
    out_file <- file.path(out_folder, paste0(matches[3], "_", matches[2] , "_10m.tif") )
    
    # Write raster
    writeRaster(s, out_file, overwrite = TRUE)
    
    # Clean up temp
    unlink(tmp_dir, recursive = TRUE)
  }
}

# Apply to all subfolders
lapply(subfolders, process_sentinel_folder)
