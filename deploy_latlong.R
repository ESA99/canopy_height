### DEPLOY LAT-LONG ###

# Document setup  ----------------------------------------------------------
start_time <- Sys.time()
start_date_chr <- format(Sys.Date(), "%Y-%m-%d")

# Create empty data frame to store timing info
timing_results <- data.frame(
  Step = character(),
  Minutes = numeric(),
  stringsAsFactors = FALSE
)

# Load necessary packages
c("sf", "terra", "tmap", "dandelion",
  "rnaturalearth", "rnaturalearthdata",
  "plyr", "dplyr", "leaflet",
  "viridis", "cols4all", "colorspace"
) |>
  lapply(library, character.only = TRUE)


# VARIABLE INPUT TABLE -----------------------------------------------------

# Input of the parameters as data frame with all combinations
# All tiles: "10TES" "17SNB" "20MMD" "32TMT" "32UQU" "33NTG" "34UFD" "35VML" "49NHC" "49UCP" "55HEV"
# Copy according image folders to: /canopy_height/deploy_example/sentinel2/2020/
# variables <- dandelion::create_param_df(tiles = c("10TES", "17SNB", "20MMD", "32TMT", "32UQU", "33NTG", "34UFD", "35VML", "49NHC", "49UCP", "55HEV"), # 
#                                         bands = c("B05", "B8A", "B11", "B12"), # "B02", "B03", "B04", "B08"
#                                         increments = c(0.05, 0.1, 0.15, 0.2, 0.25),
#                                         decrease = c("False", "True"),              # False meaning increase...
#                                         year = "2020",
#                                         base_folder = "/home/emilio/canopy_height"
# )


point_selcetion <- function(lon_grid = 20, lat_grid = 15, lon_border = c(-180,180), lat_border = c(-60,60), coast_buffer = -0.05){
  
  # Load land polygons
  land <- ne_countries(scale = "medium", returnclass = "sf")
  land <- st_transform(land, crs = 4326)
  
  # Shrink land slightly
  land_buffered <- st_buffer(land, dist = coast_buffer)
  land_buffered <- land_buffered[!st_is_empty(land_buffered), ]
  
  # Define grid (systematic longitudinal lines)
  n_lines <- lon_grid
  n_lat_points <- lat_grid
  # lons <- round(seq(-120, 180, length.out = n_lines), 0)
  # lats <- round(seq(-60, 50, length.out = n_lat_points), 0)
  
  lons <- round(seq(lon_border[1], lon_border[2], length.out = n_lines), digits = 0)
  lats <- round(seq(lat_border[1], lat_border[2], length.out = n_lat_points), digits = 0)
  
  all_points <- list()
  
  # Generate points along each longitude and keep only land points
  for (lon in lons) {
    line_points <- data.frame(lon = rep(lon, n_lat_points),
                              lat = lats)
    points_sf <- st_as_sf(line_points, coords = c("lon", "lat"), crs = 4326)
    
    # Keep only points on buffered land
    intersections <- st_intersects(points_sf, land_buffered)
    land_points <- points_sf[lengths(intersections) > 0, ]
    
    all_points[[as.character(lon)]] <- land_points
  }
  
  # Combine all land points
  systematic_points <- do.call(rbind, all_points)
  
  # Extract coordinates if needed
  new_positions <- st_coordinates(systematic_points)
  
  # Assuming new_pos is a matrix with columns X (lon) and Y (lat)
  new_pos_char <- apply(new_positions, 1, function(row) paste(row[1], row[2]))
  
  return(new_pos_char)
  
}

latlong_variables <- function (tiles = c("10TES", "33NTG", "55HEV"), new_pos = NA,year = "2020",
                               base_folder = "/home/emilio/canopy_height", worldcover = "2020",
                               result_dir = "/data/ESA99/lat_lon_results/"){
  df <- expand.grid(tile_name = tiles, 
                    new_pos = new_pos, 
                    year = year, 
                    rootDIR = base_folder,
                    result_dir = result_dir,
                    WC_year = worldcover, 
                    original = FALSE, 
                    stringsAsFactors = FALSE)
  base_folder <- normalizePath(base_folder)
  df$tile_name <- trimws(df$tile_name)
  tile_folder <- file.path(base_folder, "deploy_example", 
                           "sentinel2", year)
  df$tile_folder <- file.path(tile_folder, df$tile_name)
  
  # Split "lon lat" into separate numeric columns
  coords <- do.call(rbind, strsplit(df$new_pos, " "))
  df$Longitude <- as.numeric(coords[, 1])
  df$Latitude  <- as.numeric(coords[, 2])
  
  df$out_name <- paste0(df$tile_name, "_Lat:", df$Latitude, "_Lon:", df$Longitude)
  
  for (t in unique(tiles)) {
    extra_row <- data.frame(
      tile_name   = t,
      new_pos     = NA,
      year        = year[1],
      rootDIR     = base_folder,
      result_dir = result_dir,
      WC_year     = worldcover,
      original    = TRUE,
      tile_folder = file.path(tile_folder, t),
      Longitude   = NA,
      Latitude    = NA,
      out_name    = paste0(t, "_original"),
      stringsAsFactors = FALSE
    )
    df <- rbind(extra_row, df)
    
  }
  df$out_dir <- file.path(base_folder, "final_results")
  return(df)
}

new_pos_char <- point_selcetion(lon_grid = 17, lat_grid = 12, lon_border = c(-120,180), lat_border = c(-60,50), coast_buffer = -0.05)

variables <- latlong_variables(tiles = c("10TES"), #"33NTG", "55HEV"
                            new_pos = new_pos_char,
                            year = "2020",
                            base_folder = "/home/emilio/canopy_height",
                            result_dir = "/data/ESA99/lat_lon_results/")

# Should the difference rasters be saved?
DIFF_TIF <- FALSE
# Should loop results be saved individually as backup (csv files)?
BACKUP_SAVING <- TRUE


# General Setup -----------------------------------------------------------

# create empty result list in correct length for more efficient deployment.
# results_list <- vector("list", nrow(variables)) # create empty list, convert to df later -> more efficient   ### OLD LIST WAY -> now df
# results_df <- data.frame(
#   tile = character(nrow(variables)),
#   band = character(nrow(variables)),
#   increment = numeric(nrow(variables)),
#   decrease = character(nrow(variables)),
#   
#   average_difference = numeric(nrow(variables)),
#   avg_abs_diff = numeric(nrow(variables)),
#   
#   avg_difference_percent = numeric(nrow(variables)),
#   avg_abs_diff_perc = numeric(nrow(variables)),
#   
#   correlation = numeric(nrow(variables)),
#   std_dev = numeric(nrow(variables)),
#   out_name = character(nrow(variables)),
#   year = character(nrow(variables)),
#   stringsAsFactors = FALSE
# )


## Translation table
translation_table <- data.frame(
  BandName = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B11", "B12"), # "B10", cirrus not included
  BandNumber = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) # B8A = 9, B09 = 10
)

# Setup for Worldcover check
wc_tile_status <- data.frame(
  tile_name = unique(variables$tile_name),
  edited = FALSE
)


#### Check for data availability ####
exist_flags <- file.path(file.path(variables$rootDIR[1], "deploy_example/sentinel2/2020/"), unique(variables$tile_name)) |>
  dir.exists()

if (all(exist_flags)) {
  cat("All data folders for unique tiles exist. Starting loop deployment.\n")
} else {
  missing <- unique(variables$tile_name)[!exist_flags]
  cat("=========================================    ERROR!    =========================================\n")
  cat("========================================= DATA MISSING =========================================\n")
  stop(paste("The following folders are missing:", paste(missing, collapse = ", ")))
}

# Time estimate
mean_loop_time <- 13.38203 # minutes -> derived from timing data of past loops
finish_estimate <- Sys.time() + (nrow(variables)*mean_loop_time/60 * 3600) 
cat("Estimated finishing time:", format(finish_estimate, "%Y-%m-%d %H:%M:%S"), "\n")


# DEPLOYMENT LOOP ---------------------------------------------------------

for (v in 1:nrow(variables)) {
  start_loop_time <- Sys.time() # Loop timing
  
  cat("======================================================================================================\n")
  cat("Starting deployment number", v, "of", nrow(variables),"\n")
  cat("Tile     :",variables$tile_name[v], "\n",
      "Latitude :",variables$Latitude[v], "\n",
      "Longitude:",variables$Longitude[v], "\n" )
  
  
  # Text file creation ------------------------------------------------------
  
  # Creation of a text file with the names of the corresponding zip-folders
  output_file <- file.path(variables$rootDIR[v], "deploy_example", "image_paths", variables$year[v], paste0(variables$tile_name[v], ".txt"))
  img_folder <- file.path(variables$rootDIR[v], "deploy_example", "sentinel2", variables$year[v], variables$tile_name[v])
  zip_files <- list.files(path = img_folder, pattern = paste0(".*", variables$tile_name[v], ".*\\.zip$"), full.names = FALSE)
  
  if (dir.exists( file.path(variables$rootDIR[v], "deploy_example", "image_paths", variables$year[v]))  == TRUE) {
    cat("TXT directory exists.\n")
  } else{
    dir.create( file.path(variables$rootDIR[v], "deploy_example", "image_paths", variables$year[v]), recursive = T)
    cat("TXT file directory created.\n")
  }
  
  writeLines(zip_files, output_file)
  cat("Created zip file list as text file:", output_file, "with", length(zip_files), "entries.\n")
  
  
  # Global Variables Setup ----------------------------------------------------
  
  # Translate band name
  # band_number <- translation_table$BandNumber[translation_table$BandName == variables$band[v]]
  
  # Create & set GLOBAL VARIABLES from variables data frame
  env_vars <- c(
    tile_name = variables$tile_name[v],
    wcover = variables$WC_year[v],
    YEAR = variables$year[v],
    
    new_pos = variables$new_pos[v],
    
    # MODIFY_BANDS = band_number,
    # MODIFY_PERCENTAGE = variables$increment[v], # or rate
    # MODIFY_DECREASE = variables$decrease[v],
    
    GCHM_DEPLOY_DIR = file.path("./deploy_example","predictions", variables$year[v], variables$tile_name[v]), # important for out_dir
    DEPLOY_IMAGE_PATH = list.files(img_folder, full.names = T)[1], # just the first image of the tile
    
    experiment = start_date_chr
    # experiment = as.character(v)
    # experiment = "experiment"
  )
  ("Global environment variables set.\n")
  
  
  # Worldcover adjustment ---------------------------------------------------
  
  cat("Checking allignment, crs, and extent of the corresponding Worldcover tile.\n")
  wcover_tiles <- list.files( file.path(variables$rootDIR[v], "deploy_example/ESAworldcover/2020/sentinel2_tiles"), full.names = T )
  
  dandelion::worldcover_adjust(wcover_tiles, wc_tile_status, df = variables, w = v, img_dir = img_folder)
  # WC_CHECK_FUN(wcover_tiles, wc_tile_status) # OUTPUT FILE SET TO TEST
  cat("World cover processing completed.\n")
  
  # Bash Deployment ---------------------------------------------------------
  
  cat("#################### Start model deployment loop",v,"####################\n")
  
  cat("+++++++++ deploy_example.sh start +++++++++\n")
  withr::with_envvar(env_vars, {
    system2("./gchm/bash/deploy_example.sh")
  })
  cat("deploy_example.sh finished.\n")
  
  cat("+++++++++ Run tile deploy merge start +++++++++\n")
  withr::with_envvar(env_vars, {
    system2("./gchm/bash/run_tile_deploy_merge.sh")
  })
  cat("run_tile_deploy_merge.sh finished.\n")
  
  
  # File organization -------------------------------------------------------
  
  ### Save image with a new name to designated folder
  cat("Copying and renaming prediction files.\n")
  
  # Create Result directory if necessary
  result_path <- file.path(variables$result_dir[v], variables$tile_name[v])
  if (dir.exists( result_path )  == TRUE) {
    cat("Directory exists.\n")
  } else{
    dir.create(result_path, recursive = T)
    cat("Result directory created:",result_path,"\n")
  }
  # Copy files to Result directory and rename
  model_prediction_tif <- list.files(file.path(variables$rootDIR[v],"deploy_example/predictions", 
                                               variables$year[v], 
                                               paste0(variables$tile_name[v], "_merge")), 
                                     recursive = T, 
                                     # pattern = "_pred\\.tif$", 
                                     pattern = paste0(start_date_chr, ".*_pred\\.tif$"),
                                     full.names = T)
  new_destination <- file.path(result_path, paste0(variables$out_name[v], ".tif"))
  cat("File to be copyied and renamed:", model_prediction_tif,"\n")
  cat("***************************************************************\n")
  cat("New destination and name:", new_destination, "\n")
  file.copy(from = model_prediction_tif,
            to = new_destination,
            overwrite = T)
  cat("Saving result rasters externaly. Loop",v,"completed.\n")
  cat("***************************************************************\n")
  
  # Remove predictions and std_dev at old location 
  cat("Removing pred and StDev files at the original merge location: ")
  file.path(variables$rootDIR[v],"deploy_example/predictions",
            variables$year[v],
            paste0(variables$tile_name[v], "_merge")) %>%
    list.files(recursive = T, full.names = T) %>%
    file.remove() # delete
  cat("Predictions removed from old location.\n")
  cat("\n")
  
  # Removing unmerged prediction files
  cat("Removing original unmerged prediction files: ")
  file.path(variables$rootDIR[v],"deploy_example/predictions",
            variables$year[v],
            variables$tile_name[v]) %>%
    list.files(recursive = T, full.names = T) %>%
    file.remove() # delete
  cat("Individual unmerged files removed.\n")
  cat("\n")
  
  # out_directory <- file.path(paste0(env_vars[["GCHM_DEPLOY_DIR"]], "_merge"), "preds_inv_var_mean")
  # outputFilePath <- file.path(out_dir, paste0(env_vars[["tile_name"]], "_", env_vars[["experiment"]], "_pred.tif"))
  
  
  # Difference calculation --------------------------------------------------
  
  # # Get original prediction and manipulated prediction tif
  # cat("Calculaing the difference to the original prediction.\n")
  # preds <- list.files(result_path, full.names = T)
  # cat("List of files:", preds, "\n")
  # 
  # # ORIGINAL
  # original_pred_dir <- preds[
  #   grepl(variables$tile_name[v], preds) &
  #     grepl("original", preds)
  # ]
  # cat("Original prediction file:", original_pred_dir, "\n")
  # 
  # # MANIPULATED
  # manipulated_filepath <- file.path(result_path, 
  #                                   paste0(variables$out_name[v], ".tif"))
  # cat("Manipulated image path:",manipulated_filepath, "\n")
  # 
  # original_pred <- rast(original_pred_dir)
  # manipulated_pred <- rast(manipulated_filepath)
  # 
  # 
  # ### Calculate the difference ###
  # 
  # cat("Calculate difference between", variables$out_name[v], "and original prediction:", basename(original_pred_dir),".\n")
  # # stopifnot(compareGeom(original_pred, manipulated_pred)) # test if rasters are alligned
  # 
  # # Difference raster in meters
  # difference <- manipulated_pred - original_pred     # Eventually layer has to be selected -> [[1]] or pattern _pred -> select above...
  # 
  # # Difference raster in percent (SMAPE formulation)
  # eps <- 1e-6
  # difference_percent <- ((manipulated_pred - original_pred) /
  #                          ((abs(manipulated_pred) + abs(original_pred)) / 2 + eps)) * 100
  # 
  # # Summaries
  # avg_diff <- global(difference, fun = "mean", na.rm = TRUE)[[1]]
  # avg_abs_diff <- global(abs(difference), fun = "mean", na.rm = TRUE)[[1]]
  # correlation <- cor(values(manipulated_pred), values(original_pred), method = "pearson", use = "complete.obs") |> as.numeric()
  # std_dev <- global(difference, fun = "sd", na.rm = TRUE)[[1]]
  # 
  # avg_percent_diff <- global(difference_percent, fun = "mean", na.rm = TRUE)[[1]]
  # avg_abs_percent_diff <- global(abs(difference_percent), fun = "mean", na.rm = TRUE)[[1]]
  # 
  # cat("Average diff [m]:", round(avg_diff, digits = 2), "\n",
  #     "Avg abs diff [m]:", round(avg_abs_diff, digits = 2), "\n",
  #     "Correlation     :", round(correlation, digits = 3), "\n",
  #     "Std dev [m]     :", round(std_dev, digits = 2), "\n",
  #     "Avg diff [%]    :", round(avg_percent_diff, digits = 1), "\n",
  #     "Avg abs diff [%]:", round(avg_abs_percent_diff, digits = 1), "\n")
  # 
  # 
  # ## Difference raster saving IF DESIRED
  # if (DIFF_TIF == TRUE) {
  #   cat("Saving difference raster...")
  #   diff_path <- file.path(result_path, "DIFF")
  #   diff_file <- file.path(diff_path, paste0("DIFF_", variables$out_name[v], ".tif"))
  #   
  #   if (!dir.exists(diff_path)) {
  #     dir.create(diff_path, recursive = TRUE)
  #   }
  #   writeRaster(difference, diff_file)
  #   cat("Difference raster saved as:", diff_file,"\n")
  #   
  # } else{
  #   cat("Difference raster will not be saved.\n")
  # }
  # 
  # cat("*****",variables$out_name[v], 
  #     "| Average difference:", round(avg_diff, digits = 2), 
  #     "| Avg absolut diff:", round(avg_abs_diff, digits = 2), 
  #     "| Standard deviation:", round(std_dev, digits = 2),"*****\n")
  # 
  
  # File removal except originals --------------------------------------------
  # 
  # originals_folder <- "/home/emilio/canopy_height/results/originals"
  # 
  # if(variables$original[v]){
  #   cat("Moving original prediction to", file.path(originals_folder, basename(new_destination)),"\n")
  #   file.copy(from = new_destination,
  #             to = file.path(originals_folder, basename(new_destination)),
  #             overwrite = T)
  #   # cat("Removing file at old location.\n")
  #   # file.remove(new_destination)  # Not possible at the moment as the loop expects it to be at new_destination location
  # }  else {
  #   file.remove(new_destination)
  #   cat("Modified prediction file", basename(new_destination),"deleted.\n")
  # }
  # 
  
  # Save results ------------------------------------------------------------
  
  # Save to result dataframe
  # loop_results <- list(
  #   tile = variables$tile_name[v],
  #   band = variables$band[v],
  #   increment = variables$increment[v],
  #   decrease = variables$decrease[v],
  #   average_difference =    avg_diff,
  #   avg_abs_diff =           avg_abs_diff,
  #   avg_differece_percent = avg_percent_diff,
  #   avg_abs_diff_perc =      avg_abs_percent_diff,
  #   correlation = correlation ,
  #   std_dev = std_dev,
  #   out_name = variables$out_name[v],
  #   year = variables$year[v]
  # )
  # 
  # results_df[v, ] <- loop_results
  # cat("Result added to data frame. Loop", v, "completed.\n")
  
  # Backup saving
  # if (BACKUP_SAVING == TRUE) {
  #   cat("Backup saving individual loop result.\n")
  #   loop_results_df <- as.data.frame(loop_results, stringsAsFactors = FALSE)
  #   ifelse(dir.exists("results/loop_backup/"),"Backup saving in progress..\n", dir.create("results/loop_backup/") & cat("Backup directory created.\n"))
  #   backup_dir <- paste0("results/loop_backup/",Sys.Date(),"_loop_",v,".csv")
  #   write.csv(loop_results_df, backup_dir, row.names = FALSE)
  #   cat("******* Loop results saved individually as backup at:",backup_dir,"*******\n")
  # } else{
  #   cat("Individual loop results not backed up.\n")
  # }
  # 
  
  #*** TIMING BLOCK ***
  end_loop_time <- Sys.time()
  duration <- difftime(end_loop_time, start_loop_time, units = "mins") %>% 
    round(2) %>% 
    as.numeric()
  
  if (is.numeric(duration)) {
    
    cat("Loop", v, "completed. Elapsed time:", duration, "min.\n")
    timing_results <- rbind(timing_results,
                            data.frame(Step = paste0(v, "/", nrow(variables)), 
                                       Minutes = as.numeric(duration), 
                                       stringsAsFactors = FALSE))
  } else {
    warning("Duration is not numeric. Skipping timing log for this loop.")
  }
  
  write.csv(timing_results, paste0("documentation/TIMING/", start_date_chr, "_Timing.csv"))
  cat("******* Timing stored successfully. Loop fully completed. *******\n")
  cat("Time:",format(Sys.time(), "%Y-%m-%d %H:%M"),"\n")
  
}



# EXPORT RESULTS in a robust way -----------------------------------------------------

# cat("Preparing to save results...\n")
# 
# date_tag <- format(Sys.Date(), "%Y-%m-%d")
# 
# try_export <- try({
#   save_path <- file.path("results", paste0(start_date_chr, "_result_table.csv"))
#   dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
#   write.csv(results_df, save_path, row.names = FALSE)
#   cat("=====================================================================================================\n")
#   cat("                            Full combined results saved successfully!\n")
#   cat("=====================================================================================================\n")
#   combined_success <- TRUE
# }, silent = TRUE)
# 
# if (inherits(try_export, "try-error")) {
#   warning("Failed to save combined results as data frame. Skipping combined export.\nSaving individual result files instead.\n")
#   combined_success <- FALSE
#   
#   # Save each row individually
#   # indiv_dir <- file.path("results", paste0("export_fallback_results_", start_date_chr))
#   indiv_dir <- file.path("results", "fallback", paste0("fallback_", start_date_chr) )
#   dir.create(indiv_dir, recursive = TRUE, showWarnings = FALSE)
#   
#   cat("Saving individual result files due to fallback mechanism...\n")
#   
#   for (i in seq_len(nrow(results_df))) {
#     entry_df <- results_df[i, , drop = FALSE]   # single-row data.frame
#     file_name <- paste0(sprintf("%03d", i), "_result.csv")
#     file_path <- file.path(indiv_dir, file_name)
#     
#     tryCatch({
#       write.csv(entry_df, file_path, row.names = FALSE)
#       individual_export_success <- TRUE
#     }, error = function(e) {
#       warning(sprintf("Failed to save individual result %d: %s", i, e$message))
#       individual_export_success <- FALSE
#     })
#   }
#   
#   if (individual_export_success) {
#     cat("All individual result files saved successfully to", indiv_dir, "\n")
#   } else {
#     warning("Some individual results failed to save — check log messages above.")
#   }
#   
# } else {
#   cat("Full results table saved to", save_path, "\n")
# }



# TIMING ------------------------------------------------------------------

# Track and show time elapsed
end_time <- Sys.time()
cat("******************************* Job finished. Time elapsed:", 
    {
      secs <- as.numeric(difftime(end_time, start_time, units = "secs"))
      sprintf("%02d:%02d:%02d",
              floor(secs / 3600),
              floor((secs %% 3600) / 60),
              floor(secs %% 60))
    }," *******************************\n"
)


# Print timing table at the end
print(timing_results, row.names = FALSE)
#Average time per loop
cat("\n")
cat("Average time per loop:",
    {
      avg_time <- as.numeric(difftime(end_time, start_time, units = "secs")/nrow(variables))
      sprintf("%02d:%02d:%02d",
              floor(avg_time / 3600),
              floor((avg_time %% 3600) / 60),
              floor(avg_time %% 60))
    },"\n"
)

cat("++++++++++++++++++++++++++++ All jobs finished. Full script ran succesfully. ++++++++++++++++++++++++++++\n")



