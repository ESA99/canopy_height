# Packages ----------------------------------------------------------------
options(repos = c(CRAN = "https://cloud.r-project.org"))

packages <- c(
  "sf", "terra", "tmap", "remotes",
  "rnaturalearth", "rnaturalearthdata",
  "plyr", "dplyr", "leaflet",
  "viridis", "cols4all", "colorspace"
)

# Install any packages that are missing
installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)

if (length(to_install) > 0) {
  message("Installing missing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install, dependencies = TRUE)
}

# Install dandelion from GitHub if missing
if (!"dandelion" %in% installed) {
  message("Installing package 'dandelion' from GitHub...")
  remotes::install_github("ESA99/dandelion")
}

# Load all packages
packages <- c(packages, "dandelion")
invisible(lapply(packages, library, character.only = TRUE))



# Timing setup  ----------------------------------------------------------
start_time <- Sys.time()
start_date_chr <- format(Sys.Date(), "%Y-%m-%d")

# Create empty data frame to store timing info
timing_results <- data.frame(
  Step = character(),
  Minutes = numeric(),
  stringsAsFactors = FALSE
)

# VARIABLE INPUT TABLE -----------------------------------------------------

create_param_interactions <- function (tiles, bands, increments, decrease, year, base_folder, worldcover = "2020") {
  
  if (!is.list(bands)) { bands <- as.list(bands)  } # Ensure bands is always a list of vectors
  
  df <- expand.grid(tile_name = tiles, band = bands, decrease = decrease, 
                    increment = increments, year = year, rootDIR = base_folder, 
                    WC_year = worldcover, original = FALSE, stringsAsFactors = FALSE)
  base_folder <- normalizePath(base_folder)
  df$tile_name <- trimws(df$tile_name)
  tile_folder <- file.path(base_folder, "deploy_example", 
                           "sentinel2", year)
  df$tile_folder <- file.path(tile_folder, df$tile_name)
  df$out_name <- paste0( df$tile_name, "_",
                         vapply(df$band, function(b) paste(b, collapse = "-"), character(1)), "_",
                         sub("0\\.", "", formatC(df$increment, format = "f", digits = 2)), "_",
                         ifelse(df$decrease == "False", "I", "D")
  )
  for (t in unique(tiles)) {
    extra_row <- data.frame(tile_name = t, band = I(list(df$band[[1]])), 
                            decrease = "True", increment = 0, year = year[1], 
                            rootDIR = base_folder, WC_year = worldcover, original = TRUE, 
                            stringsAsFactors = FALSE)
    extra_row$tile_folder <- file.path(tile_folder, t)
    extra_row$out_name <- paste0(t, "_original")
    df <- rbind(extra_row, df)
  }
  df$out_dir <- file.path(base_folder, "final_results")
  return(df)
}

# Input of the parameters as data frame with all combinations
  # All tiles: "10TES" "17SNB" "20MMD" "32TMT" "32UQU" "33NTG" "34UFD" "35VML" "49NHC" "49UCP" "55HEV"
  # Copy according image folders to: /canopy_height/deploy_example/sentinel2/2020/
variables <- create_param_interactions(tiles = c("49UCP"), # "10TES", "17SNB", "20MMD", "32TMT", "32UQU", "33NTG", "34UFD", "35VML", "49NHC", "49UCP", "55HEV"
                                        bands = list(c("B02","B04"),c("B03","B05"), c("B04", "B08"), c("B03","B04","B11","B12")), # "B02", "B03", "B04", "B08", "B05", "B8A", "B11", "B12"
                                        increments = c(0.05, 0.1, 0.15, 0.2, 0.25), # 0.05, 0.1, 0.15, 0.2, 0.25
                                        decrease = c("False", "True" ),  #          # False meaning increase...
                                        year = "2020",
                                        base_folder = "/home/emilio/canopy_height"
)

# Should loop results be saved individually as backup (csv files)?
BACKUP_SAVING <- TRUE
# Should the difference rasters be saved?
DIFF_TIF <- FALSE
# Should the prediction result tif's be saved and where?
PRED_TIF <- TRUE
PRED_TIF_LOCATION <- "/data/ESA99/resultmaps_bands/I"

# General Setup -----------------------------------------------------------

# create empty result list in correct length for more efficient deployment.
# results_list <- vector("list", nrow(variables)) # create empty list, convert to df later -> more efficient   ### OLD LIST WAY -> now df
results_df <- data.frame(
  tile = character(nrow(variables)),
  band = character(nrow(variables)),
  increment = numeric(nrow(variables)),
  decrease = character(nrow(variables)),
  
  mean_height = numeric(nrow(variables)),
  
  average_difference = numeric(nrow(variables)),
  avg_abs_diff = numeric(nrow(variables)),
  
  avg_difference_percent = numeric(nrow(variables)),
  avg_abs_diff_perc = numeric(nrow(variables)),
  
  correlation = numeric(nrow(variables)),
  std_dev = numeric(nrow(variables)),
  out_name = character(nrow(variables)),
  year = character(nrow(variables)),
  stringsAsFactors = FALSE
)


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
  cat("Tile:",variables$tile_name[v], "\n",
      "Band:",variables$band[[v]], "\n",
      "Increment:",variables$increment[v], "\n",
      "Direction:", ifelse(variables$decrease[v] == "False", "Increase", "Decrease"),"\n")
  

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
  band_number <- translation_table$BandNumber[translation_table$BandName == variables$band[[v]]]
  
  # Create & set GLOBAL VARIABLES from variables data frame
  env_vars <- c(
    tile_name = variables$tile_name[v],
    wcover = variables$WC_year[v],
    YEAR = variables$year[v],
    
    MODIFY_BANDS = band_number,
    MODIFY_PERCENTAGE = variables$increment[v], # or rate
    MODIFY_DECREASE = variables$decrease[v],
    
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

  ### Save image with a new name to designated folder (out_dir)
  cat("Copying and renaming prediction files.\n")
  
  ## Create Result directory if necessary
  result_path <- file.path(variables$out_dir[v], "preds", variables$tile_name[v])
  if (dir.exists( result_path )  == TRUE) {
    cat("Result directory exists:",result_path,"\n")
  } else{
    dir.create(result_path, recursive = T)
    cat("Result directory created:",result_path,"\n")
  }
  
  ## Copy prediction files to Result directory and rename (out_name)
  model_prediction_tif <- list.files(file.path(variables$rootDIR[v],"deploy_example/predictions", 
                                               variables$year[v], 
                                               paste0(variables$tile_name[v], "_merge")), 
                                     recursive = T, 
                                     # pattern = "_pred\\.tif$", 
                                     pattern = paste0(start_date_chr, ".*_pred\\.tif$"),
                                     full.names = T)
  new_destination <- file.path(result_path, paste0(variables$out_name[v], ".tif"))
  cat("File to be copyied and renamed:", model_prediction_tif,"\n")
  cat("New destination and name:", new_destination, "\n")
  file.copy(from = model_prediction_tif,
            to = new_destination,
            overwrite = T)
  cat("Copying and renaming successfully completed.\n")
  
  ## Remove predictions and std_dev at old location 
  old_pred_location <-   file.path(variables$rootDIR[v],"deploy_example/predictions",
                                  variables$year[v],
                                  paste0(variables$tile_name[v], "_merge"))
  cat("Removing pred and StDev files at the original merge location:",old_pred_location,"\n")
  old_pred_location %>%
    list.files(recursive = T, full.names = T) %>%
    file.remove() # delete
  cat("-> DONE\n")
  
  ## Remove un-merged prediction files from Ensemble
  individual_preds_location <- file.path(variables$rootDIR[v],"deploy_example/predictions",
                                         variables$year[v],
                                         variables$tile_name[v])
  cat("Removing original unmerged prediction files:",individual_preds_location,"\n")
  individual_preds_location %>%
    list.files(recursive = T, full.names = T) %>%
    file.remove() # delete
  cat("-> DONE\n")

  
  
# Difference calculation --------------------------------------------------

  # Get original prediction and manipulated prediction tif
  cat("Calculaing the difference to the original prediction.\n")
  preds <- list.files(result_path, full.names = T)
  cat("List of files:", preds, "\n")
  
  # ORIGINAL
  original_pred_dir <- preds[
    grepl(variables$tile_name[v], preds) &
      grepl("original", preds)
  ]
  cat("Original prediction file:", original_pred_dir, "\n")
  
  # MANIPULATED
  manipulated_filepath <- file.path(result_path, 
                                    paste0(variables$out_name[v], ".tif"))
  cat("Manipulated image path:",manipulated_filepath, "\n")
  
  original_pred <- rast(original_pred_dir)
  manipulated_pred <- rast(manipulated_filepath)
  

  ### Calculate the difference ###
  
  cat("Calculate difference between", variables$out_name[v], "and original prediction:", basename(original_pred_dir),".\n")
  # stopifnot(compareGeom(original_pred, manipulated_pred)) # test if rasters are alligned
  
  # Difference raster in meters
  difference <- manipulated_pred - original_pred     # Eventually layer has to be selected -> [[1]] or pattern _pred -> select above...
  
  # Difference raster in percent (SMAPE formulation)
  eps <- 1e-6
  difference_percent <- ((manipulated_pred - original_pred) /
                           ((abs(manipulated_pred) + abs(original_pred)) / 2 + eps)) * 100
  
  # Summaries
  mean_CH <- global(manipulated_pred, fun = "mean", na.rm = TRUE)[[1]]
  avg_diff <- global(difference, fun = "mean", na.rm = TRUE)[[1]]
  avg_abs_diff <- global(abs(difference), fun = "mean", na.rm = TRUE)[[1]]
  correlation <- cor(values(manipulated_pred), values(original_pred), method = "pearson", use = "complete.obs") |> as.numeric()
  std_dev <- global(difference, fun = "sd", na.rm = TRUE)[[1]]
  
  avg_percent_diff <- global(difference_percent, fun = "mean", na.rm = TRUE)[[1]]
  avg_abs_percent_diff <- global(abs(difference_percent), fun = "mean", na.rm = TRUE)[[1]]
  
  cat("Average diff [m]:", round(avg_diff, digits = 2), "\n",
      "Avg abs diff [m]:", round(avg_abs_diff, digits = 2), "\n",
      "Correlation     :", round(correlation, digits = 3), "\n",
      "Std dev [m]     :", round(std_dev, digits = 2), "\n",
      "Avg diff [%]    :", round(avg_percent_diff, digits = 1), "\n",
      "Avg abs diff [%]:", round(avg_abs_percent_diff, digits = 1), "\n")
    


# Save Difference raster IF TRUE ------------------------------------------

  if (DIFF_TIF == TRUE) {
    cat("Saving difference raster...")
    diff_path <- file.path(result_path, "DIFF")
    diff_file <- file.path(diff_path, paste0("DIFF_", variables$out_name[v], ".tif"))
    
    if (!dir.exists(diff_path)) {
      dir.create(diff_path, recursive = TRUE)
    }
    writeRaster(difference, diff_file)
    cat("Difference raster saved as:", diff_file,"\n")
    
  } else{
    cat("Difference raster will not be saved.\n")
  }
  
  # cat("*****",variables$out_name[v], 
  #     "| Average difference:", round(avg_diff, digits = 2), 
  #     "| Avg absolut diff:", round(avg_abs_diff, digits = 2), 
  #     "| Standard deviation:", round(std_dev, digits = 2),"*****\n")
  

# File removal except originals --------------------------------------------


  # Export Result Rasters if desired (PRED_TIF = TRUE)
  if (PRED_TIF) {
    cat("### Prediction rasters will be saved at:", PRED_TIF_LOCATION,"\n")
    
    if (dir.exists(PRED_TIF_LOCATION) == FALSE) {
      dir.create(PRED_TIF_LOCATION) }
    
    file.copy(from = new_destination,
              to = file.path(PRED_TIF_LOCATION, basename(new_destination)),
              overwrite = T)
    cat("Export of result rasters successfull.\n")
    # file.remove(new_destination)
    # cat("File removed at previous destination:", new_destination,"\n")
  
  } else {
        
    # Move originals to designated folder and delete non-originals
      cat("Final prediction Rasters will not be saved. Only Originals...\n")
        originals_folder <- "/home/emilio/canopy_height/results/originals"
        if(variables$original[v]){
          cat("Moving original prediction to", file.path(originals_folder, basename(new_destination)),"\n")
          file.copy(from = new_destination,
                    to = file.path(originals_folder, basename(new_destination)),
                    overwrite = T)
          # cat("Removing file at old location.\n")
          # file.remove(new_destination)  # Not possible at the moment as the loop expects it to be at new_destination location
        }  else {
          file.remove(new_destination)
          cat("Modified prediction file", basename(new_destination),"deleted.\n")
        }
      
  }
  
  
  

# Save results ------------------------------------------------------------

  # Save to result dataframe
  loop_results <- list(
    tile = variables$tile_name[v],
    band = variables$band[[v]],
    increment = variables$increment[v],
    decrease = variables$decrease[v],
    mean_height = mean_CH,
    average_difference =    avg_diff,
    avg_abs_diff =           avg_abs_diff,
    avg_differece_percent = avg_percent_diff,
    avg_abs_diff_perc =      avg_abs_percent_diff,
    correlation = correlation ,
    std_dev = std_dev,
    out_name = variables$out_name[v],
    year = variables$year[v]
  )
  
  results_df[v, ] <- loop_results
  cat("Result added to data frame. Loop", v, "completed.\n")
  
  # Backup saving
  if (BACKUP_SAVING == TRUE) {
    cat("Backup saving individual loop result.\n")
    loop_results_df <- as.data.frame(loop_results, stringsAsFactors = FALSE)
    ifelse(dir.exists("results/loop_backup/"),"Backup saving in progress..\n", dir.create("results/loop_backup/") & cat("Backup directory created.\n"))
    backup_dir <- paste0("results/loop_backup/",Sys.Date(),"_loop_",v,".csv")
    write.csv(loop_results_df, backup_dir, row.names = FALSE)
    cat("******* Loop results saved individually as backup at:",backup_dir,"*******\n")
  } else{
    cat("Individual loop results not backed up.\n")
  }
  
  
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

cat("Preparing to save results...\n")

date_tag <- format(Sys.Date(), "%Y-%m-%d")

try_export <- try({
  save_path <- file.path("results", paste0(start_date_chr, "_result_table.csv"))
  dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(results_df, save_path, row.names = FALSE)
  cat("=====================================================================================================\n")
  cat("                            Full combined results saved successfully!\n")
  cat("=====================================================================================================\n")
  combined_success <- TRUE
}, silent = TRUE)

if (inherits(try_export, "try-error")) {
  warning("Failed to save combined results as data frame. Skipping combined export.\nSaving individual result files instead.\n")
  combined_success <- FALSE
  
  # Save each row individually
  # indiv_dir <- file.path("results", paste0("export_fallback_results_", start_date_chr))
  indiv_dir <- file.path("results", "fallback", paste0("fallback_", start_date_chr) )
  dir.create(indiv_dir, recursive = TRUE, showWarnings = FALSE)
  
  cat("Saving individual result files due to fallback mechanism...\n")
  
  for (i in seq_len(nrow(results_df))) {
    entry_df <- results_df[i, , drop = FALSE]   # single-row data.frame
    file_name <- paste0(sprintf("%03d", i), "_result.csv")
    file_path <- file.path(indiv_dir, file_name)
    
    tryCatch({
      write.csv(entry_df, file_path, row.names = FALSE)
      individual_export_success <- TRUE
    }, error = function(e) {
      warning(sprintf("Failed to save individual result %d: %s", i, e$message))
      individual_export_success <- FALSE
    })
  }
  
  if (individual_export_success) {
    cat("All individual result files saved successfully to", indiv_dir, "\n")
  } else {
    warning("Some individual results failed to save — check log messages above.")
  }
  
} else {
  cat("Full results table saved to", save_path, "\n")
}



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
},"\n")

cat("**************************** Summary ****************************\n")
cat("Total time elapsed:",round(as.numeric(difftime(end_time, start_time, units = "hours")), digits = 3),"hours.\n")
cat("Total number of loops/predictions:",nrow(variables),"\n")
cat("Tiles processed:",unique(variables$tile_name),"\n")
cat("Bands processed:",unique(variables$band),"\n")
if (BACKUP_SAVING) { cat("Backup saved for each loop.\n") } else { cat("No Backup saved.\n") }
if (PRED_TIF) { cat("Prediction TIFs saved to:", PRED_TIF_LOCATION, "\n")} else { cat("No prediction TIFs saved.\n") }
if (DIFF_TIF) { cat("Difference rasters saved to ./final_results/preds/TILE/DIFF\n") } else { cat("Difference rasters not saved.\n") }


cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
cat("++++++++++++++++++++++++++++++++ All jobs finished. Full script ran succesfully. ++++++++++++++++++++++++++++++++\n")
cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")


