# Document setup  ----------------------------------------------------------
start_time <- Sys.time()
start_date_chr <- as.character( as.Date(start_time) )

# Create empty data frame to store timing info
timing_results <- data.frame(
  Step = character(),
  Minutes = numeric(),
  stringsAsFactors = FALSE
)

# Load necessary packages
c(
  "sf", "terra", "tmap", "dandelion",
  "rnaturalearth", "rnaturalearthdata",
  "plyr", "dplyr", "leaflet",
  "viridis", "cols4all", "colorspace"
) |>
  lapply(library, character.only = TRUE)


# Variable input table -----------------------------------------------------

# Input of the parameters as data frame with all combinations
  # All tiles: "10TES" "17SNB" "20MMD" "32TMT" "32UQU" "33NTG" "34UFD" "35VML" "49NHC" "49UCP" "55HEV"
  # Copy according image folders to: /canopy_height/deploy_example/sentinel2/2020/
variables <- dandelion::create_param_df(tiles = c("49UCP", "49NHC", "55HEV"),
                                        bands = c("B02", "B03", "B04", "B08"),
                                        increments = c(0.05, 0.1, 0.15, 0.2, 0.25),
                                        decrease = c("False", "True"),              # False meaning increase...
                                        year = "2020",
                                        base_folder = "/home/emilio/canopy_height"
)

# Should the difference rasters be saved?
DIFF_TIF <- FALSE
# Should loop results be saved individually as backup (csv files)?
BACKUP_SAVING <- TRUE


# General Setup -----------------------------------------------------------

# create empty result list in correct length for more efficient deployment.
results_list <- vector("list", nrow(variables)) # create empty list, convert to df later -> more efficient

## Translation table
translation_table <- data.frame(
  BandName = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B10", "B11", "B12"),
  BandNumber = c(1, 2, 3, 4, 5, 6, 7, 8, 8.1, 9, 10, 11, 12)
)

# Setup for Worldcover check
wc_tile_status <- data.frame(
  tile_name = unique(variables$tile_name),
  edited = FALSE
)


# DEPLOYMENT LOOP ---------------------------------------------------------


for (v in 1:nrow(variables)) {
  start_loop_time <- Sys.time() # Loop timing
  
  cat("======================================================================================================\n")
  cat("Starting deployment number", v, "of", nrow(variables),"\n")
  cat("Tile:",variables$tile_name[v], "\n",
      "Band:",variables$band[v], "\n",
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
  band_number <- translation_table$BandNumber[translation_table$BandName == variables$band[v]]
  
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

  ### Save image with a new name to designated folder
  cat("Copying and renaming prediction files.\n")
  
  # Create Result directory if necessary
  result_path <- file.path(variables$out_dir[v], "preds", variables$tile_name[v])
  if (dir.exists( result_path )  == TRUE) {
    cat("Directory exists.\n")
  } else{
    dir.create(result_path, recursive = T)
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
  cat("New destination and name:", new_destination, "\n")
  file.copy(from = model_prediction_tif,
            to = new_destination,
            overwrite = T)
  cat("Copying with new name successfully completed.\n")
  
  # Remove predictions and std_dev at old location 
  cat("Removing pred and StDev files at the original merge location: ")
  file.path(variables$rootDIR[v],"deploy_example/predictions",
            variables$year[v],
            paste0(variables$tile_name[v], "_merge")) %>%
    list.files(recursive = T, full.names = T) %>%
    file.remove() # delete
  cat("\n")
  
  # Removing unmerged prediction files
  cat("Removing original unmerged prediction files: ")
  file.path(variables$rootDIR[v],"deploy_example/predictions",
            variables$year[v],
            variables$tile_name[v]) %>%
    list.files(recursive = T, full.names = T) %>%
    file.remove() # delete
  cat("\n")

  # out_directory <- file.path(paste0(env_vars[["GCHM_DEPLOY_DIR"]], "_merge"), "preds_inv_var_mean")
  # outputFilePath <- file.path(out_dir, paste0(env_vars[["tile_name"]], "_", env_vars[["experiment"]], "_pred.tif"))
  
# Difference calculation --------------------------------------------------

  # compare images to original
  cat("Calculaing the difference to the original prediction.\n")
  preds <- list.files(result_path, full.names = T)
  cat("List of files:", preds, "\n")
  
  original_pred_dir <- preds[
    grepl(variables$tile_name[v], preds) &
      grepl("original", preds)
  ]
  cat("Original prediction file:", original_pred_dir, "\n")
  
  manipulated_filepath <- file.path(result_path, 
                                    paste0(variables$out_name[v], ".tif"))
  cat("Manipulated image path:",manipulated_filepath, "\n")
  
  original_pred <- rast(original_pred_dir)
  manipulated_pred <- rast(manipulated_filepath)
  
  cat("Calculate difference between", variables$out_name[v], "and original prediction:", basename(original_pred_dir),".\n")
  # Calculate the difference
  difference <- manipulated_pred - original_pred     # Eventually layer has to be selected -> [[1]] or pattern _pred -> select above...
  avg_diff <- mean(values(difference), na.rm = TRUE)  
  avg_abs_diff <- mean(abs(values(difference)), na.rm = TRUE)
  # korrelationskoeffizient manipulated_pred, original_pred 
  correlation <- cor(values(manipulated_pred), values(original_pred), method = "pearson") |>
    as.numeric()
  # std deviation of difference
  std_dev <- sd(na.omit(values(difference)))
  
  # Calculate average percent difference
  percent_diff <- (difference / original_pred) * 100
  avg_percent_diff <- mean(values(percent_diff), na.rm = TRUE)
  # + abs avg %
  avg_abs_percent_diff <- mean(abs(values(percent_diff)), na.rm = TRUE)
  
  
  
  ## save difference rasters if wanted
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
  
  cat("*****",variables$out_name[v], 
      "| Average difference:", avg_diff, 
      "| Avg absolut diff:", avg_abs_diff, 
      "| Standard deviation:", std_dev,"*****\n")
  

# File removal except originals --------------------------------------------

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
  

# Save results ------------------------------------------------------------

    # safe result to list
   loop_results <- list(
    tile = variables$tile_name[v],
    band = variables$band[v],
    increment = variables$increment[v],
    decrease = variables$decrease[v],
    average_difference = avg_diff,
    avg_differece_percent = percent_diff,
    avg_abs_diff = avg_abs_diff,
    avg_abs_diff_perc = avg_abs_percent_diff,
    correlation = correlation ,
    std_dev = std_dev,
    out_name = variables$out_name[v],
    year = variables$year[v]
  )
  
  results_list[[v]] <- loop_results
  cat("Result added to list. Loop", v, "completed.\n")
  
  # Backup saving
  if (BACKUP_SAVING == TRUE) {
    cat("Backup saving individual loop result.\n")
    loop_results <- as.data.frame(loop_results, stringsAsFactors = FALSE)
    ifelse(dir.exists("results/loop_backup/"),"Backup saving in progress..\n", dir.create("results/loop_backup/") & cat("Backup directory created.\n"))
    write.csv(loop_results, paste0("results/loop_backup/LoopResults_",v,".csv"), row.names = FALSE)
    cat("Loop results saved individually as backup at: results/result_tables_each_loop/LoopResults_X.csv\n")
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
  
}


# Export result table -----------------------------------------------------

# OLD STRAIGHT EXPORT
# # Combine list into a data frame
# cat("Transforming result list to a data frame.\n")
# results_list_clean <- Filter(function(x) !is.null(x) && length(x) > 0, results_list)
# results_df <- do.call(rbind, lapply(results_list_clean, as.data.frame))
# save_path <- paste0("results/",Sys.Date(),"_result_table.csv")
# cat("Writing results...\n")
# write.csv(results_df, save_path, row.names = FALSE)
# cat("Results saved as table to", save_path)

# NEW EXPORT IN AS ROBUST WAY -----------------------------------------------------

cat("Transforming result list to a data frame...\n")

results_list_clean <- Filter(function(x) !is.null(x) && length(x) > 0, results_list)

# Try combining all results into one data frame
try_combined <- try({
  results_df <- do.call(rbind, results_list_clean)
  combined_success <- TRUE
}, silent = TRUE)

if (inherits(try_combined, "try-error")) {
  warning("Failed to combine results_list into a single data frame. Skipping combined export.\nSaving individual result files instead.\n")
  combined_success <- FALSE
  
  # Save individual list entries as separate CSVs
  indiv_dir <- file.path("results", "individual_results")
  dir.create(indiv_dir, recursive = TRUE, showWarnings = FALSE)
  
  cat("Saving individual result files...\n")
  
  for (i in seq_along(results_list_clean)) {
    entry <- results_list_clean[[i]]
    entry_df <- as.data.frame(entry)
    file_name <- paste0(sprintf("%03d", i), "_result.csv")
    file_path <- file.path(indiv_dir, file_name)
    
    tryCatch({
      write.csv(entry_df, file_path, row.names = FALSE)
    }, error = function(e) {
      warning(sprintf("Failed to save individual result %d: %s", i, e$message))
    })
  }
  
  cat("Individual result files saved to", indiv_dir, "\n")
  
} else {
  # Combined export succeeded
  save_path <- file.path("results", paste0(Sys.Date(), "_result_table.csv"))
  dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
  cat("Writing combined results CSV...\n")
  write.csv(results_df, save_path, row.names = FALSE)
  cat("Combined results saved to", save_path, "\n")
}


# Timing ------------------------------------------------------------------

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

cat("++++++++++++++++++++++++++++ All jobs finished. Full script ran succesfully. ++++++++++++++++++++++++++++\n")



