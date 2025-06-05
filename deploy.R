# Document setup  ----------------------------------------------------------
start_time <- Sys.time()

# Create empty data frame to store timing info
timing_results <- data.frame(
  Step = character(),
  Duration = character(),
  stringsAsFactors = FALSE
)

c(
  "sf", "terra", "tmap", "dandelion",
  "rnaturalearth", "rnaturalearthdata",
  "plyr", "dplyr", "leaflet",
  "viridis", "cols4all", "colorspace"
) |>
  lapply(library, character.only = TRUE)


# Variable input table -----------------------------------------------------

# Input of the parameters
variables <- dandelion::create_param_df(tiles = c("T31UGT", "T32ULB","T33UUT"), 
                                        bands = c("B03", "B04", "B08"),
                                        increments = c(0.05, 0.1, 0.15, 0.2),
                                        decrease = c("False", "True"),              # False meaning increase...
                                        year = "2020",
                                        base_folder = "/home/emilio/canopy_height"
)
# Should the difference rasters be saved?
DIFF_TIF <- FALSE


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
# source("/home/emilio/canopy_height/R/WC_CHECK_FUN.R")



# Deployment LOOP ---------------------------------------------------------


for (v in 1:nrow(variables)) {
  start_loop_time <- Sys.time()
  
  cat("======================================================================================================\n")
  cat("Starting deployment number", v, "of", nrow(variables),"\n")
  cat("Tile:",variables$tile_name[v], "\n",
      "Band:",variables$band[v], "\n",
      "Increment:",variables$increment[v], "\n",
      "Decrease:", ifelse(variables$decrease[v] == "False", "Increase", "Decrease"),"\n")
  

# Text file creation ------------------------------------------------------

  output_file <- file.path(variables$rootDIR[v], "deploy_example", "image_paths", variables$year[v], paste0(variables$tile_name[v], ".txt"))
  img_folder <- file.path(variables$rootDIR[v], "deploy_example", "sentinel2", variables$year[v], variables$tile_name[v])
  zip_files <- list.files(path = img_folder, pattern = paste0(".*", variables$tile_name[v], ".*\\.zip$"), full.names = FALSE)
  
  if (dir.exists( file.path(variables$rootDIR[v], "deploy_example", "image_paths", variables$year[v]))  == TRUE) {
    cat("TXT directory exists.\n")
  } else{
    dir.create( file.path(variables$rootDIR[v], "deploy_example", "image_paths", variables$year[v]), recursive = T)
  }
  
  writeLines(zip_files, output_file)
  cat("Created zip file list as text file:", output_file, "with", length(zip_files), "entries.\n")
  
  
# Global Variables Setup ----------------------------------------------------

  # Translate band name
  band_number <- translation_table$BandNumber[translation_table$BandName == variables$band[v]]
  
  # Create & set global variables from df
  env_vars <- c(
    tile_name = variables$tile_name[v],
    wcover = variables$WC_year[v],
    YEAR = variables$year[v],
    
    MODIFY_BANDS = band_number,
    MODIFY_PERCENTAGE = variables$increment[v], # or rate
    MODIFY_DECREASE = variables$decrease[v],
    
    GCHM_DEPLOY_DIR = file.path("./deploy_example","predictions", variables$year[v], variables$tile_name[v]), # important for out_dir
    DEPLOY_IMAGE_PATH = list.files(img_folder, full.names = T)[1], # just the first image of the tile
    
    experiment = as.character( as.Date(start_time) )
    # experiment = as.character(v)
    # experiment = "experiment"
  )
  ("Environment variables set.\n")
  
  
# Worldcover adjustment ---------------------------------------------------

  cat("Checking allignment, crs, and extent of the corresponding Worldcover tile.\n")
  wcover_tiles <- list.files( file.path(variables$rootDIR[v], "deploy_example/ESAworldcover/2020/sentinel2_tiles"), full.names = T )
  
  dandelion::worldcover_adjust(wcover_tiles, wc_tile_status)
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

  ### save image with a new name
  cat("Copying and renaming prediction files.\n")
  # Create Result directory if necessary
  result_path <- file.path(variables$out_dir[v], variables$tile_name[v])
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
                                     pattern = "_pred\\.tif$", 
                                     full.names = T)
  new_destination <- file.path(variables$out_dir[v], variables$tile_name[v], paste0(variables$out_name[v], ".tif"))
  cat("File to be copyied and renamed:", model_prediction_tif,"\n")
  cat("New destination and name:", new_destination, "\n")
  file.copy(from = model_prediction_tif,
            to = new_destination)
  cat("Copying with new name successfully completed.\n")
  # # Remove predictions and std_dev
  # file.path(variables$rootDIR[v],"deploy_example/predictions", 
  #           variables$year[v], 
  #           paste0(variables$tile_name[v], "_merge")) %>% 
  #   list.files(recursive = T, full.names = T) %>% 
  #   file.remove()
  
  # out_directory <- file.path(paste0(env_vars[["GCHM_DEPLOY_DIR"]], "_merge"), "preds_inv_var_mean")
  # outputFilePath <- file.path(out_dir, paste0(env_vars[["tile_name"]], "_", env_vars[["experiment"]], "_pred.tif"))
  
# Difference calculation --------------------------------------------------

  # compare images to original
  cat("Calculaing the difference to the original prediction.\n")
  preds <- list.files(file.path(variables$out_dir[v], variables$tile_name[v]), full.names = T)
  cat("List of files:", preds, "\n")
  
  original_pred_dir <- preds[
    grepl(variables$tile_name[v], preds) &
      grepl("original", preds)
  ]
  cat("Original prediction file:", original_pred_dir, "\n")
  
  manipulated_filepath <- file.path(variables$out_dir[v], 
                                    variables$tile_name[v], 
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
  correlation <- cor(values(manipulated_pred), values(original_pred), method = "pearson")
  # std deviation of difference
  std_dev <- sd(na.omit(values(difference)))
  
  ## save difference rasters if wanted
  if (DIFF_TIF == TRUE) {
    cat("Saving difference raster...")
    diff_path <- file.path(variables$out_dir[v], variables$tile_name[v], "DIFF")
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
  

# Save results ------------------------------------------------------------

    # safe result to list
  results_list[[v]] <- list(
    tile = variables$tile_name[v],
    band = variables$band[v],
    increment = variables$increment[v],
    decrease = variables$decrease[v],
    average_difference = avg_diff,
    avg_diff_absoluteVals = avg_abs_diff,
    correlation = correlation,
    std_dev = std_dev,
    out_name = variables$out_name[v],
    year = variables$year[v]
  )
  
  cat("Result added to list. Loop", v, "completed.\n")
  
  
  #*** TIMING BLOCK ***
  end_loop_time <- Sys.time()
  duration <- round(difftime(end_loop_time, start_loop_time, units = "mins"), 2)
  cat("Loop", v, "completed. Elapsed time:", duration)
  timing_results <- rbind(timing_results,
                          data.frame(Step = paste("End of Loop ",v, "/", nrow(variables)), 
                                     Duration = duration, 
                                     stringsAsFactors = FALSE) )
  
}


# Export result table -----------------------------------------------------

# Combine list into a data frame
cat("Transforming result list to a data frame.\n")
results_df <- do.call(rbind, lapply(results_list, as.data.frame))
save_path <- paste0("final_results/",Sys.Date(),"_result_table.csv")
write.csv(results_df, save_path, row.names = FALSE)
cat("Results saved as table to", save_path)


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


