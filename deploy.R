# Document setup  ----------------------------------------------------------
start_time <- Sys.time()

# Create empty data frame to store timing info
timing_results <- data.frame(
  Step = character(),
  Duration = character(),
  stringsAsFactors = FALSE
)

# setwd("/home/emilio/canopy_height")
c(
  "sf", "terra", "tmap", "dandelion",
  "rnaturalearth", "rnaturalearthdata",
  "plyr", "dplyr", "leaflet",
  "viridis", "cols4all", "colorspace"
) |>
  lapply(library, character.only = TRUE)


# Create variables df -----------------------------------------------------

# Input of the parameters
variables <- dandelion::create_param_df(tiles = c("T31UGT", "T32ULB","T33UUT"), 
                                        bands = c("B03", "B08"),
                                        increments = c(0.05, 0.1, 0.15, 0.2),
                                        decrease = c("False", "True"),              # False meaning increase...
                                        year = "2020",
                                        base_folder = "/home/emilio/canopy_height"
)
# Should the difference rasters be saved?
DIFF_TIF <- FALSE

## SETUP
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
source("/home/emilio/canopy_height/R/WC_CHECK_FUN.R")



# Deployment LOOP ---------------------------------------------------------


for (v in 1:nrow(variables)) {
  start_loop_time <- Sys.time()
  
  cat("======================================================================================================\n")
  cat("Starting deployment number", v, "of", nrow(variables),"\n")
  cat(variables$tile_name[v], variables$year[v], variables$increment[v], ifelse(variables$decrease[v] == "False", "Increase", "Decrease"),"\n")
  
  ### Create Text-File for tile v
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
  
  
  # Translate Band
  band_number <- translation_table$BandNumber[translation_table$BandName == variables$band[v]]
  
    
  #### Create & set global variables from df
  env_vars <- c(
    tile_name = variables$tile_name[v],
    wcover = variables$WC_year[v],
    YEAR = variables$year[v],
    
    MODIFY_BANDS = band_number,
    MODIFY_PERCENTAGE = variables$increment[v], # or rate
    MODIFY_DECREASE = variables$decrease[v],
    
    GCHM_DEPLOY_DIR = file.path("./deploy_example","predictions", variables$year[v], variables$tile_name[v]),
    DEPLOY_IMAGE_PATH = list.files(img_folder, full.names = T)[1], # just the first image of the tile
    
    experiment = as.character(v)
    # experiment = "experiment"
  )
  cat("Environment variables set.\n")
  
  
  ### Check the Worldcover
  cat("Checking allignment, crs, and extent of the corresponding Worldcover tile.\n")
  wcover_tiles <- list.files( file.path(variables$rootDIR[v], "deploy_example/ESAworldcover/2020/sentinel2_tiles/"), full.names = T )
  
  WC_CHECK_FUN(wcover_tiles, wc_tile_status) # OUTPUT FILE SET TO TEST
  

  ### Deploy the bash script
  cat("#################### Start model deployment loop",v,"####################\n")
  
  withr::with_envvar(env_vars, {
    system2("./gchm/bash/deploy_example.sh")
  })
  cat("deploy_example.sh finished.\n")
  
  cat("+++++++++ Run tile deploy merge start +++++++++\n")
  withr::with_envvar(env_vars, {
    system2("./gchm/bash/run_tile_deploy_merge.sh")
  })
  cat("run_tile_deploy_merge.sh finished.\n")
  
  ### save image with a new name
  # Create Result directory if necessary
  cat("Renaming prediction files.\n")
  if (dir.exists( file.path(variables$out_dir[v], variables$tile_name[v]) )  == TRUE) {
    cat("Directory exists.\n")
  } else{
    dir.create(file.path(variables$out_dir[v], variables$tile_name[v]), recursive = T)
  }
  # Copy files to Result directory and rename
  file.copy(from = list.files(file.path(variables$rootDIR[v],"deploy_example/predictions", 
                                        variables$year[v], 
                                        paste0(variables$tile_name[v], "_merge")), 
                              recursive = T, 
                              pattern = paste0(v,"_pred\\.tif$"), 
                              full.names = T),
            to = file.path(variables$out_dir[v], variables$tile_name[v], variables$out_name[v])
  )
  
  # compare images to original
  cat("Calculaing the difference to the original prediction.\n")
  preds <- list.files(file.path(variables$out_dir[v], variables$tile_name[v]), full.names = T)
  
  original_pred_dir <- preds[
    grepl(variables$tile_name[v], preds) &
      grepl("original", preds)
  ]
  
  original_pred <- rast(original_pred_dir)
  manipulated_pred <- rast(file.path(variables$out_dir[v], variables$tile_name[v], variables$out_name[v])) # NEEDS CHECK
  cat("Calculate difference between", variables$out_name[v], "and original prediction:", basename(original_pred_dir),".\n")
  
  # Calculate the difference
  difference <- manipulated_pred - original_pred     # Eventually layer has to be selected -> [[1]] or pattern _pred -> select above...
  avg_diff <- mean(values(difference), na.rm = TRUE)  
  avg_abs_diff <- mean(abs(values(difference)), na.rm = TRUE)
  # korrelationskoeffizient manipulated_pred, original_pred 
  # std deviation difference
  
  ## save difference rasters if wanted
  if (DIFF_TIF == TRUE) {

    diff_path <- file.path(variables$out_dir[v], variables$tile_name[v], "DIFF")
    diff_file <- file.path(diff_path, paste0("DIFF_", variables$out_name[v], ".tif"))
    
    if (!dir.exists(diff_path)) {
      dir.create(diff_path, recursive = TRUE)
    }
    
    writeRaster(difference, diff_file)
  }
  


  cat("*****",variables$out_name[v], "| Average difference:", avg_diff, "| Avg absolut diff:", avg_abs_diff, "*****\n")
  
    # safe result to list
  results_list[[v]] <- list(
    tile = variables$tile_name[v],
    band = variables$band[v],
    increment = variables$increment[v],
    decrease = variables$decrease[v],
    average_difference = avg_diff,
    avg_diff_absoluteVals = avg_abs_diff,
    out_name = variables$out_name[v],
    year = variables$year[v]
  )
  
  cat("Result added to list. Loop", v, "completed.\n")
  
  
  #*** TIMING BLOCK ***
  end_time <- Sys.time()
  timing_results <- rbind(timing_results,
                          data.frame(Step = paste("End of Loop ",v, "/", nrow(variables)), 
                                     Duration = round(difftime(end_time, start_loop_time, units = "mins"), 2), stringsAsFactors = FALSE) )
  
  
}


# Combine list into a data frame
results_df <- do.call(rbind, lapply(results_list, as.data.frame))
save_path <- paste0("final_results/",Sys.Date(),"_result_table.csv")
write.csv(results_df, save_path, row.names = FALSE)
cat("Results saved as table to", save_path)

# Track and show time elapsed
end_time <- Sys.time()
cat("Job finished. Time elapsed:", 
    {
      secs <- as.numeric(difftime(end_time, start_time, units = "secs"))
      sprintf("%02d:%02d:%02d",
              floor(secs / 3600),
              floor((secs %% 3600) / 60),
              floor(secs %% 60))
    },"\n"
)

# Print timing table at the end
print(timing_results, row.names = FALSE)
