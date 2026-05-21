
# Setup per Scenario -----------------------------------------------------

prepare_scenario <- function(scenario, variables) {
  
  start_loop_time <- Sys.time() # Loop timing
  
  cat("===================================================================================================================\n")
  cat("Starting scenario number", v, "of", nrow(variables),"\n")
  cat("Tile:",variables$tile[v], "\n",
  "Pixel-Shuffle:",variables$shuffle_pct[[v]], "%", "\n",
  "Patch Size:",variables$patch_size[[v]], "x", variables$patch_size[[v]], "\n")
  cat("===================================================================================================================\n")
  
  # Text file creation ------------------------------------------------------
  
  # Creation of a text file with the names of the corresponding zip-folders
  output_file <- file.path(variables$rootDIR[v], "deploy_example", "image_paths", variables$year[v], paste0(variables$tile[v], ".txt"))
  img_folder <- file.path(variables$rootDIR[v], "deploy_example", "sentinel2", variables$year[v], variables$tile[v])
  zip_files <- list.files(path = img_folder, pattern = paste0(".*", variables$tile[v], ".*\\.zip$"), full.names = FALSE)
  
  if (dir.exists( file.path(variables$rootDIR[v], "deploy_example", "image_paths", variables$year[v]))  == TRUE) {
    cat("TXT directory exists.\n")
  } else{
    dir.create( file.path(variables$rootDIR[v], "deploy_example", "image_paths", variables$year[v]), recursive = T)
    cat("TXT file directory created.\n")
  }
  
  writeLines(zip_files, output_file)
  cat("Created zip file list as text file:", output_file, "with", length(zip_files), "entries.\n")


  # Worldcover adjustment ---------------------------------------------------

  cat("Checking allignment, crs, and extent of the corresponding Worldcover tile.\n")
  wcover_tiles <- list.files( file.path(variables$rootDIR[v], "deploy_example/ESAworldcover/2020/sentinel2_tiles"), full.names = T )
  
  worldcover_adjust_new(wcover_tiles, wc_tile_status, df = variables, w = v, img_dir = img_folder)
  # dandelion::worldcover_adjust(wcover_tiles, wc_tile_status, df = variables, w = v, img_dir = img_folder)
  # WC_CHECK_FUN(wcover_tiles, wc_tile_status) # OUTPUT FILE SET TO TEST
  cat("World cover processing completed.\n")
  
}





# GLOBAL ENV-Varibles -----------------------------------------------------------

set_environment_variables <- function(scenario){
    # ROUTING
  if (scenario$manipulation_type == "shuffle") {

     env_vars <<- list(
      tile_name = variables$tile[v],
      wcover = variables$WC_year[v],
      YEAR = variables$year[v],
      
      SHUFFLE_PERCENTAGE = variables$shuffle_pct[v],
      SHUFFLE_PATCH_SIZE = variables$patch_size[v],
      
      GCHM_DEPLOY_DIR = file.path("./deploy_example","predictions", variables$year[v], variables$tile[v]), # important for out_dir
      DEPLOY_IMAGE_PATH = list.files(img_folder, full.names = T)[1], # just the first image of the tile
      
      flag = v
    )

    return(env_vars)

  } else if (scenario$manipulation_type == "spectral") {

    # Translate band name
    band_number <- translation_table$BandNumber[translation_table$BandName %in% variables$band[[v]]]
    modify_bands_str <- paste(band_number, collapse = " ")
    
    # Create & set GLOBAL VARIABLES from variables data frame
    env_vars <- c(
      tile_name = variables$tile[v],
      wcover = variables$WC_year[v],
      YEAR = variables$year[v],
      
      MODIFY_BANDS = modify_bands_str,
      MODIFY_PERCENTAGE = variables$increment[v], # or rate
      MODIFY_DECREASE = variables$decrease[v],
      
      GCHM_DEPLOY_DIR = file.path("./deploy_example","predictions", variables$year[v], variables$tile[v]), # important for out_dir
      DEPLOY_IMAGE_PATH = list.files(img_folder, full.names = T)[1], # just the first image of the tile
      
      flag = v
    )

    return(env_vars)
    
  } else if (scenario$manipulation_type == "geographical"){
    
    cat("##############################################\n")
    stop("Geographical manipulation not yet implemented!\n")
  }

}
