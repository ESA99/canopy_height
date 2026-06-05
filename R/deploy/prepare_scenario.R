
# Setup per Scenario -----------------------------------------------------

prepare_scenario <- function(scenario, variables) {
  
  start_loop_time <- Sys.time() # Loop timing
  
  cat("=============================================================================================n")
  cat("Starting scenario number", v, "of", nrow(variables),"\n")
  cat("Deployment run:", basename(run_dir),"\n")
  cat("Current Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Global process running scince:", format_runtime(runtime$start_time), "hours.\n")
  if (scenario$manipulation_type == "shuffle"){
    cat("Tile:",scenario$tile, "\n",
        "Pixel-Shuffle:",scenario$shuffle_pct, "%", "\n",
        "Patch Size:",scenario$patch_size, "x", scenario$patch_size, "\n",
        "Shuffle Type:",scenario$shuffle_type,"\n")
      # if(!is.na(scenario$subtile_size)){paste("Subtile size:", scenario$subtile_size,"\n")} else{paste("Global shuffle.\n")})
  } else if (scenario$manipulation_type == "spectral"){
    cat("Tile:",scenario$tile, "\n",
        "Band:",paste(unlist(scenario$band), collapse = "-"), "=", scenario$Colour, "\n",
        "Percentage:", scenario$increment*100, "%", "\n",
        "Direction:",ifelse(scenario$decrease == "False", "Increase", "Decrease"), "\n")
  } else if (scenario$manipulation_type == "geographical"){
    cat("Tile:",scenario$tile, "\n",
        "Shift:",scenario$shift_distance,"km to the",if(scenario$shift_direction == "N"){paste0("North")}else if (scenario$shift_direction == "S") {paste0("South")},"\n")

  }
  
  cat("=============================================================================================\n")
  
  # Text file creation ------------------------------------------------------
  
  # Creation of a text file with the names of the corresponding zip-folders
  output_file <- file.path(scenario$rootDIR, "deploy_example", "image_paths", scenario$year, paste0(scenario$tile, ".txt"))
  img_folder <- file.path(scenario$rootDIR, "deploy_example", "sentinel2", scenario$year, scenario$tile)
  zip_files <- list.files(path = img_folder, pattern = paste0(".*", scenario$tile, ".*\\.zip$"), full.names = FALSE)
  
  if (dir.exists( file.path(scenario$rootDIR, "deploy_example", "image_paths", scenario$year))  == TRUE) {
    cat("TXT directory exists.\n")
  } else{
    dir.create( file.path(scenario$rootDIR, "deploy_example", "image_paths", scenario$year), recursive = T)
    cat("TXT file directory created.\n")
  }
  
  writeLines(zip_files, output_file)
  cat("Created zip file list as text file:", output_file, "with", length(zip_files), "entries.\n")


  # Worldcover adjustment ---------------------------------------------------

  cat("Checking allignment, crs, and extent of the corresponding Worldcover tile.\n")
  wcover_tiles <- list.files( file.path(scenario$rootDIR, "deploy_example/ESAworldcover/2020/sentinel2_tiles"), full.names = T )
  
  worldcover_adjust_new(wcover_tiles, wc_tile_status, df = variables, w = v, img_dir = img_folder)
  # dandelion::worldcover_adjust(wcover_tiles, wc_tile_status, df = variables, w = v, img_dir = img_folder)
  # WC_CHECK_FUN(wcover_tiles, wc_tile_status) # OUTPUT FILE SET TO TEST
  cat("World cover processing completed.\n")
  
}





# GLOBAL ENV-Varibles -----------------------------------------------------------

set_environment_variables <- function(scenario){

  img_folder <- file.path(scenario$rootDIR, "deploy_example", "sentinel2", scenario$year, scenario$tile)

  env_vars <- list(
      tile_name = scenario$tile,
      wcover = scenario$WC_year,
      YEAR = scenario$year,
      
      GCHM_DEPLOY_DIR = file.path("./deploy_example","predictions", scenario$year, scenario$tile), # important for out_dir
      DEPLOY_IMAGE_PATH = list.files(img_folder, full.names = T)[1], # just the first image of the tile
      
      MODEL_ID = MODEL_ID,

      MODE = scenario$manipulation_type
  )

  if (scenario$manipulation_type == "shuffle") {
    env_vars$SHUFFLE_PERCENTAGE = scenario$shuffle_pct
    env_vars$SHUFFLE_PATCH_SIZE = scenario$patch_size
    env_vars$SHUFFLE_TYPE = scenario$shuffle_type
    # env_vars$SHUFFLE_SUBTILE_SIZE = scenario$subtile_size

  } else if (scenario$manipulation_type == "spectral") {
    # Translate band name
    # band_number <- band_translation$BandNumber[band_translation$BandName %in% scenario$band[]]
    # modify_bands_str <- paste(band_number, collapse = " ")
    band_number <- band_translation$BandNumber[
      match(unlist(scenario$band), band_translation$BandName)]
    modify_bands_str <- paste(na.omit(band_number), collapse = " ")
    
    env_vars$MODIFY_BANDS = modify_bands_str
    env_vars$MODIFY_PERCENTAGE = scenario$increment # or rate
    env_vars$MODIFY_DECREASE = scenario$decrease

  } else if (scenario$manipulation_type == "geographical"){

    env_vars$SHIFT_DISTANCE = scenario$shift_distance
    env_vars$SHIFT_DIRECTION = scenario$shift_direction

  }
    
  cat("Environment variables set for shuffle manipulation.\n")
  return(env_vars)

}




  # ### Remove all Variables that are NA -> let python use its default values! ###
  # env_vars <- env_vars[!sapply(env_vars, function(x) {
  #   is.null(x) ||
  #   length(x) == 0 ||
  #   (is.na(x) && !is.character(x)) ||
  #   (is.character(x) && trimws(x) == "")
  # })]