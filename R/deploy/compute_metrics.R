# COMPUTE METRICS
compute_metrics <- function(scenario, result_path, new_destination) {

  mode = scenario$manipulation_type

  # RASTERS
  original_pred <- list.files(
    result_path,
    full.names = TRUE,
    pattern = paste0(scenario$tile, ".*original")
  ) |>
    terra::rast()

  manipulated_pred <- rast(new_destination)

  # METRICS

  orig_vals <- terra::values(original_pred)
  man_vals  <- terra::values(manipulated_pred)

  keep <- complete.cases(orig_vals, man_vals)
  orig_vals <- orig_vals[keep]
  man_vals  <- man_vals[keep]

  ## Difference Raster
  if (DIFF_TIF == TRUE) {
    cat("Saving difference raster...")
    diff_path <- config$DIFF_TIF_LOCATION
    diff_file <<- file.path(diff_path, paste0("DIFF_", scenario$out_name, ".tif"))
    if (!dir.exists(diff_path)) {  dir.create(diff_path, recursive = TRUE)}
    diff_rast <- manipulated_pred - original_pred
    writeRaster(diff_rast, diff_file, overwrite = TRUE)
    cat("Difference raster saved as:", diff_file,"\n")
  } 

  ### Basic statistics
  original_mean <- mean(orig_vals)
  manipulated_mean <- mean(man_vals)

  mean_change <- manipulated_mean - original_mean
  mean_abs_change <- mean(abs(man_vals - orig_vals))
  
  relative_mean_change <- (mean_change / original_mean) * 100
  relative_mean_abs_change <- (mean_abs_change / original_mean) * 100

  ### Error metrics (distribution-agnostic)
  diff <- man_vals - orig_vals

  mae  <- mean(abs(diff))
  rmse <- sqrt(mean(diff^2))
  correlation <- cor(orig_vals, man_vals)
  
  ### Spread / variability
  original_sd <- sd(orig_vals)
  manipulated_sd <- sd(man_vals)

  sd_change <- manipulated_sd - original_sd
  relative_sd_change <- (sd_change / original_sd) * 100

  #=============== BASE VALUES RESULT DF ================
  loop_results <- list(
    mode = mode,
    run = basename(run_dir),
    tile = scenario$tile,
    out_name = scenario$out_name,
    year = scenario$year,
    original = scenario$original,

    mean_height            =     manipulated_mean,
    mean_change            =     mean_change,
    mean_abs_change        =     mean_abs_change,
    relative_mean_change   =     relative_mean_change,
    relative_mean_abs_change =   relative_mean_abs_change,

    mae = mae,
    rmse = rmse,
    correlation = correlation,

    SD = manipulated_sd,
    sd_change = sd_change,
    relative_sd_change = relative_sd_change
  )
  

  # =========================================================
  # SPATIALLY VALID MODES
  # spectral + geographical
  # =========================================================

  if (mode == "spectral") {
    loop_results$band = paste(scenario$band, collapse = "-")
    loop_results$increment = scenario$increment
    loop_results$decrease = scenario$decrease
    loop_results$Colour = scenario$Colour
  }
  
  if (mode == "geographical") {
    
  }

  # =========================================================
  # DISTRIBUTIONAL MODE (shuffle)
  # =========================================================

  if (mode == "shuffle") {

    wasserstein <- transport::wasserstein1d(orig_vals, man_vals)

    probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

    q_orig <- quantile(orig_vals, probs, na.rm = TRUE)
    q_man  <- quantile(man_vals, probs, na.rm = TRUE)
    q_shift <- q_man - q_orig
    quantile_shift <- setNames(as.numeric(q_shift),
      paste0("q", probs * 100, "_shift")  )

    
    loop_results$wasserstein = wasserstein

    loop_results$q05_shift = quantile_shift["q5_shift"]
    loop_results$q25_shift = quantile_shift["q25_shift"]
    loop_results$q50_shift = quantile_shift["q50_shift"]
    loop_results$q75_shift = quantile_shift["q75_shift"]
    loop_results$q95_shift = quantile_shift["q95_shift"]


    loop_results$shuffle_percentage = scenario$shuffle_pct
    loop_results$patch_size = scenario$patch_size
  }

  return(loop_results)
}





finish_loop <- function(loop_results, BACKUP_SAVING, loop_backup_dir, v){
  
  cat("========== LOOP RESULTS ==========\n")
  for (name in names(loop_results)) {
    
    value <- loop_results[[name]]
    
    # safely handle vectors, matrices, etc.
    if (is.null(value)) {
      value_str <- "NULL"
    } else if (length(value) > 1) {
      value_str <- paste(value, collapse = ", ")
    } else {
      value_str <- as.character(value)
    }
    
    cat(sprintf("%-30s : %s\n", name, value_str))
  }
  cat("==================================\n")
  

  ### BACKUP SAVING ###
  if (isTRUE(BACKUP_SAVING) ) {
    cat("Backup saving individual loop result.\n")
    loop_results_df <- as.data.frame(loop_results, stringsAsFactors = FALSE)
    # loop_results_df <- data.frame( #ALTERNATIVE
    #   name = names(loop_results),
    #   value = unlist(loop_results, use.names = FALSE) )
    backup_file <- paste0(loop_backup_dir,"/",v,".csv")
    write.csv(loop_results_df, backup_file, row.names = FALSE)
    cat("******* Loop results saved individually as backup at:",backup_file,"*******\n")
  } else{
    cat("Individual loop results not backed up.\n")
  }

}

