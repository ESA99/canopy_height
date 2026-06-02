save_results <- function(results){
  
  results_df <- dplyr::bind_rows(results)

  try_export <- try({
    write.csv(results_df, results_file, row.names = FALSE)
    cat("=====================================================================================================\n")
    cat("                            Full combined results saved successfully!\n")
    cat("=====================================================================================================\n")
    combined_success <- TRUE
  }, silent = TRUE)


  if (inherits(try_export, "try-error")) {
    warning("Failed to save combined results as data frame. Starting fallback export.\n")
    combined_success <- FALSE
    
    # Save each row individually
    fallback_dir <- file.path(run_dir, "fallback" )
    dir.create(fallback_dir, recursive = TRUE, showWarnings = FALSE)
    
    cat("Saving individual result files due to fallback mechanism...\n")
    
    individual_export_success <- logical(length(results))
      
    for (i in seq_along(results)) {

      scenario_result <- results[[i]]
      if (is.null(scenario_result)) next # skip NULL entries safely
      scenario_df <- dplyr::bind_rows(scenario_result)

      file_path <- file.path(fallback_dir, paste0(sprintf("%03d", i), "_result.csv") )

      individual_export_success[i] <- tryCatch({
        write.csv(scenario_df, file_path, row.names = FALSE)
        TRUE
      }, error = function(e) {
        warning(sprintf("Failed to save individual result %d: %s", i, e$message ))
        FALSE
      })
    }
    
    if (all(individual_export_success)) {
      cat("All fallback result files saved successfully to", fallback_dir, "\n")
    } else {
      failed_ids <- which(!individual_export_success)

      warning(sprintf(
        "%d individual result(s) failed to save: %s",
        length(failed_ids),
        paste(failed_ids, collapse = ", ")
      ))
    }

  } else {

    cat("Full results table saved to", results_file, "\n")
  }

}



script_summary <- function(results){

  results_df <- dplyr::bind_rows(results)

  # TIMING
  # for (i in seq_along(results)) {
  #   r <- results[[i]]
  #   cat(
  #     sprintf(
  #       "Loop %03d | Tile: %-8s | Mode: %-12s | Time: %.2f min\n",
  #       i,r$tile,r$mode,r$time_min)
  #   )
  # }
  for (i in seq_along(results)) {
    r <- results[[i]]
    if (is.null(r) || nrow(r) == 0) next
    cat(sprintf(
      "Loop %03d | Tile: %-8s | Mode: %-12s | Time: %.2f min\n",
         i,        r$tile[1],   r$mode[1],     r$time_min[1]    ) 
    )
  }

  cat("Average time per loop:", mean(results_df$time_min, na.rm = TRUE), "minutes.\n")
  
  # Final Summary
  cat("**************************** Summary ****************************\n")
  cat("Process finished at:", format(Sys.time(), "%Y-%m-%d %H:%M"),"\n")
  cat("Total number of loops/predictions:",nrow(variables),"\n")
  cat("Tiles processed:",unique(variables$tile),"\n")
  cat("Manipulation method:",unique(variables$manipulation_type), "\n" )
  if (variables$manipulation_type[1] == "shuffle"){
    cat("Amount shuffled:",paste(unique(unlist(variables$shuffle_pct))[unique(unlist(variables$shuffle_pct)) != 0], collapse = " "),"%\n") }
  if (variables$manipulation_type[1] == "spectral"){
    bands_clean <- unique(sapply(variables$band, function(x) {paste(x, collapse = "-")}))
    cat("Bands processed:", paste(bands_clean, collapse = " "), "\n")
    cat("Bands processed:",unique(variables$Colour),"\n")}
  if (BACKUP_SAVING) { cat("Backup saved for each loop to: ",loop_backup_dir,"\n") } else { cat("No Backup saved.\n") }
  if (PRED_TIF) { cat("Prediction TIFs saved to:", PRED_TIF_LOCATION, "\n")} else { cat("No prediction TIFs saved.\n") }
  if (DIFF_TIF) { cat("Difference rasters saved to ", config$DIFF_TIF_LOCATION,"\n") } else { cat("Difference rasters not saved.\n") }


  cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
  cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
  cat("++++++++++++++++++++++++++++++++ All jobs finished. Full script ran succesfully. ++++++++++++++++++++++++++++++++\n")
  cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
  cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
}