# Setup for Worldcover check
wc_tile_status <- data.frame(
  wc_tile = unique(variables$tile),
  edited = FALSE
)


#### Check for data availability ####
exist_flags <- file.path(file.path(variables$rootDIR[1], "deploy_example/sentinel2/2020/"), unique(variables$tile)) |>
  dir.exists()

if (all(exist_flags)) {
  cat("All data folders for unique tiles exist. Starting loop deployment.\n")
} else {
  missing <- unique(variables$tile)[!exist_flags]
  cat("=========================================    ERROR!    =========================================\n")
  cat("========================================= DATA MISSING =========================================\n")
  stop(paste("The following folders are missing:", paste(missing, collapse = ", ")))
}

# Time estimate
working_time <- (nrow(variables)*mean_loop_time/60 * 3600)
finish_estimate <- Sys.time() +  working_time
working_hours <- working_hours <- sprintf("%02d:%02d:%02d", as.integer(working_time %/% 3600),
  as.integer((working_time %% 3600) %/% 60),as.integer(working_time %% 60) )
cat("Estimated working time:",nrow(variables), "x", mean_loop_time, "min. =", working_hours,"h.\n")
cat("Estimated finishing time:", format(finish_estimate, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Tiles in process:", as.character(unique(variables$tile)), "\n")
cat("Percentage of pixels to be shuffled:", as.character(unique(variables$shuffle_pct)), "\n")
if (is.na(variables$patch_size[1]) ) {
  cat("Global pixel shuffling enabled.\n")
}else{
  cat("Local pixel shuffle:", variables$patch_size[1], "X", variables$patch_size[1], "\n")
}