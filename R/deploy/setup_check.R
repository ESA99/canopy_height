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
cat("Manipulation method: ", variables$manipulation_type[1],"\n")
cat("Tiles in process:", as.character(unique(variables$tile)), "\n")

if ("shuffle" %in% variables$manipulation_type){
  cat("Ammount of shuffle:",paste(unique(unlist(variables$shuffle_pct))[unique(unlist(variables$shuffle_pct)) != 0], collapse = " "),"%\n")
  cat("Patch size:", variables$patch_size[1], "X", variables$patch_size[1], "\n")
  if (is.na(param_specs$shuffle$subtile_size)){ cat("Global pixel shuffle.\n") } else {
    cat("Subtile Size:", param_specs$shuffle$subtile_size, "\n")}
} else if ("spectral" %in% variables$manipulation_type){
  cat("Bands to be modified:", paste(unique(sapply(variables$band, paste, collapse = "-")), collapse = " "),"\n")
  cat("Increments: ", as.character(unique(variables$increment)*100),"%\n")
  cat("Decrease: ", param_specs$spectral$decrease,"\n")
} else if ("geographical" %in% variables$manipulation_type){
  stop("Geographical manipulation not yet implemented!")
}

