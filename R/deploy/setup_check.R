# Check manipulation type
allowed <- c("shuffle", "spectral", "geographical")
manip <- base_specs$manipulation

stopifnot(length(manip) == 1)
if (!manip %in% allowed) stop("Invalid manipulation type.")


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
mean_loop_time_sec <- mean_loop_times_sec[variables$manipulation_type[1]]
working_time_sec <- nrow(variables) * mean_loop_time_sec
finish_estimate <- Sys.time() +  working_time_sec
working_hms <- sprintf("%02d:%02d:%02d", working_time_sec %/% 3600, 
(working_time_sec %% 3600) %/% 60, working_time_sec %% 60 )
cat("START TIME:              ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Estimated working time:  ",nrow(variables), "x", mean_loop_time_sec/60, "min. =", working_hms,"h.\n")
cat("Estimated finishing time:", format(finish_estimate, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Manipulation method:     ", variables$manipulation_type[1],"\n")
cat("Tiles in process:        ", as.character(unique(variables$tile)), "\n")

if ("shuffle" %in% variables$manipulation_type){
  cat("Ammount of shuffle:    ", paste(unique(unlist(variables$shuffle_pct))[unique(unlist(variables$shuffle_pct)) != 0], collapse = " "),"%\n")
  cat("Patch sizes:           ", unique(variables$patch_size), "\n")
  cat("Shuffle Type:          ", param_specs$shuffle$shuffle_type,"\n")
  # if (is.na(param_specs$shuffle$subtile_size)){ cat("Global pixel shuffle.\n") } else {
  #   cat("Subtile Size:", param_specs$shuffle$subtile_size, "\n")}
} else if ("spectral" %in% variables$manipulation_type){
  cat("Bands to be modified:  ", paste(unique(sapply(variables$band, paste, collapse = "-")), collapse = " "),"\n")
  cat("Increments:            ", as.character(unique(variables$increment)*100),"%\n")
  cat("Decrease:              ", param_specs$spectral$decrease,"\n")
} else if ("geographical" %in% variables$manipulation_type){
  cat("Shifting distances:    ",paste(unique(unlist(variables$shift_distance))[unique(unlist(variables$shift_distance)) != 0], collapse = " "),"km\n")
  cat("Directions:            ",unique(variables$shift_direction),"\n")
}

# Setup for Worldcover check
wc_tile_status <- data.frame(
  wc_tile = unique(variables$tile),
  edited = FALSE
)