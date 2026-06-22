# Run folder setup -------------------------------------------------------

# # Sequential numbering
# existing <- list.dirs("results/runs", FALSE, FALSE)
# ids <- as.integer(sub(".*_", "",
# grep(paste0("^", runtime$start_date_chr, "_", base_specs$manipulation, "_[0-9]+$"),
# existing, value = TRUE)
# ))
# run_ID <- paste0( runtime$start_date_chr, "_", base_specs$manipulation, "_", max(ids, 0, na.rm = TRUE) + 1 )

# # Random letters + milliseconds -> not random because of set.seed()!!
# run_ID <- sprintf(
#   "%s_%s_%s_%s",
#   runtime$start_date_chr,
#   base_specs$manipulation,
#   format(Sys.time(), "%H%M%OS3"),
#   paste(sample(c(letters, LETTERS, 0:9), 4, TRUE), collapse = "")
# )

# Parallel deployment group ID
run_ID <- sprintf(
  "%s_%s_%s",
  runtime$start_date_chr,
  base_specs$manipulation,
  tile_group  # g1, g2, ...
)

run_dir <- file.path("results/runs", run_ID)

if (isFALSE(DEBUG)){ # Dont create Directories if in debug mode
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  message("Run Directory created: ", run_dir, "\n")
  
  loop_backup_dir <- file.path(run_dir, "loop_backups")
  dir.create(loop_backup_dir, recursive = TRUE, showWarnings = FALSE)
  message("Backup directory: ", loop_backup_dir, "\n")
}

results_file <- file.path(run_dir, "results.csv")
results_file_global <- file.path("results", paste0(run_ID, "_results.csv"))
timing_file  <- file.path(run_dir, "timing.csv")
metadata_file <- file.path(run_dir, "metadata.json")


# SNAPSHOT ---------------------------------------------------------------

write_metadata <- function(config, param_specs, run_dir) {

  metadata <- list(
    run_id = basename(run_dir),
    start_time = as.character(Sys.time()),
    base_dir = config$rootDIR,
    tiles = config$tile,
    year = config$year,
    manipulation = config$manipulation,
    parameters = param_specs,
    seed = GLOBAL_SEED,
    Backups = BACKUP_SAVING,
    prediction_tifs = PRED_TIF,
    pred_tif_location = ifelse(PRED_TIF, PRED_TIF_LOCATION, NA),
    difference_tifs = DIFF_TIF,
    diff_tif_location = ifelse(DIFF_TIF, EXPORT_TIF_LOC, NA)
  )

  jsonlite::write_json(metadata,
                       path = metadata_file,
                       pretty = TRUE,
                       auto_unbox = TRUE)
  
  message("Metadata written to: ", metadata_file, "\n")

  writeLines(capture.output(sessionInfo()),
           file.path(run_dir, "sessionInfo.txt"))
  
  message("Session Info saved to: ", file.path(run_dir, "sessionInfo.txt"), "\n")
}
