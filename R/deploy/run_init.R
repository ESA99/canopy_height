# Run folder setup -------------------------------------------------------

run_id <- format(Sys.time(), "%H%M")

run_dir <- file.path("results", paste0(start_date_chr, "_run_", run_id))

dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
message("Run Directory created: ", run_dir, "\n")

loop_backup_dir <- file.path(run_dir, "loop_backups")
dir.create(loop_backup_dir, recursive = TRUE, showWarnings = FALSE)
message("Backup directory: ", loop_backup_dir, "\n")

results_file <- file.path(run_dir, "results.csv")
timing_file  <- file.path(run_dir, "timing.csv")
metadata_file <- file.path(run_dir, "metadata.json")


# Translation table ------------------------------------------------------
translation_table <- data.frame(
  BandName = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B11", "B12"), # "B10", cirrus not included
  BandNumber = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), # B8A = 9, B09 = 10
  Colour = c("Aerosol", "Blue", "Green", "Red", "RedEdge","None","None", "NIR", "NIR2", "WaterVapour", "SWIR1", "SWIR2")
)


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
}
