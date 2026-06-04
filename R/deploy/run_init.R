# Run folder setup -------------------------------------------------------

existing <- list.dirs("results", FALSE, FALSE)
ids <- as.integer(sub(".*_", "",
  grep(paste0("^", runtime$start_date_chr, "_", base_specs$manipulation, "_[0-9]+$"),
       existing, value = TRUE)
))

run_dir <- file.path(
  "results/runs",
  paste0(
    runtime$start_date_chr, "_",
    base_specs$manipulation, "_",
    max(ids, 0, na.rm = TRUE) + 1
  )
)

dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
message("Run Directory created: ", run_dir, "\n")

loop_backup_dir <- file.path(run_dir, "loop_backups")
dir.create(loop_backup_dir, recursive = TRUE, showWarnings = FALSE)
message("Backup directory: ", loop_backup_dir, "\n")

results_file <- file.path(run_dir, "results.csv")
timing_file  <- file.path(run_dir, "timing.csv")
metadata_file <- file.path(run_dir, "metadata.json")


# Translation tables ------------------------------------------------------
band_translation <- data.frame(
  BandName = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B11", "B12"), # "B10", cirrus not included
  BandNumber = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), # B8A = 9, B09 = 10
  Colour = c("Aerosol", "Blue", "Green", "Red", "RedEdge","None","None", "NIR", "NIR2", "WaterVapour", "SWIR1", "SWIR2")
)

shift_limits <- data.frame(
  tile = c("10TES", "17SNB", "20MMD", "33NTG", "32TMT",
    "32UQU", "34UFD", "35VML", "49NHC", "55HEV", "49UCP"),
  max_km_N = c(4700, 5700, 10100, 9300, 4600,
               4500, 4000, 2800, 9700, 14000, 4500),
  max_km_S = c(15100, 14100, 9800, 10600, 15200,
               15300, 15800, 17000, 10200, 5800, 15300),
  stringsAsFactors = FALSE
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

  writeLines(capture.output(sessionInfo()),
           file.path(run_dir, "sessionInfo.txt"))
  
  message("Session Info saved to: ", file.path(run_dir, "sessionInfo.txt"), "\n")
}
