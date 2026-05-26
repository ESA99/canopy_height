organize_prediction_files <- function(scenario, start_date_chr) {
  
  ### Save image with a new name to designated folder (out_dir)
  cat("Copying and renaming prediction files.\n")
  result_path <- file.path(scenario$out_dir,"preds",scenario$tile)

  if (!dir.exists(result_path)) {
    dir.create(result_path, recursive = TRUE)
    cat("Result directory created:",result_path,"\n")
  }

  merge_dir <- file.path(
    scenario$rootDIR,
    "deploy_example/predictions",
    scenario$year,
    paste0(scenario$tile, "_merge")
  )

  model_prediction_tif <- list.files(
    merge_dir,
    recursive = TRUE,
    pattern = paste0(scenario$tile, ".*_pred\\.tif$"),
    full.names = TRUE
  )

  if (length(model_prediction_tif) == 0) {
   stop("No prediction file found in: ", merge_dir)
  }

  new_destination <- file.path(
    result_path,
    paste0(scenario$out_name, ".tif")
  )

  cat("File to be copyied and renamed:", model_prediction_tif,"\n")
  cat("New destination and name:", new_destination, "\n")
  file.copy(model_prediction_tif, new_destination, overwrite = TRUE)
  cat("Copying and renaming successfully completed.\n")

  # cleanup
  cat("Removing pred and StDev files at the original merge location:",merge_dir,"\n")
  file.remove(list.files(merge_dir, full.names = TRUE, recursive = TRUE))
  
  individual_preds_location <- file.path(
                                  scenario$rootDIR,
                                  "deploy_example/predictions",
                                  scenario$year,
                                  scenario$tile
                                )
  cat("Removing original unmerged prediction files:",individual_preds_location,"\n")
  individual_preds_location %>%
    list.files(recursive = T, full.names = T) %>%
    file.remove() # delete
  cat("-> DONE\n")


  return(list(new_destination = new_destination, result_path = result_path))
}