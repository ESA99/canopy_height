# File organization -------------------------------------------------------

  ### Save image with a new name to designated folder (out_dir)
  cat("Copying and renaming prediction files.\n")
  
  ## Create Result directory if necessary
  result_path <- file.path(variables$out_dir[v], "preds", variables$tile[v])
  if (dir.exists( result_path )  == TRUE) {
    cat("Result directory exists:",result_path,"\n")
  } else{
    dir.create(result_path, recursive = T)
    cat("Result directory created:",result_path,"\n")
  }
  
  ## Copy prediction files to Result directory and rename (out_name)
  model_prediction_tif <- list.files(file.path(variables$rootDIR[v],"deploy_example/predictions", 
                                               variables$year[v], 
                                               paste0(variables$tile[v], "_merge")), 
                                     recursive = T, 
                                     # pattern = "_pred\\.tif$", 
                                     pattern = paste0(start_date_chr, ".*_pred\\.tif$"),
                                     full.names = T)
  new_destination <- file.path(result_path, paste0(variables$out_name[v], ".tif"))
  cat("File to be copyied and renamed:", model_prediction_tif,"\n")
  cat("New destination and name:", new_destination, "\n")
  file.copy(from = model_prediction_tif,
            to = new_destination,
            overwrite = T)
  cat("Copying and renaming successfully completed.\n")
  
  ## Remove predictions and std_dev at old location 
  old_pred_location <-   file.path(variables$rootDIR[v],"deploy_example/predictions",
                                  variables$year[v],
                                  paste0(variables$tile[v], "_merge"))
  cat("Removing pred and StDev files at the original merge location:",old_pred_location,"\n")
  old_pred_location %>%
    list.files(recursive = T, full.names = T) %>%
    file.remove() # delete
  cat("-> DONE\n")
  
  ## Remove un-merged prediction files from Ensemble
  individual_preds_location <- file.path(variables$rootDIR[v],"deploy_example/predictions",
                                         variables$year[v],
                                         variables$tile[v])
  cat("Removing original unmerged prediction files:",individual_preds_location,"\n")
  individual_preds_location %>%
    list.files(recursive = T, full.names = T) %>%
    file.remove() # delete
  cat("-> DONE\n")

  