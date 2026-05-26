# Deployment script of the analysis

source("R/deploy/config.R")
source("R/deploy/logging.R")
source("R/deploy/create_variables_df.R")
source("R/deploy/run_init.R")
source("R/deploy/prepare_scenario.R")
source("R/deploy/worldcover_adjust.R")
source("R/deploy/organize_outputs.R")
source("R/deploy/compute_metrics.R")
source("R/deploy/export.R")



# Scenario Setup -----------------------------------------------------

base_specs <- list(
  # tile = c("10TES", "17SNB", "20MMD", "32TMT", "32UQU", "33NTG", "34UFD", "35VML", "49NHC", "49UCP", "55HEV"),
  tile = c("49NHC"), # for testing
  year = c("2020"),
  WC_year = c("2020"),
  rootDIR = "/home/emilio/canopy_height",
  manipulation = c("shuffle") 
  # manipulation = c("spectral")
  # manipulation = c("geographical")
)

param_specs <- list(
  
  shuffle = list(
    # shuffle_pct = c(2.5, 5, 10, 15, 20, 25, 30, 50, 65, 80, 100),
    shuffle_pct = c(10), # for testing
    patch_size = c(1)
  ),
  
  spectral = list(
    band = c("B02", "B03", "B04", "B05", "B08", "B8A", "B11", "B12"),
    increment = c(0.05, 0.1, 0.15, 0.2, 0.25),
    decrease = c(FALSE,TRUE)
  ),

  geographical = list(
    km_steps = c(100, 200, 300, 500, 1000, 1500, 2000, 3000, 5000)
    # , direction = c("N","S")
  )
)

write_metadata(base_specs, param_specs, run_dir)

variables <- create_param_grid(base_specs, param_specs)

source("R/deploy/setup_check.R")


# CORE LOOP --------------------------------------------------------------

results <- vector("list", nrow(variables))
timing_results <- vector("list", nrow(variables))

for (v in seq_len(nrow(variables)) ) {
  
  timer <- start_timer()

  scenario <- variables[v, ]
  scenario$run_dir <- run_dir
  scenario$loop_backup_dir <- file.path(run_dir,"loop_backup")

  prepare_scenario(scenario, variables)
      # adjust_worldcover(scenario, prep$img_folder)

  # Global Variable Setup
  env_vars <- set_environment_variables(scenario)

  # BASH DEPLOYMENT OF THE MODEL
  cat("#################### Start model deployment loop",v,"####################\n")
  cat("+++++++++ Run tile deploy merge start +++++++++\n")
  withr::with_envvar(env_vars, {
    system2("./gchm/bash/run_tile_deploy_merge.sh")
  })
  cat("### run_tile_deploy_merge.sh finished. ###\n")

  # Organize files and delete temp's
  paths <- organize_prediction_files(scenario, start_date_chr)
  
  ## COMPUTE METRICS
  loop_results <- compute_metrics(scenario$tile, paths$result_path, paths$new_destination, mode = scenario$manipulation_type)
  finish_loop(loop_results,BACKUP_SAVING,loop_backup_dir,v)
  
  
  # save_outputs(metrics, scenario)
  # Timing?
  
  time_min <- end_timer(timer)
  loop_results$time_min <- time_min
  timing_results[[v]] <- time_min

  results[[v]] <- loop_results
  
}

## SAVE RESULTS
save_results(results)

script_summary()
