# Deployment script of the analysis

source("R/deploy/config.R")
source("R/deploy/logging.R")
source("R/deploy/create_variables_df.R")
source("R/deploy/run_init.R")
source("R/deploy/prepare_scenario.R")
source("R/deploy/organize_outputs.R")
# source("R/compute_metrics.R")
# source("R/save_outputs.R")

source("R/deploy/worldcover_adjust_new.R")



# Scenario Setup -----------------------------------------------------

base_specs <- list(
  tile = c("10TES", "17SNB", "20MMD", "32TMT", "32UQU", "33NTG", "34UFD", "35VML", "49NHC", "49UCP", "55HEV"),
  year = c("2020"),
  WC_year = c("2020"),
  rootDIR = "/home/emilio/canopy_height",
  manipulation = c("shuffle") 
  # manipulation = c("spectral")
)

param_specs <- list(
  
  shuffle = list(
    shuffle_pct = c(2.5, 5, 10, 15, 20, 25, 30, 50, 65, 80, 100),
    patch_size = c(1)
  ),
  
  spectral = list(
    band = c("B02", "B03", "B04", "B05", "B08", "B8A", "B11", "B12"),
    increment = c(0.05, 0.1, 0.15, 0.2, 0.25),
    decrease = c(FALSE,TRUE)
  )
)

write_metadata(base_specs, param_specs, run_dir)

variables <- create_param_grid(base_specs, param_specs)

source("R/deploy/setup_check.R")


# CORE LOOP --------------------------------------------------------------

results <- vector("list", nrow(variables))
timing_results <- vector("list", nrow(variables))

for (v in seq_len(nrow(variables)) ) {
  
  scenario <- variables[v, ]
  scenario$run_dir <- run_dir
  scenario$loop_backup_dir <- file.path(run_dir,"loop_backup")

  timer <- start_timer()

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

  source("R/deploy/organize_outputs.R")
  


  metrics$time_min <- end_timer(timer)

  save_outputs(metrics, scenario)

  results[[v]] <- metrics

  # Timing?
}

results_df <- dplyr::bind_rows(results)
# Export
