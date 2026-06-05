start_timer <- function() {
  list(start = Sys.time())
}

end_timer <- function(timer) {
  elapsed_min <- as.numeric(
    difftime(Sys.time(), timer$start, units = "mins")
  )

  list(
    hours = elapsed_min / 60,
    minutes = elapsed_min,
    seconds = elapsed_min * 60
  )
}

log_result <- function(results_list, v, result) {
  results_list[[v]] <- result
  results_list
}

# start_time <- Sys.time()
# start_date_chr <- format(Sys.Date(), "%Y-%m-%d")

runtime <- list(
  start_time = Sys.time(),
  start_date_chr = format(Sys.Date(), "%Y-%m-%d")
)

format_runtime <- function(start_time) {
  elapsed <- Sys.time() - start_time
  
  h <- as.integer(elapsed) %/% 3600
  m <- as.integer(elapsed) %% 3600 %/% 60
  s <- as.integer(elapsed) %% 60
  
  sprintf("%02d:%02d:%02d", h, m, s)
}


# TIMING Calculation
# mean_loop_time <- 9.5 # minutes -> derived from timing data of past loops
mean_loop_times_sec <- c(shuffle = 5, spectral = 9, geographical = 6)*60

# OLD SETUP -----------------------------------------------------------

# # Create empty data frame to store timing info
# timing_results <- data.frame(
#   Step = character(),
#   Minutes = numeric(),
#   stringsAsFactors = FALSE
# )
  # --->> Better write it differently later instead of creating empty data? using [[v]]