# Create empty data frame to store timing info
timing_results <- data.frame(
  Step = character(),
  Duration = character(),
  stringsAsFactors = FALSE
)

# Example steps (replace with real work blocks)
for (i in 1:3) {
  step_name <- paste("Iteration", i)
  start_time <- Sys.time()
  
  # ---- Your work here ----
  Sys.sleep(runif(1, 1, 2))  # Simulated work
  # ------------------------
  

  # TIMING BLOCK
  end_time <- Sys.time()
  timing_results <- rbind(timing_results,
    data.frame(Step = paste0("step_name",v), 
               Duration = round(difftime(end_time, start_time, units = "mins"), 2), stringsAsFactors = FALSE) )
}

# Print timing table at the end
print(timing_results, row.names = FALSE)


#####


start_time <- Sys.time()

# Code block 1
for (i in 1:1000) {
  # some work
}
end_time <- Sys.time()
print(paste("Block 1 took", round(difftime(end_time, start_time, units = "mins"), 2), "minutes"))


