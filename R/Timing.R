
# Inspect Timing results ----------------------------------------------------------

library(tidyverse)
lines <- readLines("documentation/times.txt")
lines <- lines[-1] # Remove the header

# Extract Step and Duration using regular expressions
df <- tibble(raw = lines) %>%
  mutate(
    Step = str_extract(raw, "End of Loop\\s+\\d+\\s?/\\s?\\d+"),
    Duration = str_extract(raw, "\\d+\\.\\d+\\s+mins"),
    Duration = as.numeric(str_extract(Duration, "\\d+\\.\\d+"))
  ) %>%
  select(Step, Duration)

df <- df %>%
  mutate(
    StepTime = Duration - lag(Duration)
  ) %>%
  mutate(
    StepTime = if_else(is.na(StepTime), Duration, StepTime)
  ) %>% 
  na.omit(df)

mean(df$StepTime)
which.min(df$StepTime)
which.max(df$StepTime)
min(df$StepTime)
max(df$StepTime)
# View result
print(df)





# How to time -------------------------------------------------------------

##  Track individual loops in a table

# Create empty data frame to store timing info
timing_results <- data.frame(
  Step = character(),
  Duration = character(),
  stringsAsFactors = FALSE
)

for (i in 1:3) {
  step_name <- paste("Iteration", i)
  start_time <- Sys.time()
  
  Sys.sleep(runif(1, 1, 2))  # Simulated work

  # TIMING BLOCK
  end_time <- Sys.time()
  timing_results <- rbind(timing_results,
    data.frame(Step = paste0("step_name",v), 
               Duration = round(difftime(end_time, start_time, units = "mins"), 2), stringsAsFactors = FALSE) )
}

print(timing_results, row.names = FALSE)


## Global timing

start_time <- Sys.time()
# Code block 1
end_time <- Sys.time()
print(paste("Block 1 took", round(difftime(end_time, start_time, units = "mins"), 2), "minutes"))


