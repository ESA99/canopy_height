
# Inspect Timing results ----------------------------------------------------------

library(tidyverse)
# lines <- readLines("documentation/2025-06-12_Timing.txt")
# lines <- lines[-1] # Remove the header
# 
# df <- read.csv(textConnection(lines), header = FALSE) # read as CSV

df <- read.csv("documentation/TIMING/2025-09-25_Timing.csv") # read as CSV
colnames(df) <- c("Loop", "Step", "Minutes") # assign column names

mean(df$Minutes)
min(df$Minutes)
max(df$Minutes)
which.min(df$Minutes)
which.max(df$Minutes)
# View result
print(df)

sum(df$Minutes)/60

### Time estimate for future calculations

num_iterations <- 246
# mean_time <- mean(df$Minutes)
mean_time <- 13.38203

(t <- num_iterations*mean_time/60) # hours
(finishing_time <- Sys.time() + (t * 3600) )

# Hour converter - works also for minutes and seconds
hours <- 54.86
minutes <- round((hours - floor(hours)) * 60)
cat(floor(hours), "hours and", minutes, "minutes\n")


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




# OLD TIMING TABLES -------------------------------------------------------



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
