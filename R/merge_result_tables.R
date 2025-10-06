library(ggplot2)
library(ggpubr)
library(dplyr)
library(viridis)

original_data <- "/home/emilio/canopy_height/results/2025-09-25_combined_results_49UCP_55HEV.csv"
data1 <- read.csv(original_data, row.names = 1)
original_data <- "/home/emilio/canopy_height/results/2025-10-03_result_table_49NHC_35VML_34UFD_33NTG_32UQU_32TMT.csv"
data2 <- read.csv(original_data)
original_data <- "/home/emilio/canopy_height/results/2025-09-26_combined_results_20MMD_10TES_17SNB.csv"
data3 <- read.csv(original_data)

head(data1)
head(data2)
head(data3)

results <- rbind(data1,data2, data3)

names(results)[names(results) == "avg_differece_percent"] <- "avg_difference_percent"
head(results)

write.csv(results, "/home/emilio/canopy_height/results/2025-10-06_full_results.csv", row.names = FALSE)
