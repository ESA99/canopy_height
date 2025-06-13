# Define the folder path
folder_path <- "final_results/individual_results"

# Create the list of full file paths
file_names <- sprintf("%03d_result.csv", 1:99)
full_paths <- file.path(folder_path, file_names)

# Read, rename 7th column, and store in a list
df_list <- lapply(full_paths, function(file) {
  df <- read.csv(file)
  colnames(df)[7] <- "Correlation"
  return(df)
})

# Combine all into one data frame
combined_df <- do.call(rbind, df_list)


write.csv(combined_df, 
          file.path("final_results", paste0(Sys.Date(), "_result_table.csv")), 
          row.names = F)
