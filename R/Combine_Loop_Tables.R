
folder_path <- "results/loop_backup"

full_paths <- list.files(folder_path, full.names = T)

df_list <- lapply(full_paths, function(file) {
  df <- read.csv(file)
  return(df)
})

combined_df <- do.call(rbind, df_list)

u <- unique(combined_df$tile)
write.csv(combined_df, 
          file.path("results", paste0(Sys.Date(), "_combined_results_",paste(u, collapse = "_"),".csv")), 
          row.names = F)


