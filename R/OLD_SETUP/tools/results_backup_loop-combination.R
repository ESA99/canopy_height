library(data.table)

all_files <- list.files("/home/emilio/canopy_height/results/loop_backup/", full.names = T)

name_date <- substr(basename(all_files[1]), 1, 10)

# fread each file and stack
df_list <- lapply(all_files, fread)
big_dt <- rbindlist(df_list)

# Save combined CSV
fwrite(big_dt, paste0("/home/emilio/canopy_height/results/",name_date,"_combined_results.csv") )


file.remove(all_files)



########
# Combine loop results V1


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

