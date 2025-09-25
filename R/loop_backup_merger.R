library(data.table)

all_files <- list.files("/home/emilio/canopy_height/results/loop_backup/", full.names = T)

name_date <- substr(basename(all_files[1]), 1, 10)

# fread each file and stack
df_list <- lapply(all_files, fread)
big_dt <- rbindlist(df_list)

# Save combined CSV
fwrite(big_dt, paste0("/home/emilio/canopy_height/results/",name_date,"_combined_results.csv") )


file.remove(all_files)
