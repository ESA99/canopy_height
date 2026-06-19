# TOOLS
library(data.table)


# Merge loop backups -----------------------------------------------------

merge_backup_files <- function(directory, export = TRUE){

  backup_files <- list.files(directory, pattern = "\\.csv$", full.names = T)

  file_id <- as.integer(tools::file_path_sans_ext(basename(backup_files)))
  backup_files <- backup_files[order(file_id)]  

  date <- sub(".*/([0-9]{4}-[0-9]{2}-[0-9]{2})_.*", "\\1", backup_files) |> unique()

  df_list <- lapply(backup_files, fread)
  big_dt <- rbindlist(df_list)

  if (export){
    out_file <- paste0(dirname(directory),"/",date,"_combined_results.csv")
    fwrite(big_dt, out_file)
    message("Combined table of loop backups saved to: ", out_file)
  }

  return(big_dt)
}


# Add Location Column ----------------------------------------------------

add_location_column <- function(result_table, order.by.mean = FALSE) {

  # Ensure tile column exists
  if (!"tile" %in% names(result_table)) {
    result_table$tile <- NA
  }

  # Tile → Location mapping
  tile_label <- c(
    "55HEV" = "Australia",
    "20MMD" = "Brazil",
    "33NTG" = "Cameroon",
    "32UQU" = "Germany",
    "35VML" = "Finland",
    "49NHC" = "Malaysia",
    "49UCP" = "Mongolia",
    "34UFD" = "Poland",
    "32TMT" = "Switzerland",
    "10TES" = "USA East",
    "17SNB" = "USA West"
  )

  
  if (order.by.mean) {
    tile_order <- result_table %>%
      dplyr::filter(original) %>%
      dplyr::distinct(tile, mean_height) %>%
      dplyr::arrange(mean_height) %>%
      dplyr::pull(tile)
  } else {
    tile_order <- names(tile_label)
  }

  # Add Location as factor
  # result_table$location <- factor(
  #   result_table$tile,
  #   levels = names(tile_label),
  #   labels = tile_label
  # )
  result_table$location <- factor(
    result_table$tile,
    levels = tile_order,
    labels = tile_label[tile_order]
  )

  return(result_table)
}


# Add 0 Increment rows if missing ----------------------------------------

# Checks for tile-band combinations without 0-Increment row and adds them if needed
add_zero_step_rows <- function(result_table) {
  
  # Identify which combinations are missing a zero increment row
  missing_zero_rows <- result_table %>%
    distinct(tile, band) %>%  # Identify all tile-band combinations
    anti_join(
      result_table %>% filter(increment == 0), # check which combinations are missing
      by = c("tile", "band")
    )
  
  if (nrow(missing_zero_rows) == 0) {
    message("No columns missing")
  } else {
    
    # Create the zero increment rows
    zero_increment_rows <- missing_zero_rows %>%
      mutate(
        increment = 0,
        decrease = "True",
        out_name = paste(tile, band, "original", sep = "_"),
        year = 2020
      )
    
    # Add missing columns
    missing_cols <- setdiff(names(result_table), names(zero_increment_rows))
    zero_increment_rows[missing_cols] <- 0.0  # Add all missing columns as 0.0
    
    # Combine with original table
    new_table <- bind_rows(result_table, zero_increment_rows)
    
    cat(nrow(missing_zero_rows), "rows added to table.\n")
    return(new_table)
  }
}



# SAVE PLOT by format ----------------------------------------------------

save_geo_plot <- function(plotname, format = c("wide", "medium", "tall"), plot = ggplot2::last_plot()) {
  
  format <- match.arg(format)
  
  date_prefix <- format(Sys.Date(), "%Y-%m-%d")
  
  file_path <- paste0(
    "plots/geographic/",
    date_prefix, "_", plotname, ".png"
  )
  
  dims <- switch(
    format,
    wide   = list(width = 270, height = 175),
    medium = list(width = 250, height = 200),
    tall   = list(width = 200, height = 250)
  )
  
  ggsave(
    filename = file_path,
    plot = plot,
    width = dims$width,
    height = dims$height,
    units = "mm",
    dpi = 300,
    bg = "white"
  )
  
  message("Saved plot to: ", file_path)
}

save_shuffle_plot <- function(plotname, format = c("wide", "medium", "tall"), plot = ggplot2::last_plot()) {
  
  format <- match.arg(format)
  
  date_prefix <- format(Sys.Date(), "%Y-%m-%d")
  
  file_path <- paste0(
    "plots/shuffle/",
    date_prefix, "_", plotname, ".png"
  )
  
  dims <- switch(
    format,
    wide   = list(width = 270, height = 175),
    medium = list(width = 250, height = 200),
    tall   = list(width = 200, height = 250)
  )
  
  ggsave(
    filename = file_path,
    plot = plot,
    width = dims$width,
    height = dims$height,
    units = "mm",
    dpi = 300,
    bg = "white"
  )
  
  message("Saved plot to: ", file_path)
}

save_spectral_plot <- function(plotname, format = c("wide", "medium", "tall"), plot = ggplot2::last_plot()) {
  
  format <- match.arg(format)
  
  date_prefix <- format(Sys.Date(), "%Y-%m-%d")
  
  file_path <- paste0(
    "plots/spectral/",
    date_prefix, "_", plotname, ".png"
  )
  
  dims <- switch(
    format,
    wide   = list(width = 270, height = 175),
    medium = list(width = 250, height = 200),
    tall   = list(width = 200, height = 250)
  )
  
  ggsave(
    filename = file_path,
    plot = plot,
    width = dims$width,
    height = dims$height,
    units = "mm",
    dpi = 300,
    bg = "white"
  )
  
  message("Saved plot to: ", file_path)
}