create_param_grid <- function(base_specs, params) {

  grids <- list()

  if ("shuffle" %in% base_specs$manipulation) {
    # Expand grid when shuffling
    grids$shuffle <- expand.grid(
      tile = base_specs$tile,
      year = base_specs$year,
      WC_year = base_specs$WC_year,
      shuffle_pct = as.numeric(params$shuffle$shuffle_pct),
      patch_size = as.numeric(params$shuffle$patch_size),
      manipulation_type = "shuffle",
      rootDIR = base_specs$rootDIR,
      out_dir = file.path(base_specs$rootDIR, "final_results"),
      original = FALSE
    )

  }

  if ("spectral" %in% base_specs$manipulation) {
    # Expand grid when spectral manipulation
    grids$spectral <- expand.grid(
      tile = base_specs$tile,
      year = base_specs$year,
      WC_year = base_specs$WC_year,
      band = params$spectral$band,
      increment = params$spectral$increment,
      decrease = params$spectral$decrease,
      manipulation_type = "spectral",
      rootDIR = base_specs$rootDIR,
      out_dir = file.path(base_specs$rootDIR, "final_results"),
      original = FALSE
    )


  }

  df <- dplyr::bind_rows(grids)

  # Create Out_Names
  df$out_name <- apply(df, 1, function(row) {

    parts <- c(row["tile"], row["manipulation_type"])

    if (!is.na(row["band"])) {
      parts <- c(parts, row["band"])
    }

    if (!is.na(row["increment"])) {
      parts <- c(parts, paste0(ifelse(row["decrease"],"D_","I_"), row["increment"]))
    }

    if (!is.na(row["shuffle_pct"])) {
    parts <- c(parts, sprintf("%0.1f", as.numeric(row["shuffle_pct"])))
    }

    if (!is.na(row["patch_size"])) {
      parts <- c(parts, paste0(row["patch_size"], "x", row["patch_size"]))
    }

    paste(parts, collapse = "_")
  })

  # ADD ORIGINAL ROWS
  if ("shuffle" %in% base_specs$manipulation){
      for (i in unique(base_specs$tile)) {
        original_row <- data.frame(
                          tile = i,
                          year = base_specs$year,
                          WC_year = base_specs$WC_year,
                          shuffle_pct = c(0),
                          patch_size = params$shuffle$patch_size,
                          manipulation_type = "shuffle",
                          rootDIR = base_specs$rootDIR,
                          out_dir = file.path(base_specs$rootDIR, "final_results"),
                          out_name = paste0(i,"_original"),
                          original = TRUE
                        )
        
      df <- rbind(original_row, df)

      }
  } else if ("spectral" %in% base_specs$manipulation) {
      for (i in unique(base_specs$tile)) {
        original_row <- data.frame(
                          tile = i,
                          year = base_specs$year,
                          WC_year = base_specs$WC_year,
                          band = "B07",
                          increment = c(0),
                          decrease = FALSE,
                          manipulation_type = "spectral",
                          rootDIR = base_specs$rootDIR,
                          out_dir = file.path(base_specs$rootDIR, "final_results"),
                          out_name = paste0(i,"_original"),
                          original = TRUE
                        )
        
        df <- rbind(original_row, df)
      }
    }


      

  return(df)

}
