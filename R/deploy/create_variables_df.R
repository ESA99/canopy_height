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

    df <- dplyr::bind_rows(grids$shuffle)

    # Add out name
    df$out_name <- apply(df, 1, function(row) {
      parts <- c(row["tile"], row["manipulation_type"])
      if (!is.na(row["shuffle_pct"])) {
      parts <- c(parts, sprintf("%0.1f", as.numeric(row["shuffle_pct"])))
      }
      if (!is.na(row["patch_size"])) {
        parts <- c(parts, paste0(row["patch_size"], "x", row["patch_size"]))
      }
      paste(parts, collapse = "_")
    })


    # Add original row
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

    message("Variable Table created. Scenario: Shuffle. ", nrow(df), " entries.\n")
  

  } else if ("spectral" %in% base_specs$manipulation) {
    
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

    df <- dplyr::bind_rows(grids$spectral)

    # Add out name
    df$out_name <- apply(df, 1, function(row) {
      band_string <- paste(unlist(row["band"]), collapse = "-")
      parts <- c(row["tile"], row["manipulation_type"])
      if (!is.na(band_string) && band_string != "") {
        parts <- c(parts, band_string)
      }
      if (!is.na(row["increment"])) {
        parts <- c(parts, paste0(ifelse(row["decrease"] == "True","D_","I_"), 
        as.integer(as.numeric(row["increment"]) * 100) ))
      }
      paste(parts, collapse = "_")
    })

    # Add original row
    for (i in unique(base_specs$tile)) {
      original_row <- data.frame(
                        tile = i,
                        year = base_specs$year,
                        WC_year = base_specs$WC_year,
                        # band = "B02",
                        band = I(list(params$spectral$band[[1]])),
                        increment = c(0),
                        decrease = "False",
                        manipulation_type = "spectral",
                        rootDIR = base_specs$rootDIR,
                        out_dir = file.path(base_specs$rootDIR, "final_results"),
                        out_name = paste0(i,"_original"),
                        original = TRUE
                      )
      df <- rbind(original_row, df)
    }
    
    # df$Colour <- translation_table$Colour[
    #     match(df$band, translation_table$BandName)
    #   ]
    df$Colour <- vapply(
      df$band,
      function(bands) {
        paste(
          translation_table$Colour[
            match(bands, translation_table$BandName)
          ],
          collapse = "-"
        )
      },
      character(1)
    )
    message("Variable Table created. Scenario: Spectral. ", nrow(df), " entries.\n")


  } else if  ("geographical" %in% base_specs$manipulation) {
      # Shift coordinates N-S by X km
    
    
    message("Variable Table created. Scenario: Geographical. ", nrow(df), " entries.\n")

  }

  return(df)

}
