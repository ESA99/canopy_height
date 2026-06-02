create_param_grid <- function(base_specs, param_specs) {

  grids <- list()

  # ========================== SHUFFLE ========================== #
  if ("shuffle" %in% base_specs$manipulation) {
    
    grids$shuffle <- expand.grid(
      tile = base_specs$tile,
      year = base_specs$year,
      WC_year = base_specs$WC_year,
      shuffle_pct = as.numeric(param_specs$shuffle$shuffle_pct),
      patch_size = as.numeric(param_specs$shuffle$patch_size),
      subtile_size = param_specs$shuffle$subtile_size,
      manipulation_type = "shuffle",
      rootDIR = base_specs$rootDIR,
      out_dir = file.path(base_specs$rootDIR, "final_results"),
      original = FALSE
    )

    df <- dplyr::bind_rows(grids$shuffle)
    if (nrow(df) == 0) {
      stop("Shuffle variable grid has 0 rows. Check param_specs$shuffle inputs.")
    }

    # Add out name
    df$out_name <- apply(df, 1, function(row) {
      parts <- c(row["tile"], row["manipulation_type"])
      if (!is.na(row["shuffle_pct"])) {
      parts <- c(parts, sprintf("%0.1f", as.numeric(row["shuffle_pct"])))
      }
      if (!is.na(row["patch_size"])) {
        parts <- c(parts, paste0(row["patch_size"], "x", row["patch_size"]))
      }
      if (!is.na(row["subtile_size"])) {
        parts <- c(parts, paste0("ST",as.character(row["subtile_size"])) )
      }

      paste(parts, collapse = "_")
    })


    # Add original row
    for (i in unique(base_specs$tile)) {
      original_rows <- data.frame(
                        tile = i,
                        year = base_specs$year,
                        WC_year = base_specs$WC_year,
                        shuffle_pct = c(0),
                        patch_size = param_specs$shuffle$patch_size,
                        subtile_size = NA,
                        manipulation_type = "shuffle",
                        rootDIR = base_specs$rootDIR,
                        out_dir = file.path(base_specs$rootDIR, "final_results"),
                        out_name = paste0(i,"_original"),
                        original = TRUE
                      )
      df <- rbind(original_rows, df)
    }

    message("***** Variable Table created. Scenario: Shuffle. ", nrow(df), " entries. *****\n")
  

  # ========================== SPECTRAL ========================== #
  } else if ("spectral" %in% base_specs$manipulation) {
    
    grids$spectral <- expand.grid(
      tile = base_specs$tile,
      year = base_specs$year,
      WC_year = base_specs$WC_year,
      band = param_specs$spectral$band,
      increment = param_specs$spectral$increment,
      decrease = param_specs$spectral$decrease,
      manipulation_type = "spectral",
      rootDIR = base_specs$rootDIR,
      out_dir = file.path(base_specs$rootDIR, "final_results"),
      original = FALSE
    ) 

    df <- dplyr::bind_rows(grids$spectral)
    if (nrow(df) == 0) {
      stop("Spectral variable grid has 0 rows. Check param_specs$spectral inputs.")
    }

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
      original_rows <- data.frame(
                        tile = i,
                        year = base_specs$year,
                        WC_year = base_specs$WC_year,
                        # band = "B02",
                        band = I(list(param_specs$spectral$band[[1]])),
                        increment = c(0),
                        decrease = "False",
                        manipulation_type = "spectral",
                        rootDIR = base_specs$rootDIR,
                        out_dir = file.path(base_specs$rootDIR, "final_results"),
                        out_name = paste0(i,"_original"),
                        original = TRUE
                      )
      df <- rbind(original_rows, df)
    }
    
    # df$Colour <- band_translation$Colour[
    #     match(df$band, band_translation$BandName)
    #   ]
    df$Colour <- vapply(
      df$band,
      function(bands) {
        paste(
          band_translation$Colour[
            match(bands, band_translation$BandName)
          ],
          collapse = "-"
        )
      },
      character(1)
    )
    message("***** Variable Table created. Scenario: Spectral. ", nrow(df), " entries. *****\n")


  # ========================== GEOGRAPHICAL ========================== #
  } else if  ("geographical" %in% base_specs$manipulation) {
      # Shift coordinates N-S by X km
    
    grids$geographical <- expand.grid(
      tile = base_specs$tile,
      year = base_specs$year,
      WC_year = base_specs$WC_year,
      shift_distance = param_specs$geographical$shift_distance,
      shift_direction = param_specs$geographical$shift_direction,
      manipulation_type = "geographical",
      rootDIR = base_specs$rootDIR,
      out_dir = file.path(base_specs$rootDIR, "final_results"),
      original = FALSE
    )

    df <- dplyr::bind_rows(grids$geographical)
    if (nrow(df) == 0) {
      stop("Geographical variable grid has 0 rows. Check param_specs$geographical inputs.")
    }
    
    # Out Name
    df$out_name <- apply(df, 1, function(row) {
      parts <- c(row["tile"], row["manipulation_type"])
      if (!is.na(row["shift_distance"])) {
        parts <- c(parts, sprintf("%d", as.numeric(row["shift_distance"])))
      }
      if (!is.na(row["shift_direction"])) {
        parts <- c(parts, as.character(row["shift_direction"]) )
      }
      paste(parts, collapse = "_")
    })

    # Original Rows
    for (i in unique(base_specs$tile)) {
      original_rows <- data.frame(
                        tile = i,
                        year = base_specs$year,
                        WC_year = base_specs$WC_year,
                        shift_distance = c(0),
                        shift_direction = "S",
                        manipulation_type = "geographical",
                        rootDIR = base_specs$rootDIR,
                        out_dir = file.path(base_specs$rootDIR, "final_results"),
                        out_name = paste0(i,"_original"),
                        original = TRUE
                      )
      df <- rbind(original_rows, df)
    }

    # Filter scnearios by max possible shift distance
    invalid_msg <- df %>%
      left_join(shift_limits, by = "tile") %>%
      filter((shift_direction == "N" & shift_distance > max_km_N) | (shift_direction == "S" & shift_distance > max_km_S) ) %>%
        dplyr::select( tile, shift_distance, shift_direction, max_km_N, max_km_S ) %>%
          dplyr::transmute(
            msg = sprintf("%s: %d km %s (max N=%d, max S=%d)",
              tile, shift_distance, shift_direction, max_km_N, max_km_S )  ) %>%
                dplyr::pull(msg)


    message("Excluded shifts:\n", paste(invalid_msg, collapse = "\n"))


    df <- df %>%
      left_join(shift_limits, by = "tile") %>%
      filter(
        shift_distance == 0 |
        (shift_direction == "N" & shift_distance <= max_km_N) |
        (shift_direction == "S" & shift_distance <= max_km_S)
      ) %>%
      select(-max_km_N, -max_km_S)

    message("***** Variable Table created. Scenario: Geographical. ", nrow(df), " entries. *****\n")


  }

  return(df)

}
