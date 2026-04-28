create_param_df_new <- function(tiles,
                            bands = NULL,
                            increments = NULL,
                            decrease = NULL,
                            shuffle_pct = NULL,   # NEW
                            year,
                            base_folder,
                            worldcover = "2020") {

  base_folder <- normalizePath(base_folder)

  # ---- Build parameter list dynamically ----
  params <- list(
    tile_name = tiles,
    year = year,
    rootDIR = base_folder,
    WC_year = worldcover,
    original = FALSE
  )

  if (!is.null(bands)) params$band <- bands
  if (!is.null(increments)) params$increment <- increments
  if (!is.null(decrease)) params$decrease <- decrease
  if (!is.null(shuffle_pct)) params$shuffle_pct <- shuffle_pct

  # ---- Create combinations ----
  df <- do.call(expand.grid, c(params, stringsAsFactors = FALSE))

  # ---- Paths ----
  tile_folder <- file.path(base_folder, "deploy_example", "sentinel2", year)
  df$tile_name <- trimws(df$tile_name)
  df$tile_folder <- file.path(tile_folder, df$tile_name)

  # ---- Output name (adaptive) ----
  df$out_name <- apply(df, 1, function(row) {

    parts <- c(row["tile_name"])

    if ("band" %in% names(df)) {
      parts <- c(parts, paste(unlist(row["band"]), collapse = "-"))
    }

    if ("increment" %in% names(df)) {
      parts <- c(parts,
                 sub("0\\.", "", formatC(as.numeric(row["increment"]), format = "f", digits = 2)))
    }

    if ("decrease" %in% names(df)) {
      parts <- c(parts, ifelse(row["decrease"] == "False", "I", "D"))
    }

    if ("shuffle_pct" %in% names(df)) {
      parts <- c(parts, paste0("shuffle", row["shuffle_pct"]))
    }

    paste(parts, collapse = "_")
  })

  # ---- Add original rows ----
  for (t in unique(tiles)) {

    extra_row <- df[1, , drop = FALSE]

    # tile-specific values
    extra_row$tile_name <- t
    extra_row$year <- year[1]
    extra_row$rootDIR <- base_folder
    extra_row$WC_year <- worldcover
    extra_row$original <- TRUE
    extra_row$tile_folder <- file.path(tile_folder, t)
    extra_row$out_name <- paste0(t, "_original")

    # ---- SAFE DEFAULTS ----
    if ("band" %in% names(df)) {
      extra_row$band <- I(list(df$band[[1]]))
    }

    if ("increment" %in% names(df)) {
      extra_row$increment <- 0
    }

    if ("decrease" %in% names(df)) {
      extra_row$decrease <- "True"
    }

    if ("shuffle_pct" %in% names(df)) {
      extra_row$shuffle_pct <- 0
    }

    df <- rbind(extra_row, df)
  }

  df$out_dir <- file.path(base_folder, "final_results")

  return(df)
}
