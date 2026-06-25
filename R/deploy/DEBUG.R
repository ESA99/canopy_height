apply_debug_base_specs <- function(base_specs, DEBUG = FALSE) {

  if (!DEBUG) return(base_specs)

  base_specs$tile <- c("17SNB")
  base_specs$year <- c("2020")
  base_specs$WC_year <- c("2020")
  
  base_specs$manipulation <- c(base_specs$manipulation[1]) # ensure manipulation is consistent with test runs

  return(base_specs)
}

apply_debug_params <- function(param_specs, interactions = FALSE, DEBUG = FALSE) {

  if (!DEBUG) return(param_specs)

  # SHUFFLE DEBUG
  if (!is.null(param_specs$shuffle)) {
    param_specs$shuffle$shuffle_type <- c("local")
    param_specs$shuffle$shuffle_pct <- c(10)
    param_specs$shuffle$patch_size <- c(64)
  }

  # SPECTRAL DEBUG
  if (!is.null(param_specs$spectral)) {
    param_specs$spectral$band <- list("B02")
    param_specs$spectral$increment <- c(0.15)
    param_specs$spectral$decrease <- c("False")
  }
  if (!is.null(param_specs$spectral) && interactions == TRUE) {
    param_specs$spectral$band <- list("B02",c("B03", "B04"))
  }


  # GEOGRAPHICAL DEBUG
  if (!is.null(param_specs$geographical)) {
    param_specs$geographical$shift_distance <- c(100, 2000)
    param_specs$geographical$shift_direction <- c("S")
  }

  return(param_specs)
}