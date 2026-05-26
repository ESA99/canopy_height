# R/deploy/config.R ------------------------------------------------------------
# Global libraries -------------------------------------------------------

options(repos = c(CRAN = "https://cloud.r-project.org"))

packages <- c(
  "sf", "terra", "remotes", "tmap",
  "rnaturalearth", "rnaturalearthdata",
  "plyr", "dplyr", "leaflet",
  "viridis", "cols4all", "colorspace", "transport"
)

# Install any packages that are missing
installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)

if (length(to_install) > 0) {
  message("Installing missing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install, dependencies = TRUE)
}

# Install dandelion from GitHub if missing
if (!"dandelion" %in% installed) {
  message("Installing package 'dandelion' from GitHub...")
  remotes::install_github("ESA99/dandelion")
}

# Load all packages
packages <- c(packages, "dandelion")
invisible(lapply(packages, library, character.only = TRUE))



# ROOT Directories -------------------------------------------------------

BASE_DIR <- "/home/emilio/canopy_height"

# Export location of tifs and backup
EXPORT_TIF_LOC <- file.path("/data/ESA99/export",format(Sys.Date(), "%Y-%m-%d"))
dir.create(EXPORT_TIF_LOC)

PRED_TIF_LOCATION <- file.path(EXPORT_TIF_LOC, "predictions")
dir.create(PRED_TIF_LOCATION)
# PRED_TIF_LOCATION <- "/data/ESA99/resultmaps_bands/I"
# PRED_TIF_LOCATION <- file.path("/data/ESA99/pred_tif", format(Sys.Date(), "%Y-%m-%d"))


# MODEL SELECTION --------------------------------------------------------

MODEL_ID = 2

# FLAGS ------------------------------------------------------------------

BACKUP_SAVING <- TRUE    # Should loop results be saved individually as backup (csv files)?
DIFF_TIF <- FALSE        # Should the difference rasters be saved?
PRED_TIF <- TRUE         # Should the prediction result tif's be saved and where?


# Global Seed ------------------------------------------------------------

GLOBAL_SEED <- 42
set.seed(GLOBAL_SEED)


