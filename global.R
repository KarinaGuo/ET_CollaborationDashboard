# global.R

library(shiny)
library(shinymanager)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(reactable)
library(shinyjs)
library(leaflet)
library(ggridges)
# library(CoordinateCleaner) # Uncomment when implemented

# Set data directory
data_dir <- "Data"

# Add resource path for figures directory so the browser can serve the images
addResourcePath("figures", "Figures")


# Load credentials for shinymanager
# shinymanager requires lowercase 'user' and 'password' columns
if (file.exists(file.path(data_dir, "User_permissions.csv"))) {
  credentials <- read.csv(file.path(data_dir, "User_permissions.csv"), stringsAsFactors = FALSE)
  if ("User" %in% names(credentials)) credentials$user <- credentials$User
  if ("Password" %in% names(credentials)) credentials$password <- credentials$Password
} else {
  stop("User_permissions.csv not found in Data directory.")
}

# Ensure ALA occurrence data exists for all species in the tracking file
if (file.exists(file.path(data_dir, "Species_tracking.csv"))) {
  tracking_data_startup <- read.csv(file.path(data_dir, "Species_tracking.csv"), stringsAsFactors = FALSE)
  
  for (sp in unique(tracking_data_startup$Species)) {
    file_name <- paste0(gsub(" ", "_", sp), "_Occurrence_Data.csv")
    out_path <- file.path(data_dir, file_name)
    
    if (!file.exists(out_path)) {
      message(sprintf("ALA data missing for %s. Generating...", sp))
      # Pass the target species to the script environment
      target_species <- sp
      source("Figure_generation_scripts/Species_occurrence_data.R", local = TRUE)
    }
  }
  
  # Check for new species missing in Phenology_data.csv and append placeholders
  if (file.exists(file.path(data_dir, "Phenology_data.csv"))) {
    pheno_data <- read.csv(file.path(data_dir, "Phenology_data.csv"), stringsAsFactors = FALSE)
    missing_sp <- setdiff(unique(tracking_data_startup$Species), unique(pheno_data$Species))
    
    if (length(missing_sp) > 0) {
      message(sprintf("Adding placeholder phenology data for %s...", paste(missing_sp, collapse = ", ")))
      new_rows <- data.frame(
        Species = rep(missing_sp, each = 2),
        PhenologyEvent = rep(c("Flowering", "Fruiting"), times = length(missing_sp)),
        StartMonth = NA,
        EndMonth = NA,
        ShortRef = "TBD",
        FullRef = "To be determined. Please update Phenology_data.csv.",
        stringsAsFactors = FALSE
      )
      pheno_data <- dplyr::bind_rows(pheno_data, new_rows)
      write.csv(pheno_data, file.path(data_dir, "Phenology_data.csv"), row.names = FALSE)
    }
  }
}

# Generate Scoring COI data on startup
if (file.exists("Figure_generation_scripts/Scoring_calculations.R")) {
  message("Running Scoring_calculations.R to update COI scores...")
  source("Figure_generation_scripts/Scoring_calculations.R", local = TRUE)
}
