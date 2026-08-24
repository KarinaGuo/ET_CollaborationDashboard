library(galah)
library(dplyr)
library(CoordinateCleaner)

# Configure galah for the Atlas of Living Australia
galah_config(email = "karinag.work@gmail.com")

if (!exists("target_species")) {
  stop("target_species must be defined before sourcing this script.")
}

message(sprintf("Fetching ALA occurrence data for %s...", target_species))

# Fetch data for the target species restricted to Australia
# We select coordinates for mapping purposes
occ_data <- galah_call() |>
  galah_identify(target_species) |>
  galah_filter(country == "Australia") |>
  galah_select(decimalLatitude, decimalLongitude, species) |>
  atlas_occurrences() |> 
  mutate(
    decimalLongitude = as.numeric(decimalLongitude),
    decimalLatitude = as.numeric(decimalLatitude)
  ) |>
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude))

occ_cleaned <- occ_data |>
  clean_coordinates(
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "species",
    # Specify the tests you want to run
    tests = c(
      "capitals",      # records near country capitals (often default centroid inputs)
      "centroids",     # country or province centroids
      "equal",         # equal lat/lon (e.g., 50, 50)
      "gbif",          # near GBIF headquarters
      "institutions",  # near known biodiversity institutions (zoos, museums)
      "seas",          # records falling in the ocean
      "zeros"          # exact 0,0 coordinates
    ),
    # Return only the clean records (removes the anomalous ones automatically)
    value = "clean"
  )

# The file name is expected to be GenusSpecies_Occurrence_Data.csv (with space replaced by underscore)
file_name <- paste0(gsub(" ", "_", target_species), "_Occurrence_Data.csv")
out_path <- file.path("Data", file_name)

write.csv(occ_cleaned, out_path, row.names = FALSE)
message(sprintf("Saved %d records to %s", nrow(occ_data), out_path))
