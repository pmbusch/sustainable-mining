# ============================================================================ #
# Aggregate WaterGAP Grid Data to Basin Level
#
# Loads ensemble gridded data for all scenarios and variables
# Aggregates to basin level using pre-computed grid-basin lookup table
# Saves intermediate basin-level data for CF calculations
#
# Author: Pablo Busch
# Date: Feb 2026
# ============================================================================ #

# Load required packages ----
source('Scripts/00-Libraries.R', encoding = 'UTF-8')
library(sf)
library(lubridate)

# Set paths ----
input_dir_watergap <- "Parameters/WaterGAP"
output_dir <- "Parameters/WaterGAP"

# Load pre-computed grid-basin lookup table ----
cat("\n=== LOADING GRID-BASIN LOOKUP TABLE ===\n")
lookup_file <- file.path(input_dir_watergap, "grid_basin_lookup.csv")

if (!file.exists(lookup_file)) {
  stop("Grid-basin lookup table not found!\n", "Run script 05b-Create_Grid_Basin_Lookup.R first to create it.")
}

grid_basin_lookup <- read_csv(lookup_file, show_col_types = FALSE) |>
  dplyr::select(lon, lat, Basin_ID, overlap_fraction)

cat("Lookup table loaded:", nrow(grid_basin_lookup), "rows\n")
cat("Unique grid cells:", n_distinct(grid_basin_lookup$lon, grid_basin_lookup$lat), "\n")
cat("Unique basins:", n_distinct(grid_basin_lookup$Basin_ID), "\n")

# Get list of all ensemble files ----
cat("\n=== FINDING ENSEMBLE FILES ===\n")
intermediate_dir <- file.path(input_dir_watergap, "intermediate")
ensemble_files <- list.files(intermediate_dir, pattern = "^ensemble_.*\\.rds$", full.names = TRUE)

if (length(ensemble_files) == 0) {
  stop("No ensemble files found. Run script 05-WaterGAP_Process.R first.")
}

# Parse file names to get scenario-variable combinations
file_info <- tibble(file_path = ensemble_files) %>%
  mutate(
    file_name = basename(file_path),
    scenario = str_extract(file_name, "(?<=ensemble_)[^_]+"),
    variable = str_remove(str_extract(file_name, "(?<=_)[^.]+(?=\\.rds)"), "^.*_")
  )

cat("Found", nrow(file_info), "ensemble files:\n")
print(file_info %>% select(scenario, variable))

# Process each scenario-variable combination ----
cat("\n=== PROCESSING ENSEMBLE FILES ===\n")

basin_data_list <- list()

for (i in 1:nrow(file_info)) {
  current_scenario <- file_info$scenario[i]
  current_variable <- file_info$variable[i]
  current_file <- file_info$file_path[i]

  cat("\n--- Processing:", current_scenario, "-", current_variable, "---\n")
  cat("Loading file:", basename(current_file), "\n")

  # Load ensemble data
  ensemble_data <- readRDS(current_file)

  # Check for NAs
  na_count <- sum(is.na(ensemble_data$value_m3_month_median))
  cat("  Rows:", nrow(ensemble_data), "\n")
  cat("  Value range:", range(ensemble_data$value_m3_month_median, na.rm = TRUE), "\n")
  cat("  NA values:", na_count, "\n")

  # Join with lookup table
  cat("  Joining with lookup table...\n")
  basin_grid_overlap <- ensemble_data %>% inner_join(grid_basin_lookup, by = c("lon", "lat"))

  cat("  Join complete:", nrow(basin_grid_overlap), "rows\n")

  # Check overlap fraction
  cat("  Overlap fraction range:", range(basin_grid_overlap$overlap_fraction, na.rm = TRUE), "\n")

  # Aggregate to basin level
  cat("  Aggregating to basin level...\n")
  basin_data <- basin_grid_overlap %>%
    mutate(weighted_value = value_m3_month_median * overlap_fraction) %>%
    group_by(Basin_ID, year, month, scenario, variable) %>%
    summarise(value_m3_month = sum(weighted_value, na.rm = TRUE), .groups = "drop")

  cat("  Basin data rows:", nrow(basin_data), "\n")
  cat("  Unique basins:", n_distinct(basin_data$basin_id), "\n")
  cat("  Value range:", range(basin_data$value_m3_month, na.rm = TRUE), "\n")
  cat("  NA values:", sum(is.na(basin_data$value_m3_month)), "\n")

  # Store in list
  basin_data_list[[i]] <- basin_data

  # Clean up
  rm(ensemble_data, basin_grid_overlap, basin_data)
  gc()
}

# Combine all basin data ----
cat("\n=== COMBINING ALL BASIN DATA ===\n")
basin_data_all <- bind_rows(basin_data_list)

cat("Total rows:", nrow(basin_data_all), "\n")
cat("Scenarios:", paste(unique(basin_data_all$scenario), collapse = ", "), "\n")
cat("Variables:", paste(unique(basin_data_all$variable), collapse = ", "), "\n")
cat("Year range:", range(basin_data_all$year), "\n")
cat("Unique basins:", n_distinct(basin_data_all$Basin_ID), "\n")

# Quality checks ----
cat("\n=== QUALITY CHECKS ===\n")

# Check for NAs by variable
na_summary <- basin_data_all %>%
  group_by(scenario, variable) %>%
  summarise(
    n_rows = n(),
    na_values = sum(is.na(value_m3_month)),
    na_percent = 100 * na_values / n_rows,
    .groups = "drop"
  )

cat("NA summary by scenario-variable:\n")
print(na_summary)

# Check value ranges by variable
range_summary <- basin_data_all %>%
  group_by(scenario, variable) %>%
  summarise(
    min_value = min(value_m3_month, na.rm = TRUE),
    max_value = max(value_m3_month, na.rm = TRUE),
    mean_value = mean(value_m3_month, na.rm = TRUE),
    median_value = median(value_m3_month, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nValue range summary by scenario-variable:\n")
print(range_summary)

# Check basin coverage
basin_coverage <- basin_data_all %>%
  distinct(scenario, variable, Basin_ID) %>%
  group_by(scenario, variable) %>%
  summarise(n_basins = n(), .groups = "drop")

cat("\nBasin coverage by scenario-variable:\n")
print(basin_coverage)

# Save intermediate basin data ----
cat("\n=== SAVING INTERMEDIATE BASIN DATA ===\n")
output_file <- file.path(output_dir, "basin_data_intermediate.csv")
write_csv(basin_data_all, output_file)

cat("Basin data saved to:", output_file, "\n")

# EoF
