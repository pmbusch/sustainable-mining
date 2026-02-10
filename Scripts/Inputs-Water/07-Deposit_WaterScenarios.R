# ============================================================================ #
# Join Deposit Data with Future Water Scenario CFs
#
# Loads deposit database and basin-level CF projections (from script 06).
# Pivots CF data wide by period, so each deposit keeps one row with columns
# for cf, availability, and demand per 5-year period.
# Saves one file per scenario x climate_model combination.
#
# Author: Pablo Busch
# Date: Feb 2026
# ============================================================================ #

# Load required packages ----
source('Scripts/00-Libraries.R', encoding = 'UTF-8')

# Set paths ----
deposit_file <- "Parameters/Deposit.csv"
cf_file <- "Parameters/WaterGAP/basin_cf_data_5yr.csv"
output_dir <- "Parameters/WaterScenarios"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Load data ----
cat("\n=== LOADING DATA ===\n")
deposit <- read.csv(deposit_file)
basin_cf <- fread(cf_file)

cat("Deposits:", nrow(deposit), "\n")
cat("Scenarios:", paste(unique(basin_cf$scenario), collapse = ", "), "\n")
cat("Climate models:", paste(unique(basin_cf$climate_model), collapse = ", "), "\n")
cat("Periods:", paste(unique(basin_cf$period_label), collapse = ", "), "\n")

# Loop over scenario x climate_model ----
combos <- unique(basin_cf[, .(scenario, climate_model)])
cat("\n=== PROCESSING", nrow(combos), "COMBINATIONS ===\n")

for (i in 1:nrow(combos)) {
  sc <- combos$scenario[i]
  cm <- combos$climate_model[i]
  cat("\n--- [", i, "/", nrow(combos), "]", sc, "-", cm, "---\n")

  # Filter and pivot wide by period: one row per Basin_ID, columns per period
  # fmt: skip
  cf_wide <- basin_cf[scenario == sc & climate_model == cm,
                       .(Basin_ID, period_label, cf, availability_m3_yr, demand_m3_yr)] %>%
    rename(aware_available=availability_m3_yr,aware_demand=demand_m3_yr) %>%
    mutate(period_label=str_replace(period_label, "–", "_")) %>%
    pivot_wider(
      names_from = period_label,
      values_from = c(cf, aware_available, aware_demand),
      names_sep = "_"
    )

  # Join to deposits (one row per deposit)
  deposit_out <- deposit |>
    dplyr::select(Name, ID, Basin_ID, water_cons_2025, water) |>
    left_join(as_tibble(cf_wide), by = "Basin_ID") |>
    # recalculate water footprint
    rename(
      water_footprint_2025_2030 = cf_2025_2030,
      water_footprint_2030_2035 = cf_2030_2035,
      water_footprint_2035_2040 = cf_2035_2040,
      water_footprint_2040_2045 = cf_2040_2045,
      water_footprint_2045_2050 = cf_2045_2050
    ) |>
    mutate(across(starts_with("water_footprint"), ~ . * water)) |> # calculate water footprint for mining consumption
    mutate(across(contains("availability"), ~ . + water_cons_2025)) # add baseline water mining consumption

  # Column names

  cat("  Rows:", nrow(deposit_out), " | New columns:", ncol(cf_wide) - 1, "\n")

  # Save
  out_file <- file.path(output_dir, paste0("Deposit_", sc, "_", cm, ".csv"))
  fwrite(deposit_out, out_file)
  cat("  Saved:", basename(out_file), "\n")

  rm(cf_wide, deposit_out)
}

cat("\n=== DONE ===\n")

# EoF
