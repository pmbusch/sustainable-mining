# =============================================================================
# Figure 4 — Data Preparation: Load Metrics and Combine with Sample Inputs
#
# Purpose:
#   1. Load all Metrics CSVs from Results/Optimization/Samples
#   2. Reshape to one row per sample x epsilon level
#   3. Join with LHS sample inputs
#   4. Reconstruct deposit real values
#   5. Join demand per sample
#   6. Compute derived columns (billion-unit costs, cobalt slack %)
#   7. Save joined dataset to Results/SensitivityAnalysis/sa_results_full.csv
#
# Dependencies: Run Fig4_PrepareData_Demand.R first to generate
#               Parameters/demand_per_sample.csv
#
# Author:  Pablo Busch
# Date:    2026
# =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
source("Scripts/00a-Common Variables.R", encoding = "UTF-8")

library(janitor)

demand_per_sample <- read.csv("Parameters/demand_per_sample.csv")


# -----------------------------------------------------------------------------
# 1. CONFIGURATION ------------------------------------------------------------
# -----------------------------------------------------------------------------

PATH_RESULTS <- "Results/Optimization/Samples"
PATH_SAMPLES <- "Parameters/samples.csv"
PATH_OUTPUT <- "Results/SensitivityAnalysis/"

# Epsilon level lookup — filename prefix -> numeric epsilon
EPSILON_MAP <- c("Base" = 0.00, "Water_Eps01" = 0.01, "Water_Eps05" = 0.05, "Water_Eps10" = 0.10, "Water_Eps25" = 0.25)

# Parameters to extract from Metrics CSV
# fmt: skip
METRIC_PARAMS <- c("Cost","Water impact","Slack cost","Slack Copper","Slack Nickel","Slack Cobalt","Slack Lithium")

# -----------------------------------------------------------------------------
# 2. LOAD ALL METRICS FILES ---------------------------------------------------
# -----------------------------------------------------------------------------

cat("Loading metrics files...\n")

metric_files <- list.files(PATH_RESULTS, pattern = "_Metrics\\.csv$", recursive = TRUE, full.names = TRUE)

cat(sprintf(
  "  Found %d metrics files across %d sample folders\n",
  length(metric_files),
  length(unique(dirname(metric_files)))
))

metrics_raw <- lapply(metric_files, function(fp) {
  # sample_id from parent folder name (e.g. "0035")
  sample_id <- as.integer(basename(dirname(fp)))

  # epsilon from filename prefix
  file_prefix <- sub("_Metrics\\.csv$", "", basename(fp))
  epsilon <- EPSILON_MAP[file_prefix]

  if (is.na(epsilon)) {
    return(NULL)
  }

  df <- tryCatch(read_csv(fp, show_col_types = FALSE), error = function(e) NULL)
  if (is.null(df)) {
    return(NULL)
  }

  df %>%
    filter(Parameter %in% METRIC_PARAMS) %>%
    select(Parameter, Value) %>%
    mutate(sample_id = sample_id, epsilon = epsilon)
})

metrics_raw <- bind_rows(metrics_raw)
cat(sprintf("  Loaded %d rows from metrics files\n", nrow(metrics_raw)))

# -----------------------------------------------------------------------------
# 3. RESHAPE TO WIDE FORMAT — one row per sample x epsilon --------------------
# -----------------------------------------------------------------------------

metrics_wide <- metrics_raw %>%
  pivot_wider(id_cols = c(sample_id, epsilon), names_from = Parameter, values_from = Value) %>%
  clean_names() %>%
  rename(
    water_impact = water_impact,
    cost = cost,
    slack_cost = slack_cost,
    slack_copper = slack_copper,
    slack_nickel = slack_nickel,
    slack_cobalt = slack_cobalt,
    slack_lithium = slack_lithium
  ) %>%
  mutate(slack_total = slack_copper + slack_nickel + slack_cobalt + slack_lithium) %>%
  arrange(sample_id, epsilon)

cat(sprintf("  Reshaped to %d rows x %d columns\n", nrow(metrics_wide), ncol(metrics_wide)))


# -----------------------------------------------------------------------------
# 4. LOAD AND JOIN SAMPLE INPUTS ----------------------------------------------
# -----------------------------------------------------------------------------

cat("\nLoading sample inputs...\n")
samples <- read_csv(PATH_SAMPLES, show_col_types = FALSE)
cat(sprintf("  Samples loaded: %d rows x %d columns\n", nrow(samples), ncol(samples)))

# Join on sample_id
data_full <- metrics_wide %>% left_join(samples, by = "sample_id")

cat(sprintf("  Joined dataset: %d rows x %d columns\n", nrow(data_full), ncol(data_full)))

# Check for unmatched samples
n_unmatched <- sum(is.na(data_full$demand_level))
if (n_unmatched > 0) {
  warning(sprintf("  %d rows have no matching sample inputs", n_unmatched))
}

# -----------------------------------------------------------------------------
# 4b. RECONSTRUCT DEPOSIT REAL VALUES -----------------------------------------
#
#   Each [0,1] quantile draw q maps to a physical value via:
#     real_value_i = low_i + q * (high_i - low_i)   (per deposit row i)
#   Since this is linear in q, the resource-weighted mean reduces to two
#   scalars per group (intercept + slope), applied vectorised across all samples.
#
#   Groups: Cu/Ni/Co — one draw per mineral; Li — one draw per mine-type group.
#   Weight: resources_{Mineral} column in the deposit database.
#   Output: adds {param}_real_{mineral_group} columns to data_full, keeping
#           the original [0,1] quantile columns for ML features.
# -----------------------------------------------------------------------------

cat("\nReconstructing deposit real values...\n")

deposit_raw <- read_csv("Parameters/Deposit.csv", show_col_types = FALSE)

# Assign primary mineral per deposit — argmax of grade_resource_* (NA-safe)
grade_res_cols <- paste0("grade_resource_", c("Copper", "Nickel", "Cobalt", "Lithium"))
deposit_raw <- deposit_raw %>%
  rowwise() %>%
  mutate(primary_mineral = {
    vals <- c_across(all_of(grade_res_cols))
    if (all(is.na(vals))) {
      NA_character_
    } else {
      c("Copper", "Nickel", "Cobalt", "Lithium")[which.max(replace_na(vals, -Inf))]
    }
  }) %>%
  ungroup()

# Assign mine_type_group (matches Julia MINE_TYPE_MAP)
mine_type_map_r <- c(
  "Brine" = "brine",
  "Brine DLE" = "brine_DLE",
  "Clay" = "other_lithium",
  "Combined" = "other",
  "Hard Rock" = "other_lithium",
  "Open Pit" = "other",
  "Other" = "other",
  "Underground" = "other"
)
deposit_raw <- deposit_raw %>% mutate(mine_type_group = coalesce(mine_type_map_r[mine_type], "other"))

# Deposit params: sample column prefix => (low_col, high_col)
DEPOSIT_PARAMS_R <- list(
  water = list(low = "water_low", high = "water_high"),
  opex = list(low = "OPEX_ore_low", high = "OPEX_ore_high"),
  capex_open = list(low = "CAPEX_opening_low", high = "CAPEX_opening_high"),
  capex_exp = list(low = "CAPEX_exp_low", high = "CAPEX_exp_high")
)

# Build lookup: for each param × mineral_group, compute weighted intercept and slope.
real_lookup <- bind_rows(lapply(names(DEPOSIT_PARAMS_R), function(param_key) {
  low_col <- DEPOSIT_PARAMS_R[[param_key]]$low
  high_col <- DEPOSIT_PARAMS_R[[param_key]]$high
  if (!(low_col %in% names(deposit_raw)) || !(high_col %in% names(deposit_raw))) {
    warning(sprintf("Skipping %s — low/high columns not found", param_key))
    return(NULL)
  }

  bind_rows(
    # Cu, Ni, Co — one global draw per mineral
    lapply(c("Copper", "Nickel", "Cobalt"), function(m) {
      dep_m <- deposit_raw %>% filter(primary_mineral == m, !is.na(.data[[low_col]]), !is.na(.data[[high_col]]))
      w_col <- paste0("resources_", m)
      if (nrow(dep_m) == 0 || !(w_col %in% names(dep_m))) {
        return(NULL)
      }
      w <- replace_na(dep_m[[w_col]], 0)
      if (sum(w) == 0) {
        return(NULL)
      }
      tibble(
        sample_col = paste0(param_key, "_", m),
        real_col = paste0(param_key, "_real_", m),
        wm_intercept = weighted.mean(dep_m[[low_col]], w, na.rm = TRUE),
        wm_slope = weighted.mean(dep_m[[high_col]] - dep_m[[low_col]], w, na.rm = TRUE)
      )
    }),
    # Lithium — one draw per mine-type group
    lapply(c("brine", "brine_DLE", "other_lithium"), function(mt) {
      dep_li <- deposit_raw %>%
        filter(primary_mineral == "Lithium", mine_type_group == mt, !is.na(.data[[low_col]]), !is.na(.data[[high_col]]))
      if (nrow(dep_li) == 0) {
        return(NULL)
      }
      w <- replace_na(dep_li[["resources_Lithium"]], 0)
      if (sum(w) == 0) {
        return(NULL)
      }
      tibble(
        sample_col = paste0(param_key, "_Lithium_", mt),
        real_col = paste0(param_key, "_real_Lithium_", mt),
        wm_intercept = weighted.mean(dep_li[[low_col]], w, na.rm = TRUE),
        wm_slope = weighted.mean(dep_li[[high_col]] - dep_li[[low_col]], w, na.rm = TRUE)
      )
    })
  )
}))

# Apply lookup vectorised: add _real_ columns to samples then join to data_full
samples_real <- samples
for (i in seq_len(nrow(real_lookup))) {
  sc <- real_lookup$sample_col[i]
  rc <- real_lookup$real_col[i]
  if (sc %in% names(samples_real)) {
    samples_real[[rc]] <- real_lookup$wm_intercept[i] + samples_real[[sc]] * real_lookup$wm_slope[i]
  }
}

real_cols_added <- intersect(real_lookup$real_col, names(samples_real))
data_full <- data_full %>% left_join(samples_real %>% select(sample_id, all_of(real_cols_added)), by = "sample_id")

cat(sprintf("  Added %d real-value columns to data_full\n", length(real_cols_added)))
cat(sprintf("  Real-value columns: %s\n", paste(real_cols_added, collapse = ", ")))

# -----------------------------------------------------------------------------
# 4c. JOIN DEMAND PER SAMPLE --------------------------------------------------
# -----------------------------------------------------------------------------

data_full <- data_full |>
  left_join(demand_per_sample, by = "sample_id") |>
  mutate(slack_percent = slack_total / mineral_demand)

# -----------------------------------------------------------------------------
# 4d. COMPUTE DERIVED COLUMNS -------------------------------------------------
# -----------------------------------------------------------------------------

data_full <- data_full |>
  mutate(
    cost_B = cost / 1e3, # billion USD
    water_impact_B = water_impact / 1e3, # billion m³ world-eq
    slack_cobalt_pct = slack_cobalt / demand_Cobalt
  )

# -----------------------------------------------------------------------------
# SAVE JOINED DATASET ---------------------------------------------------------
# -----------------------------------------------------------------------------

write_csv(data_full, paste0(PATH_OUTPUT, "sa_results_full.csv"))
cat(sprintf("\nFull dataset saved: %s\n", paste0(PATH_OUTPUT, "sa_results_full.csv")))
