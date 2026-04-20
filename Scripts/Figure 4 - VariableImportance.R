# =============================================================================
# Sensitivity Analysis — Results Loading and ML Variable Importance
# Mining Supply Optimisation Model — Water Impact Drivers
#
# Purpose:
#   1. Load all Metrics CSVs from Results/Optimization/Samples
#   2. Reshape to one row per sample x epsilon level
#   3. Join with LHS sample inputs
#   4. Fit random forest models for variable importance analysis
#      Targets: water_impact, cost, slack_total
#      Features: raw draws (~40 cols) + aggregated mineral-level draws
#
# Author:  Pablo Busch
# Date:    2026
# =============================================================================

# LIBRARY -----------

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
source("Scripts/00a-Common Variables.R", encoding = "UTF-8")

library(randomForest)
library(vip) # variable importance plots
library(janitor) # clean_names()

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
  length(metric_files), # 6 per sample
  length(unique(dirname(metric_files)))
))

metrics_raw <- lapply(metric_files, function(fp) {
  # sample_id from parent folder name (e.g. "035")
  sample_id <- as.integer(basename(dirname(fp)))

  # scenario from grandparent folder (e.g. "Desalination_StrictFish")
  scenario_folder <- basename(dirname(dirname(fp)))
  desal <- if_else(grepl("^Desal", scenario_folder), "Desalination", "NoDesalination")
  fish <- if_else(grepl("StrictFish", scenario_folder), "StrictFish", "BaselineFish")

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
    mutate(sample_id = sample_id, epsilon = epsilon, desal = desal, fish = fish)
})

metrics_raw <- bind_rows(metrics_raw)
cat(sprintf("  Loaded %d rows from metrics files\n", nrow(metrics_raw)))
table(metrics_raw$desal, metrics_raw$fish)

# -----------------------------------------------------------------------------
# 3. RESHAPE TO WIDE FORMAT — one row per sample x epsilon --------------------
# -----------------------------------------------------------------------------

metrics_wide <- metrics_raw %>%
  pivot_wider(id_cols = c(sample_id, epsilon, desal, fish), names_from = Parameter, values_from = Value) %>%
  clean_names() %>% # standardise column names
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
# Lookup also stores the sample column name (quantile draw) and real-value column name.
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
# 5. EXPLORATORY PLOTS --------------------------------------------------------
# -----------------------------------------------------------------------------

library(GGally)
library(scales)

data_full <- data_full |> left_join(demand_per_sample) |> mutate(slack_percent = slack_total / mineral_demand)

## --- 5a. Scatter: water impact vs cost, colored by slack --------------------

p_scatter <- data_full %>%
  mutate(cost = cost / 1e3, water_impact = water_impact / 1e3) %>%
  ggplot(aes(y = cost, x = water_impact, fill = slack_percent)) +
  geom_point(alpha = 0.5, size = 1.2,shape= 21) +
  scale_fill_viridis_c(option = "magma", direction = -1, labels = scales::percent) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = dollar_format(big.mark = ",", prefix = "$")) +
  labs(
    subtitle = "Each point = one sample x epsilon",
    y = "Cost (billion USD)",
    x = "Water Impact (billion m³ world-eq)",
    fill = "% Demand unmet"
  ) +
  theme_pb_large()
p_scatter

# fmt: skip
ggsave("Figures/Sensitivity/scatter_water_cost.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)


p_scatter <- data_full %>%
  mutate(cost = cost / 1e3, water_impact = water_impact / 1e3) %>%
  ggplot(aes(y = cost, x = water_impact, fill = epsilon)) +
  geom_point(alpha = 0.5, size = 1.2,shape= 21) +
  scale_fill_viridis_c(option = "magma", direction = -1, labels = scales::percent) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = dollar_format(big.mark = ",", prefix = "$")) +
  labs(
    subtitle = "Each point = one sample x epsilon",
    y = "Cost (billion USD)",
    x = "Water Impact (billion m³ world-eq)",
    fill = "Cost increase tolerance (ε)"
  ) +
  theme_pb_large()
p_scatter

# fmt: skip
ggsave("Figures/Sensitivity/scatter_water_cost_eps.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)


## --- 5b. Histograms: distribution of key output variables -------------------
data_full %>%
  mutate(
    slack_copper = slack_copper / demand_Copper,
    slack_nickel = slack_nickel / demand_Nickel,
    slack_cobalt = slack_cobalt / demand_Cobalt,
    slack_lithium = slack_lithium / demand_Lithium,
    slack_total = slack_total / mineral_demand
  ) %>%
  select(sample_id, water_impact, cost, slack_total, slack_copper, slack_nickel, slack_cobalt, slack_lithium) %>%
  pivot_longer(-sample_id, names_to = "variable", values_to = "value") %>%
  mutate(
    variable = recode(
      variable,
      "water_impact" = "Water Impact (M m³ world-eq)",
      "cost" = "Cost (M USD)",
      "slack_total" = "Slack Total",
      "slack_copper" = "Slack Copper",
      "slack_nickel" = "Slack Nickel",
      "slack_cobalt" = "Slack Cobalt",
      "slack_lithium" = "Slack Lithium"
    )
  ) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.8) +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  scale_x_continuous(labels = label_comma()) +
  labs(title = "Distribution of Key Output Variables", x = NULL, y = "Count") +
  theme_pb_large() +
  theme(strip.background = element_rect(fill = "grey90"))

# fmt: skip
ggsave("Figures/Sensitivity/histograms.png",ggplot2::last_plot(),units = 'cm',dpi = 600,width = 8.7 * 2,height = 8.7 * 2
)


# -----------------------------------------------------------------------------
# 6. FEATURE ENGINEERING — aggregated mineral-level draws ---------------------
#
#   Raw draws: {param}_{Mineral}_{mine_type} (kept as-is)
#   Aggregated: mean across mine-type groups per mineral per param
#   e.g. opex_Lithium = mean(opex_Lithium_brine, opex_Lithium_brine_DLE, opex_Lithium_other_lithium)
# -----------------------------------------------------------------------------

cat("\nEngineering aggregated features...\n")

PARAMS <- c("opex", "capex_open", "capex_exp", "water")
MINERALS <- c("Copper", "Nickel", "Cobalt", "Lithium")

for (p in PARAMS) {
  for (m in MINERALS) {
    # Find all raw columns matching this param x mineral
    pattern <- paste0("^", p, "_", m)
    raw_cols <- grep(pattern, names(data_full), value = TRUE, ignore.case = TRUE)
    if (length(raw_cols) == 0) {
      next
    }

    # Mean across mine-type groups = aggregated feature
    agg_col <- paste0(p, "_", m, "_agg")
    data_full[[agg_col]] <- rowMeans(data_full[, raw_cols], na.rm = TRUE)
  }
}

# Identify raw vs aggregated feature columns
raw_feature_cols <- names(samples)[names(samples) != "sample_id"]
agg_feature_cols <- grep("_agg$", names(data_full), value = TRUE)

cat(sprintf("  Raw feature columns:        %d\n", length(raw_feature_cols)))
cat(sprintf("  Aggregated feature columns: %d\n", length(agg_feature_cols)))


# -----------------------------------------------------------------------------
# 6. SAVE JOINED DATASET ------------------------------------------------------
# -----------------------------------------------------------------------------

write_csv(data_full, paste0(PATH_OUTPUT, "sa_results_full.csv"))
cat(sprintf("\nFull dataset saved: %s\n", paste0(PATH_OUTPUT, "sa_results_full.csv")))


# -----------------------------------------------------------------------------
# 7. ML — RANDOM FOREST VARIABLE IMPORTANCE -----------------------------------
#
#   Three target variables: water_impact, cost, slack_total
#   Two feature sets per target:
#     a) raw    — all original sample draws
#     b) aggregated — mineral-level aggregates (fewer, more stable)
#   One model per target x feature set x epsilon level
# -----------------------------------------------------------------------------

## 7a Water Impact ---------------

# to billion
data_full <- data_full |> mutate(cost = cost / 1e3, water_impact = water_impact / 1e3)

data_full %>%
  filter(cost < 5000) %>%
  mutate(
    water_bin = cut(water_impact, breaks = c(0, 2000, 2500, 3000, 3500, 4000, 4500, 5000, Inf), include.lowest = TRUE)
  ) %>%
  count(water_bin) %>%
  print(n = Inf)


set.seed(25032026)

# Feature columns — only from samples, no data_full columns
feature_cols <- names(samples)[names(samples) != "sample_id"]
feature_cols <- c(feature_cols, c("epsilon", "desal", "fish")) # add categorical scenario features

# Bins
breaks <- c(0, 2000, 2500, 3000, 3500, 4000, 4500, 5000, Inf)
labels <- c("<2000", "2000-2500", "2500-3000", "3000-3500", "3500-4000", "4000-4500", "4500-5000", ">5000")
mean_vals <- c(2000, 2250, 2750, 3250, 3750, 4250, 4750, 5000)

# Note 2x2 design
table(data_full$desal, data_full$fish)

# Filter and bin
df_rf <- data_full %>%
  # filter(cost < 5000) %>%
  # filter(desal == "Desalination", fish == "BaselineFish") %>%
  mutate(water_bin = cut(water_impact, breaks = breaks, labels = labels, include.lowest = TRUE)) %>%
  drop_na(all_of(c(feature_cols, "water_impact", "water_bin")))

cat(sprintf("Rows after cost filter: %d\n", nrow(df_rf)))
print(count(df_rf, water_bin))


## 7a Full model no bins ----------------
rf_full <- randomForest(
  x = df_rf[, feature_cols],
  y = df_rf$water_impact,
  ntree = 100,
  importance = TRUE,
  na.action = na.omit
)
cat(sprintf("OOB R² = %.3f\n", tail(rf_full$rsq, 1)))

# Extract %IncMSE, floor negatives at 0, normalize to 100%
imp_full <- importance(rf_full, type = 1) %>%
  as.data.frame() %>%
  rownames_to_column("feature") %>%
  rename(importance = `%IncMSE`) %>%
  arrange(desc(importance)) |>
  mutate(
    importance = pmax(importance, 0),
    importance = importance / sum(importance) * 100,
    r_squared = tail(rf_full$rsq, 1),
    n = nrow(df_rf)
  )


## 7a-SHAP — SHAP values from full RF, decomposed across water impact range -----

library(fastshap)

# Compute approximate SHAP values via Monte Carlo (fastshap)
pfun <- function(object, newdata) predict(object, newdata = newdata)
shap_matrix <- fastshap::explain(rf_full, X = as.data.frame(df_rf[, feature_cols]), pred_wrapper = pfun, nsim = 10)

# Bind |SHAP| matrix with water impact; one row per observation
shap_df <- as_tibble(shap_matrix) %>%
  mutate(
    water_impact = df_rf$water_impact,
    water_bin = cut(water_impact, breaks = breaks, labels = labels, include.lowest = TRUE)
  )

# Within each bin: mean(|SHAP|) per feature, normalised to sum to 1
shap_bins <- shap_df %>%
  group_by(water_bin) %>%
  summarise(across(all_of(feature_cols), ~ mean(abs(.x), na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-water_bin, names_to = "feature", values_to = "mean_abs_shap") %>%
  group_by(water_bin) %>%
  mutate(importance = mean_abs_shap / sum(mean_abs_shap) * 100) %>%
  ungroup() %>%
  left_join(tibble(water_bin = labels, bin_center = mean_vals), by = "water_bin") %>%
  mutate(water_bin = factor(water_bin, levels = labels))

# Top 10 features by global mean |SHAP|; collapse remainder to "Other"
top_shap_features <- shap_bins %>%
  group_by(feature) %>%
  summarise(global_imp = mean(importance, na.rm = TRUE), .groups = "drop") %>%
  slice_max(global_imp, n = 10) %>%
  pull(feature)

df_shap_plot <- shap_bins %>%
  mutate(
    feature_plot = if_else(feature %in% top_shap_features, feature, "Other") %>%
      factor(levels = rev(c(top_shap_features, "Other")))
  ) %>%
  group_by(water_bin, bin_center, feature_plot) %>%
  summarise(importance = sum(importance), .groups = "drop")

df_shap_plot %>%
  ggplot(aes(x = bin_center, y = importance / 100, fill = feature_plot)) +
  geom_area(position = "fill", color = "black", linewidth = 0.15) +
  scale_fill_brewer(palette = "Paired", name = "Feature") +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_percent(), name = "Relative Feature Contribution") +
  labs(
    title = "SHAP Feature Importance across Water Impact Distribution",
    subtitle = "Mean |SHAP| per bin | Full RF model | Top 10 features shown",
    x = "Water Impact (billion m³ world-eq)"
  ) +
  theme_pb_large() +
  theme(legend.position = "right")

# fmt: skip
ggsave("Figures/Figure4-shap.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)


## b --- Fit one RF per bin ------------------------------------------------------
importance_bins <- lapply(labels, function(bin) {
  df_bin <- df_rf %>% filter(water_bin == bin)
  cat(sprintf("\n  Bin %s — %d rows\n", bin, nrow(df_bin)))

  if (nrow(df_bin) < 30) {
    cat("    Skipping — insufficient data\n")
    return(NULL)
  }

  # Drop zero-variance features within this bin
  # valid_cols <- feature_cols[sapply(df_bin[feature_cols], function(x) sd(x, na.rm = TRUE) > 0)]

  rf <- randomForest(
    # x = df_bin[, valid_cols],
    x = df_bin[, feature_cols],
    y = df_bin$water_impact,
    ntree = 100,
    importance = TRUE,
    na.action = na.omit
  )

  cat(sprintf("    OOB R² = %.3f\n", tail(rf$rsq, 1)))

  # Extract %IncMSE, floor negatives at 0, normalize to 100%
  imp <- importance(rf, type = 1) %>%
    as.data.frame() %>%
    rownames_to_column("feature") %>%
    rename(importance = `%IncMSE`) %>%
    mutate(
      importance = pmax(importance, 0),
      importance = importance / sum(importance) * 100,
      water_bin = bin,
      r_squared = tail(rf$rsq, 1),
      n = nrow(df_bin)
    )

  imp
})

importance_bins_df <- bind_rows(importance_bins)

# Stacked bar plot
# Keep top 10 features by mean importance across bins, collapse rest to "Other"
top_features <- importance_bins_df %>%
  group_by(feature) %>%
  summarise(mean_imp = mean(importance, na.rm = TRUE)) %>%
  slice_max(mean_imp, n = 10) %>%
  pull(feature) |>
  unique()

importance_bins_df |> group_by(water_bin) |> slice_max(importance)


df_plot <- importance_bins_df %>%
  mutate(
    feature_plot = if_else(feature %in% top_features, feature, "Other") |>
      factor(levels = rev(c(top_features, "Other"))),
    water_bin = factor(water_bin, levels = labels),
  ) %>%
  group_by(water_bin, feature_plot) %>%
  summarise(importance = sum(importance), .groups = "drop") |>
  left_join(tibble(water_bin = labels, water_bin_mean = mean_vals), by = "water_bin") |>
  mutate(water_bin = factor(water_bin, levels = labels))


df_plot |>
  filter(importance > 0) |>
  ggplot(aes(x = water_bin, y = importance, fill = feature_plot)) +
  geom_col(position = "stack",col="black",linewidth=0.2) +
  # geom_area() +
  scale_fill_brewer(palette = "Paired", name = "Feature") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Variable Importance per Water Impact Bin",
    subtitle = "One RF per bin | Cost < 5000 B USD | Top 10 features shown",
    x = "Water Impact Bin (Million m³ world-eq)",
    y = "% Importance (%IncMSE, sums to 100%)"
  ) +
  theme_pb_large() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    strip.background = element_rect(fill = "grey90")
  )


# fmt: skip
ggsave("Figures/Figure4-water.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)

df_area <- importance_bins_df

importance_bins_df |>
  filter(importance > 0) |>
  left_join(tibble(water_bin = labels, water_bin_mean = mean_vals), by = "water_bin") |>
  ggplot(aes(x = water_bin_mean, y = importance, fill = feature)) +
  geom_area(col="black",linewidth=0.2) +
  # geom_area() +
  scale_fill_brewer(palette = "Paired", name = "Feature") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Variable Importance per Water Impact Bin",
    subtitle = "One RF per bin | Cost < 5000 B USD | Top 10 features shown",
    x = "Water Impact Bin (Million m³ world-eq)",
    y = "% Importance (%IncMSE, sums to 100%)"
  ) +
  theme_pb_large() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    strip.background = element_rect(fill = "grey90")
  )


# fmt: skip
ggsave("Figures/Figure4-water.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7)


# =============================================================================
# 8. ML — LIGHTGBM VARIABLE IMPORTANCE ACROSS OUTPUT DISTRIBUTION -------------
#
#   Same bins as Section 7 (fixed breaks on water_impact, cost < 5000 filter).
#   For each bin, LightGBM explains within-bin variation using the same raw
#   sample features.  Gain-based importance is normalised to 100% per bin.
#   X-axis = bin mean value (mean_vals from Section 7).
# =============================================================================

library(lightgbm)
set.seed(25032026)

lgbm_features <- feature_cols # same raw draws as Section 7

df_lgbm <- data_full %>%
  # filter(cost < 5000, !is.na(water_impact)) %>%
  mutate(water_bin = cut(water_impact, breaks = breaks, labels = labels, include.lowest = TRUE)) %>%
  drop_na(all_of(c(lgbm_features, "water_impact", "water_bin")))

cat(sprintf("\nFitting LightGBM models for %d bins...\n", length(labels)))

lgbm_importance_bins <- lapply(seq_along(labels), function(b) {
  bin_label <- labels[b]
  df_bin <- df_lgbm %>% filter(water_bin == bin_label)

  if (nrow(df_bin) < 30) {
    cat(sprintf("  Bin %s — %d rows — skipped\n", bin_label, nrow(df_bin)))
    return(NULL)
  }

  # Drop zero-variance features within this bin
  valid_cols <- lgbm_features[sapply(df_bin[lgbm_features], function(x) sd(x, na.rm = TRUE) > 0)]

  dtrain <- lgb.Dataset(data = as.matrix(df_bin[, valid_cols]), label = df_bin$water_impact, free_raw_data = FALSE)

  model <- lgb.train(
    params = list(
      objective = "regression",
      metric = "rmse",
      num_leaves = 8,
      min_data_in_leaf = 3,
      learning_rate = 0.1,
      feature_fraction = 0.8,
      verbose = -1
    ),
    data = dtrain,
    nrounds = 100,
    verbose = -1
  )

  lgb.importance(model, percentage = FALSE) %>%
    as_tibble() %>%
    rename(feature = Feature, gain = Gain) %>%
    select(feature, gain) %>%
    mutate(
      importance = pmax(gain, 0),
      importance = importance / sum(importance) * 100,
      water_bin = bin_label,
      bin_center = mean_vals[b],
      n = nrow(df_bin)
    )
})

lgbm_importance_df <- bind_rows(lgbm_importance_bins)
cat(sprintf(
  "  Fitted %d bins, %d skipped\n",
  sum(!sapply(lgbm_importance_bins, is.null)),
  sum(sapply(lgbm_importance_bins, is.null))
))

# Top 10 features by mean importance; collapse rest to "Other"
top_lgbm_features <- lgbm_importance_df %>%
  group_by(feature) %>%
  summarise(mean_imp = mean(importance, na.rm = TRUE), .groups = "drop") %>%
  slice_max(mean_imp, n = 10) %>%
  pull(feature)

df_lgbm_plot <- lgbm_importance_df %>%
  mutate(
    feature_plot = if_else(feature %in% top_lgbm_features, feature, "Other") %>%
      factor(levels = rev(c(top_lgbm_features, "Other"))),
    water_bin = factor(water_bin, levels = labels)
  ) %>%
  group_by(water_bin, bin_center, feature_plot) %>%
  summarise(importance = sum(importance), .groups = "drop")

df_lgbm_plot %>%
  ggplot(aes(x = water_bin, y = importance, fill = feature_plot)) +
  geom_col(position = "stack", color = "black", linewidth = 0.2) +
  scale_fill_brewer(palette = "Paired", name = "Feature") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "LightGBM Variable Importance per Water Impact Bin",
    subtitle = "One GBM per bin | Cost < 5,000 B USD | Top 10 features shown",
    x = "Water Impact Bin (billion m³ world-eq)",
    y = "% Importance (Gain, sums to 100%)"
  ) +
  theme_pb_large() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")

# fmt: skip
ggsave("Figures/Figure4-lgbm.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)

# =============================================================================
# 8a. LIGHTGBM SHAP — global model, exact SHAP via predcontrib ----------------
#
#   One LightGBM model on all data (no binning).  Native exact SHAP values via
#   predict(..., predcontrib = TRUE) — no sampling, faster and more accurate
#   than fastshap.  SHAP matrix is then binned post-hoc by water_impact to show
#   how contributions shift across the output distribution.
# =============================================================================

df_shap_in <- data_full %>% filter(!is.na(water_impact)) %>% drop_na(all_of(c(lgbm_features, "water_impact")))

df_shap_in$desal <- ifelse(df_shap_in$desal == "Desalination", 1, 0)
df_shap_in$fish <- ifelse(df_shap_in$fish == "BaselineFish", 1, 0)

X_shap <- as.matrix(df_shap_in[, lgbm_features])

dtrain_global <- lgb.Dataset(X_shap, label = df_shap_in$water_impact, free_raw_data = FALSE)

lgbm_global <- lgb.train(
  params = list(
    objective = "regression",
    metric = "rmse",
    num_leaves = 31,
    min_data_in_leaf = 5,
    learning_rate = 0.05,
    num_iterations = 300,
    verbose = -1
  ),
  data = dtrain_global,
  verbose = -1
)

# predcontrib = TRUE returns a matrix (n_obs × n_features + 1); last col is bias — drop it
shap_mat <- predict(lgbm_global, X_shap, type = "contrib")
shap_mat <- shap_mat[, seq_len(ncol(shap_mat) - 1)] # drop last column
colnames(shap_mat) <- lgbm_features

# Bind with water_impact and assign bins
shap_df_lgbm <- as_tibble(shap_mat) %>%
  mutate(
    water_impact = df_shap_in$water_impact,
    water_bin = cut(water_impact, breaks = breaks, labels = labels, include.lowest = TRUE)
  )

# Within each bin: mean(|SHAP|) per feature, normalised to sum to 100%
shap_bins_lgbm <- shap_df_lgbm %>%
  group_by(water_bin) %>%
  summarise(across(all_of(lgbm_features), ~ mean(abs(.x), na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-water_bin, names_to = "feature", values_to = "mean_abs_shap") %>%
  group_by(water_bin) %>%
  mutate(importance = mean_abs_shap / sum(mean_abs_shap) * 100) %>%
  ungroup() %>%
  left_join(tibble(water_bin = labels, bin_center = mean_vals), by = "water_bin") %>%
  mutate(water_bin = factor(water_bin, levels = labels))

# Top 10 features by global mean |SHAP|; collapse remainder to "Other"
top_shap_lgbm <- shap_bins_lgbm %>%
  group_by(feature) %>%
  summarise(global_imp = mean(importance, na.rm = TRUE), .groups = "drop") %>%
  slice_max(global_imp, n = 10) %>%
  pull(feature)

shap_bins_lgbm %>%
  mutate(
    feature_plot = if_else(feature %in% top_shap_lgbm, feature, "Other") %>%
      factor(levels = rev(c(top_shap_lgbm, "Other")))
  ) %>%
  group_by(water_bin, bin_center, feature_plot) %>%
  summarise(importance = sum(importance), .groups = "drop") %>%
  ggplot(aes(x = bin_center, y = importance / 100, fill = feature_plot)) +
  geom_area(position = "fill", color = "black", linewidth = 0.15) +
  scale_fill_brewer(palette = "Paired", name = "Feature") +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_percent(), name = "Relative Feature Contribution") +
  coord_cartesian(expand = F, clip = "off") +
  labs(
    title = "LightGBM SHAP — Feature Contributions across Water Impact Distribution",
    subtitle = "Global model | Exact SHAP | Mean |SHAP| per bin, normalised to 100%",
    x = "Water Impact (billion m³ world-eq)"
  ) +
  theme_pb_large() +
  theme(legend.position = "right")

# fmt: skip
ggsave("Figures/Figure4-lgbm-shap.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)

# =============================================================================
# 9 DENSITY PLOTS — water impact by categorical drivers ------------------------------
# =============================================================================

data_full %>%
  mutate(cost = cost / 1e3, water_impact = water_impact / 1e3) %>%
  select(
    water_impact,
    cost,
    desal_cost,
    fish_threshold,
    recovery_Copper,
    water_Copper,
    water_Lithium_brine,
    water_Lithium_brine_DLE,
    mineral_demand
  ) %>%
  summarise(across(
    everything(),
    list(
      p10 = ~ quantile(., 0.10, na.rm = TRUE),
      p25 = ~ quantile(., 0.25, na.rm = TRUE),
      p50 = ~ quantile(., 0.50, na.rm = TRUE),
      p75 = ~ quantile(., 0.75, na.rm = TRUE),
      p90 = ~ quantile(., 0.90, na.rm = TRUE),
      min = ~ min(., na.rm = TRUE),
      max = ~ max(., na.rm = TRUE)
    )
  )) %>%
  pivot_longer(everything(), names_to = c("variable", "stat"), names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  print(n = Inf)

# --- Categorize variables ----------------------------------------------------
df_density <- data_full %>%
  mutate(cost = cost / 1e3, water_impact = water_impact / 1e3) %>%
  # filter(cost < 3000) |>
  mutate(
    epsilon_cat = case_when(
      epsilon == 0.0 ~ "Cost Optimal",
      epsilon == 0.01 ~ "1% Cost Increase",
      epsilon == 0.05 ~ "5% Cost Increase",
      epsilon == 0.10 ~ "10% Cost Increase",
      TRUE ~ "25% Cost Increase"
    ),
    cost_cat = case_when(cost < 3500 ~ "Cost <$3,500", cost > 4800 ~ "cost >$4,800", TRUE ~ "Cost $3,500-$4,800"),
    desal_cat = case_when(
      desal == "Desalination" & desal_cost < 0.8 ~ "Water Desalination < $0.8/m³",
      desal == "Desalination" & desal_cost >= 0.8 ~ "Water Desalination > $0.8/m³",
      desal == "NoDesalination" ~ "No desalination"
    ),
    fish_cat = case_when(
      fish == "StrictFish" ~ "All Basins",
      fish == "BaselineFish" & fish_threshold < 70 ~ "Fish Index <70",
      fish == "BaselineFish" & fish_threshold >= 70 & fish_threshold <= 90 ~ "Fish Index 70-90",
      fish == "BaselineFish" & fish_threshold > 90 ~ "Fish Index >90"
    ),
    recovery_cu_cat = case_when(
      recovery_Copper < 0.70 ~ "Copper recovery <70%",
      recovery_Copper > 0.85 ~ "Copper recovery >85%",
      TRUE ~ "Copper recovery 70-85%"
    ),
    recovery_co_cat = case_when(
      recovery_Cobalt < 0.70 ~ "Cobalt recovery <70%",
      recovery_Cobalt > 0.85 ~ "Cobalt recovery >85%",
      TRUE ~ "Cobalt recovery 70-85%"
    ),
    recovery_li_cat = case_when(
      recovery_Lithium < 0.70 ~ "Lithium recovery <70%",
      recovery_Lithium > 0.85 ~ "Lithium recovery >85%",
      TRUE ~ "Lithium recovery 70-85%"
    ),
    water_cu_cat = case_when(
      water_Copper < 0.25 ~ "Water Copper <p25",
      water_Copper > 0.75 ~ "Water Copper >p75",
      TRUE ~ "Water Copper p25-p75"
    ),
    water_li_brine_cat = case_when(
      water_Lithium_brine < 0.25 ~ "Water Li Brine <p25",
      water_Lithium_brine > 0.75 ~ "Water Li Brine >p75",
      TRUE ~ "Water Li Brine p25-p75"
    ),
    water_li_dle_cat = case_when(
      water_Lithium_brine_DLE < 0.25 ~ "Water Li DLE <p25",
      water_Lithium_brine_DLE > 0.75 ~ "Water Li DLE >p75",
      TRUE ~ "Water Li DLE p25-p75"
    ),
    mineral_demand_cat = case_when(
      mineral_demand < 1000 ~ "Demand <1,000 Mt",
      mineral_demand > 1100 ~ "Demand >1,100 Mt",
      TRUE ~ "Demand 1,000-1,100 Mt"
    ),
    share_lfp_cat = case_when(share_LFP < 0.7 ~ "LFP Share <70%", TRUE ~ "LFP Share >70%")
  ) %>%
  pivot_longer(
    cols = c(
      epsilon_cat,
      cost_cat,
      desal_cat,
      fish_cat,
      recovery_cu_cat,
      recovery_co_cat,
      recovery_li_cat,
      water_cu_cat,
      water_li_brine_cat,
      water_li_dle_cat,
      mineral_demand_cat,
      share_lfp_cat
    ),
    names_to = "driver",
    values_to = "category"
  ) %>%
  mutate(
    driver = recode(
      driver,
      "epsilon_cat" = "Cost Increase",
      "cost_cat" = "Total Cost",
      "desal_cat" = "Desalination Cost",
      "fish_cat" = "Fish Threshold",
      "recovery_cu_cat" = "Recovery Rate Copper",
      "recovery_co_cat" = "Recovery Rate Cobalt",
      "recovery_li_cat" = "Recovery Rate Lithium",
      "water_cu_cat" = "Water Intensity Copper",
      "water_li_brine_cat" = "Water Intensity Li Brine",
      "water_li_dle_cat" = "Water Intensity Li DLE",
      "mineral_demand_cat" = "Mineral Demand",
      "share_lfp_cat" = "LFP Share"
    ),
    # Order categories low -> mid -> high within each driver
    category = factor(
      category,
      levels = c(
        "Cost Optimal",
        "1% Cost Increase",
        "5% Cost Increase",
        "10% Cost Increase",
        "25% Cost Increase",
        "Cost <$3,500",
        "Cost $3,500-$4,800",
        "cost >$4,800",
        "Water Desalination < $0.8/m³",
        "Water Desalination > $0.8/m³",
        "No desalination",
        "Fish Index <70",
        "Fish Index 70-90",
        "Fish Index >90",
        "All Basins",
        "Copper recovery <70%",
        "Copper recovery 70-85%",
        "Copper recovery >85%",
        "Cobalt recovery <70%",
        "Cobalt recovery 70-85%",
        "Cobalt recovery >85%",
        "Lithium recovery <70%",
        "Lithium recovery 70-85%",
        "Lithium recovery >85%",
        "Water Copper <p25",
        "Water Copper p25-p75",
        "Water Copper >p75",
        "Water Li Brine <p25",
        "Water Li Brine p25-p75",
        "Water Li Brine >p75",
        "Water Li DLE <p25",
        "Water Li DLE p25-p75",
        "Water Li DLE >p75",
        "Demand <1,000 Mt",
        "Demand 1,000-1,100 Mt",
        "Demand >1,100 Mt",
        "LFP Share <70%",
        "LFP Share >70%"
      )
    )
  )


colors_cut <- c(
  "Cost Optimal" = "black",
  "1% Cost Increase" = "#90CAF9",
  "5% Cost Increase" = "#42A5F5",
  "10% Cost Increase" = "#0D47A1",
  "25% Cost Increase" = "#0B3D91",
  "Cost <$3,500" = "#90CAF9",
  "Cost $3,500-$4,800" = "#42A5F5",
  "cost >$4,800" = "#0D47A1",
  "Water Desalination < $0.8/m³" = "#A5D6A7",
  "Water Desalination > $0.8/m³" = "#1B5E20",
  "No desalination" = "#041310ff",
  "Fish Index <70" = "#FFCC80",
  "Fish Index 70-90" = "#FFA726",
  "Fish Index >90" = "#E65100",
  "All Basins" = "#880E00",
  "Copper recovery <70%" = "#F48FB1",
  "Copper recovery 70-85%" = "#E91E63",
  "Copper recovery >85%" = "#880E4F",
  "Cobalt recovery <70%" = "#B2EBF2",
  "Cobalt recovery 70-85%" = "#00838F",
  "Cobalt recovery >85%" = "#004D40",
  "Lithium recovery <70%" = "#D7CCC8",
  "Lithium recovery 70-85%" = "#6D4C41",
  "Lithium recovery >85%" = "#3E2723",
  "Water Copper <p25" = "#B3E5FC",
  "Water Copper p25-p75" = "#0288D1",
  "Water Copper >p75" = "#01579B",
  "Water Li Brine <p25" = "#E1BEE7",
  "Water Li Brine p25-p75" = "#8E24AA",
  "Water Li Brine >p75" = "#4A148C",
  "Water Li DLE <p25" = "#F8BBD0",
  "Water Li DLE p25-p75" = "#C2185B",
  "Water Li DLE >p75" = "#880E4F",
  "Demand <1,000 Mt" = "#DCEDC8",
  "Demand 1,000-1,100 Mt" = "#7CB342",
  "Demand >1,100 Mt" = "#33691E",
  "LFP Share <70%" = "#FFB300",
  "LFP Share >70%" = "#E65100"
)

names(df_density)
df_density |> group_by(driver) |> tally()

df_density2 <- df_density %>% filter(desal == "Desalination", fish == "StrictFish")

data_fig <- df_density |>
  mutate(slack_cobalt = slack_cobalt / demand_Cobalt) |>
  filter(
    # fmt: skip
    # driver %in% c("Fish Threshold", "Mineral Demand", "LFP Share", "Recovery Rate Copper", "Water Intensity Copper", "Recovery Rate Cobalt")
    # fmt: skip
    driver %in% c("Desalination Cost", "Mineral Demand", "Recovery Rate Copper", "Water Intensity Copper",  "Water Intensity Li Brine","Recovery Rate Lithium")
  )

ggplot(data_fig, aes(x = water_impact, color = category)) +
  # geom_density(aes(y = after_stat(count / sum(count))), alpha = 0.4, linewidth = 0.7) +
  geom_density(alpha = 0.4, linewidth = 0.7) +
  geom_text(
  data = data_fig %>%
    group_by(driver, category) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(driver) %>%
    mutate(nudge_y = (row_number() - 1) * 0.0001),
  # aes(x = 0.2-nudge_y*300, y = 0.0004 + nudge_y, label = category, color = category), # cobalt
  aes(x = 0.2-nudge_y*300, y = 0.0003 + nudge_y/2, label = category, color = category),
  hjust       = 0,
  size        = 2.8,
  show.legend = FALSE
) +
  # geom_histogram(alpha = 0.6, position = "identity", bins = 40) +
  # facet_wrap(~driver, scales = "free_y") +
  facet_grid(desal ~ driver) +
  scale_fill_manual(values = colors_cut) +
  scale_color_manual(values = colors_cut) +
  scale_x_continuous(labels = label_comma(), name = "Water Impact (billion m³ world-eq)") +
  # scale_x_continuous(labels = scales::percent, name = "% Unmet Cobalt Demand") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "% of runs", fill = NULL, color = NULL) +
  theme_pb_large() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    legend.position = "none",
    legend.text = element_text(size = 8)
  )

# fmt: skip
ggsave("Figures/Fig4-dens.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*3, height = 8.7*3)
# ggsave("Figures/Fig4-dens-cobalt.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*3, height = 8.7*3)

# =============================================================================
# 9A SINGLE DENSITY PLOTS — Water Impact, 2×2 grid ---------------------------
# =============================================================================

library(patchwork)

data_single <- data_full

X_LIM_WATER <- c(0, 9.5e3)

# Shared style layers applied to every panel
style_water <- list(
  scale_x_continuous(labels = label_comma(), name = "Water Impact (billion m³ world-eq)"),
  scale_y_continuous(labels = scales::percent),
  coord_cartesian(xlim = X_LIM_WATER, expand = FALSE, clip = "off"),
  labs(y = "% of simulations", fill = NULL, color = NULL),
  theme_pb_large(),
  theme(legend.position = "none")
)

## --- Panel 1: Mineral Demand -------------------------------------------------
data_p1 <- data_single |>
  filter(desal == "NoDesalination", fish == "StrictFish") |>
  mutate(
    cat = case_when(
      mineral_demand < 1000 ~ "Demand <1,000 Mt",
      mineral_demand > 1100 ~ "Demand >1,100 Mt",
      TRUE ~ "Demand 1,000-1,100 Mt"
    )
  )
lbl_p1 <- data_p1 |>
  group_by(cat) |>
  summarise(
    x = density(water_impact)$x[which.max(density(water_impact)$y)],
    y = max(density(water_impact)$y),
    .groups = "drop"
  )

p_demand <- ggplot(data_p1, aes(x = water_impact, color = cat)) +
  geom_density(linewidth = 0.7) +
  geom_text_repel(
    data = lbl_p1,
    aes(x = x, y = y, label = cat, color = cat),
    show.legend = FALSE,
    size = 2.8,
    nudge_y = 1e-5,
    seed = 1
  ) +
  scale_color_manual(values = colors_cut) +
  labs(title = "Avoid - Demand") +
  style_water
p_demand

## --- Panel 2: Cost Increase Tolerance ----------------------------------------
data_p2 <- data_single |>
  filter(desal == "NoDesalination", fish == "StrictFish") |>
  mutate(
    cat = case_when(
      epsilon == 0.00 ~ "Cost Optimal",
      epsilon == 0.01 ~ "1% Cost Increase",
      epsilon == 0.10 ~ "10% Cost Increase",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(cat))
lbl_p2 <- data_p2 |>
  group_by(cat) |>
  summarise(
    x = density(water_impact)$x[which.max(density(water_impact)$y)],
    y = max(density(water_impact)$y),
    .groups = "drop"
  )

p_epsilon <- ggplot(data_p2, aes(x = water_impact, color = cat)) +
  geom_density(linewidth = 0.7) +
  geom_text_repel(
    data = lbl_p2,
    aes(x = x, y = y, label = cat, color = cat),
    show.legend = FALSE,
    size = 2.8,
    nudge_y = 1e-5,
    seed = 1
  ) +
  scale_color_manual(values = colors_cut) +
  labs(title = "Shift - Increase cost to reduce water impact") +
  style_water
p_epsilon

## --- Panel 3: Desalination Availability --------------------------------------
data_p3 <- data_single |>
  filter(fish == "StrictFish") |>
  mutate(cat = case_when(desal == "Desalination" ~ "Desalination", TRUE ~ "No Desalination"))

colors_cut <- c(colors_cut, "Desalination" = "#1B5E20", "No Desalination" = "#041310ff")

lbl_p3 <- data_p3 |>
  group_by(cat) |>
  summarise(
    x = density(water_impact)$x[which.max(density(water_impact)$y)],
    y = max(density(water_impact)$y),
    .groups = "drop"
  )

p_desal <- ggplot(data_p3, aes(x = water_impact, color = cat)) +
  geom_density(linewidth = 0.7) +
  geom_text_repel(
    data = lbl_p3,
    aes(x = x, y = y, label = cat, color = cat),
    show.legend = FALSE,
    size = 2.8,
    nudge_y = 1e-5,
    seed = 1
  ) +
  scale_color_manual(values = colors_cut) +
  labs(title = "Improve - Desalination") +
  style_water
p_desal

## --- Panel 4: Copper Technology (recovery × water intensity) -----------------
cu_colors <- c("High recovery, Low water" = "#1B5E20", "Low recovery, High water" = "#B71C1C")

data_p4 <- data_single |>
  filter(desal == "NoDesalination", fish == "StrictFish") |>
  mutate(
    cat = case_when(
      recovery_Copper > 0.85 & water_Copper < 0.25 ~ "High recovery, Low water",
      recovery_Copper < 0.70 & water_Copper > 0.75 ~ "Low recovery, High water",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(cat))
lbl_p4 <- data_p4 |>
  group_by(cat) |>
  summarise(
    x = density(water_impact)$x[which.max(density(water_impact)$y)],
    y = max(density(water_impact)$y),
    .groups = "drop"
  )

p_copper <- ggplot(data_p4, aes(x = water_impact, color = cat)) +
  geom_density(linewidth = 0.7) +
  geom_text_repel(
    data = lbl_p4,
    aes(x = x, y = y, label = cat, color = cat),
    show.legend = FALSE,
    size = 2.8,
    nudge_y = 1e-5,
    seed = 1
  ) +
  scale_color_manual(values = cu_colors) +
  labs(title = "Improve - Copper mining process") +
  style_water
p_copper

## --- 2×2 grid ----------------------------------------------------------------
(p_demand | p_epsilon) / (p_desal | p_copper)

# fmt: skip
ggsave("Figures/Fig4-dens-2x2.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 17.4, height = 17.4)


# =============================================================================
# PARALLEL COORDINATES PLOT — water impact drivers ---------------------------

# =============================================================================
library(GGally)
library(scales)

# --- Prepare data ------------------------------------------------------------
# Select variables and categorize water impact for coloring
df_parallel <- data_full %>%
  # mutate(water_impact = slack_cobalt / demand_Cobalt) |> # slack cobalt
  filter(!is.na(water_impact)) %>%
  mutate(
    water_cat = case_when(water_impact < 2000 ~ "Low (<2,000)", water_impact > 4500 ~ "High (>4,500)", TRUE ~ "Mid"),
    water_cat = factor(water_cat, levels = c("Low (<2,000)", "Mid", "High (>4,500)"))
    # water_cat = case_when(water_impact < 0.02 ~ "Low (<2%)", water_impact > 0.2 ~ "High (>20%)", TRUE ~ "Mid"),
    # water_cat = factor(water_cat, levels = c("Low (<2%)", "Mid", "High (>20%)"))
  ) %>%
  select(water_cat, water_impact, cost, fish_threshold, mineral_demand, desal_cost, recovery_Copper, water_Copper) %>%
  drop_na()


axis_labels <- df_parallel %>%
  select(-water_cat) %>%
  summarise(across(everything(), list(min = min, max = max), na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = c("variable", "stat"), names_sep = "_(?=min$|max$)") %>%
  mutate(
    axis_num = match(
      variable,
      c("water_impact", "cost", "fish_threshold", "mineral_demand", "desal_cost", "recovery_Copper", "water_Copper")
    ),
    y_pos = if_else(stat == "max", 1.05, -0.05),
    label = formatC(value, format = "fg", digits = 3, flag = "#")
  )


# --- Plot --------------------------------------------------------------------
ggparcoord(
  df_parallel,
  columns = 2:ncol(df_parallel), # all except water_cat
  groupColumn = 1, # color by water_cat
  scale = "uniminmax", # scale each axis 0-1
  alphaLines = 0.15,
  splineFactor = 1 # smooth lines
) +
  geom_text(
    data    = axis_labels,
    aes(x = axis_num, y = y_pos, label = label),
    size = 6 * 5 / 14 * 0.8,
    color   = "grey40",
    inherit.aes = FALSE
  ) +
  scale_color_manual(
    values = c("Low (<2,000)" = "#1D9E75", "Mid" = "#B4B2A9", "High (>4,500)" = "#BA7517"),
    name = "Water impact"
  ) +
  # scale_color_manual(
  #   values = c("Low (<2%)" = "#1D9E75", "Mid" = "#B4B2A9", "High (>20%)" = "#BA7517"),
  #   name = "Unmet cobalt demand"
  # ) +
  scale_x_continuous(
    breaks = 1:7,
    labels = c(
      #"Water impact",
      "Unmet cobalt demand",
      "Total cost",
      "Fish index",
      "Mineral demand",
      "Desal cost",
      "Cu recovery",
      "Cu water intensity"
    )
  ) +
  labs(
    title = "Parallel coordinates — water impact drivers",
    subtitle = "Each line = one sample. Scaled 0–1 per axis. Color = water impact level.",
    x = NULL,
    y = "Relative value (min–max scaled)"
  ) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1.0), labels = c("Min", "P25", "Median", "P75", "Max")) +
  theme_pb_large() +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# fmt: skip
ggsave("Figures/Fig4-paralel-cobalt.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)

# =============================================================================
# 9B LIGHTGBM SHAP — Cobalt Slack ---------------------------------------------
#
#   Replicates Section 8a with slack_cobalt_pct as the target.
#   Exact SHAP via predcontrib = TRUE; binned post-hoc by cobalt slack level.
# =============================================================================

data_full <- data_full |> mutate(slack_cobalt_pct = slack_cobalt / demand_Cobalt)

range(data_full$slack_cobalt_pct, na.rm = TRUE)
cobalt_breaks <- c(0, 0.05, 0.10, 0.20, 0.40, 0.60, 1.01)
cobalt_labels <- c("<5%", "5-10%", "10-20%", "20-40%", "40-60%", ">60%")
cobalt_mean_vals <- c(2.5, 7.5, 15, 30, 50, 80) # midpoint in % for x-axis

df_shap_co <- data_full |> filter(!is.na(slack_cobalt_pct)) |> drop_na(all_of(c(lgbm_features, "slack_cobalt_pct")))

X_shap_co <- as.matrix(df_shap_co[, lgbm_features])

dtrain_co <- lgb.Dataset(X_shap_co, label = df_shap_co$slack_cobalt_pct, free_raw_data = FALSE)

lgbm_cobalt <- lgb.train(
  params = list(
    objective = "regression",
    metric = "rmse",
    num_leaves = 31,
    min_data_in_leaf = 5,
    learning_rate = 0.05,
    num_iterations = 300,
    verbose = -1
  ),
  data = dtrain_co,
  verbose = -1
)

shap_co_raw <- predict(lgbm_cobalt, X_shap_co, type = "contrib")
shap_co_mat <- shap_co_raw[, seq_len(ncol(shap_co_raw) - 1)]
colnames(shap_co_mat) <- lgbm_features

shap_df_co <- as_tibble(shap_co_mat) |>
  mutate(
    slack_cobalt_pct = df_shap_co$slack_cobalt_pct,
    cobalt_bin = cut(slack_cobalt_pct, breaks = cobalt_breaks, labels = cobalt_labels, include.lowest = TRUE)
  )

shap_bins_co <- shap_df_co |>
  group_by(cobalt_bin) |>
  summarise(across(all_of(lgbm_features), ~ mean(abs(.x), na.rm = TRUE)), .groups = "drop") |>
  pivot_longer(-cobalt_bin, names_to = "feature", values_to = "mean_abs_shap") |>
  group_by(cobalt_bin) |>
  mutate(importance = mean_abs_shap / sum(mean_abs_shap) * 100) |>
  ungroup() |>
  left_join(tibble(cobalt_bin = cobalt_labels, bin_center = cobalt_mean_vals), by = "cobalt_bin") |>
  mutate(cobalt_bin = factor(cobalt_bin, levels = cobalt_labels))

top_shap_co <- shap_bins_co |>
  group_by(feature) |>
  summarise(global_imp = mean(importance, na.rm = TRUE), .groups = "drop") |>
  slice_max(global_imp, n = 10) |>
  pull(feature)

shap_bins_co |>
  mutate(
    feature_plot = if_else(feature %in% top_shap_co, feature, "Other") |> factor(levels = rev(c(top_shap_co, "Other")))
  ) |>
  group_by(cobalt_bin, bin_center, feature_plot) |>
  summarise(importance = sum(importance), .groups = "drop") |>
  ggplot(aes(x = bin_center, y = importance / 100, fill = feature_plot)) +
  geom_area(position = "fill", color = "black", linewidth = 0.15) +
  scale_fill_brewer(palette = "Paired", name = "Feature") +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_y_continuous(labels = label_percent(), name = "Relative Feature Contribution") +
  labs(
    title = "LightGBM SHAP — Feature Contributions across Cobalt Slack Distribution",
    subtitle = "Global model | Exact SHAP | Mean |SHAP| per bin, normalised to 100%",
    x = "Unmet Cobalt Demand (%)"
  ) +
  theme_pb_large() +
  coord_cartesian(expand = F, clip = "off") +
  theme(legend.position = "right")

# fmt: skip
ggsave("Figures/Figure4-lgbm-shap-cobalt.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)

# =============================================================================
# 9B SINGLE DENSITY PLOTS — Cobalt Slack, 2×2 grid ---------------------------
# =============================================================================

X_LIM_COBALT <- c(0, 1)

style_cobalt <- list(
  scale_x_continuous(labels = scales::percent, name = "Unmet Cobalt Demand (%)"),
  scale_y_continuous(labels = scales::percent),
  coord_cartesian(xlim = X_LIM_COBALT, expand = FALSE, clip = "off"),
  labs(y = "% of simulations", fill = NULL, color = NULL),
  theme_pb_large(),
  theme(legend.position = "none")
)

## --- Panel 1: Mineral Demand -------------------------------------------------
data_co1 <- data_full |>
  filter(desal == "NoDesalination", fish == "StrictFish") |>
  mutate(
    cat = case_when(
      mineral_demand < 1000 & ni_co_ratio < 9 ~ "Low demand, Low Ni/Co",
      mineral_demand < 1000 & ni_co_ratio >= 9 ~ "Low demand, High Ni/Co",
      mineral_demand > 1100 & ni_co_ratio < 9 ~ "High demand, Low Ni/Co",
      mineral_demand > 1100 & ni_co_ratio >= 9 ~ "High demand, High Ni/Co",
      TRUE ~ NA_character_
    )
  )
# mutate(
#   cat = case_when(
#     mineral_demand < 1000 & share_LFP < 0.7 ~ "Low demand, Low LFP",
#     mineral_demand < 1000 & share_LFP >= 0.7 ~ "Low demand, High LFP",
#     mineral_demand > 1100 & share_LFP < 0.7 ~ "High demand, Low LFP",
#     mineral_demand > 1100 & share_LFP >= 0.7 ~ "High demand, High LFP",
#     TRUE ~ NA_character_
#   )
# )

lbl_co1 <- data_co1 |>
  group_by(cat) |>
  summarise(
    x = density(slack_cobalt_pct)$x[which.max(density(slack_cobalt_pct)$y)],
    y = max(density(slack_cobalt_pct)$y),
    .groups = "drop"
  )

pct_label <- data_co1 |>
  summarise(pct = mean(slack_cobalt_pct < 1, na.rm = TRUE)) |>
  mutate(label = paste0(round(pct * 100, 1), "% of simulations\nwith slack <1%"))

p_co_demand <- ggplot(data_co1, aes(x = slack_cobalt_pct, color = cat)) +
  geom_density(linewidth = 0.7) +
  geom_text_repel(
    data = lbl_co1,
    aes(x = x, y = y, label = cat, color = cat),
    show.legend = FALSE,
    size = 2.8,
    nudge_y = 0.01,
    seed = 1
  ) +
  scale_color_manual(
    values = c(
      "Low demand, Low Ni/Co" = "#2166ac",
      "Low demand, High Ni/Co" = "#92c5de",
      "High demand, Low Ni/Co" = "#d6604d",
      "High demand, High Ni/Co" = "#b2182b"
    )
  ) +
  labs(title = "Avoid - Demand") +
  style_cobalt
p_co_demand

## --- Panel 2: Fish index ----------------------------------------
data_co2 <- data_full |>
  filter(desal == "NoDesalination", fish == "BaselineFish") |>
  mutate(
    cat = case_when(
      fish == "StrictFish" ~ "All Basins",
      fish == "BaselineFish" & fish_threshold < 70 ~ "Fish Index <70",
      fish == "BaselineFish" & fish_threshold >= 70 & fish_threshold <= 90 ~ "Fish Index 70-90",
      fish == "BaselineFish" & fish_threshold > 90 ~ "Fish Index >90"
    )
  ) |>
  filter(!is.na(cat))
lbl_co2 <- data_co2 |>
  group_by(cat) |>
  summarise(
    x = density(slack_cobalt_pct)$x[which.max(density(slack_cobalt_pct)$y)],
    y = max(density(slack_cobalt_pct)$y),
    .groups = "drop"
  )

p_co_fish <- ggplot(data_co2, aes(x = slack_cobalt_pct, color = cat)) +
  geom_density(linewidth = 0.7) +
  geom_text_repel(
    data = lbl_co2,
    aes(x = x, y = y, label = cat, color = cat),
    show.legend = FALSE,
    size = 2.8,
    nudge_y = 0.01,
    seed = 1
  ) +
  scale_color_manual(values = colors_cut) +
  labs(title = "Shift - Fish basins") +
  style_cobalt
p_co_fish

## --- Panel 3: Recovery copper --------------------------------------
data_co3 <- data_full |>
  filter(desal == "NoDesalination", fish == "StrictFish") |>
  mutate(
    cat = case_when(
      recovery_Copper < 0.70 ~ "Copper recovery <70%",
      recovery_Copper > 0.85 ~ "Copper recovery >85%",
      TRUE ~ "Copper recovery 70-85%"
    )
  )
lbl_co3 <- data_co3 |>
  group_by(cat) |>
  summarise(
    x = density(slack_cobalt_pct)$x[which.max(density(slack_cobalt_pct)$y)],
    y = max(density(slack_cobalt_pct)$y),
    .groups = "drop"
  )

p_cu_recovery <- ggplot(data_co3, aes(x = slack_cobalt_pct, color = cat)) +
  geom_density(linewidth = 0.7) +
  geom_text_repel(
    data = lbl_co3,
    aes(x = x, y = y, label = cat, color = cat),
    show.legend = FALSE,
    size = 2.8,
    nudge_y = 0.01,
    seed = 1
  ) +
  scale_color_manual(values = colors_cut) +
  labs(title = "Improve - Copper Recovery Rate") +
  style_cobalt
p_cu_recovery

## --- Panel 4: Cobalt Recovery ------------------------------------------------
data_co4 <- data_full |>
  filter(desal == "NoDesalination", fish == "StrictFish") |>
  mutate(
    cat = case_when(
      recovery_Cobalt < 0.70 ~ "Cobalt recovery <70%",
      recovery_Cobalt > 0.85 ~ "Cobalt recovery >85%",
      TRUE ~ "Cobalt recovery 70-85%"
    )
  )
lbl_co4 <- data_co4 |>
  group_by(cat) |>
  summarise(
    x = density(slack_cobalt_pct)$x[which.max(density(slack_cobalt_pct)$y)],
    y = max(density(slack_cobalt_pct)$y),
    .groups = "drop"
  )

p_co_recovery <- ggplot(data_co4, aes(x = slack_cobalt_pct, color = cat)) +
  geom_density(linewidth = 0.7) +
  geom_text_repel(
    data = lbl_co4,
    aes(x = x, y = y, label = cat, color = cat),
    show.legend = FALSE,
    size = 2.8,
    nudge_y = 0.01,
    seed = 1
  ) +
  scale_color_manual(values = colors_cut) +
  labs(title = "Improve - Cobalt Recovery Rate") +
  style_cobalt
p_co_recovery

## --- 2×2 grid ----------------------------------------------------------------
(p_co_demand | p_co_fish) / (p_cu_recovery | p_co_recovery)

# fmt: skip
ggsave("Figures/Fig4-cobalt-2x2.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 17.4, height = 17.4)
