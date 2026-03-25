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

# Parse each file: extract sample_id and epsilon from path
metrics_raw <- lapply(metric_files, function(fp) {
  # sample_id from parent folder name (e.g. "035")
  sample_id <- as.integer(basename(dirname(fp)))

  # epsilon from filename prefix (e.g. "Water_Eps25_Metrics.csv" -> "Water_Eps25")
  file_prefix <- sub("_Metrics\\.csv$", "", basename(fp))
  epsilon <- EPSILON_MAP[file_prefix]

  if (is.na(epsilon)) {
    return(NULL)
  } # skip unrecognised files

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

# Bins
breaks <- c(0, 2000, 2500, 3000, 3500, 4000, 4500, 5000, Inf)
labels <- c("<2000", "2000-2500", "2500-3000", "3000-3500", "3500-4000", "4000-4500", "4500-5000", ">5000")
mean_vals <- c(2000, 2250, 2750, 3250, 3750, 4250, 4750, 5000)

# Filter and bin
df_rf <- data_full %>%
  filter(cost < 5000) %>%
  mutate(water_bin = cut(water_impact, breaks = breaks, labels = labels, include.lowest = TRUE)) %>%
  drop_na(all_of(c(feature_cols, "water_impact", "water_bin")))

cat(sprintf("Rows after cost filter: %d\n", nrow(df_rf)))
print(count(df_rf, water_bin))

# --- Fit one RF per bin ------------------------------------------------------
importance_bins <- lapply(labels, function(bin) {
  df_bin <- df_rf %>% filter(water_bin == bin)
  cat(sprintf("\n  Bin %s — %d rows\n", bin, nrow(df_bin)))

  if (nrow(df_bin) < 30) {
    cat("    Skipping — insufficient data\n")
    return(NULL)
  }

  # Drop zero-variance features within this bin
  valid_cols <- feature_cols[sapply(df_bin[feature_cols], function(x) sd(x, na.rm = TRUE) > 0)]

  rf <- randomForest(
    x = df_bin[, valid_cols],
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
  pull(feature)

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

# =============================================================================
# 9 DENSITY PLOTS — water impact by categorical drivers ------------------------------
# =============================================================================

data_full %>%
  select(
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
  filter(cost < 4000) |>
  mutate(
    cost_cat = case_when(cost < 3500 ~ "Cost <$3,500", cost > 4800 ~ "cost >$4,800", TRUE ~ "Cost $3,500-$4,800"),
    desal_cat = case_when(desal_cost < 0.8 ~ "Water Desalination < $0.8/m³", TRUE ~ "Water Desalination > $0.8/m³"),
    fish_cat = case_when(
      fish_threshold < 70 ~ "Fish Index <70",
      fish_threshold > 90 ~ "Fish Index >90",
      TRUE ~ "Fish Index 70-90"
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
        "Cost <$3,500",
        "Cost $3,500-$4,800",
        "cost >$4,800",
        "Water Desalination < $0.8/m³",
        "Water Desalination > $0.8/m³",
        "Fish Index <70",
        "Fish Index 70-90",
        "Fish Index >90",
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
  "Cost <$3,500" = "#90CAF9",
  "Cost $3,500-$4,800" = "#42A5F5",
  "cost >$4,800" = "#0D47A1",
  " Water Desalination < $0.8/m³" = "#A5D6A7",
  "Water Desalination > $0.8/m³" = "#1B5E20",
  "Fish Index <70" = "#FFCC80",
  "Fish Index 70-90" = "#FFA726",
  "Fish Index >90" = "#E65100",
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
data_fig <- df_density |>
  mutate(slack_cobalt = slack_cobalt / demand_Cobalt) |>
  filter(
    # fmt: skip
    # driver %in% c("Fish Threshold", "Mineral Demand", "LFP Share", "Recovery Rate Copper", "Water Intensity Copper", "Recovery Rate Cobalt")
    # fmt: skip
    driver %in% c("Fish Threshold", "Mineral Demand", "Recovery Rate Copper", "Water Intensity Copper",  "Water Intensity Li Brine","Recovery Rate Lithium")
  )

ggplot(data_fig, aes(x = water_impact, fill = category, color = category)) +
  geom_density(aes(y = after_stat(count / sum(count))), alpha = 0.4, linewidth = 0.7) +
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
  facet_wrap(~driver, scales = "free_y") +
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
