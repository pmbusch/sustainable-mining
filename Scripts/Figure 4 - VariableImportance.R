# =============================================================================
# Figure 4 — Variable Importance (LightGBM SHAP + Density Panels)
#
# Dependencies: Run Fig4_PrepareData_Results.R first to generate
#               Results/SensitivityAnalysis/sa_results_full.csv
#
# Panels:
#   Panel A — LightGBM SHAP feature contributions across water impact (8a)
#   Panel B — Single density plots: water impact drivers, 2×2 grid (9A)
#   SI      — LightGBM SHAP + density plots for cobalt slack (9B)
#
# Author:  Pablo Busch
# Date:    2026
# =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
source("Scripts/00a-Common Variables.R", encoding = "UTF-8")

library(lightgbm)
library(patchwork)

# -----------------------------------------------------------------------------
# LOAD PRECOMPUTED RESULTS ----------------------------------------------------
# -----------------------------------------------------------------------------

data_full <- read_csv("Results/SensitivityAnalysis/sa_results_full.csv", show_col_types = FALSE)

# Load samples for feature column names
samples <- read_csv("Parameters/samples.csv", show_col_types = FALSE)

set.seed(25032026)

# Feature columns: samples already includes desal and fish as 0/1 Bernoulli draws
feature_cols  <- names(samples)[names(samples) != "sample_id"]
feature_cols  <- c(feature_cols, "epsilon")
lgbm_features <- feature_cols

# Bin parameters — water impact in billion m³
breaks    <- c(0, 2000, 2500, 3000, 3500, 4000, 4500, 5000, Inf)
labels    <- c("<2000", "2000-2500", "2500-3000", "3000-3500", "3500-4000", "4000-4500", "4500-5000", ">5000")
mean_vals <- c(2000, 2250, 2750, 3250, 3750, 4250, 4750, 5000)

# Colors used in density panels (9A and 9B only)
colors_cut <- c(
  "Cost Optimal"             = "black",
  "1% Cost Increase"         = "#90CAF9",
  "10% Cost Increase"        = "#0D47A1",
  "Desalination"             = "#1B5E20",
  "No Desalination"          = "#041310ff",
  "Demand <1,000 Mt"         = "#DCEDC8",
  "Demand 1,000-1,100 Mt"    = "#7CB342",
  "Demand >1,100 Mt"         = "#33691E",
  "Fish Index <70"           = "#FFCC80",
  "Fish Index 70-90"         = "#FFA726",
  "Fish Index >90"           = "#E65100",
  "All Basins"               = "#880E00",
  "Copper recovery <70%"     = "#F48FB1",
  "Copper recovery 70-85%"   = "#E91E63",
  "Copper recovery >85%"     = "#880E4F",
  "Cobalt recovery <70%"     = "#B2EBF2",
  "Cobalt recovery 70-85%"   = "#00838F",
  "Cobalt recovery >85%"     = "#004D40"
)

# =============================================================================
# 8a. LIGHTGBM SHAP — global model, exact SHAP via predcontrib ----------------
#
#   One LightGBM model on all data (no binning).  Native exact SHAP values via
#   predict(..., predcontrib = TRUE) — no sampling, faster and more accurate
#   than fastshap.  SHAP matrix is then binned post-hoc by water_impact_B to show
#   how contributions shift across the output distribution.
# =============================================================================

df_shap_in <- data_full %>% filter(!is.na(water_impact_B)) %>% drop_na(all_of(c(lgbm_features, "water_impact_B")))

X_shap <- as.matrix(df_shap_in[, lgbm_features])

dtrain_global <- lgb.Dataset(X_shap, label = df_shap_in$water_impact_B, free_raw_data = FALSE)

lgbm_global <- lgb.train(
  params = list(
    objective    = "regression",
    metric       = "rmse",
    num_leaves   = 31,
    min_data_in_leaf = 5,
    learning_rate = 0.05,
    num_iterations = 300,
    verbose      = -1
  ),
  data    = dtrain_global,
  verbose = -1
)

# predcontrib = TRUE returns a matrix (n_obs × n_features + 1); last col is bias — drop it
shap_mat <- predict(lgbm_global, X_shap, type = "contrib")
shap_mat <- shap_mat[, seq_len(ncol(shap_mat) - 1)] # drop last column
colnames(shap_mat) <- lgbm_features

# Bind with water_impact_B and assign bins
shap_df_lgbm <- as_tibble(shap_mat) %>%
  mutate(
    water_impact_B = df_shap_in$water_impact_B,
    water_bin = cut(water_impact_B, breaks = breaks, labels = labels, include.lowest = TRUE)
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
  filter(desal == 0, fish == 1) |>
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
    x = density(water_impact_B)$x[which.max(density(water_impact_B)$y)],
    y = max(density(water_impact_B)$y),
    .groups = "drop"
  )

p_demand <- ggplot(data_p1, aes(x = water_impact_B, color = cat)) +
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
  filter(desal == 0, fish == 1) |>
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
    x = density(water_impact_B)$x[which.max(density(water_impact_B)$y)],
    y = max(density(water_impact_B)$y),
    .groups = "drop"
  )

p_epsilon <- ggplot(data_p2, aes(x = water_impact_B, color = cat)) +
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
  filter(fish == 1) |>
  mutate(cat = case_when(desal == 1 ~ "Desalination", TRUE ~ "No Desalination"))

colors_cut <- c(colors_cut, "Desalination" = "#1B5E20", "No Desalination" = "#041310ff")

lbl_p3 <- data_p3 |>
  group_by(cat) |>
  summarise(
    x = density(water_impact_B)$x[which.max(density(water_impact_B)$y)],
    y = max(density(water_impact_B)$y),
    .groups = "drop"
  )

p_desal <- ggplot(data_p3, aes(x = water_impact_B, color = cat)) +
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
  filter(desal == 0, fish == 1) |>
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
    x = density(water_impact_B)$x[which.max(density(water_impact_B)$y)],
    y = max(density(water_impact_B)$y),
    .groups = "drop"
  )

p_copper <- ggplot(data_p4, aes(x = water_impact_B, color = cat)) +
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
# FIGURE 4 SI — COBALT SLACK --------------------------------------------------
# =============================================================================

# =============================================================================
# 9B LIGHTGBM SHAP — Cobalt Slack ---------------------------------------------
#
#   Replicates Section 8a with slack_cobalt_pct as the target.
#   Exact SHAP via predcontrib = TRUE; binned post-hoc by cobalt slack level.
# =============================================================================

cobalt_breaks    <- c(0, 0.05, 0.10, 0.20, 0.40, 0.60, 1.01)
cobalt_labels    <- c("<5%", "5-10%", "10-20%", "20-40%", "40-60%", ">60%")
cobalt_mean_vals <- c(2.5, 7.5, 15, 30, 50, 80) # midpoint in % for x-axis

range(data_full$slack_cobalt_pct, na.rm = TRUE)

df_shap_co <- data_full |> filter(!is.na(slack_cobalt_pct)) |> drop_na(all_of(c(lgbm_features, "slack_cobalt_pct")))

X_shap_co <- as.matrix(df_shap_co[, lgbm_features])

dtrain_co <- lgb.Dataset(X_shap_co, label = df_shap_co$slack_cobalt_pct, free_raw_data = FALSE)

lgbm_cobalt <- lgb.train(
  params = list(
    objective    = "regression",
    metric       = "rmse",
    num_leaves   = 31,
    min_data_in_leaf = 5,
    learning_rate = 0.05,
    num_iterations = 300,
    verbose      = -1
  ),
  data    = dtrain_co,
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
  filter(desal == 0, fish == 1) |>
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
      "Low demand, Low Ni/Co"  = "#2166ac",
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
  filter(desal == 0, fish == 0) |>
  mutate(
    cat = case_when(
      fish_threshold < 70 ~ "Fish Index <70",
      fish_threshold >= 70 & fish_threshold <= 90 ~ "Fish Index 70-90",
      fish_threshold > 90 ~ "Fish Index >90"
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
  filter(desal == 0, fish == 1) |>
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
  filter(desal == 0, fish == 1) |>
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
