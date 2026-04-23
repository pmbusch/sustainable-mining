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
dim(data_full)

# Load samples for feature column names
samples <- read_csv("Parameters/samples.csv", show_col_types = FALSE)
dim(samples)

set.seed(25032026)

# Feature columns: samples already includes desal and fish as 0/1 Bernoulli draws
feature_cols <- names(samples)[names(samples) != "sample_id"]
feature_cols <- c(feature_cols, "epsilon")
lgbm_features <- feature_cols

# Bin parameters — water impact in billion m³
range(data_full$water_impact_B)
ggplot(data_full, aes(water_impact_B)) +
  geom_histogram(fill = "grey", col = "black", binwidth = 200) +
  scale_x_continuous(breaks = seq(0, 10000, 1000))

# fmt: skip
breaks <- c(0, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500, 6000, 6500, 7000, 7500, 8000,8500,9000, Inf)
# fmt: skip
labels <- c("<1000","1000-1500","1500-2000","2000-2500","2500-3000","3000-3500","3500-4000","4000-4500","4500-5000","5000-5500","5500-6000","6000-6500","6500-7000","7000-7500","7500-8000","8000-8500","8500-9000",">9000")
# fmt: skip
mean_vals <- c(1000, 1250, 1750, 2250, 2750, 3250, 3750, 4250, 4750, 5250, 5750, 6250, 6750, 7250, 7750,8250,8750, 9000)


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
top_shap_lgbm

water_display_names <- c(
  "Shift (cost increase)",
  "Water desalination available",
  "Ore copper water cons",
  "Copper recovery rate",
  "Mineral demand",
  "Max production rate copper",
  "Protect fish basins",
  "Copper extraction costs",
  "Lithium recovery rate",
  "Other"
)

water_colors <- c(
  "Shift (cost increase)" = "#2166AC", # dark blue   - economic policy
  "Water desalination available" = "#1B7A8A", # dark teal   - water technology
  "Ore copper water cons" = "#8C4E03", # dark brown  - ore/earth
  "Copper recovery rate" = "#D95F02", # dark orange - copper tech
  "Mineral demand" = "#762A83", # dark purple - demand
  "Max production rate copper" = "#B2182B", # dark red    - production constraint
  "Protect fish basins" = "#1A7837", # dark green  - ecology
  "Copper extraction costs" = "#A6761D", # dark amber  - costs
  "Lithium recovery rate" = "#cab2d6",
  "Other" = "#525252" # dark gray
)

plot_data_water <- shap_bins_lgbm %>%
  mutate(
    feature_plot = if_else(feature %in% top_shap_lgbm, feature, "Other"),
    display_name = case_when(
      feature_plot == "epsilon" ~ "Shift (cost increase)",
      feature_plot == "desal" ~ "Water desalination available",
      feature_plot == "water_Copper" ~ "Ore copper water cons",
      feature_plot == "recovery_Copper" ~ "Copper recovery rate",
      feature_plot == "demand_level" ~ "Mineral demand",
      feature_plot == "depletion_Copper" ~ "Max production rate copper",
      feature_plot %in% c("fish", "fish_threshold") ~ "Protect fish basins",
      feature_plot == "opex_Copper" ~ "Copper extraction costs",
      feature_plot == "recovery_Lithium" ~ "Lithium recovery rate",
      TRUE ~ feature_plot
    )
  ) %>%
  group_by(water_bin, bin_center, display_name) %>%
  summarise(importance = sum(importance), .groups = "drop") %>%
  mutate(display_name = factor(display_name, levels = rev(water_display_names)))

write_csv(plot_data_water, "Figures/Data_Figures/Fig4c8a_water_shap.csv")

label_data_water <- plot_data_water %>%
  filter(bin_center == 4750) %>%
  arrange(desc(display_name)) %>%
  mutate(
    norm_imp = importance / sum(importance),
    ymax = cumsum(norm_imp),
    ymin = lag(ymax, default = 0),
    ymid = (ymax + ymin) / 2
  )

p_vi <- ggplot(plot_data_water, aes(x = bin_center, y = importance / 100, fill = display_name)) +
  geom_area(position = "fill", color = "black", linewidth = 0.15) +
  geom_text(
    data = label_data_water,
    aes(x = 4750, y = ymid, label = display_name),
    hjust = 0, vjust = 0.5, size = 2.0, color = "white", fontface = "bold",
    show.legend = FALSE, lineheight = 0.85
  ) +
  # fmt: skip
  annotate("text",x = Inf, y = Inf,label = "a",hjust = 1.2, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "white") +
  scale_fill_manual(values = water_colors) +
  scale_x_continuous(labels = label_comma(), breaks = c(0, 2, 4, 6, 8) * 1e3) +
  scale_y_continuous(labels = label_percent(), name = "Relative importance") +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(title = "Variable importance on water footprint ", x = "Water Scarcity Footprint 2025-2050 (billion m³-eq)") +
  theme_pb_large() +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold", colour = "#222222", hjust = 0.5))
p_vi
# fmt: skip
ggsave("Figures/Test_FigurePanels/Figure4-lgbm-shap.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 9, height = 8.7)

# =============================================================================
# 9A SINGLE DENSITY PLOTS — Water Impact, 2×2 grid ---------------------------
# =============================================================================

library(patchwork)

data_single <- data_full

range(data_single$water_impact_B)
X_LIM_WATER <- c(0, 9e3)

scaling <- 250 # count plot, as X axis range is 9K
clipping <- 11500 # clips extended line range

# Shared style layers applied to every panel
style_water <- list(
  scale_x_continuous(labels = label_comma(), breaks = c(0, 2, 4, 6, 8) * 1e3),
  scale_y_continuous(labels = label_comma()),
  coord_cartesian(xlim = X_LIM_WATER, expand = FALSE, clip = "off"),
  labs(y = "No. of simulations (n = 10,000)", fill = NULL, color = NULL),
  theme_pb_large(),
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10, face = "bold", colour = "#222222", hjust = 0.5),
  )
)

## --- Panel 1: Mineral Demand -------------------------------------------------
skimr::skim(data_single$mineral_demand)
quantile(data_single$mineral_demand, probs = c(0.05, 0.333, 0.5, 0.666, 0.95))
quantile(data_single$mineral_demand, probs = c(0.25, 0.5, 0.75))
# split at median
data_p1 <- data_single |>
  mutate(
    cat = case_when(
      mineral_demand < 1050 ~ "Demand <1,050 Mt",
      mineral_demand >= 1050 ~ "Demand >1,050 Mt",
      TRUE ~ "NA"
    )
  )
write_csv(data_p1, "Figures/Data_Figures/Fig4c9a1_dens_demand.csv")

colors_cut1 <- c("Demand <1,050 Mt" = "#6a51a3", "Demand >1,050 Mt" = "#3f007d")

lbl_p1 <- data_p1 |>
  group_by(cat) |>
  summarise(
    x = density(water_impact_B)$x[which.max(density(water_impact_B)$y)],
    y = max(density(water_impact_B)$y) * n() * scaling,
    .groups = "drop"
  )

data_p1 <- data_p1 |>
  group_by(cat) |>
  group_modify(
    ~ {
      d <- density(.x$water_impact_B)
      data.frame(x = d$x, y = d$y * scaling * nrow(.x))
    }
  ) |>
  filter(x <= clipping)

p_demand <- ggplot(data_p1, aes(x = x, y = y, color = cat)) +
  # geom_density(aes(y = after_stat(count * scaling)), linewidth = 1) +
  geom_line(linewidth = 1) +
  geom_text_repel(
    data = lbl_p1,
    aes(x = x, y = y, label = cat, color = cat),
    show.legend = FALSE,
    size = 2.8,
    nudge_y = 150,
    nudge_x = c(-500, 1500),
    seed = 1
  ) +
  # fmt: skip
  annotate("text",x = Inf, y = Inf,label = "b",hjust = 1.2, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_color_manual(values = colors_cut1) +
  labs(title = "Avoid - Demand", x = "Water Footprint") +
  style_water
p_demand

## --- Panel 2: Cost Increase Tolerance ----------------------------------------
data_p2 <- data_single |>
  mutate(
    cat = case_when(
      epsilon == 0.00 ~ "Cost Optimal",
      epsilon == 0.01 ~ "1% Cost Increase",
      epsilon == 0.10 ~ "10% Cost Increase",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(cat))
write_csv(data_p2, "Figures/Data_Figures/Fig4c9a2_dens_epsilon.csv")

colors_cut <- c("Cost Optimal" = "#08306b", "1% Cost Increase" = "#045a8d", "10% Cost Increase" = "#3690c0")

lbl_p2 <- data_p2 |>
  group_by(cat) |>
  summarise(
    x = density(water_impact_B)$x[which.max(density(water_impact_B)$y)],
    y = max(density(water_impact_B)$y) * n() * scaling,
    .groups = "drop"
  )

# calculated density automatically, to extend clipping to certain range only
data_p2 <- data_p2 |>
  group_by(cat) |>
  group_modify(
    ~ {
      d <- density(.x$water_impact_B)
      data.frame(x = d$x, y = d$y * scaling * nrow(.x))
    }
  ) |>
  filter(x <= clipping)

p_epsilon <- ggplot(data_p2, aes(x = x, y = y, color = cat)) +
  # geom_density(linewidth = 1, aes(y = after_stat(count) * scaling)) +
  geom_line(linewidth = 1) +
  geom_text_repel(
    data = lbl_p2,
    aes(x = x, y = y, label = cat, color = cat),
    show.legend = FALSE,
    size = 2.8,
    nudge_y = c(30, 75, 30),
    seed = 1
  ) +
  # fmt: skip
  annotate("text",x = Inf, y = Inf,label = "c",hjust = 1.2, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_color_manual(values = colors_cut) +
  labs(title = "Shift - Increase cost\nto reduce water impact", x = "") +
  style_water
p_epsilon

## --- Panel 3: Desalination Availability --------------------------------------
data_p3 <- data_single |> mutate(cat = case_when(desal == 1 ~ "Desalination", TRUE ~ "No Desalination"))
write_csv(data_p3, "Figures/Data_Figures/Fig4c9a3_dens_desal.csv")

colors_cut <- c(colors_cut, "Desalination" = "#1B7A8A", "No Desalination" = "#041310")

lbl_p3 <- data_p3 |>
  group_by(cat) |>
  summarise(
    x = density(water_impact_B)$x[which.max(density(water_impact_B)$y)],
    y = max(density(water_impact_B)$y) * n() * scaling,
    .groups = "drop"
  )

data_p3 <- data_p3 |>
  group_by(cat) |>
  group_modify(
    ~ {
      d <- density(.x$water_impact_B)
      data.frame(x = d$x, y = d$y * scaling * nrow(.x))
    }
  ) |>
  filter(x <= clipping)

p_desal <- ggplot(data_p3, aes(x = x, y = y, color = cat)) +
  # geom_density(linewidth = 1, aes(y = after_stat(count) * scaling)) +
  geom_line(linewidth = 1) +
  geom_text_repel(
    data = lbl_p3,
    aes(x = x, y = y, label = cat, color = cat),
    show.legend = FALSE,
    size = 2.8,
    nudge_y = c(200, 80),
    nudge_x = c(0, 3000),
    seed = 1
  ) +
  # fmt: skip
  annotate("text",x = Inf, y = Inf,label = "d",hjust = 1.2, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_color_manual(values = colors_cut) +
  labs(title = "Improve - Desalination", x = "Water Scarcity Footprint 2025-2050 (billion m³ world-eq)") +
  style_water
p_desal

## --- Panel 4: Copper Technology (recovery × water intensity) -----------------

data_p4 <- data_single |>
  mutate(
    cat = case_when(
      recovery_Copper > 0.85 & water_Copper < 0.25 ~ "High recovery, Low water",
      recovery_Copper < 0.70 & water_Copper > 0.75 ~ "Low recovery,\nHigh water",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(cat))
write_csv(data_p4, "Figures/Data_Figures/Fig4c9a4_dens_copper.csv")
lbl_p4 <- data_p4 |>
  group_by(cat) |>
  summarise(
    x = density(water_impact_B)$x[which.max(density(water_impact_B)$y)],
    y = max(density(water_impact_B)$y) * n() * scaling,
    .groups = "drop"
  )

data_p4 <- data_p4 |>
  group_by(cat) |>
  group_modify(
    ~ {
      d <- density(.x$water_impact_B)
      data.frame(x = d$x, y = d$y * scaling * nrow(.x))
    }
  ) |>
  filter(x <= clipping)

cu_colors <- c("High recovery, Low water" = "#7f2704", "Low recovery,\nHigh water" = "#f16913")
p_copper <- ggplot(data_p4, aes(x = x, y = y, color = cat)) +
  # geom_density(linewidth = 1, aes(y = after_stat(count) * scaling)) +
  geom_line(linewidth = 1) +
  geom_text_repel(
    data = lbl_p4,
    aes(x = x, y = y, label = cat, color = cat),
    show.legend = FALSE,
    size = 2.8,
    nudge_y = c(25, 25),
    nudge_x = c(0, 500),
    seed = 1
  ) +
  # fmt: skip
  annotate("text",x = Inf, y = Inf,label = "e",hjust = 1.2, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_color_manual(values = cu_colors) +
  labs(title = "Improve - Copper\nmining process", x = "") +
  style_water
p_copper

## --- 2×2 grid ----------------------------------------------------------------
(p_demand | p_epsilon) / (p_desal | p_copper)

# fmt: skip
ggsave("Figures/Test_FigurePanels/Fig4-dens-2x2.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18, height = 17.4)

# MERGE FIGURE ---------------------
design <- "
AAB
CDE
"
(p_vi + p_demand + p_epsilon + p_desal + p_copper) + plot_layout(design = design)
ggsave("Figures/Figure4.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18, height = 17.4)
ggsave("Figures/Figure4.svg", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18, height = 17.4)


# =============================================================================
# FIGURE 4 SI — COBALT SLACK --------------------------------------------------
# =============================================================================

# =============================================================================
# 9B LIGHTGBM SHAP — Cobalt Slack ---------------------------------------------
#
#   Replicates Section 8a with slack_cobalt_pct as the target.
#   Exact SHAP via predcontrib = TRUE; binned post-hoc by cobalt slack level.
# =============================================================================

range(data_full$slack_cobalt_pct)

data_full |>
  filter(slack_cobalt_pct > 0.01) |>
  ggplot(aes(slack_cobalt_pct)) +
  geom_histogram(fill = "grey", col = "black", binwidth = 0.005)
# scale_x_continuous(breaks = seq(0, 10000, 1000))

# fmt: skip
cobalt_breaks <- c(0.01, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90)
# fmt: skip
cobalt_labels <- c("<5%", "5-10%", "10-15%", "15-20%", "20-25%", "25-30%", "30-35%", "35-40%", "40-45%", "45-50%", "50-55%", "55-60%", "60-65%", "65-70%", "70-75%", "75-80%", "80-85%", "85-90%")
# fmt: skip
cobalt_mean_vals <- c(1, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 52.5, 57.5, 62.5, 67.5, 72.5, 77.5, 82.5, 87.5)

df_shap_co <- data_full |>
  filter(!is.na(slack_cobalt_pct)) |>
  drop_na(all_of(c(lgbm_features, "slack_cobalt_pct"))) |>
  filter(slack_cobalt_pct > 0.01)

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

top_shap_co

cobalt_display_names <- c(
  "Protect fish basins",
  "Water desalination available",
  "Cobalt recovery",
  "Nickel/cobalt ratio in batteries for electric vehicles",
  "Mineral demand",
  "Max production rate copper",
  "Nickel recovery",
  "Share LFP battery chemistry",
  "Max production rate nickel",
  "Other"
)

cobalt_colors <- c(
  "Protect fish basins" = "#1A7837", # dark green   - ecology
  "Nickel/cobalt ratio in batteries for electric vehicles" = "#023858", # dark navy    - battery/EV
  "Water desalination available" = "#1B7A8A", # dark teal   - water technology
  "Cobalt recovery" = "#08519C", # cobalt blue  - cobalt tech
  "Max production rate copper" = "#B2182B", # dark red     - Cu constraint
  "Mineral demand" = "#762A83", # dark purple  - demand
  "Nickel recovery" = "#02818A", # dark teal    - nickel tech
  "Max production rate nickel" = "#D95F02", # dark orange  - Ni constraint
  "Share LFP battery chemistry" = "#4A4094", # dark violet  - battery chem
  "Other" = "#525252" # dark gray
)

plot_data_co <- shap_bins_co |>
  mutate(
    feature_plot = if_else(feature %in% top_shap_co, feature, "Other"),
    display_name = case_when(
      feature_plot %in% c("fish_threshold", "fish") ~ "Protect fish basins",
      feature_plot == "ni_co_ratio" ~ "Nickel/cobalt ratio in batteries for electric vehicles",
      feature_plot == "desal" ~ "Water desalination available",
      feature_plot == "recovery_Cobalt" ~ "Cobalt recovery",
      feature_plot == "demand_level" ~ "Mineral demand",
      feature_plot == "depletion_Copper" ~ "Max production rate copper",
      feature_plot == "recovery_Nickel" ~ "Nickel recovery",
      feature_plot == "depletion_Nickel" ~ "Max production rate nickel",
      feature_plot == "share_LFP" ~ "Share LFP battery chemistry",
      TRUE ~ feature_plot
    )
  ) |>
  group_by(cobalt_bin, bin_center, display_name) |>
  summarise(importance = sum(importance), .groups = "drop") |>
  mutate(display_name = factor(display_name, levels = rev(cobalt_display_names)))

write_csv(plot_data_co, "Figures/Data_Figures/Fig4c9b_cobalt_shap.csv")

label_data_co <- plot_data_co |>
  filter(bin_center == 7.5) |>
  arrange(desc(display_name)) |>
  mutate(
    norm_imp = importance / sum(importance),
    ymax = cumsum(norm_imp),
    ymin = lag(ymax, default = 0),
    ymid = (ymax + ymin) / 2
  )

p_vi_co <- ggplot(plot_data_co, aes(x = bin_center, y = importance / 100, fill = display_name)) +
  geom_area(position = "fill", color = "black", linewidth = 0.15) +
  geom_text(
    data = label_data_co,
    aes(x = 7.5, y = ymid, label = display_name),
    hjust = 0, vjust = 0.5, size = 2.0, color = "white", fontface = "bold",
    show.legend = FALSE, lineheight = 0.85
  ) +
  # fmt: skip
  annotate("text",x = Inf, y = Inf,label = "a",hjust = 1.2, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "white") +
  scale_fill_manual(values = cobalt_colors) +
  scale_x_continuous(labels = label_percent(scale = 1), breaks = c(1, 20, 40, 60, 80)) +
  scale_y_continuous(labels = label_percent(), name = "Relative importance") +
  labs(title = "Variable importance on unmet cobalt demand", x = "Unmet cobalt demand (%)") +
  theme_pb_large() +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold", colour = "#222222", hjust = 0.5))
p_vi_co

# fmt: skip
ggsave("Figures/Test_FigurePanels/Figure4-lgbm-shap-cobalt.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 9, height = 8.7)

# =============================================================================
# 9B SINGLE DENSITY PLOTS — Cobalt Slack, 2×2 grid ---------------------------
# =============================================================================

range(data_full$slack_cobalt_pct)
X_LIM_COBALT <- c(0, 0.8)
data_cobalt <- data_full |> filter(slack_cobalt_pct > 0.01) # at least higher than 1%

style_cobalt <- list(
  scale_x_continuous(labels = scales::percent, name = "Unmet Cobalt Demand (%)", breaks = c(0.01, 0.2, 0.4, 0.6, 0.8)),
  scale_y_continuous(labels = label_comma()),
  coord_cartesian(xlim = X_LIM_COBALT, expand = FALSE, clip = "off"),
  labs(y = "No of simulations (n=10,000)", fill = NULL, color = NULL),
  theme_pb_large(),
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10, face = "bold", colour = "#222222", hjust = 0.5),
  )
)

# same scaling as figure main
scaling <- 1 / (12000 / 250 / 0.9) * nrow(data_cobalt) / nrow(data_full)

## --- Panel 1: Mineral Demand -------------------------------------------------
data_co1 <- data_cobalt |>
  mutate(
    cat = case_when(
      mineral_demand < 1050 & ni_co_ratio < 9 ~ "Low demand, Low Ni/Co",
      mineral_demand < 1050 & ni_co_ratio >= 9 ~ "Low demand, High Ni/Co",
      mineral_demand >= 1050 & ni_co_ratio < 9 ~ "High demand, Low Ni/Co",
      mineral_demand >= 1050 & ni_co_ratio >= 9 ~ "High demand, High Ni/Co",
      TRUE ~ NA_character_
    )
  )
write_csv(data_co1, "Figures/Data_Figures/Fig4c9b1_cobalt_demand.csv")
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
    y = max(density(slack_cobalt_pct)$y) * n() * scaling,
    .groups = "drop"
  )

pct_label <- data_co1 |>
  summarise(pct = mean(slack_cobalt_pct < 1, na.rm = TRUE)) |>
  mutate(label = paste0(round(pct * 100, 1), "% of simulations\nwith slack <1%"))

p_co_demand <- ggplot(data_co1, aes(x = slack_cobalt_pct, color = cat)) +
  geom_density(linewidth = 1, aes(y = after_stat(count) * scaling)) +
  geom_text_repel(
    data = lbl_co1,
    aes(x = x, y = y, label = cat, color = cat),
    show.legend = FALSE,
    size = 2.8,
    nudge_y = 0.01,
    seed = 1
  ) +
  # fmt: skip
  annotate("text",x = Inf, y = Inf,label = "b",hjust = 1.2, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
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
data_co2 <- data_cobalt |>
  filter(fish == 0) |> # swtich to protect on
  mutate(
    cat = case_when(
      fish_threshold < 70 ~ "Protect Fish >70",
      fish_threshold < 80 ~ "Protect Fish >80",
      fish_threshold < 90 ~ "Protect Fish >90"
    )
  ) |>
  filter(!is.na(cat))
write_csv(data_co2, "Figures/Data_Figures/Fig4c9b2_cobalt_fish.csv")
lbl_co2 <- data_co2 |>
  group_by(cat) |>
  summarise(
    x = density(slack_cobalt_pct)$x[which.max(density(slack_cobalt_pct)$y)],
    y = max(density(slack_cobalt_pct)$y) * n() * scaling,
    .groups = "drop"
  )

colors_cut <- c("Protect Fish >70" = "#244422FF", "Protect Fish >80" = "#3B7D31FF", "Protect Fish >90" = "#88AB38FF")
p_co_fish <- ggplot(data_co2, aes(x = slack_cobalt_pct, color = cat)) +
  geom_density(linewidth = 1, aes(y = after_stat(count) * scaling)) +
  geom_text_repel(
    data = lbl_co2,
    aes(x = x, y = y, label = cat, color = cat),
    show.legend = FALSE,
    size = 2.8,
    nudge_y = 0.01,
    seed = 1
  ) +
  # fmt: skip
  annotate("text",x = Inf, y = Inf,label = "c",hjust = 1.2, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_color_manual(values = colors_cut) +
  labs(title = "Shift - Protect fish basins") +
  style_cobalt
p_co_fish

## --- Panel 3: Desalination  --------------------------------------
data_co3 <- data_cobalt |> mutate(cat = case_when(desal == 1 ~ "Desalination", TRUE ~ "No Desalination"))
write_csv(data_co3, "Figures/Data_Figures/Fig4c9b3_cobalt_desal.csv")
lbl_co3 <- data_co3 |>
  group_by(cat) |>
  summarise(
    x = density(slack_cobalt_pct)$x[which.max(density(slack_cobalt_pct)$y)],
    y = max(density(slack_cobalt_pct)$y) * n() * scaling,
    .groups = "drop"
  )

colors_cut <- c(colors_cut, "Desalination" = "#1B7A8A", "No Desalination" = "#041310")

p_desal_co <- ggplot(data_co3, aes(x = slack_cobalt_pct, color = cat)) +
  geom_density(linewidth = 1, aes(y = after_stat(count) * scaling)) +
  geom_text_repel(
    data = lbl_co3,
    aes(x = x, y = y, label = cat, color = cat),
    show.legend = FALSE,
    size = 2.8,
    nudge_y = 0.01,
    seed = 1
  ) +
  # fmt: skip
  annotate("text",x = Inf, y = Inf,label = "d",hjust = 1.2, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_color_manual(values = colors_cut) +
  labs(title = "Improve - Desalination") +
  style_cobalt
p_desal_co

## --- Panel 4: Cobalt Recovery ------------------------------------------------
data_co4 <- data_cobalt |>
  mutate(
    cat = case_when(
      recovery_Cobalt < 0.70 ~ "Cobalt recovery <70%",
      recovery_Cobalt >= 0.7 ~ "Cobalt recovery >70%",
      TRUE ~ "NA"
    )
  )
write_csv(data_co4, "Figures/Data_Figures/Fig4c9b4_cobalt_co_recovery.csv")
lbl_co4 <- data_co4 |>
  group_by(cat) |>
  summarise(
    x = density(slack_cobalt_pct)$x[which.max(density(slack_cobalt_pct)$y)],
    y = max(density(slack_cobalt_pct)$y) * n() * scaling,
    .groups = "drop"
  )

colors_cut <- c("Cobalt recovery <70%" = "#4292c6", "Cobalt recovery >70%" = "#08306b")


p_co_recovery <- ggplot(data_co4, aes(x = slack_cobalt_pct, color = cat)) +
  geom_density(linewidth = 1, aes(y = after_stat(count) * scaling)) +
  geom_text_repel(
    data = lbl_co4,
    aes(x = x, y = y, label = cat, color = cat),
    show.legend = FALSE,
    size = 2.8,
    nudge_y = 0.01,
    seed = 1
  ) +
  # fmt: skip
  annotate("text",x = Inf, y = Inf,label = "e",hjust = 1.2, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black") +
  scale_color_manual(values = colors_cut) +
  labs(title = "Improve - Cobalt\nRecovery Rate") +
  style_cobalt
p_co_recovery

## --- 2×2 grid ----------------------------------------------------------------
(p_co_demand | p_co_fish) / (p_desal_co | p_co_recovery)

# fmt: skip
ggsave("Figures/Test_FigurePanels/Fig4-cobalt-2x2.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18, height = 17.4)

# Merge cobalt -------------
design <- "
AAB
CDE
"
(p_vi_co + p_co_demand + p_co_fish + p_desal_co + p_co_recovery) + plot_layout(design = design)
ggsave("Figures/Figure4_cobalt.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18, height = 17.4)
ggsave("Figures/Figure4_cobalt.svg", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18, height = 17.4)
