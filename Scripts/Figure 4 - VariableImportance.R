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
  "Relocate extraction",
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
  "Relocate extraction" = "#2166AC", # dark blue   - economic policy
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
      feature_plot == "epsilon" ~ "Relocate extraction",
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

# Note: plot_data_water feeds p_vi, which is superseded by p_vi_3cat below
# (both write to Figures/Figure4.png; the 3-category version wins) — not saved.

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
  scale_x_continuous(labels = ~ scales::comma(. / 1e3), breaks = c(0, 2, 4, 6, 8) * 1e3) +
  scale_y_continuous(labels = label_percent(), name = "Relative importance") +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(title = "Variable importance on water footprint ", x = "Scarce Water Use 2025-2050 (trillion m³-eq)") +
  theme_pb_large() +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold", colour = "#222222", hjust = 0.5))
p_vi
# fmt: skip
ggsave("Figures/Test_FigurePanels/Figure4-lgbm-shap.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 9, height = 8.7)

# =============================================================================
# 8b. HORIZONTAL 3-BAR CHART — Low / Mid / High water impact ------------------
#
#   Same SHAP data as 8a (shap_df_lgbm), binned into 3 impact levels.
#   One horizontal bar per level; fill = variable importance (same palette).
# =============================================================================

shap_3cat <- shap_df_lgbm %>%
  mutate(
    impact_level = case_when(
      water_impact_B < 3000 ~ "<3",
      water_impact_B <= 6000 ~ "3-6",
      water_impact_B > 6000 ~ ">6"
    ),
    impact_level = factor(impact_level, levels = rev(c(">6", "3-6", "<3")))
  )

# Mean |SHAP| per feature per category, normalised to proportion
shap_3cat_display <- shap_3cat %>%
  pivot_longer(all_of(lgbm_features), names_to = "feature", values_to = "shap_val") %>%
  group_by(impact_level, feature) %>%
  summarise(mean_abs_shap = mean(abs(shap_val), na.rm = TRUE), .groups = "drop") %>%
  group_by(impact_level) %>%
  mutate(importance = mean_abs_shap / sum(mean_abs_shap)) %>%
  ungroup() %>%
  mutate(
    feature_plot = if_else(feature %in% top_shap_lgbm, feature, "Other"),
    display_name = case_when(
      feature_plot == "epsilon" ~ "Relocate extraction",
      feature_plot == "desal" ~ "Water desalination available",
      feature_plot == "water_Copper" ~ "Ore copper water cons",
      feature_plot == "recovery_Copper" ~ "Copper recovery rate",
      feature_plot == "demand_level" ~ "Mineral demand",
      feature_plot == "depletion_Copper" ~ "Max production rate copper",
      feature_plot %in% c("fish", "fish_threshold") ~ "Protect fish basins",
      feature_plot == "opex_Copper" ~ "Copper extraction costs",
      feature_plot == "recovery_Lithium" ~ "Lithium recovery rate",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(impact_level, display_name) %>%
  summarise(importance = sum(importance), .groups = "drop") %>%
  group_by(impact_level) %>%
  mutate(importance = importance / sum(importance)) %>%
  ungroup() %>%
  mutate(display_name = factor(display_name, levels = rev(water_display_names)))

write_csv(shap_3cat_display, "Figures/Data_Figures/Fig4a_water_shap_3cat.csv")

# Label midpoints for direct annotation (only segments wide enough to label)
label_3cat <- shap_3cat_display %>%
  filter(impact_level == "3-6") |>
  arrange(impact_level, desc(display_name)) %>%
  group_by(impact_level) %>%
  mutate(xmax = cumsum(importance), xmin = lag(xmax, default = 0), xmid = (xmax + xmin) / 2) %>%
  ungroup()

p_vi_3cat <- ggplot(shap_3cat_display, aes(x = impact_level, y = importance, fill = display_name)) +
  geom_col(position = "fill", color = "black", linewidth = 0.15,width=1) +
  geom_text(
    data = label_3cat,
    aes(y = xmid, x = impact_level, label = display_name),
    hjust = 0.5, vjust = 0.5, size = 2, color = "white", fontface = "bold",
    show.legend = FALSE, lineheight = 0.85
  ) +
  # fmt: skip
  annotate("text", x = Inf, y = Inf, label = "a", hjust = 1.2, vjust = 1.2, fontface = "bold", size = 14 * 5 / 14 * 0.8, colour = "white") +
  scale_fill_manual(values = water_colors) +
  scale_y_continuous(labels = label_percent(), name = "Relative importance") +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(title = "Variable importance", x = expression("Scarce Water Use (trillion " ~ m^3 * "-eq)")) +
  theme_pb_large() +
  theme(legend.position = "none", plot.title = element_text(size = 9, face = "bold", colour = "#222222", hjust = 0.5))

p_vi_3cat
# fmt: skip
ggsave("Figures/Test_FigurePanels/Figure4-lgbm-shap-3cat.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 9, height = 8.7)

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
  scale_x_continuous(labels = ~ scales::comma(. / 1e3), breaks = c(0, 2, 4, 6, 8) * 1e3),
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
# Note: feeds p_demand, superseded by p_hdr_b below (both write Figures/Figure4.png) — not saved.

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
  labs(title = "Avoid - Demand", x = "Scarce Water Use") +
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
# Note: feeds p_epsilon, superseded by p_hdr_c below (both write Figures/Figure4.png) — not saved.

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
  labs(title = "Relocate - Increase cost\nto reduce water impact", x = "") +
  style_water
p_epsilon

## --- Panel 3: Desalination Availability --------------------------------------
data_p3 <- data_single |> mutate(cat = case_when(desal == 1 ~ "Desalination", TRUE ~ "No Desalination"))
# Note: feeds p_desal, superseded by p_hdr_d below (both write Figures/Figure4.png) — not saved.

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
  labs(title = "Desalination", x = "Scarce Water Use 2025-2050 (trillion m³ world-eq)") +
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
# Note: feeds p_copper, superseded by p_hdr_e below (both write Figures/Figure4.png) — not saved.
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
(p_vi + p_demand + p_epsilon + p_desal + p_copper) +
  plot_layout(design = design) &
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
ggsave("Figures/Figure4.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18, height = 17.4)
ggsave("Figures/Figure4.svg", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18, height = 17.4)
clean_svg("Figures/Figure4.svg")


## Conditional density figures ---------------------------------------
# =============================================================================
# CONDITIONAL DENSITY GRID — 2 (desal) × 3 (epsilon) panels, color = copper
# =============================================================================

X_LIM_WATER <- c(0, 9e3)
scaling <- 250
clipping <- 11500

skimr::skim(data_full$water_impact_B)

table(data_full$epsilon)
range(data_full$recovery_Copper)
range(data_full$water_Copper)
data_grid <- data_full |>
  mutate(
    cat_desal = case_when(desal == 1 ~ "Desalination", TRUE ~ "No Desalination"),
    cat_epsilon = case_when(
      epsilon == 0.00 ~ "Cost Optimal",
      epsilon %in% c(0.01, 0.05) ~ "1-5% Cost Increase",
      epsilon %in% c(0.10, 0.25) ~ "10-25% Cost Increase",
      TRUE ~ NA_character_
    ),
    copper_score = recovery_Copper - water_Copper,
    cat_copper = case_when(
      ntile(copper_score, 3) == 3 ~ "High recovery, Low water",
      ntile(copper_score, 3) == 2 ~ "Mid",
      ntile(copper_score, 3) == 1 ~ "Low recovery, High water"
    ),
    cat_demand = case_when(mineral_demand < 1050 ~ "Low demand", mineral_demand >= 1050 ~ "High demand")
  ) |>
  filter(!is.na(cat_epsilon), !is.na(cat_copper)) |>
  mutate(
    cat_desal = factor(cat_desal, levels = c("No Desalination", "Desalination")),
    cat_epsilon = factor(cat_epsilon, levels = c("Cost Optimal", "1-5% Cost Increase", "10-25% Cost Increase")),
    cat_copper = factor(cat_copper, levels = c("High recovery, Low water", "Mid", "Low recovery, High water")),
    cat_demand = factor(cat_demand, levels = c("Low demand", "High demand"))
  )
nrow(data_grid)
table(data_grid$cat_copper)

data_dens <- data_grid |>
  group_by(cat_desal, cat_copper, cat_epsilon, cat_demand) |>
  group_modify(
    ~ {
      d <- density(.x$water_impact_B)
      data.frame(x = d$x, y = d$y * scaling * nrow(.x))
    }
  ) |>
  filter(x <= clipping) |>
  ungroup()

lbl_grid <- data_grid |>
  group_by(cat_desal, cat_copper, cat_epsilon, cat_demand) |>
  summarise(
    x = density(water_impact_B)$x[which.max(density(water_impact_B)$y)],
    y = max(density(water_impact_B)$y) * n() * scaling,
    .groups = "drop"
  )

epsilon_colors <- c("Cost Optimal" = "#08306b", "1-5% Cost Increase" = "#045a8d", "10-25% Cost Increase" = "#3690c0")

pct_grid <- data_grid |>
  group_by(cat_desal, cat_copper, cat_epsilon, cat_demand) |>
  summarise(pct = mean(water_impact_B < 3000) * 100, .groups = "drop")

p_grid <- ggplot(data_dens, aes(x = x, y = y, color = cat_epsilon)) +
  geom_line(linewidth = 0.7) +
  geom_text_repel(
    data = lbl_grid,
    aes(x = x, y = y, label = cat_epsilon, color = cat_epsilon),
    show.legend = FALSE,
    size = 2.5,
    nudge_y = 50,
    seed = 1
  ) +
  geom_area(
  data = data_dens |> filter(x <= 3000),
  aes(x = x, y = y, fill = cat_epsilon),
  alpha = 0.15,
  position = "identity"
) +
  scale_fill_manual(values = epsilon_colors, guide = "none") +
  geom_text(
  data = pct_grid |> left_join(lbl_grid, by = c("cat_desal", "cat_copper", "cat_epsilon", "cat_demand")),
  aes(x = x, y = y, label = paste0(round(pct, 1), "%"), color = cat_epsilon),
  vjust = -0.5,
  size = 2.5,
  nudge_y=-40,
  show.legend = FALSE
) +
  scale_color_manual(values = epsilon_colors) +
  scale_x_continuous(labels = label_comma(), breaks = c(0, 2, 4, 6, 8) * 1e3) +
  scale_y_continuous(labels = label_comma()) +
  coord_cartesian(xlim = X_LIM_WATER, expand = FALSE, clip = "off") +
  facet_grid(rows = vars(cat_desal, cat_demand), cols = vars(cat_copper)) +
  geom_vline(xintercept = 3000, linetype = "dashed", linewidth = 0.5, color = "grey50") +
  labs(x = "Scarce Water Use 2025-2050 (billion m³ world-eq)", y = "No. of simulations (n = 10,000)", color = NULL) +
  theme_pb_large() +
  theme(legend.position = "none", strip.text = element_text(size = 9, face = "bold"))

p_grid

ggsave("Figures/Test_FigurePanels/Fig4-dens-grid.png", p_grid, units = "cm", dpi = 600, width = 18, height = 17.4)


# =============================================================================
# 10a. BIVARIATE COST vs WATER IMPACT — Scatter with density regions ----------
#
#   X: water_impact_B (billion m³-eq), Y: cost_B (USD billion, total system cost)
#   5 patchwork panels, one per conditioning variable.
#   Version A: stat_ellipse at 33% (dashed) and 66% (solid) + median points
#   Version B: ggdensity::geom_hdr at probs 0.33/0.66 + geom_hdr_points
# =============================================================================

library(ggdensity)

X_LIM_BI <- c(0, 9500)
Y_LIM_BI <- c(2000, 9000)

style_bivariate <- list(
  scale_x_continuous(labels = ~ scales::comma(. / 1e3), breaks = c(0, 3, 6, 9) * 1e3),
  scale_y_continuous(labels = ~ scales::comma(. / 1e3), breaks = c(3, 6, 9) * 1e3),
  coord_cartesian(xlim = X_LIM_BI, ylim = Y_LIM_BI, expand = FALSE, clip = "off"),
  labs(
    x = expression("Scarce Water Use (trillion " ~ m^3 * "-eq)"),
    y = "Cost 2025-2050 (USD trillion)",
    color = NULL,
    fill = NULL
  ),
  theme_pb_large(),
  theme(legend.position = "none", plot.title = element_text(size = 9, face = "bold", colour = "#222222", hjust = 0.5))
)

## --- Panel data preparation --------------------------------------------------

quantile(data_full$mineral_demand, probs = c(0.05, 0.333, 0.5, 0.666, 0.95))
quantile(data_full$mineral_demand, probs = c(0.25, 0.5, 0.75))
data_bi1 <- data_full |>
  mutate(cat = case_when(mineral_demand < 1000 ~ "Demand\n<1,000 Mt", mineral_demand >= 1100 ~ "Demand\n>1,100 Mt")) |>
  drop_na(cat, water_impact_B, cost_B)

data_bi2 <- data_full |>
  filter(epsilon %in% c(0, 0.05, 0.25)) |>
  mutate(
    cat = case_when(epsilon == 0 ~ "Cost\nOptimal", epsilon == 0.05 ~ "+5% Cost\nIncrease", epsilon == 0.25 ~ "+25%")
  ) |>
  drop_na(cat, water_impact_B, cost_B)

data_bi3 <- data_full |>
  mutate(cat = if_else(desal == 1, "Desalination", "No desalination")) |>
  drop_na(cat, water_impact_B, cost_B)

# Same joint split as Panel 4 in section 9a
data_bi4 <- data_full |>
  mutate(
    cat = case_when(
      recovery_Copper > 0.85 & water_Copper < 0.25 ~ "High recovery,\nLow water",
      recovery_Copper < 0.70 & water_Copper > 0.75 ~ "Low recovery,\nHigh water",
      TRUE ~ NA_character_
    )
  ) |>
  drop_na(cat, water_impact_B, cost_B)

# Best/Worst combined — print n; warn in comment if < 200 expected
data_bi5 <- data_full |>
  mutate(
    cat = case_when(
      epsilon == 0.10 &
        desal == 1 &
        # mineral_demand < 1000 &
        recovery_Copper > 0.85 &
        water_Copper < 0.25 ~ "+10% Cost\nCu ↑Recov. ↓Water\nDesalination",
      epsilon == 0 &
        desal == 0 &
        # mineral_demand >= 1100 &
        recovery_Copper < 0.70 &
        water_Copper > 0.75 ~ "Cost optimal\nCu ↓Recov. ↑Water\nNo desalination",
      TRUE ~ NA_character_
    )
  ) |>
  drop_na(cat, water_impact_B, cost_B)

n_best <- sum(data_bi5$cat == "+10% Cost\nCu ↑Recov. ↓Water\nDesalination")
n_worst <- sum(data_bi5$cat == "Cost optimal\nCu ↓Recov. ↑Water\nNo desalination")
cat(sprintf("Panel k — Best case n = %d | Worst case n = %d\n", n_best, n_worst))
# WARNING: joint filter may yield <200 obs; ellipses/HDRs may be unreliable if so

# Panels b-f (p_hdr_b..p_hdr_f) are the ones assembled into the final Figures/Figure4.png
data_fig_bf <- bind_rows(
  data_bi1 |> mutate(panel = "b") |> select(panel, cat, water_impact_B, cost_B),
  data_bi2 |> mutate(panel = "c") |> select(panel, cat, water_impact_B, cost_B),
  data_bi3 |> mutate(panel = "d") |> select(panel, cat, water_impact_B, cost_B),
  data_bi4 |> mutate(panel = "e") |> select(panel, cat, water_impact_B, cost_B),
  data_bi5 |> mutate(panel = "f") |> select(panel, cat, water_impact_B, cost_B)
)
write_csv(data_fig_bf, "Figures/Data_Figures/Fig4bf_bivariate.csv")

## --- Color palettes (consistent with section 9a) ----------------------------

colors_bi1 <- c("Demand\n<1,000 Mt" = "#6a51a3", "Demand\n>1,100 Mt" = "#3f007d")
colors_bi2 <- c("Cost\nOptimal" = "#08306b", "+5% Cost\nIncrease" = "#3690c0", "+25%" = "#74c6e8")
colors_bi3 <- c("Desalination" = "#1B7A8A", "No desalination" = "#041310")
colors_bi4 <- c("High recovery,\nLow water" = "#7f2704", "Low recovery,\nHigh water" = "#f16913")
colors_bi5 <- c(
  "+10% Cost\nCu ↑Recov. ↓Water\nDesalination" = "#1A7837",
  "Cost optimal\nCu ↓Recov. ↑Water\nNo desalination" = "#B2182B"
)

## --- Median points per panel -------------------------------------------------

compute_bi_medians <- function(df) {
  df |>
    group_by(cat) |>
    summarise(x = median(water_impact_B, na.rm = TRUE), y = median(cost_B, na.rm = TRUE), .groups = "drop")
}

med_bi1 <- compute_bi_medians(data_bi1)
med_bi2 <- compute_bi_medians(data_bi2)
med_bi3 <- compute_bi_medians(data_bi3)
med_bi4 <- compute_bi_medians(data_bi4)
med_bi5 <- compute_bi_medians(data_bi5)

## --- ggdensity HDR — darker core (33%), lighter outer ring (66%) --

make_hdr_panel <- function(data, medians, colors, title, letter, text_labels = NULL) {
  p <- ggplot(data, aes(x = water_impact_B, y = cost_B, color = cat, fill = cat)) +
    geom_hdr(probs = 0.66, alpha = 0.15) +
    geom_hdr(probs = 0.33, alpha = 0.40) +
    geom_point(data = medians, aes(x = x, y = y), size = 3, shape = 16) +
    # fmt: skip
    annotate("text", x = Inf, y = Inf, label = letter, hjust = 1.3, vjust = 1.2, fontface = "bold", size = 14 * 5 / 14 * 0.8, colour = "black") +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(title = title) +
    style_bivariate

  if (!is.null(text_labels)) {
    p <- p +
      geom_text(
      data = text_labels,
      aes(x = x, y = y, label = label, color = label),
      inherit.aes = F,
      show.legend = FALSE, size = 2.8, fontface = "bold"
    )
  }
  p
}

text_1 <- tibble(x = c(6.5, 1.5) * 1e3, y = c(3, 8.5) * 1e3, label = names(colors_bi1))
text_2 <- tibble(x = c(8.1, 5.8, 1.5) * 1e3, y = c(5.5, 6.8, 7) * 1e3, label = names(colors_bi2))
text_3 <- tibble(x = c(6.5, 3) * 1e3, y = c(2.8, 8.2) * 1e3, label = names(colors_bi3))
text_4 <- tibble(x = c(6, 2.2) * 1e3, y = c(2.5, 8.6) * 1e3, label = names(colors_bi4))
text_5 <- tibble(x = c(5, 3.1) * 1e3, y = c(3, 8.3) * 1e3, label = names(colors_bi5))


p_hdr_b <- make_hdr_panel(data_bi1, med_bi1, colors_bi1, "Avoid demand", "b", text_1) +
  # fmt: skip
  annotate("text", x = 4.9e3, y = 6e3, label = "Median",size = 3, hjust = 0.5, fontface = "bold",color = "#3f007d", alpha = 1) +
  # fmt: skip
  annotate("text", x = 4.6e3, y = 7.0e3, label = "33%",size = 3, hjust = 0.5, fontface = "bold",color = "#3f007d", alpha = 0.75) +
  # fmt: skip
  annotate("text", x = 4.6e3, y = 8.7e3, label = "66%",size = 3, hjust = 0.5, fontface = "bold",color = "#3f007d", alpha = 0.5)
p_hdr_c <- make_hdr_panel(data_bi2, med_bi2, colors_bi2, "Relocate to low\nwater impact basins", "c", text_2)
p_hdr_d <- make_hdr_panel(data_bi3, med_bi3, colors_bi3, "Desalination", "d", text_3)
p_hdr_e <- make_hdr_panel(data_bi4, med_bi4, colors_bi4, "Improve copper\nmining process", "e", text_4)
p_hdr_f <- make_hdr_panel(data_bi5, med_bi5, colors_bi5, "Joint conditions", "f", text_5)

p_hdr_b
# fmt: skip
ggsave("test.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 6, height = 17/2)

(p_vi_3cat | p_hdr_b | p_hdr_c) /
  (p_hdr_d | p_hdr_e | p_hdr_f) &
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
# fmt: skip
ggsave("Figures/Figure4.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18, height = 17)
ggsave("Figures/Figure4.svg", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18, height = 17)
clean_svg("Figures/Figure4.svg")


# =============================================================================
# FIGURE 4 — REDESIGN (matches Illustrator mockup "Figure_4__Sensitivities_.ai")
# DESIGN---------------------------------------------------
#
# ADDITIVE sections: paste after section 10a in Figure_4_-_VariableImportance.R
# (or source at the end). Requires objects from the original script:
#   shap_3cat, top_shap_lgbm, lgbm_features,
#   data_bi1..data_bi4, med_bi1..med_bi4, theme_pb_large(), clean_svg()
# Original sections 8b and 10a are left untouched; panel f (data_bi5) is DROPPED.
#
# Design spec extracted programmatically from the .ai file (pt sizes, hex colors,
# opacities, label positions converted to data coordinates). Values marked
# "# TUNE" were estimated and need one visual pass; everything else is measured.
#
# OPEN ITEMS (decided provisionally, revisit):
#   [1] Star "Least cost" reference = hardcoded (6510, 4550) as measured from
#       the mockup. Replace with a computed definition (TODO below).
#   [2] Panel a rows assume relabel of existing bins: <3 -> Low, 3-6 -> Mid,
#       >6 -> High water stress. If rows should condition on a different
#       variable, change `stress_level` construction only.
# =============================================================================

library(ggdensity)
library(patchwork)
library(scales)

# -----------------------------------------------------------------------------
# 0. SHARED DESIGN CONSTANTS (measured from mockup) -----------------------------------
# -----------------------------------------------------------------------------

# Spectral palette, 8 consolidated categories, left-to-right stacking order
vi_display_order <- c(
  "Relocate extraction",
  "Desalinate",
  "Water conservation (copper)",
  "Copper recovery rate",
  "Demand",
  "Production rate (copper)",
  "Avoid fish biodiversity",
  "All other"
)

vi_colors <- c(
  "Relocate extraction" = "#5E4FA2",
  "Desalinate" = "#3288BD",
  "Water conservation (copper)" = "#66C2A5",
  "Copper recovery rate" = "#ABDDA4",
  "Demand" = "#E6F598",
  "Production rate (copper)" = "#FDAE61",
  "Avoid fish biodiversity" = "#F46D43",
  "All other" = "#9E0142"
)

# In-bar % label color: white on dark segments, near-black on light ones
vi_label_col <- c(
  "Relocate extraction" = "white",
  "Desalinate" = "white",
  "Water conservation (copper)" = "#222222",
  "Copper recovery rate" = "#222222",
  "Demand" = "#222222",
  "Production rate (copper)" = "#222222",
  "Avoid fish biodiversity" = "white",
  "All other" = "white"
)

# Panel hue = the lever's color in panel a (deliberate cross-reference)
hue_relocate <- "#5E4FA2"
hue_desal <- "#3288BD"
hue_copper <- "#ABDDA4"
hue_demand <- "#E6F598"

# HDR fill opacities: c(outer 66%, inner 33%)
alpha_light <- c(0.20, 0.40)
alpha_mid <- c(0.40, 0.60) # only used in panel b (3 scenarios)
alpha_dark <- c(0.60, 0.80)

# Least-cost reference (crosshair + star), data units (billion m3-eq, USD billion)
# TODO(Pablo): [1] replace with computed reference, e.g. the deterministic
# least-cost run or median(epsilon == 0 & desal == 0). Measured from mockup:
opt <- read.csv("Results/Optimization/DemandScenario/NZE/Base_Metrics.csv")
star_ref <- tibble(
  x = filter(opt, Parameter == "Water impact")$Value / 1e3,
  y = filter(opt, Parameter == "Cost")$Value / 1e3
)

# Font sizes (pt) — ggplot geom_text size is in mm, hence /.pt
sz_title <- 11 # panel titles & letters, bold
sz_axis <- 9 # axis text and axis titles
sz_inplot <- 8 # scenario labels inside panels
sz_hdr7 <- 7 # panel-a category headers, bold
sz_bar9 <- 9 # panel-a in-bar %, bold

# -----------------------------------------------------------------------------
# 8c. PANEL A — HORIZONTAL STACKED BARS BY WATER STRESS -----------------------
# -----------------------------------------------------------------------------

# [2] Relabel of the existing 3 impact bins (design decision, same data)
shap_stress <- shap_3cat %>%
  mutate(
    stress_level = case_when(
      water_impact_B < 3000 ~ "Low",
      water_impact_B <= 6000 ~ "Mid",
      water_impact_B > 6000 ~ "High"
    ),
    # first level plots at the BOTTOM of a discrete y axis -> High on top
    stress_level = factor(stress_level, levels = c("Low", "Mid", "High"))
  )

vi_stress_display <- shap_stress %>%
  pivot_longer(all_of(lgbm_features), names_to = "feature", values_to = "shap_val") %>%
  group_by(stress_level, feature) %>%
  summarise(mean_abs_shap = mean(abs(shap_val), na.rm = TRUE), .groups = "drop") %>%
  group_by(stress_level) %>%
  mutate(importance = mean_abs_shap / sum(mean_abs_shap)) %>%
  ungroup() %>%
  mutate(
    feature_plot = if_else(feature %in% top_shap_lgbm, feature, "Other"),
    display_name = case_when(
      feature_plot == "epsilon" ~ "Relocate extraction",
      feature_plot == "desal" ~ "Desalinate",
      feature_plot == "water_Copper" ~ "Water conservation (copper)",
      feature_plot == "recovery_Copper" ~ "Copper recovery rate",
      feature_plot == "demand_level" ~ "Demand",
      feature_plot == "depletion_Copper" ~ "Production rate (copper)",
      feature_plot %in% c("fish", "fish_threshold") ~ "Avoid fish biodiversity",
      TRUE ~ "All other" # folds opex_Copper, recovery_Lithium, Other
    )
  ) %>%
  group_by(stress_level, display_name) %>%
  summarise(importance = sum(importance), .groups = "drop") %>%
  group_by(stress_level) %>%
  mutate(importance = importance / sum(importance)) %>%
  ungroup() %>%
  mutate(display_name = factor(display_name, levels = vi_display_order))

write_csv(vi_stress_display, "Figures/Data_Figures/Fig4a_water_shap_stress.csv")

# Segment midpoints (proportion units) for in-bar % labels — all 8 labeled per row
vi_bar_labels <- vi_stress_display %>%
  arrange(stress_level, display_name) %>%
  group_by(stress_level) %>%
  mutate(
    xmax = cumsum(importance),
    xmin = lag(xmax, default = 0),
    xmid = (xmax + xmin) / 2,
    lab = percent(importance, accuracy = 1),
    col = vi_label_col[as.character(display_name)]
  ) %>%
  ungroup()

# Category headers above the top bar, two staggered rows (positions converted
# from mockup page coords to x-proportion; row 1 = upper, row 2 = lower)  # TUNE
vi_headers <- tribble(
  ~x    , ~row , ~label                         ,
  0.149 ,    2 , "Relocate extraction"          ,
  0.363 ,    2 , "Desalinate"                   ,
  0.473 ,    1 , "Water conservation\n(copper)" ,
  0.641 ,    1 , "Copper recovery\nrate"        ,
  0.740 ,    2 , "Demand"                       ,
  0.758 ,    1 , "Production rate\n(copper)"    ,
  0.872 ,    1 , "Avoid fish\nbiodiversity"     ,
  0.975 ,    2 , "All other"
) %>%
  mutate(y = if_else(row == 1, 4.05, 3.6)) # TUNE
# header colors, explicit and in the same row order as the tribble above
vi_headers$color <- unname(vi_colors[c(
  "Relocate extraction",
  "Desalinate",
  "Water conservation (copper)",
  "Copper recovery rate",
  "Demand",
  "Production rate (copper)",
  "Avoid fish biodiversity",
  "All other"
)])

p_vi_stress <- ggplot(vi_stress_display, aes(y = stress_level, x = importance, fill = display_name)) +
  # white gaps between segments and between bars
  geom_col(position = position_fill(reverse = TRUE), color = "white", linewidth = 0.5, width = 0.72) +
  # NOTE: if segment order appears mirrored (Relocate on the right), drop reverse = TRUE
  geom_text(
    data = vi_bar_labels,
    aes(x = xmid, y = stress_level, label = lab, color = I(col)),
    size = sz_bar9 / .pt, fontface = "bold", show.legend = FALSE
  ) +
  # colored category headers above the top bar
  geom_text(
    data = vi_headers,
    aes(x = x, y = y, label = label, color = I(color)),
    inherit.aes = FALSE, size = sz_hdr7 / .pt, fontface = "bold",
    lineheight = 0.85, show.legend = FALSE
  ) +
  # panel letter, top-left
  annotate(
    "text",
    x = 0.005,
    y = 4.1,
    label = "a",
    hjust = 0,
    fontface = "bold",
    size = sz_title / .pt,
    colour = "black"
  ) + # TUNE
  # vertical double-headed arrow next to High/Low axis labels
  annotate(
    "segment",
    x = -0.055,
    xend = -0.055,
    y = 1.35,
    yend = 2.65,
    arrow = arrow(ends = "both", length = unit(1.6, "mm"), type = "closed"),
    linewidth = 0.4,
    colour = "#222222"
  ) + # TUNE
  scale_fill_manual(values = vi_colors) +
  scale_x_continuous(labels = c("0", "20%", "40%", "60%", "80%", "100%"), breaks = seq(0, 1, 0.2), name = NULL, ) +
  scale_y_discrete(labels = c("Low", "", "High"), name = "Water stress") +
  coord_cartesian(xlim = c(0, 1), clip = "off", expand = FALSE, ylim = c(0.5, 4.5)) +
  labs(title = "Variable importance") +
  theme_pb_large() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = sz_title, face = "bold", colour = "#222222", hjust = 0.5),
    axis.text = element_text(size = sz_axis),
    axis.title.y = element_text(size = sz_axis, hjust = 0.3),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    # room for headers above and the arrow at left
    plot.margin = margin(t = 14, r = 6, b = 2, l = 14, unit = "pt") # TUNE
  )

p_vi_stress

# -----------------------------------------------------------------------------
# 10b. REDESIGNED HDR PANELS b–e ----------------------------------------------
#   Single hue per panel; scenarios via fill-opacity (66% outer / 33% inner);
#   no strokes; crosshair through least-cost star; open white median circles;
#   short in-plot labels at positions converted from the mockup.
# -----------------------------------------------------------------------------

style_bivariate_v2 <- list(
  scale_x_continuous(labels = function(x) ifelse(x %% 3000 == 0, comma(x / 1e3), ""), breaks = 0:9 * 1e3),
  scale_y_continuous(labels = function(x) ifelse(x %% 3000 == 0, comma(x / 1e3), ""), breaks = 3:9 * 1e3),
  coord_cartesian(xlim = X_LIM_BI, ylim = Y_LIM_BI, expand = FALSE, clip = "off"),
  labs(x = "Total stress-weighted water use 2025-2050\n(trillion m³-eq)", y = "Total cost 2025-2050 ($T)"),
  theme_pb_large(),
  theme(
    legend.position = "none",
    plot.title = element_text(size = sz_title, face = "bold", colour = "#222222", hjust = 0.5),
    axis.text = element_text(size = sz_axis, lineheight = 0.1),
    axis.title = element_text(size = sz_axis),
    axis.title.x = element_text(size = sz_axis, hjust = 0.6),
  )
)

# One scenario = two nested HDR layers, fixed fill + fixed alpha, no outline.
# Draw order in `alphas` must be light -> dark (overlaps composite correctly).
hdr_layers <- function(data, cats, hue, alphas) {
  layers <- list()
  for (i in seq_along(cats)) {
    d <- filter(data, cat == cats[i])
    layers <- c(
      layers,
      list(
        geom_hdr(data = d, probs = 0.66, fill = hue, color = NA, alpha = alphas[[i]][1]),
        geom_hdr(data = d, probs = 0.33, fill = hue, color = NA, alpha = alphas[[i]][2])
      )
    )
  }
  layers
}

make_hdr_panel_v2 <- function(data, cats, hue, alphas, medians, labels_df, title, letter) {
  ggplot(data, aes(x = water_impact_B, y = cost_B)) +
    hdr_layers(data, cats, hue, alphas) +
    # crosshair through least-cost reference (above fills, below points/labels)
    geom_vline(xintercept = star_ref$x, linewidth = 0.25, colour = "#222222") +
    geom_hline(yintercept = star_ref$y, linewidth = 0.25, colour = "#222222") +
    # least-cost star (unicode glyph; if font lacks it, swap to shape = 8)
    annotate("text", x = star_ref$x, y = star_ref$y, label = "\u2605", size = 4.2, colour = "#989898") +
    # open white median circles
    geom_point(data = medians, aes(x = x, y = y), inherit.aes = FALSE,
               shape = 21, fill = NA, colour = "white", size = 2.1, stroke = 0.9) +
    # short scenario labels (color per row via I())
    geom_text(data = labels_df, aes(x = x, y = y, label = label, color = I(col)),
              inherit.aes = FALSE, size = sz_inplot / .pt, show.legend = FALSE) +
    # panel letter, top-right
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = letter,
      hjust = 1.3,
      vjust = 1.2,
      fontface = "bold",
      size = sz_title / .pt,
      colour = "black"
    ) +
    labs(title = title) +
    style_bivariate_v2
}

## --- scenario category strings (must match data_bi* exactly) -----------------
cat_b <- c("Cost\nOptimal", "+5% Cost\nIncrease", "+25%") # light -> dark
cat_c <- c("No desalination", "Desalination")
cat_d <- c("Low recovery,\nHigh water", "High recovery,\nLow water")
cat_e <- c("Demand\n>1,100 Mt", "Demand\n<1,000 Mt")

## --- in-plot labels (positions converted from mockup; data units) ------------
lab_b <- tibble(
  x = c(1680, 2860, 4670, 7710),
  y = c(4870, 4030, 3800, 3130),
  label = c("+25%", "+5%", "0%", "Least cost"),
  col = c("white", "white", "white", "#989898")
)
lab_c <- tibble(x = c(4770, 2850), y = c(5460, 3750), label = c("No", "Yes"), col = c("white", "white"))
lab_d <- tibble(x = c(5430, 2300), y = c(6040, 3410), label = c("Worst", "Best"), col = c("#7FA177", "#7FA177"))
lab_e <- tibble(x = c(5170, 2050), y = c(6030, 3280), label = c(">1100 Mt", "<1000 Mt"), col = c("#ABAD5D", "#ABAD5D"))

## --- panels: new order b Relocate, c Desalinate, d Copper recovery, e Demand -
p_hdr_b2 <- make_hdr_panel_v2(
  data_bi2,
  cat_b,
  hue_relocate,
  list(alpha_light, alpha_mid, alpha_dark),
  med_bi2,
  lab_b,
  "Relocate",
  "b"
) +
  # mini HDR legend, top-left of panel b only (two concentric outlines)   # TUNE
  annotate(
    "path",
    x = 1020 + 950 * cos(seq(0, 2 * pi, length.out = 80)),
    y = 8000 + 500 * sin(seq(0, 2 * pi, length.out = 80)),
    colour = "black",
    linewidth = 0.3
  ) +
  annotate(
    "path",
    x = 720 + 420 * cos(seq(0, 2 * pi, length.out = 80)),
    y = 8000 + 340 * sin(seq(0, 2 * pi, length.out = 80)),
    colour = "black",
    linewidth = 0.3
  ) +
  annotate("text", x = 700, y = 8000, label = "33%", size = 6 / .pt, colour = "black") +
  annotate("text", x = 1200, y = 8000, label = "66%", size = 6 / .pt, colour = "black", hjust = 0)

p_hdr_c2 <- make_hdr_panel_v2(
  data_bi3,
  cat_c,
  hue_desal,
  list(alpha_light, alpha_dark),
  med_bi3,
  lab_c,
  "Desalinate",
  "c"
)
p_hdr_d2 <- make_hdr_panel_v2(
  data_bi4,
  cat_d,
  hue_copper,
  list(alpha_light, alpha_dark),
  med_bi4,
  lab_d,
  "Increase copper recovery",
  "d"
)
p_hdr_e2 <- make_hdr_panel_v2(
  data_bi1,
  cat_e,
  hue_demand,
  list(alpha_light, alpha_dark),
  med_bi1,
  lab_e,
  "Reduce demand",
  "e"
)

# -----------------------------------------------------------------------------
# FINAL ASSEMBLY — full-width panel a over 2x2 grid; panel f dropped -----------
# Page = 13.6 x 17.9 cm (1.5-column width), row heights measured from mockup
# -----------------------------------------------------------------------------

p_fig4_v2 <- p_vi_stress /
  (p_hdr_b2 | p_hdr_c2) /
  (p_hdr_d2 | p_hdr_e2) +
  plot_layout(heights = c(1, 1.8, 1.8)) &
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

ggsave("Figures/Figure4.png", p_fig4_v2, units = "cm", dpi = 600, width = 13.6, height = 17.9)
ggsave("Figures/Figure4.svg", p_fig4_v2, units = "cm", dpi = 600, width = 13.6, height = 17.9)
clean_svg("Figures/Figure4.svg")

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

# Note: feeds the cobalt SI figure (Figures/ExtData-Figures/Figure4_cobalt.png), not saved.

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
    aes(x = 2, y = ymid, label = display_name),
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
# Note: feeds the cobalt SI figure (Figures/ExtData-Figures/Figure4_cobalt.png), not saved.
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
# Note: feeds the cobalt SI figure (Figures/ExtData-Figures/Figure4_cobalt.png), not saved.
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
  labs(title = "Relocate - Protect fish basins") +
  style_cobalt
p_co_fish

## --- Panel 3: Desalination  --------------------------------------
data_co3 <- data_cobalt |> mutate(cat = case_when(desal == 1 ~ "Desalination", TRUE ~ "No Desalination"))
# Note: feeds the cobalt SI figure (Figures/ExtData-Figures/Figure4_cobalt.png), not saved.
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
  labs(title = "Desalination") +
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
# Note: feeds the cobalt SI figure (Figures/ExtData-Figures/Figure4_cobalt.png), not saved.
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
(p_vi_co + p_co_demand + p_co_fish + p_desal_co + p_co_recovery) +
  plot_layout(design = design) &
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
# fmt: skip
ggsave("Figures/ExtData-Figures/Figure4_cobalt.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18, height = 17.4)
# fmt: skip
ggsave("Figures/ExtData-Figures/Figure4_cobalt.svg", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18, height = 17.4)
clean_svg("Figures/ExtData-Figures/Figure4_cobalt.svg")
