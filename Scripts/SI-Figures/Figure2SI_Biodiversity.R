# ============================================================
# Figure 2 SI - Fish Biodiversity Protection Scenarios
# Same design as the redesigned main Figure 2
# ("Scripts/Figure2_ParetoCurves.R"), using the fish-rich
# basin protection scenarios (all under the NZE demand
# trajectory) in place of the 3 demand scenarios.
# Panels:
# a) Cost vs. water-stress Pareto curves, by protection level
# b) Share of total demand / water stress / cost, by mineral
# c-f) Mineral-level decomposition by protection level
#      (Copper / Lithium / Nickel / Cobalt)
# PBH Jul 2026
# ============================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/00a-Common Variables.R", encoding = "UTF-8")

# ============================================================
# 00. COMMON INPUTS ------------------------------------------
# ============================================================

label_text <- 7

demand <- read.csv("Parameters/IEA_Demand.csv")

# fmt: skip
metric_levels <- c("No Water Constraint","0%","0.5%","1%","2%","3%","4%","5%","6%","8%","10%","12%","15%","20%","25%")

# Demand is fixed at NZE for all biodiversity runs
dem_tot_nze <- demand |>
  dplyr::select(Scenario, Year, Copper, Nickel, Lithium, Cobalt) |>
  pivot_longer(c(-Scenario, -Year), names_to = "Mineral", values_to = "Demand") |>
  filter(Scenario == "NZE") |>
  group_by(Mineral) |>
  reframe(mtons = sum(Demand) / 1e3) |>
  ungroup()

# Compiled in "Scripts/Figure2_PrepareData.R"
mineral_decomp_biod <- read.csv("Results/Processed/mineral_decomp_biod.csv")

# ============================================================
# PANEL A - FISH-RICH BASIN PROTECTION SCENARIOS ---------------
# ============================================================

runs <- list.files("Results/Optimization/BioDScenario/NZE/", pattern = "Metrics.*", recursive = TRUE, full.names = TRUE)

obj <- do.call(
  rbind,
  lapply(runs, function(folder_path) {
    transform(
      read.csv(folder_path),
      folder_path = folder_path,
      file_name = basename(folder_path),
      Scenario = basename(dirname(folder_path))
    )
  })
)

obj <- obj |>
  mutate(
    metric = case_when(
      str_detect(file_name, "NoWater") ~ "No Water Constraint",
      str_detect(file_name, "Base") ~ "0%",
      str_detect(file_name, "Eps00") ~ "0.5%",
      str_detect(file_name, "Eps01") ~ "1%",
      str_detect(file_name, "Eps02") ~ "2%",
      str_detect(file_name, "Eps03") ~ "3%",
      str_detect(file_name, "Eps04") ~ "4%",
      str_detect(file_name, "Eps05") ~ "5%",
      str_detect(file_name, "Eps06") ~ "6%",
      str_detect(file_name, "Eps08") ~ "8%",
      str_detect(file_name, "Eps10") ~ "10%",
      str_detect(file_name, "Eps12") ~ "12%",
      str_detect(file_name, "Eps15") ~ "15%",
      str_detect(file_name, "Eps20") ~ "20%",
      str_detect(file_name, "Eps25") ~ "25%"
    ) |>
      factor(levels = metric_levels)
  ) |>
  mutate(
    Scenario = case_when(
      Scenario == "none" ~ "All Basins",
      Scenario == "FI99" ~ "Protect Fish-Rich Basins > 99.9",
      Scenario == "FI95" ~ "Protect Fish-Rich Basins > 95",
      Scenario == "FI90" ~ "Protect Fish-Rich Basins > 90",
      Scenario == "FI85" ~ "Protect Fish-Rich Basins > 85",
      Scenario == "FI80" ~ "Protect Fish-Rich Basins > 80",
      Scenario == "FI75" ~ "Protect Fish-Rich Basins > 75",
      Scenario == "FI74" ~ "Protect Fish-Rich Basins > 74",
      Scenario == "FI73" ~ "Protect Fish-Rich Basins > 73",
      Scenario == "FI72" ~ "Protect Fish-Rich Basins > 72",
      Scenario == "FI71" ~ "Protect Fish-Rich Basins > 71",
      Scenario == "FI70" ~ "Protect Fish-Rich Basins > 70",
      Scenario == "FI65" ~ "Protect Fish-Rich Basins > 65",
      Scenario == "FI60" ~ "Protect Fish-Rich Basins > 60",
      Scenario == "FI55" ~ "Protect Fish-Rich Basins > 55",
      Scenario == "FI50" ~ "Protect Fish-Rich Basins > 50",
      TRUE ~ Scenario
    )
  )

df_biod <- obj |>
  dplyr::select(-Units) |>
  filter(!is.na(Scenario)) |>
  pivot_wider(names_from = Parameter, values_from = Value) |>
  mutate(Water_cons = Water / 1e3, Cost = Cost / 1e3, Water_Impact = `Water impact` / 1e3) |>
  mutate(`Water impact` = NULL) |>
  arrange(Scenario, metric) |>
  mutate(slope = -(Cost - lag(Cost)) / (Water_Impact - lag(Water_Impact)))

base_water <- df_biod |>
  filter(metric == "0%") |>
  rename(water_base = Water_Impact) |>
  dplyr::select(Scenario, water_base)

df_biod <- df_biod |>
  filter(!str_detect(metric, "Water")) |>
  left_join(base_water, by = "Scenario") |>
  mutate(water_red_pct = (Water_Impact - water_base) / water_base * 100)

desalination_cost <- 0.5
df_close_b <- df_biod |> group_by(Scenario) |> slice_min(abs(slope - desalination_cost), n = 1) |> ungroup()
desalination_cost2 <- 0.25
df_close_b2 <- df_biod |> group_by(Scenario) |> slice_min(abs(slope - desalination_cost2), n = 1) |> ungroup()

df_biod <- df_biod |>
  mutate(scen_val = case_when(Scenario == "All Basins" ~ 100, TRUE ~ as.numeric(str_extract(Scenario, "\\d+\\.?\\d*"))))

## Contour polygons (interpolation and extrapolation) ------------------
ext_pts_b <- df_biod |>
  group_by(scen_val) |>
  arrange(Water_Impact) |>
  summarise(
    x_left = first(Water_Impact),
    y_left = first(Cost),
    s_left = (Cost[2] - Cost[1]) / (Water_Impact[2] - Water_Impact[1]),
    x_right = last(Water_Impact),
    y_right = last(Cost),
    s_right = (Cost[n()] - Cost[n() - 1]) / (Water_Impact[n()] - Water_Impact[n() - 1]),
    .groups = "drop"
  )

extra_l_b <- ext_pts_b |>
  rowwise() |>
  mutate(Water_Impact = list(seq(min(df_biod$Water_Impact) * 0.9, x_left, length.out = 30)[-30])) |>
  tidyr::unnest(Water_Impact) |>
  mutate(Cost = y_left + s_left * (Water_Impact - x_left)) |>
  ungroup() |>
  select(scen_val, Water_Impact, Cost)

extra_r_b <- ext_pts_b |>
  rowwise() |>
  mutate(Water_Impact = list(seq(x_right, max(df_biod$Water_Impact) * 1.1, length.out = 30)[-1])) |>
  tidyr::unnest(Water_Impact) |>
  mutate(Cost = y_right + s_right * (Water_Impact - x_right)) |>
  ungroup() |>
  select(scen_val, Water_Impact, Cost)

data_fig_ext_b <- bind_rows(df_biod |> select(scen_val, Water_Impact, Cost), extra_l_b, extra_r_b) |>
  arrange(scen_val, Water_Impact)

# add fake curve below minimum/above max to anchor polygon fill
data_fig_ext_b <- rbind(
  data_fig_ext_b,
  filter(data_fig_ext_b, scen_val == 100) |> mutate(scen_val = 101, Cost = Cost - 2000),
  filter(data_fig_ext_b, scen_val == 50) |> mutate(scen_val = 49, Cost = Cost + 10000)
)

scens_b <- sort(unique(data_fig_ext_b$scen_val), decreasing = TRUE)

bands_b <- purrr::map_dfr(seq_len(length(scens_b) - 1), function(i) {
  s1 <- scens_b[i]
  s2 <- scens_b[i + 1]
  d1 <- filter(data_fig_ext_b, scen_val == s1)
  d2 <- filter(data_fig_ext_b, scen_val == s2)
  tibble(
    Water_Impact = c(d1$Water_Impact, rev(d2$Water_Impact)),
    Cost = c(d1$Cost, rev(d2$Cost)),
    band = paste0(s1, "-", s2),
    band_legend = s1
  )
})

selected_scens <- c(
  "All Basins",
  "Protect Fish-Rich Basins > 99.9",
  "Protect Fish-Rich Basins > 90",
  "Protect Fish-Rich Basins > 70",
  "Protect Fish-Rich Basins > 60"
)
dict_scens <- tibble(
  Scenario = selected_scens,
  Scenario2 = c("No protection", "Protect > 99.9", "Protect > 90", "Protect > 70", "Protect > 60")
)

data_fig_a <- df_biod |>
  filter(Scenario %in% selected_scens) |>
  left_join(dict_scens, by = "Scenario") |>
  mutate(Scenario = Scenario2)
df_close_a <- df_close_b |>
  filter(Scenario %in% selected_scens) |>
  left_join(dict_scens, by = "Scenario") |>
  mutate(Scenario = Scenario2)
df_close_a2 <- df_close_b2 |>
  filter(Scenario %in% selected_scens) |>
  left_join(dict_scens, by = "Scenario") |>
  mutate(Scenario = Scenario2)
df_opt_a <- data_fig_a |> filter(metric == "0%", Scenario == "No protection")

xlim_a_full <- c(min(data_fig_a$Water_Impact) * 0.9, max(data_fig_a$Water_Impact) * 1.1)
ylim_a_full <- c(min(data_fig_a$Cost) * 0.9, max(data_fig_a$Cost) * 1.1)

arrow_dx <- 0.15 * diff(xlim_a_full)
arrow_dy <- 0.15 * diff(ylim_a_full)
x_lwf <- xlim_a_full[1] + 0.5 * diff(xlim_a_full)
y_lwf <- ylim_a_full[1] + 0.9 * diff(ylim_a_full)
x_hc <- xlim_a_full[1] + 0.05 * diff(xlim_a_full)
y_hc0 <- ylim_a_full[1] + 0.8 * diff(ylim_a_full)

label_text_scen <- label_text + 6

# Desalination reference labels are shown only for the "No protection" curve
df_close_a_panel <- df_close_a
df_close_a2_panel <- df_close_a2

df_close_a_panel_text <- df_close_a |> filter(Scenario == "No protection")
df_close_a2_panel_text <- df_close_a2 |> filter(Scenario == "No protection")

scen_colors_biod <- c(
  "No protection" = "#C44E00",
  "Protect > 99.9" = "#8AAC2E",
  "Protect > 90" = "#5E8A1A",
  "Protect > 70" = "#2E7A25",
  "Protect > 60" = "#1A4D12"
)

p_a <- ggplot(data_fig_a, aes(Water_Impact, Cost, col = Scenario, group = Scenario)) +
  geom_polygon(
    data = bands_b,
    aes(Water_Impact, Cost, group = band, fill = band_legend),
    color = "white",
    linewidth = 0.1,
    inherit.aes = FALSE
  ) +
  scale_fill_stepsn(
    # fmt: skip
    colours = alpha(rev(c("#fff7bc","#EBCF2EFF", "#B4BF3AFF", "#88AB38FF", "#5E9432FF", "#3B7D31FF", "#225F2FFF", "#244422FF")), 0.5),
    limits = c(49, 101),
    breaks = c(50, 60, 70, 80, 90, 100),
    labels = c("> 50", "> 60", "> 70", "> 80", "> 90", "> 99.9"),
    name = "Fish-rich basin\nprotection level",
    guide = guide_colorbar(
      barwidth = unit(0.3, "cm"),
      direction = "vertical",
      ticks.colour = "black",
      ticks.linewidth = 0.1,
      frame.colour = "black",
      frame.linewidth = 0.1,
      title.position = "top",
      reverse = TRUE,
      title.hjust = 0.5,
      title.theme = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      label.theme = element_text(angle = 0, hjust = 0.5, vjust = 0.5)
    )
  ) +
  # Guide lines only span from the axes to the optimal point
  geom_segment(
    data = df_opt_a,
    aes(x = Water_Impact, xend = Water_Impact, y = ylim_a_full[1], yend = Cost),
    inherit.aes = FALSE,
    linetype = "dashed",
    col = "#999999",
    linewidth = 0.3
  ) +
  geom_segment(
    data = df_opt_a,
    aes(x = xlim_a_full[1], xend = Water_Impact, y = Cost, yend = Cost),
    inherit.aes = FALSE,
    linetype = "dashed",
    col = "#999999",
    linewidth = 0.3
  ) +
  geom_line(linewidth = 1.1) +
  geom_text(
    data = filter(data_fig_a, metric == "0%"),
    aes(label = Scenario),
    size = label_text_scen * 5 / 14 * 0.8,
    fontface = "bold",
    hjust = 0,
    lineheight = 0.8,
    nudge_x = 1000 * c(-1.8, -1.3, -1.3, 0.1, 0.1),
    nudge_y = 100 * c(-1, 2.2, -1.5, 0.2, -0.2)
  ) +
  # Desalination reference (all curves): $0.5/m3 (white-filled point)
  geom_segment(
    data = df_close_a_panel,
    aes(x = Water_Impact, y = Cost, xend = Water_Impact - 300 / desalination_cost / 2, yend = Cost + 300 / 2),
    linetype = "dashed",
    color = "black",
    linewidth = 0.3,
    arrow = arrow(length = unit(0.12, "cm"), type = "closed")
  ) +
  geom_point(data = df_close_a_panel, shape = 21, fill = "white", color = "black", size = 2, stroke = 0.4) +
  # fmt: skip
  annotate("text", x = df_close_a_panel_text$Water_Impact, y = df_close_a_panel_text$Cost - 200,
    label = paste0("Desalination\n", desalination_cost * 100, "¢/m³"),
    color = "black", size = label_text * 5 / 14 * 0.8, hjust = 0.5, angle = -30) +
  # Desalination reference (all curves): $0.25/m3 (red-filled point)
  geom_segment(
    data = df_close_a2_panel,
    aes(x = Water_Impact, y = Cost, xend = Water_Impact - 300 / desalination_cost2 / 2, yend = Cost + 300 / 2),
    linetype = "dashed",
    color = "black",
    linewidth = 0.3,
    arrow = arrow(length = unit(0.12, "cm"), type = "closed")
  ) +
  geom_point(data = df_close_a2_panel, shape = 21, fill = "#f72f26", color = "black", size = 2, stroke = 0.4) +
  # fmt: skip
  annotate("text", x = df_close_a2_panel_text$Water_Impact, y = df_close_a2_panel_text$Cost - 200,
    label = paste0("Desalination\n", desalination_cost2 * 100, "¢/m³"),
    color = "black", size = label_text * 5 / 14 * 0.8, hjust = 0.5, angle = -20) +
  # fmt: skip
  annotate("text", x = df_opt_a$Water_Impact - 0.02 * diff(xlim_a_full), y = df_opt_a$Cost - 0.03 * diff(ylim_a_full),
    label = "Least cost", col = "#666666", size = label_text * 5 / 14 * 0.8, hjust = 1, vjust = 1, angle = 0) +
  geom_text(data = df_opt_a, label = "★", col = "#666666", size = 4.5) +
  # ARROWS
  annotate(
    "segment",
    col = "black",
    x = x_lwf + arrow_dx,
    y = y_lwf,
    xend = x_lwf,
    yend = y_lwf,
    arrow = arrow(length = unit(0.15, "cm")),
    linewidth = 1
  ) +
  annotate(
    "text",
    x = x_lwf + arrow_dx * 1.05,
    y = y_lwf,
    label = "Lower water stress",
    col = "black",
    size = (label_text + 5) * 5 / 14 * 0.8,
    hjust = 0
  ) +
  annotate(
    "segment",
    col = "black",
    x = x_hc + 300,
    y = y_hc0,
    xend = x_hc + 300,
    yend = y_hc0 + arrow_dy,
    arrow = arrow(length = unit(0.15, "cm")),
    linewidth = 1
  ) +
  annotate(
    "text",
    x = x_hc + 300 + 0.015 * diff(xlim_a_full),
    y = y_hc0,
    label = "Higher cost",
    col = "black",
    size = (label_text + 5) * 5 / 14 * 0.8,
    angle = 0,
    vjust = 1
  ) +
  labs(
    x = expression("Total stress-weighted water use 2025-2050 (trillion " ~ m^3 * "-eq)"),
    y = "Total cost 2025-2050 ($T)",
    col = "",
    title = "Cost versus water stress (fish-rich basin protection in Net-zero demand scenario)"
  ) +
  scale_y_continuous(labels = ~ paste0("$", scales::comma(. / 1e3))) +
  scale_x_continuous(labels = ~ scales::comma(. / 1e3)) +
  scale_color_manual(values = scen_colors_biod) +
  guides(color = "none") +
  coord_cartesian(xlim = xlim_a_full, ylim = ylim_a_full, expand = FALSE) +
  theme_pb_large() +
  theme(
    legend.position = "right",
    legend.key.height = unit(1, "null"), # stretch colorbar to match panel height
    plot.title = element_text(size = 10, face = "bold", colour = "#222222", hjust = 0.5)
  )
p_a

ggsave("Figures/Test_FigurePanels/Fig2SI_Biodiversity_Demand.png", p_a, units = "cm", dpi = 600, width = 9, height = 9)

# ============================================================
# PANEL B - SHARE OF TOTAL ------------------------------------
# Demand share uses the fixed NZE demand split; water-stress and
# cost shares are averaged across the 5 protection-level scenarios
# at each scenario's least-cost (0%) point
# ============================================================

mineral_stack_levels <- rev(c("Copper", "Nickel", "Lithium", "Cobalt")) # stacking order left-to-right

biod_scens_decomp <- c("All Basins", "Fish Index < 99.9", "Fish Index < 90", "Fish Index < 70", "Fish Index < 60")

share_demand_b <- dem_tot_nze |>
  mutate(Share = mtons / sum(mtons)) |>
  mutate(Row = "Demand") |>
  dplyr::select(Mineral, Share, Row)

opt_decomp_b <- mineral_decomp_biod |> filter(Scenario %in% biod_scens_decomp, metric == "0%")

share_cost_b <- opt_decomp_b |>
  mutate(total = copper_cost + nickel_cost + cobalt_cost + lithium_cost) |>
  transmute(
    Scenario,
    Copper = copper_cost / total,
    Lithium = lithium_cost / total,
    Nickel = nickel_cost / total,
    Cobalt = cobalt_cost / total
  ) |>
  pivot_longer(-Scenario, names_to = "Mineral", values_to = "Share") |>
  group_by(Mineral) |>
  reframe(Share = mean(Share)) |>
  mutate(Row = "Cost")

share_water_b <- opt_decomp_b |>
  mutate(total = copper_water + nickel_water + cobalt_water + lithium_water) |>
  transmute(
    Scenario,
    Copper = copper_water / total,
    Lithium = lithium_water / total,
    Nickel = nickel_water / total,
    Cobalt = cobalt_water / total
  ) |>
  pivot_longer(-Scenario, names_to = "Mineral", values_to = "Share") |>
  group_by(Mineral) |>
  reframe(Share = mean(Share)) |>
  mutate(Row = "Water stress")

y_levels_b <- c("Cost", "Water stress", "Demand", "Header") # bottom-to-top plot order; Header is a blank row for direct labels

data_fig_b <- bind_rows(share_demand_b, share_water_b, share_cost_b) |>
  mutate(Row = factor(Row, levels = y_levels_b), Mineral = factor(Mineral, levels = mineral_stack_levels))

# Segment midpoints (all minerals) — used for both the value labels and the header positions
lbl_mid_b <- data_fig_b |>
  group_by(Row) |>
  arrange(desc(Mineral)) |>
  mutate(cum_share = cumsum(Share), mid_share = cum_share - Share / 2) |>
  ungroup()

# Value labels (Copper, Nickel, Lithium — Cobalt segment is usually too thin to label)
lbl_share_b <- lbl_mid_b |> filter(Mineral %in% c("Copper", "Nickel", "Lithium"))

# Direct-label mineral header, centered above each mineral's bar segments, in place of a legend
lbl_header_b <- lbl_mid_b |> group_by(Mineral) |> reframe(x = mean(mid_share)) |> mutate(Row = "Header")

p_b <- ggplot(data_fig_b, aes(y = Row, x = Share, fill = Mineral)) +
  geom_col(orientation = "y", color = "black", linewidth = 0.15, width = 0.65) +
  geom_text(
    data = lbl_share_b,
    aes(x = mid_share, y = Row, label = scales::percent(Share, accuracy = 1)),
    inherit.aes = FALSE,
    color = "black",
    fontface = "bold",
    size = (label_text+2) * 5 / 14 * 0.8
  ) +
  geom_text(
    data = lbl_header_b,
    aes(x = x, y = Row, label = Mineral, color = Mineral),
    inherit.aes = FALSE,
    fontface = "bold",
    size = (label_text + 4) * 5 / 14 * 0.8
  ) +
  scale_fill_manual(values = minerals_colors, guide = "none") +
  scale_color_manual(values = minerals_colors, guide = "none") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
  scale_y_discrete(
    limits = y_levels_b,
    labels = c("Cost" = "Cost", "Water stress" = "Water stress", "Demand" = "Demand", "Header" = "")
  ) +
  labs(title = "Share of total (average across 5 protection-level scenarios)", x = NULL, y = NULL) +
  theme_pb_large() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10, face = "bold", colour = "#222222", hjust = 0.5),
    panel.grid = element_blank(),
    axis.ticks.y = element_blank()
  )
p_b

ggsave("Figures/Test_FigurePanels/Fig2SI_ShareOfTotal.png", p_b, units = "cm", dpi = 600, width = 17, height = 6)

# ============================================================
# PANELS C-F - MINERAL DECOMPOSITION BY PROTECTION LEVEL --------
# One panel per mineral, each with the 5 protection-level curves,
# coloured by that mineral's own palette colour. Only panel c
# (Copper) carries direct scenario labels.
# ============================================================

data_fig_cf <- mineral_decomp_biod |>
  mutate(metric = factor(metric, levels = metric_levels)) |>
  filter(Scenario %in% biod_scens_decomp, metric != "No Water Constraint") |>
  dplyr::select(Scenario, metric, contains("perTon")) |>
  pivot_longer(cols = -c(Scenario, metric), names_to = c("Mineral", "Type"), names_sep = "_", values_to = "Value") |>
  pivot_wider(names_from = Type, values_from = Value)

names(data_fig_cf) <- names(data_fig_cf) |> str_remove("perTon")

# Reference point for dashed lines: All-Basins (no protection) optimal cost, per mineral
df_opt_cf <- data_fig_cf |> filter(Scenario == "All Basins", metric == "0%")

cf_scen_labels <- c(
  "All Basins" = "No protection",
  "Fish Index < 99.9" = "Protect > 99.9",
  "Fish Index < 90" = "Protect > 90",
  "Fish Index < 70" = "Protect > 70",
  "Fish Index < 60" = "Protect > 60"
)

# Same scenario colors as panel a, keyed by the raw Scenario values used in panels c-f
scen_colors_cf <- setNames(scen_colors_biod[cf_scen_labels], names(cf_scen_labels))

data_fig_cf |>
  group_by(Mineral) |>
  reframe(minC = min(Cost) / 1e3, maxC = max(Cost) / 1e3, minW = min(Water) / 1e3, maxW = max(Water) / 1e3)

ylim_cf <- list(Copper = c(2, 6), Lithium = c(10, 40), Nickel = c(6, 9), Cobalt = c(5, 45)) # $/kg

make_panel_cf <- function(
  mineral_name,
  label_scenarios = FALSE,
  label_hjust = c(
    "All Basins" = 0.1,
    "Fish Index < 99.9" = 0.3,
    "Fish Index < 90" = 0.5,
    "Fish Index < 70" = 0.7,
    "Fish Index < 60" = 0.9
  )
) {
  d <- filter(data_fig_cf, Mineral == mineral_name) |> mutate(Cost_kg = Cost / 1e3, Water_kg = Water / 1e3)
  opt <- filter(df_opt_cf, Mineral == mineral_name) |> mutate(Cost_kg = Cost / 1e3, Water_kg = Water / 1e3)
  mineral_col <- minerals_colors[[mineral_name]]

  p <- ggplot(d, aes(Water_kg, Cost_kg, group = Scenario, color = Scenario)) + geom_line(linewidth = 1)

  if (label_scenarios) {
    d_lab <- d |> mutate(ScenarioLabel = cf_scen_labels[Scenario])
    for (scen in names(label_hjust)) {
      p <- p +
        geomtextpath::geom_textpath(
          data = filter(d_lab, Scenario == scen),
          aes(label = ScenarioLabel),
          hjust = label_hjust[[scen]],
          vjust = 0,
          size = (label_text + 2) * 5 / 14 * 0.8,
          text_only = TRUE,
          color = scen_colors_cf[[scen]]
        )
    }
  }

  p +
    geom_text(data = opt, label = "★", col = "#666666", size = 3.2) +
    labs(
      x = "Stress-weighted water use intensity\n(m³-eq/kg metal)",
      y = "Unit cost ($/kg metal)",
      title = mineral_name
    ) +
    scale_y_continuous(labels = dollar_format(prefix = "$"), expand = expansion(0, 0)) +
    scale_x_continuous(labels = scales::label_comma()) +
    scale_color_manual(values = scen_colors_cf) +
    coord_cartesian(ylim = ylim_cf[[mineral_name]]) +
    guides(color = "none") +
    theme_pb_large() +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      axis.title.x = element_text(lineheight = 0.75),
      plot.title = element_text(size = 10, face = "bold", colour = mineral_col, hjust = 0.5)
    )
}

p_c <- make_panel_cf("Copper", label_scenarios = TRUE) +
  # fmt: skip
  annotate("text", x = Inf, y = Inf, label = "c", hjust = 1.8, vjust = 1.8, fontface = "bold", size = 14 * 5 / 14 * 0.8, colour = "black")
p_d <- make_panel_cf("Lithium") +
  # fmt: skip
  annotate("text", x = Inf, y = Inf, label = "d", hjust = 1.8, vjust = 1.8, fontface = "bold", size = 14 * 5 / 14 * 0.8, colour = "black")
p_e <- make_panel_cf("Nickel") +
  # fmt: skip
  annotate("text", x = Inf, y = Inf, label = "e", hjust = 1.8, vjust = 1.8, fontface = "bold", size = 14 * 5 / 14 * 0.8, colour = "black")
p_f <- make_panel_cf("Cobalt") +
  # fmt: skip
  annotate("text", x = Inf, y = -Inf, label = "f", hjust = 1.8, vjust = -1, fontface = "bold", size = 14 * 5 / 14 * 0.8, colour = "black")

library(patchwork)
p_cf <- (p_c | p_d) / (p_e | p_f)
p_cf

ggsave(
  "Figures/Test_FigurePanels/Fig2SI_cf_MineralByProtectionLevel.png",
  p_cf,
  units = "cm",
  dpi = 600,
  width = 17,
  height = 12
)
ggsave(
  "Figures/Test_FigurePanels/Fig2SI_cf_MineralByProtectionLevel.svg",
  p_cf,
  units = "cm",
  dpi = 600,
  width = 17,
  height = 12
)
group_svg_layers("Figures/Test_FigurePanels/Fig2SI_cf_MineralByProtectionLevel.svg")

# ============================================================
# ASSEMBLE FIGURE ---------------------------------------
# ============================================================
library(patchwork)

p_a_fig <- p_a +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = "a",
    hjust = 1.8,
    vjust = 1.8,
    fontface = "bold",
    size = 14 * 5 / 14 * 0.8,
    colour = "black"
  ) +
  theme(plot.margin = margin(0.1, 0.1, 0, 0.1, "cm"))
p_b_fig <- p_b +
  annotate(
    "text",
    x = -Inf,
    y = Inf,
    label = "b",
    hjust = -1,
    vjust = 1.8,
    fontface = "bold",
    size = 14 * 5 / 14 * 0.8,
    colour = "black"
  ) +
  theme(plot.margin = margin(0.1, 0.1, 0, 0.1, "cm"))

fig2_si <- p_a_fig /
  p_b_fig /
  p_cf +
  plot_layout(heights = c(9.5, 3.2, 10)) &
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
fig2_si

ggsave("Figures/Figure2_SI_Biodiversity.png", fig2_si, units = "cm", dpi = 600, width = 17, height = 23)
ggsave("Figures/Figure2_SI_Biodiversity.svg", fig2_si, units = "cm", dpi = 600, width = 17, height = 23)
group_svg_layers("Figures/Figure2_SI_Biodiversity.svg")

# EoF
