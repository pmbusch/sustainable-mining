# ============================================================
# Figure 2 - Trade-offs between Cost and Freshwater Impacts
# Panels:
# a) Cost vs. water-stress Pareto curves, by demand scenario
# b) Share of total demand / water stress / cost, by mineral
# c-f) Mineral-level decomposition by demand scenario
#      (Copper / Lithium / Nickel / Cobalt)
# Note: biodiversity scenarios moved to
#   "Scripts/SI Figures/Figure 2SI - Biodiversity.R"
# Note: desalination cost curves in Fig2_DesalCurves.R
# PBH Nov 2025 / Refactor Jul 2026
# ============================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/00a-Common Variables.R", encoding = "UTF-8")

# ============================================================
# 00. COMMON INPUTS ------------------------------------------
# ============================================================

label_text <- 7

demand <- read.csv("Parameters/IEA_Demand.csv")
(dict_scen <- tibble(Scenario = Scenario, name = scen_name))

# fmt: skip
metric_levels <- c("No Water Constraint","0%","0.5%","1%","2%","3%","4%","5%","6%","8%","10%","12%","15%","20%","25%")

dem_tot <- demand |>
  dplyr::select(Scenario, Year, Copper, Nickel, Lithium, Cobalt) |>
  pivot_longer(c(-Scenario, -Year), names_to = "Mineral", values_to = "Demand") |>
  group_by(Scenario, Mineral) |>
  reframe(mtons = sum(Demand) / 1e3) |>
  ungroup() |>
  mutate(label_dem = paste0(Scenario, " ", round(mtons, 0), " Mt"))

# Compiled in "Scripts/Fig2_PrepareData.R"
mineral_decomp_demand <- read.csv("Results/Processed/mineral_decomp_demand.csv")

# ============================================================
# PANEL A - DEMAND SCENARIOS ---------------------------------
# ============================================================

runs <- list.files("Results/Optimization/DemandScenario", pattern = "Metrics.*", recursive = TRUE, full.names = TRUE)
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
  )

table(obj$Scenario)
table(obj$metric)
table(obj$Parameter)

# Add demand and slope (USD/m3 of water saved)
df_demand <- obj |>
  filter(!is.na(Scenario)) |>
  dplyr::select(-Units) |>
  pivot_wider(names_from = Parameter, values_from = Value) |>
  mutate(
    Water_cons = Water / 1e3, # billion m3
    Cost = Cost / 1e3, # billion USD (discounted)
    Water_Impact = `Water impact` / 1e3, # billion m3 world-eq
    lab_metric = case_when(
      metric == "0%" ~ "Optimal Cost",
      metric == "0.5%" ~ paste0(metric, " Cost Increase"),
      TRUE ~ as.character(metric)
    )
  ) |>
  mutate(`Water impact` = NULL) |>
  arrange(Scenario, metric) |>
  mutate(
    slope = -(Cost - lag(Cost)) / (Water_Impact - lag(Water_Impact)),
    label_slope = if_else(metric %in% c("1%", "5%", "10%", "25%"), paste0(round(slope, 2), " * ' USD/m'^3"), "")
  )

# Get % of water reduction between first point
base_water <- df_demand |>
  filter(metric == "0%") |>
  rename(water_base = Water_Impact) |>
  dplyr::select(Scenario, water_base)

df_demand <- df_demand |>
  filter(!str_detect(metric, "Water")) |>
  left_join(base_water, by = "Scenario") |>
  mutate(water_red_pct = (Water_Impact - water_base) / water_base * 100) |>
  mutate(
    label_water_red = if_else(metric %in% c("0%", "1%", "3%", "4%", "8%"), "", paste0(round(water_red_pct), "%")),
    label_water_red_full = if_else(
      metric %in% c("1%", "5%", "15%", "25%"),
      paste0("Cost: +", metric, " ~ Water: ", round(water_red_pct), "%"),
      ""
    )
  ) |>
  mutate(
    scen_name = case_when(
      Scenario == "APS" ~ "Pledges",
      Scenario == "NZE" ~ "Net-zero",
      Scenario == "SPS" ~ "Current policies",
      TRUE ~ Scenario
    )
  )

desalination_cost <- 0.5 # USD per m3
# pick closest point to slope to show tangent line representing desalination cost
df_close_a <- df_demand |> group_by(Scenario) |> slice_min(abs(slope - desalination_cost), n = 1) |> ungroup()
# second desaliantion cost show
desalination_cost2 <- 0.25 # USD per m3
df_close_a2 <- df_demand |> group_by(Scenario) |> slice_min(abs(slope - desalination_cost2), n = 1) |> ungroup()


## Contour polygons (interpolation and extrapolation) ------------------
# Method: draw polygons across many curves
dem_total <- dem_tot |> group_by(Scenario) |> reframe(mtons = sum(mtons)) |> ungroup()

data_fig2_a <- df_demand |> left_join(dem_total, by = "Scenario") |> arrange(desc(mtons), Water_Impact)

ext_pts_a <- data_fig2_a |>
  filter(!(Scenario %in% c("SPS", "APS", "NZE"))) |> # do not drawn contous between 3 demand scenarios
  group_by(mtons) |>
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

extra_l_a <- ext_pts_a |>
  rowwise() |>
  mutate(Water_Impact = list(seq(1e3, x_left, length.out = 30)[-30])) |>
  tidyr::unnest(Water_Impact) |>
  mutate(Cost = y_left + s_left * (Water_Impact - x_left)) |>
  ungroup() |>
  select(mtons, Water_Impact, Cost)

extra_r_a <- ext_pts_a |>
  rowwise() |>
  mutate(Water_Impact = list(seq(x_right, 8e3, length.out = 30)[-1])) |>
  tidyr::unnest(Water_Impact) |>
  mutate(Cost = y_right + s_right * (Water_Impact - x_right)) |>
  ungroup() |>
  select(mtons, Water_Impact, Cost)

# avoid bands for 3 demand scenarios shown
data_fig_ext_a <- bind_rows(
  data_fig2_a |> filter(!(Scenario %in% c("SPS", "APS", "NZE"))) |> select(mtons, Water_Impact, Cost),
  extra_l_a,
  extra_r_a
) |>
  arrange(mtons, Water_Impact)
scens_a <- sort(unique(data_fig_ext_a$mtons), decreasing = TRUE)


bands_a <- purrr::map_dfr(seq_len(length(scens_a) - 1), function(i) {
  s1 <- scens_a[i]
  s2 <- scens_a[i + 1]
  d1 <- filter(data_fig_ext_a, mtons == s1)
  d2 <- filter(data_fig_ext_a, mtons == s2)
  tibble(
    Water_Impact = c(d1$Water_Impact, rev(d2$Water_Impact)),
    Cost = c(d1$Cost, rev(d2$Cost)),
    band = paste0(s1, "-", s2),
    band_legend = round(as.numeric(s1), 0),
    band_factor = factor(band_legend)
  )
})

data_fig_a <- df_demand |> filter(Scenario %in% c("APS", "SPS", "NZE"))
df_close_a <- df_close_a |> filter(Scenario %in% c("APS", "SPS", "NZE"))
df_close_a2 <- df_close_a2 |> filter(Scenario %in% c("APS", "SPS", "NZE"))
df_opt_a <- df_demand |> filter(metric == "0%", Scenario == "NZE")
df_slope_nze <- df_demand |> filter(Scenario == "NZE", metric == "25%")

write.csv(data_fig_a, "Figures/Data_Figures/Fig2a.csv", row.names = FALSE)


# Panel range (used for coord_cartesian and for fraction-based annotation placement)
xlim_a_full <- c(min(data_fig_a$Water_Impact) * 0.95, max(data_fig_a$Water_Impact) * 1.05)
ylim_a_full <- c(min(data_fig_a$Cost) * 0.95, max(data_fig_a$Cost) * 1.05)

# "Less water footprint" arrow anchored at (0.5, 0.8) of the panel range; "Higher cost" near upper-left
arrow_dx <- 0.15 * diff(xlim_a_full)
arrow_dy <- 0.15 * diff(ylim_a_full)
x_lwf <- xlim_a_full[1] + 0.5 * diff(xlim_a_full)
y_lwf <- ylim_a_full[1] + 0.8 * diff(ylim_a_full)
x_hc <- xlim_a_full[1] + 0.12 * diff(xlim_a_full)
y_hc0 <- ylim_a_full[1] + 0.5 * diff(ylim_a_full)

label_text_scen <- label_text + 6 # scenario labels, bold and considerably bigger

# Desalination reference points/labels are shown only for the NZE curve
df_close_a_panel <- df_close_a |> filter(Scenario == "NZE")
df_close_a2_panel <- df_close_a2 |> filter(Scenario == "NZE")

p_a <- ggplot(data_fig_a, aes(Water_Impact, Cost, col = Scenario)) +
  geom_polygon(
    data = bands_a,
    aes(Water_Impact, Cost, group = band, fill = band_legend),
    # alpha = 0.5,
    color = "white",
    linewidth = 0.1,
    inherit.aes = FALSE
  ) +
  scale_fill_gradientn(
    colours = alpha(rev(RColorBrewer::brewer.pal(11, "Spectral")[3:10]), 0.5),
    limits = c(850, 1300),
    breaks = seq(850, 1300, 150),
    labels = c("850", "1,000", "1,150", "1,300"),
    name = "Metal demand\n2025-2050\n(Mt metal)",
    guide = guide_colorbar(
      barwidth = unit(0.3, "cm"),
      direction = "vertical",
      ticks.colour = "black",
      ticks.linewidth = 0.1,
      frame.colour = "black",
      frame.linewidth = 0.1,
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      label.theme = element_text(angle = 0, hjust = 0.5, vjust = 0.5)
    )
  ) +
  # Guide lines only span from the axes to the optimal point (not across the whole panel)
  geom_segment(
    data = df_opt_a,
    aes(x = Water_Impact, xend = Water_Impact, y = 0, yend = Cost),
    inherit.aes = FALSE,
    linetype = "dashed",
    col = "#999999",
    linewidth = 0.3
  ) +
  geom_segment(
    data = df_opt_a,
    aes(x = 0, xend = Water_Impact, y = Cost, yend = Cost),
    inherit.aes = FALSE,
    linetype = "dashed",
    col = "#999999",
    linewidth = 0.3
  ) +
  geom_line(linewidth = 1.1) +
  geom_text(
    data = filter(data_fig_a, metric == "0%"),
    aes(label = scen_name),
    nudge_y = 60 * c(1.5, -2, 2),
    nudge_x = 500 * c(-1, -2.1, -1),
    size = label_text_scen * 5 / 14 * 0.8,
    fontface = "bold",
    hjust = 0.5,
    lineheight = 0.7
  ) +
  # Desalination reference (NZE curve only): $0.5/m3 (white-filled point)
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
  annotate("text", x = df_close_a_panel$Water_Impact, y = ylim_a_full[1] + 0.04 * diff(ylim_a_full),
    label = paste0("'Desalination ", desalination_cost * 100, "¢/m'^3"),
    color = "black", size = label_text * 5 / 14 * 0.8, parse = TRUE, hjust = 0.5, angle = -20) +
  # Desalination reference (NZE curve only): $0.25/m3 (red-filled point)
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
  annotate("text", x = df_close_a2_panel$Water_Impact, y = ylim_a_full[1] + 0.09 * diff(ylim_a_full),
    label = paste0("'Desalination ", desalination_cost2 * 100, "¢/m'^3"),
    color = "#f72f26", size = label_text * 5 / 14 * 0.8, parse = TRUE, hjust = 0.5, angle = -20) +
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
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = x_lwf + arrow_dx * 1.05,
    y = y_lwf + 0.02 * diff(ylim_a_full),
    label = "Less water footprint",
    col = "black",
    size = (label_text + 2) * 5 / 14 * 0.8,
    hjust = 1
  ) +
  annotate(
    "segment",
    col = "black",
    x = x_hc,
    y = y_hc0,
    xend = x_hc,
    yend = y_hc0 + arrow_dy,
    arrow = arrow(length = unit(0.15, "cm")),
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = x_hc + 0.015 * diff(xlim_a_full),
    y = y_hc0 + arrow_dy * 0.5,
    label = "Higher cost",
    col = "black",
    size = (label_text + 2) * 5 / 14 * 0.8,
    angle = 90,
    vjust = 0
  ) +
  labs(
    x = expression("Total stress-weighted water use 2025-2050 (trillion " ~ m^3 * "-eq)"),
    y = "Total cost 2025-2050 ($T)",
    col = "",
    title = "Cost versus water stress (all materials)"
  ) +
  scale_y_continuous(labels = ~ paste0("$", scales::comma(. / 1e3))) +
  scale_x_continuous(labels = ~ scales::comma(. / 1e3)) +
  scale_color_manual(values = c("NZE" = "#C44E00", "APS" = "#5E8A00", "SPS" = "#1A6FA4")) +
  guides(color = "none") +
  coord_cartesian(xlim = xlim_a_full, ylim = ylim_a_full, expand = FALSE) +
  theme_pb_large() +
  theme(
    legend.position = "right",
    legend.key.height = unit(1, "null"), # stretch colorbar to match panel height
    plot.title = element_text(size = 10, face = "bold", colour = "#222222", hjust = 0.5)
  )
p_a

# fmt: skip
ggsave("Figures/Test_FigurePanels/Fig2_Demand.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 9, height = 9)

# ============================================================
# PANEL A - ANIMATION FRAMES ---------------------------------
# Progressive build for presentation/animation
# ============================================================

df_close_a_nze <- df_close_a |> filter(Scenario == "NZE")
df_close_a2_nze <- df_close_a2 |> filter(Scenario == "NZE")
df_close_a_other <- df_close_a |> filter(Scenario != "NZE")
df_close_a2_other <- df_close_a2 |> filter(Scenario != "NZE")

# Fig2a1: NZE line only — no contours, no labels, no desalination
p_a1 <- ggplot(filter(data_fig_a, Scenario == "NZE"), aes(Water_Impact, Cost, col = Scenario)) +
  geom_vline(xintercept = df_opt_a$Water_Impact, linetype = "dashed", col = "#999999", linewidth = 0.3) +
  geom_hline(yintercept = df_opt_a$Cost, linetype = "dashed", col = "#999999", linewidth = 0.3) +
  geom_line(linewidth = 1) +
  # fmt: skip
  annotate("segment", col = "#999999", x = 3200, y = 3150, xend = 1600, yend = 3150, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("text", x = 1700, y = 3200, label = "Less water footprint", col = "#999999", size = label_text * 5 / 14 * 0.8, hjust = 0) +
  # fmt: skip
  annotate("segment", col = "#999999", x = 1600, y = 5600, xend = 1600, yend = 5100, arrow = arrow(length = unit(0.1, "cm"))) +
  # fmt: skip
  annotate("text", x = 1800, y = 5350, label = "Less cost", col = "#999999", size = label_text * 5 / 14 * 0.8, vjust = 0, angle = 90) +
  labs(x = expression("Scarce Water Use 2025-2050 (trillion " ~ m^3 * "-eq)"), y = "Cost 2025-2050 (trillion USD)") +
  scale_y_continuous(
    labels = ~ paste0("$", scales::comma(. / 1e3)),
    sec.axis = sec_axis(
      ~ (. - df_opt_a$Cost) / df_opt_a$Cost,
      name = "Change in Cost relative to Optimal Cost NZE (%)",
      labels = scales::percent
    )
  ) +
  scale_x_continuous(
    labels = ~ scales::comma(. / 1e3),
    sec.axis = sec_axis(
      ~ (. - df_opt_a$Water_Impact) / df_opt_a$Water_Impact,
      name = "Change in Water Stress relative to Optimal Cost NZE (%)",
      labels = scales::percent
    )
  ) +
  scale_color_manual(values = c("NZE" = "#C44E00", "APS" = "#5E8A00", "SPS" = "#1A6FA4")) +
  guides(color = "none") +
  coord_cartesian(
    xlim = c(min(data_fig_a$Water_Impact) * 0.95, max(data_fig_a$Water_Impact) * 1.05),
    ylim = c(min(data_fig_a$Cost) * 0.95, max(data_fig_a$Cost) * 1.05),
    expand = FALSE
  ) +
  theme_pb_large() +
  theme(
    legend.position = "none",
    axis.title.y.right = element_text(size = 7, colour = "#AAAAAA"),
    axis.text.y.right = element_text(size = 7, colour = "#AAAAAA"),
    axis.title.x.top = element_text(size = 7, colour = "#AAAAAA"),
    axis.text.x.top = element_text(size = 7, colour = "#AAAAAA"),
    axis.ticks.y.right = element_line(color = "#AAAAAA"),
    axis.ticks.x.top = element_line(color = "#AAAAAA")
  )

# Fig2a2: Add desalination points and text for NZE
p_a2 <- p_a1 +
  geom_segment(
    data = df_close_a_nze,
    aes(
      x = Water_Impact + 300 / desalination_cost / 2,
      xend = df_close_a_nze$Water_Impact - 300 / desalination_cost / 2,
      y = df_close_a_nze$Cost - 300 / 2,
      yend = df_close_a_nze$Cost + 300 / 2
    ),
    linetype = "dashed",
    color = "#0072B2",
    linewidth = 0.25
  ) +
  geom_point(data = df_close_a_nze, col = "#0072B2", size = 1) +
  # fmt: skip
  annotate("text", x = df_close_a_nze$Water_Impact + 100, y = df_close_a_nze$Cost + 20,
    label = paste0("'$' * ", desalination_cost, " * ' per m'^3"),
    color = "#0072B2", size = label_text * 5 / 14 * 0.8, parse = TRUE, hjust = 0) +
  geom_segment(
    data = df_close_a2_nze,
    aes(
      x = Water_Impact + 300 / desalination_cost2 / 2,
      xend = df_close_a2_nze$Water_Impact - 300 / desalination_cost2 / 2,
      y = df_close_a2_nze$Cost - 300 / 2,
      yend = df_close_a2_nze$Cost + 300 / 2
    ),
    linetype = "dashed",
    color = "#023858",
    linewidth = 0.25
  ) +
  geom_point(data = df_close_a2_nze, col = "#023858", size = 1) +
  # fmt: skip
  annotate("text", x = df_close_a2_nze$Water_Impact + 100, y = df_close_a2_nze$Cost + 20,
    label = paste0("'$' * ", desalination_cost2, " * ' per m'^3"),
    color = "#023858", size = label_text * 5 / 14 * 0.8, parse = TRUE, hjust = 0)

# Fig2a3: Add APS/SPS curves, desal points for remaining scenarios, and direct scenario labels
p_a3 <- p_a2 +
  geom_line(data = filter(data_fig_a, Scenario != "NZE"), linewidth = 0.5) +
  geom_segment(
    data = df_close_a_other,
    aes(
      x = Water_Impact + 300 / desalination_cost / 2,
      xend = df_close_a_other$Water_Impact - 300 / desalination_cost / 2,
      y = df_close_a_other$Cost - 300 / 2,
      yend = df_close_a_other$Cost + 300 / 2
    ),
    linetype = "dashed",
    color = "#0072B2",
    linewidth = 0.25
  ) +
  geom_point(data = df_close_a_other, col = "#0072B2", size = 1) +
  geom_segment(
    data = df_close_a2_other,
    aes(
      x = Water_Impact + 300 / desalination_cost2 / 2,
      xend = df_close_a2_other$Water_Impact - 300 / desalination_cost2 / 2,
      y = df_close_a2_other$Cost - 300 / 2,
      yend = df_close_a2_other$Cost + 300 / 2
    ),
    linetype = "dashed",
    color = "#023858",
    linewidth = 0.25
  ) +
  geom_point(data = df_close_a2_other, col = "#023858", size = 1) +
  geom_text(
    data = filter(data_fig_a, metric == "0%"),
    aes(label = scen_name),
    nudge_y = 60 * c(1.5, -2, 2),
    nudge_x = 500 * c(-1, -2.1, -1),
    size = label_text * 5 / 14 * 0.8,
    hjust = 0.5,
    lineheight = 0.7
  ) +
  # fmt: skip
  annotate("text", x = df_opt_a$Water_Impact + 100, y = df_opt_a$Cost - 60,
    label = "Optimal Cost", col = "#999999", size = label_text * 5 / 14 * 0.8, hjust = 1, angle = 90) +
  geom_point(data = df_opt_a, col = "#999999", size = 0.6)

# Save animation frames (png + svg)
ggsave("Figures/Fig 2 Animation/Fig2a1.png", p_a1, units = "cm", dpi = 600, width = 9, height = 9)
ggsave("Figures/Fig 2 Animation/Fig2a1.svg", p_a1, units = "cm", dpi = 600, width = 9, height = 9)
ggsave("Figures/Fig 2 Animation/Fig2a2.png", p_a2, units = "cm", dpi = 600, width = 9, height = 9)
ggsave("Figures/Fig 2 Animation/Fig2a2.svg", p_a2, units = "cm", dpi = 600, width = 9, height = 9)
ggsave("Figures/Fig 2 Animation/Fig2a3.png", p_a3, units = "cm", dpi = 600, width = 9, height = 9)
ggsave("Figures/Fig 2 Animation/Fig2a3.svg", p_a3, units = "cm", dpi = 600, width = 9, height = 9)

# ============================================================
# PANEL B - SHARE OF TOTAL ------------------------------------
# Demand / water-stress / cost share by mineral, averaged across
# the 3 demand scenarios at each scenario's least-cost (0%) point
# ============================================================

demand_scens <- c("NZE", "APS", "SPS")
mineral_stack_levels <- rev(c("Copper", "Nickel", "Lithium", "Cobalt")) # stacking order left-to-right

share_demand_b <- dem_tot |>
  filter(Scenario %in% demand_scens) |>
  group_by(Scenario) |>
  mutate(Share = mtons / sum(mtons)) |>
  ungroup() |>
  group_by(Mineral) |>
  reframe(Share = mean(Share)) |>
  mutate(Row = "Demand")

opt_decomp_b <- mineral_decomp_demand |> filter(Scenario %in% demand_scens, metric == "0%")

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

write.csv(data_fig_b, "Figures/Data_Figures/Fig2b_ShareOfTotal.csv", row.names = FALSE)

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
    size = label_text * 5 / 14 * 0.8
  ) +
  geom_text(
    data = lbl_header_b,
    aes(x = x, y = Row, label = Mineral, color = Mineral),
    inherit.aes = FALSE,
    fontface = "bold",
    size = (label_text + 3) * 5 / 14 * 0.8
  ) +
  scale_fill_manual(values = minerals_colors, guide = "none") +
  scale_color_manual(values = minerals_colors, guide = "none") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
  scale_y_discrete(
    limits = y_levels_b,
    labels = c("Cost" = "Cost", "Water stress" = "Water stress", "Demand" = "Demand", "Header" = "")
  ) +
  labs(title = "Share of total", x = NULL, y = NULL) +
  theme_pb_large() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10, face = "bold", colour = "#222222", hjust = 0.5),
    panel.grid = element_blank(),
    axis.ticks.y = element_blank()
  )
p_b

ggsave("Figures/Test_FigurePanels/Fig2_ShareOfTotal.png", p_b, units = "cm", dpi = 600, width = 17, height = 6)

# ============================================================
# PANELS C-F - MINERAL DECOMPOSITION BY DEMAND SCENARIO --------
# One panel per mineral (Copper/Lithium/Nickel/Cobalt), each with
# the NZE/APS/SPS curves, coloured by that mineral's own palette
# colour. Only panel c (Copper) carries direct scenario labels.
# ============================================================

data_fig_cf <- mineral_decomp_demand |>
  mutate(metric = factor(metric, levels = metric_levels)) |>
  filter(metric != "No Water Constraint") |>
  dplyr::select(Scenario, metric, contains("perTon")) |>
  pivot_longer(cols = -c(Scenario, metric), names_to = c("Mineral", "Type"), names_sep = "_", values_to = "Value") |>
  pivot_wider(names_from = Type, values_from = Value) |>
  mutate(
    lab_metric = case_when(
      metric == "0%" ~ "Optimal Cost",
      metric == "0.5%" ~ paste0(metric, " Cost Increase"),
      TRUE ~ as.character(metric)
    )
  )

names(data_fig_cf) <- names(data_fig_cf) |> str_remove("perTon")

write.csv(data_fig_cf, "Figures/Data_Figures/Fig2cf_MineralByDemandScenario.csv", row.names = FALSE)

# Reference point for dashed lines: NZE optimal (least) cost, per mineral
df_opt_cf <- data_fig_cf |> filter(Scenario == "NZE", metric == "0%")

cf_scen_labels <- c(NZE = "Net-zero", APS = "Pledges", SPS = "Current policies")

ylim_cf <- list(Copper = c(2, 4), Lithium = c(10, 20), Nickel = c(5, 10), Cobalt = c(8, 12)) # $/kg

make_panel_cf <- function(
  mineral_name,
  panel_letter,
  label_scenarios = FALSE,
  label_hjust = c(NZE = 0.15, APS = 0.5, SPS = 0.82),
  panel_accur_x = 1,
  panel_accur_y = 1
) {
  d <- filter(data_fig_cf, Mineral == mineral_name) |> mutate(Cost_kg = Cost / 1e3, Water_kg = Water / 1e3)
  opt <- filter(df_opt_cf, Mineral == mineral_name) |> mutate(Cost_kg = Cost / 1e3, Water_kg = Water / 1e3)
  mineral_col <- minerals_colors[[mineral_name]]

  p <- ggplot(d, aes(Water_kg, Cost_kg, group = Scenario)) + geom_line(linewidth = 1, color = mineral_col)

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
          color = mineral_col
        )
    }
  }

  p +
    geom_text(data = opt, label = "★", col = "#666666", size = 3.2) +
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = panel_letter,
      hjust = 1.8,
      vjust = 1.8,
      fontface = "bold",
      size = 14 * 5 / 14 * 0.8,
      colour = "black"
    ) +
    labs(
      x = "Stress-weighted water use intensity\n(m³-eq/kg metal)",
      y = "Unit cost ($/kg metal)",
      title = mineral_name
    ) +
    scale_y_continuous(labels = dollar_format(accuracy = panel_accur_y, prefix = "$")) +
    scale_x_continuous(labels = scales::label_comma(accuracy = panel_accur_x)) +
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

p_c <- make_panel_cf("Copper", "c", label_scenarios = TRUE, panel_accur_y = 0.1)
p_d <- make_panel_cf("Lithium", "d")
p_e <- make_panel_cf("Nickel", "e", panel_accur_x = 0.1)
p_f <- make_panel_cf("Cobalt", "f") +
  scale_x_continuous(labels = scales::label_comma(accuracy = 0.01), breaks = c(0.55, 0.6, 0.65))


library(patchwork)
p_cf <- (p_c | p_d) / (p_e | p_f)
p_cf

# fmt: skip
ggsave("Figures/Test_FigurePanels/Fig2cf_MineralByDemandScenario.png", p_cf, units = 'cm', dpi = 600, width = 17, height = 12)
# fmt: skip
ggsave("Figures/Test_FigurePanels/Fig2cf_MineralByDemandScenario.svg", p_cf, units = 'cm', dpi = 600, width = 17, height = 12)
clean_svg("Figures/Test_FigurePanels/Fig2cf_MineralByDemandScenario.svg")

# ============================================================
# ASSEMBLE FIGURE ---------------------------------------
# ============================================================
library(patchwork)

# fmt: skip
p_a_fig <- p_a + annotate("text", x = Inf, y = Inf, label = "a", hjust = 1.8, vjust = 1.8, fontface = "bold", size = 14 * 5 / 14 * 0.8, colour = "black") +
  theme(plot.margin = margin(0.1, 0.1, 0, 0.1, "cm"))
# fmt: skip
p_b_fig <- p_b + annotate("text", x = Inf, y = Inf, label = "b", hjust = 1.8, vjust = 1.8, fontface = "bold", size = 14 * 5 / 14 * 0.8, colour = "black") +
  theme(plot.margin = margin(0.1, 0.1, 0, 0.1, "cm"))

fig2 <- p_a_fig /
  p_b_fig /
  p_cf +
  plot_layout(heights = c(9.5, 3.2, 10)) &
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
fig2

ggsave("Figures/Figure2.png", fig2, units = "cm", dpi = 600, width = 17, height = 23)
ggsave("Figures/Figure2.svg", fig2, units = "cm", dpi = 600, width = 17, height = 23)
clean_svg("Figures/Figure2.svg")

# EoF
