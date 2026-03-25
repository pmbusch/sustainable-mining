# ============================================================
# Figure 2 - Trade-offs between Cost and Freshwater Impacts
# Panels:
# a) Demand scenarios
# b) Mineral-level decomposition (NZE)
# c) Biodiversity scenarios
# d) Mineral-level decomposition (Fish Index < 70)
# PBH Nov 2025 / Refactor Mar 2026
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

# Compiled in "Scripts/Figure 2- Precompute.R"
mineral_decomp_demand <- read.csv("Results/Processed/mineral_decomp_demand.csv")
mineral_decomp_biod <- read.csv("Results/Processed/mineral_decomp_biod.csv")
mineral_decomp_CostDes <- read.csv("Results/Processed/mineral_decomp_CostDes.csv")

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
      Scenario == "APS" ~ "Announced Pledges\nScenario",
      Scenario == "NZE" ~ "Net Zero Emissions\nDemand Scenario",
      Scenario == "SPS" ~ "Stated Policies\nScenario",
      TRUE ~ Scenario
    )
  )

desalination_cost <- 0.5 # USD per m3
# pick closest point to slope to show tangent line representing desalination cost
df_close_a <- df_demand |> group_by(Scenario) |> slice_min(abs(slope - desalination_cost), n = 1) |> ungroup()


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
    band_legend = round(as.numeric(s1), 0)
  )
})

# label some of the curves - filter is for later crop of figure
labels_curves <- data_fig_ext_a |>
  mutate(lab_tons = paste0(scales::comma(round(mtons, 0)), " Mt")) |>
  filter(Water_Impact > 1500, Water_Impact < 7500, Cost > 3000, Cost < 5500)
curves_toLabel <- unique(labels_curves$lab_tons)[c(5, 7, 9, 12, 14, 16, 18)] # hand picked
labels_curves <- labels_curves |> filter(lab_tons %in% curves_toLabel)

data_fig_a <- df_demand |> filter(Scenario %in% c("APS", "SPS", "NZE"))
df_close_a <- df_close_a |> filter(Scenario %in% c("APS", "SPS", "NZE"))
df_opt_a <- df_demand |> filter(metric == "0%", Scenario == "NZE")
df_slope_nze <- df_demand |> filter(Scenario == "NZE", metric == "25%")

write.csv(data_fig_a, "Figures/Data_Figures/Fig2a.csv", row.names = FALSE)

p_a <- ggplot(data_fig_a, aes(Water_Impact, Cost, col = Scenario)) +
  geom_polygon(
    data = bands_a,
    aes(Water_Impact, Cost, group = band, fill = band_legend),
    alpha = 0.5,
    color = "white",
    linewidth = 0.1,
    inherit.aes = FALSE
  ) +
  geomtextpath::geom_textpath(
    data = labels_curves,
    aes(Water_Impact, Cost, group = mtons, label = lab_tons),
    size = (label_text - 1) * 5 / 14 * 0.8,
    text_only = T,
    vjust = 0.2,
    hjust = 0.8,
    col = "#737373",
    inherit.aes = F
  ) +
  scale_fill_gradientn(
    colours = rev(RColorBrewer::brewer.pal(11, "Spectral")),
    limits = c(800, 1500),
    breaks = c(800, 1500),
    labels = c("Low\nDemand", "High\nDemand"),
    name = ""
  ) +
  geom_vline(xintercept = df_opt_a$Water_Impact, linetype = "dashed", col = "#999999", linewidth = 0.3) +
  geom_hline(yintercept = df_opt_a$Cost, linetype = "dashed", col = "#999999", linewidth = 0.3) +
  geom_line(linewidth = 0.8) +
  # geom_point(size = 0.5) +
  geom_text(
    data = filter(data_fig_a, metric == "0%"),
    aes(label = scen_name),
    nudge_y = 60 * c(1.5, -2, 2),
    nudge_x = 500 * c(-1, -2.1, -1),
    size = label_text * 5 / 14 * 0.8,
    hjust = 0.5,
    lineheight = 0.7
  ) +
  # geom_text(
  #   data = filter(data_fig_a, str_detect(Scenario, "NZE")),
  #   aes(label = label_slope),
  #   col = "#6c8364",
  #   nudge_y = 45,
  #   size = label_text * 5 / 14 * 0.8,
  #   parse = TRUE,
  #   hjust = 0
  # ) +
  # annotate(
  #   "text",
  #   x = df_slope_nze$Water_Impact + 1200,
  #   y = df_slope_nze$Cost + 30,
  #   col = "#6c8364",
  #   label = "Implied Cost of Water Reduction",
  #   size = label_text * 5 / 14 * 0.8,
  #   hjust = 0
  # ) +
  # geom_point(data = filter(data_fig_a, str_detect(Scenario, "NZE"), label_slope != ""), col = "#6c8364", size = 0.6) +
  geom_segment(
    data = df_close_a,
    aes(
      x = Water_Impact + 1000 / desalination_cost / 2,
      xend = df_close_a$Water_Impact - 1000 / desalination_cost / 2,
      y = df_close_a$Cost - 1000 / 2,
      yend = df_close_a$Cost + 1000 / 2
    ),
    linetype = "dashed",
    color = "#0072B2",
    linewidth = 0.25
  ) +
  geom_point(data = df_close_a, col = "#0072B2", size = 1) +
  # fmt: skip
  annotate("text",x = df_close_a$Water_Impact[2] + 100,y = df_close_a$Cost[2] + 20,
    label = paste0("'Desalination: $' * ", desalination_cost, " * ' per m'^3"),
    color = "#0072B2",size = label_text * 5 / 14 * 0.8,parse = TRUE,hjust = 0) +
  # fmt: skip
  annotate("text",x = df_opt_a$Water_Impact + 100,y = df_opt_a$Cost + 100,
    label = "Optimal Cost",col = "#999999",size = label_text * 5 / 14 * 0.8,hjust = 0.1,angle = 90) +
  geom_point(data = df_opt_a, col = "#999999", size = 0.6) +
  labs(
    x = expression("Freshwater Impact 2025-2050 (billion " ~ m^3 * "-eq)"),
    y = "Cost 2025-2050 (billion USD)",
    col = ""
  ) +
  scale_y_continuous(
    labels = dollar_format(big.mark = ",", prefix = "$"),
    sec.axis = sec_axis(
      ~ (. - df_opt_a$Cost) / df_opt_a$Cost,
      name = "Change in Cost relative to Optimal Cost NZE (%)",
      labels = scales::percent
    )
  ) +
  scale_x_continuous(
    labels = scales::label_comma(),
    sec.axis = sec_axis(
      ~ (. - df_opt_a$Water_Impact) / df_opt_a$Water_Impact,
      name = "Change in Freshwater Impact relative to Optimal Cost NZE (%)",
      labels = scales::percent
    )
  ) +
  scale_color_manual(values = demand_colors) +
  guides(color = "none", fill = "none") +
  coord_cartesian(
    xlim = c(min(data_fig_a$Water_Impact) * 0.95, max(data_fig_a$Water_Impact) * 1.05),
    ylim = c(min(data_fig_a$Cost) * 0.95, max(data_fig_a$Cost) * 1.05),
    expand = FALSE
  ) +
  theme_pb_large() +
  theme(
    legend.position = "none",
    axis.title.y.right = element_text(size = 7),
    axis.text.y.right = element_text(size = 7),
    axis.title.x.top = element_text(size = 7),
    axis.text.x.top = element_text(size = 7)
  )
p_a

# fmt: skip
ggsave("Figures/Fig2_Demand.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)

# ============================================================
# PANEL B - MINERAL DECOMPOSITION (NZE) ----------------------
# For demand scenario NZE
# ============================================================

data_fig_b <- mineral_decomp_demand |>
  mutate(metric = factor(metric, levels = metric_levels)) |>
  filter(Scenario == "NZE", metric != "No Water Constraint") |>
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

names(data_fig_b) <- names(data_fig_b) |> str_remove("perTon")

write.csv(data_fig_b, "Figures/Data_Figures/Fig2b_MineralDemand.csv", row.names = FALSE)

p_b <- ggplot(data_fig_b, aes(Water, Cost, col = Mineral)) +
  geom_line() +
  # geom_point(size = 0.5) +
  geom_text(
    data = filter(data_fig_b, metric == "0%", !(Mineral %in% c("Cobalt","Nickel"))),
    aes(label = Mineral),
    nudge_y = -500 * c( 1, 1),
    nudge_x = c(0,-1000),
    size = label_text * 5 / 14 * 0.8,
    hjust = 0.5
  ) +
  labs(x = expression("Freshwater Impact (" ~ m^3 * "-eq per ton)"), y = "Cost (USD per ton)", col = "") +
  scale_y_continuous(labels = dollar_format(big.mark = ",", prefix = "$")) +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_color_manual(values = minerals_colors) +
  theme_pb_large() +
  theme(panel.grid = element_blank(), legend.position = "none", strip.placement = "outside")
p_b

# Zoom version cobalt and nickel
data_fig_b |> group_by(Mineral) |> reframe(max_w = max(Water), min_w = min(Water), max_c = max(Cost), min_c = min(Cost))
pb_nickel <- p_b %+%
  (data = filter(data_fig_b, Mineral == 'Nickel')) +
  geom_text(
    data = filter(data_fig_b, metric == "0%",(Mineral =="Nickel")),
    aes(label = Mineral),
    nudge_y = 100,nudge_x=-150,
    size = label_text * 5 / 14 * 0.8,
    hjust = 0.5
  ) +
  coord_cartesian(xlim = c(300, 650), ylim = c(7100, 7230), expand = FALSE, clip = "on") +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = c(300, 400, 500, 600), labels = scales::label_comma()) +
  scale_y_continuous(breaks = c(7100, 7150, 7200), labels = dollar_format(big.mark = ",", prefix = "$")) +
  theme(
    axis.title = element_text(color = "#999999"),
    panel.border = element_rect(color = "#999999", fill = NA),
    axis.text = element_text(size = 4, color = "#999999"),
    plot.margin = margin(0, 0, 0, 0),
    panel.spacing = unit(0, "pt")
  )

pb_cobalt <- p_b %+%
  (data = filter(data_fig_b, Mineral == 'Cobalt')) +
  geom_text(
    data = filter(data_fig_b, metric == "0%", Mineral=="Cobalt"),
    aes(label = Mineral),
    nudge_y = 300,
    nudge_x = -15,
    size = label_text * 5 / 14 * 0.8,
    hjust = 0.5
  ) +
  coord_cartesian(xlim = c(640, 780), ylim = c(10840, 11580), expand = FALSE, clip = "on") +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = c(650, 700, 750), labels = scales::label_comma()) +
  scale_y_continuous(breaks = c(11000, 11250, 11500), labels = dollar_format(big.mark = ",", prefix = "$")) +
  theme(
    axis.title = element_text(color = "#999999"),
    panel.border = element_rect(color = "#999999", fill = NA),
    axis.text = element_text(size = 4, color = "#999999"),
    plot.margin = margin(0, 0, 0, 0),
    panel.spacing = unit(0, "pt")
  )


library(patchwork)
p_b_fig <- p_b +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = "b",
    hjust = 1.2,
    vjust = 1.2,
    fontface = "bold",
    size = 14 * 5 / 14 * 0.8,
    colour = "black"
  ) +
  labs(title = "NZE Demand Scenario") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # fmt: skip
  annotate("segment",xend = 2000,y = 7200,x = 13500,yend = 7200,arrow = arrow(length = unit(0.15, "cm")),linewidth = 0.2,colour = "#999999") +
  # fmt: skip
  annotate("segment",xend = 2000,y = 11200,x = 13500,yend = 11200,arrow = arrow(length = unit(0.15, "cm")),linewidth = 0.2,colour = "#999999") +
  inset_element(pb_nickel, left = 0.35, bottom = 0.02, right = 0.78, top = 0.32) +
  inset_element(pb_cobalt, left = 0.35, bottom = 0.37, right = 0.78, top = 0.67)
p_b_fig

# fmt: skip
ggsave("Figures/Fig2B.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)

# ============================================================
# PANEL C - BIODIVERSITY SCENARIOS -------------------------
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
      Scenario == "FI99" ~ "Fish Index < 99.9",
      Scenario == "FI95" ~ "Fish Index < 95",
      Scenario == "FI90" ~ "Fish Index < 90",
      Scenario == "FI85" ~ "Fish Index < 85",
      Scenario == "FI80" ~ "Fish Index < 80",
      Scenario == "FI75" ~ "Fish Index < 75",
      Scenario == "FI74" ~ "Fish Index < 74",
      Scenario == "FI73" ~ "Fish Index < 73",
      Scenario == "FI72" ~ "Fish Index < 72",
      Scenario == "FI71" ~ "Fish Index < 71",
      Scenario == "FI70" ~ "Fish Index < 70",
      Scenario == "FI65" ~ "Fish Index < 65",
      Scenario == "FI60" ~ "Fish Index < 60",
      Scenario == "FI55" ~ "Fish Index < 55",
      Scenario == "FI50" ~ "Fish Index < 50",
      TRUE ~ Scenario
    )
  )
table(obj$Scenario)

df_biod <- obj |>
  dplyr::select(-Units) |>
  filter(!is.na(Scenario)) |>
  # dplyr::select(-Units) |>
  pivot_wider(names_from = Parameter, values_from = Value) |>
  mutate(
    Water_cons = Water / 1e3,
    Cost = Cost / 1e3,
    Water_Impact = `Water impact` / 1e3,
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
    label_slope = if_else(metric %in% c("0.5%", "4%", "15%"), paste0(round(slope, 2), " * ' USD/m'^3"), "")
  )

table(df_biod$Scenario)
table(df_biod$metric)

base_water <- df_biod |>
  filter(metric == "0%") |>
  rename(water_base = Water_Impact) |>
  dplyr::select(Scenario, water_base)

df_biod <- df_biod |>
  filter(!str_detect(metric, "Water")) |>
  left_join(base_water, by = "Scenario") |>
  mutate(water_red_pct = (Water_Impact - water_base) / water_base * 100) |>
  mutate(label_water_red = if_else(metric %in% c("0%", "1%", "3%", "4%", "8%"), "", paste0(round(water_red_pct), "%")))

desalination_cost <- 0.5
df_close_c <- df_biod |> group_by(Scenario) |> slice_min(abs(slope - desalination_cost), n = 1) |> ungroup()

df_biod <- df_biod |>
  mutate(scen_val = case_when(Scenario == "All Basins" ~ 100, TRUE ~ as.numeric(str_extract(Scenario, "\\d+\\.?\\d*"))))

table(df_biod$scen_val)

# Contour polygons
ext_pts_c <- df_biod |>
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

extra_l_c <- ext_pts_c |>
  rowwise() |>
  mutate(Water_Impact = list(seq(min(df_biod$Water_Impact) * 0.9, x_left, length.out = 30)[-30])) |>
  tidyr::unnest(Water_Impact) |>
  mutate(Cost = y_left + s_left * (Water_Impact - x_left)) |>
  ungroup() |>
  select(scen_val, Water_Impact, Cost)

extra_r_c <- ext_pts_c |>
  rowwise() |>
  mutate(Water_Impact = list(seq(x_right, max(df_biod$Water_Impact) * 1.1, length.out = 30)[-1])) |>
  tidyr::unnest(Water_Impact) |>
  mutate(Cost = y_right + s_right * (Water_Impact - x_right)) |>
  ungroup() |>
  select(scen_val, Water_Impact, Cost)

data_fig_ext_c <- bind_rows(df_biod |> select(scen_val, Water_Impact, Cost), extra_l_c, extra_r_c) |>
  arrange(scen_val, Water_Impact)

# add fake curve below minimium to show polygon fill
data_fig_ext_c <- rbind(
  data_fig_ext_c,
  filter(data_fig_ext_c, scen_val == 100) |> mutate(scen_val = 110, Cost = Cost - 2000),
  filter(data_fig_ext_c, scen_val == 50) |> mutate(scen_val = 30, Cost = Cost + 10000)
)

scens_c <- sort(unique(data_fig_ext_c$scen_val), decreasing = TRUE)

bands_c <- purrr::map_dfr(seq_len(length(scens_c) - 1), function(i) {
  s1 <- scens_c[i]
  s2 <- scens_c[i + 1]
  d1 <- filter(data_fig_ext_c, scen_val == s1)
  d2 <- filter(data_fig_ext_c, scen_val == s2)
  tibble(
    Water_Impact = c(d1$Water_Impact, rev(d2$Water_Impact)),
    Cost = c(d1$Cost, rev(d2$Cost)),
    band = paste0(s1, "-", s2),
    band_legend = s1
  )
})

data_fig_c <- df_biod |>
  filter(Scenario %in% c("All Basins", "Fish Index < 99.9", "Fish Index < 90", "Fish Index < 70", "Fish Index < 60"))

df_close_c <- df_close_c |>
  filter(Scenario %in% c("All Basins", "Fish Index < 99.9", "Fish Index < 90", "Fish Index < 70", "Fish Index < 60"))

df_opt_c <- data_fig_c |> filter(metric == "0%", Scenario == "All Basins")

write.csv(data_fig_c, "Figures/Data_Figures/Fig2c_Biodiversity_points.csv", row.names = FALSE)

p_c <- ggplot(data_fig_c, aes(Water_Impact, Cost, col = Scenario, group = Scenario)) +
  geom_polygon(
    data = bands_c,
    aes(Water_Impact, Cost, group = band, fill = band_legend),
    alpha = 0.5,
    color = "white",
    linewidth = 0.1,
    inherit.aes = FALSE
  ) +
  scale_fill_gradientn(
    colours = rev(c("#EBCF2EFF", "#B4BF3AFF", "#88AB38FF", "#5E9432FF", "#3B7D31FF", "#225F2FFF", "#244422FF"))
  ) +
  geom_vline(xintercept = df_opt_c$Water_Impact, linetype = "dashed", linewidth = 0.3, col = "#999999") +
  geom_hline(yintercept = df_opt_c$Cost, linetype = "dashed", linewidth = 0.3, col = "#999999") +
  geom_line(linewidth = 0.8) +
  # geom_point(size = 0.5) +
  geom_text(
    data = filter(data_fig_c, metric == "0%"),
    aes(label = Scenario),
    size = label_text * 5 / 14 * 0.8,
    hjust = 0,
    nudge_y = 100 * c(-1, 1.2, -1.2, 1.2, 0),
    nudge_x = 1000 * c(-1.5, -1, -0.8, 0, 0.3)
  ) +
  geom_segment(
    data = df_close_c,
    aes(
      x = Water_Impact + 1000 / desalination_cost / 2,
      xend = df_close_c$Water_Impact - 1000 / desalination_cost / 2,
      y = df_close_c$Cost - 1000 / 2,
      yend = df_close_c$Cost + 1000 / 2
    ),
    linetype = "dashed",
    color = "#0072B2",
    linewidth = 0.25
  ) +
  geom_point(data = df_close_c, col = "#0072B2", size = 1) +
  # fmt: skip
  annotate("text",x = df_close_c$Water_Impact[4] + 100,y = df_close_c$Cost[4] + 150,
    label = paste0("'Desalination: $' * ", desalination_cost, " * ' per m'^3"),
    color = "#0072B2",size = label_text * 5 / 14 * 0.8,parse = TRUE,hjust = 0
  ) +
  geom_point(data = df_opt_a, col = "#999999", size = 0.6) +
  # fmt: skip
  annotate("text",x = df_opt_c$Water_Impact + 300,y = df_opt_c$Cost - 100,label = "Optimal Cost",col = "#999999",size = label_text * 5 / 14 * 0.8,hjust = 0.1) +
  labs(
    x = expression("Freshwater Impact 2025-2050 (billion " ~ m^3 * "-eq)"),
    y = "Cost 2025-2050 (billion USD)",
    col = ""
  ) +
  scale_y_continuous(
    labels = dollar_format(big.mark = ",", prefix = "$"),
    sec.axis = sec_axis(
      ~ (. - df_opt_c$Cost) / df_opt_c$Cost,
      name = "Change in Cost relative to Optimal Cost All Basins (%)",
      labels = scales::percent
    )
  ) +
  scale_x_continuous(
    labels = scales::label_comma(),
    sec.axis = sec_axis(
      ~ (. - df_opt_c$Water_Impact) / df_opt_c$Water_Impact,
      name = "Change in Freshwater Impact relative to Optimal Cost All Basins (%)",
      labels = scales::percent
    )
  ) +
  scale_color_manual(values = c("#3B4CC0", "#2C7FB8", "#6A00A8", "#9C179E", "#D01C8B")) +
  coord_cartesian(
    xlim = c(min(data_fig_c$Water_Impact) * 0.9, max(data_fig_c$Water_Impact) * 1.1),
    ylim = c(min(data_fig_c$Cost) * 0.9, max(data_fig_c$Cost) * 1.1),
    expand = FALSE
  ) +
  guides(color = "none", fill = "none") +
  theme_pb_large() +
  theme(
    panel.grid = element_blank(),
    axis.title.y.right = element_text(size = 7),
    axis.text.y.right = element_text(size = 7),
    axis.title.x.top = element_text(size = 7),
    axis.text.x.top = element_text(size = 7)
  )
p_c
# fmt: skip
ggsave("Figures/Fig2_Biodiversity.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)

# ============================================================
# PANEL D - MINERAL DECOMPOSITION (FISH INDEX < 70) ------------
# ============================================================

data_fig_d <- mineral_decomp_biod |>
  mutate(metric = factor(metric, levels = metric_levels)) |>
  filter(Scenario == "Fish Index < 70", metric != "No Water Constraint") |>
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

names(data_fig_d) <- names(data_fig_d) |> str_remove("perTon")

write.csv(data_fig_d, "Figures/Data_Figures/Fig2d_MineralFI70.csv", row.names = FALSE)

p_d <- ggplot(data_fig_d, aes(Water, Cost, col = Mineral)) +
  geom_line() +
  # geom_point(size = 0.5) +
  geom_text(
    data = filter(data_fig_d, metric == "0%",!(Mineral %in% c("Cobalt","Nickel"))),
    aes(label = Mineral),
    nudge_y = 1000 * c(-1, -1),
    nudge_x = c(0,-1000),
    size = label_text * 5 / 14 * 0.8,
    hjust = 0.5
  ) +
  labs(x = expression("Freshwater Impact (" ~ m^3 * "-eq per ton)"), y = "Cost (USD per ton)", col = "") +
  scale_y_continuous(labels = dollar_format(big.mark = ",", prefix = "$")) +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_color_manual(values = minerals_colors) +
  # coord_cartesian(expand = F, xlim = c(0, 45000), ylim = c(0, 32000)) +
  theme_pb_large() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title.y.right = element_text(size = 5),
    axis.text.y.right = element_text(size = 5),
    axis.title.x.top = element_text(size = 5),
    axis.text.x.top = element_text(size = 5),
    strip.placement = "outside"
  )
p_d

# Zoom version cobalt and nickel
data_fig_d |> group_by(Mineral) |> reframe(max_w = max(Water), min_w = min(Water), max_c = max(Cost), min_c = min(Cost))
pd_nickel <- p_d %+%
  (data = filter(data_fig_d, Mineral == 'Nickel')) +
  geom_text(
    data = filter(data_fig_d, metric == "0%",Mineral == "Nickel"),
    aes(label = Mineral),
    nudge_y = 500,
    nudge_x = -100,
    size = label_text * 5 / 14 * 0.8,
    hjust = 0.5
  ) +
  coord_cartesian(xlim = c(360, 770), ylim = c(7450, 8200), expand = FALSE, clip = "on") +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = c(400, 500, 600, 700), labels = scales::label_comma()) +
  scale_y_continuous(breaks = c(7500, 8000), labels = dollar_format(big.mark = ",", prefix = "$")) +
  theme(
    axis.title = element_text(color = "#999999"),
    panel.border = element_rect(color = "#999999", fill = NA),
    axis.text = element_text(size = 4, color = "#999999"),
    plot.margin = margin(0, 0, 0, 0),
    panel.spacing = unit(0, "pt")
  )

pd_cobalt <- p_d %+%
  (data = filter(data_fig_d, Mineral == 'Cobalt')) +
  geom_text(
    data = filter(data_fig_d, metric == "0%",Mineral == "Cobalt"),
    aes(label = Mineral),
    nudge_y = 600,
    nudge_x = -80,
    size = label_text * 5 / 14 * 0.8,
    hjust = 0.5
  ) +
  coord_cartesian(xlim = c(360, 720), ylim = c(41.9e3, 43400), expand = FALSE, clip = "on") +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = c(400, 500, 600, 700), labels = scales::label_comma()) +
  scale_y_continuous(breaks = c(42e3, 42.5e3, 43e3), labels = dollar_format(big.mark = ",", prefix = "$")) +
  theme(
    axis.title = element_text(color = "#999999"),
    panel.border = element_rect(color = "#999999", fill = NA),
    axis.text = element_text(size = 4, color = "#999999"),
    plot.margin = margin(0, 0, 0, 0),
    panel.spacing = unit(0, "pt")
  )


library(patchwork)
p_d_fig <- p_d +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = "d",
    hjust = 1.2,
    vjust = 1.2,
    fontface = "bold",
    size = 14 * 5 / 14 * 0.8,
    colour = "black"
  ) +
  labs(title = "Fish Index < 70") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # fmt: skip
  annotate("segment",xend = 3000,y = 8e3,x = 15000,yend = 8e3,arrow = arrow(length = unit(0.15, "cm")),linewidth = 0.2,colour = "#999999") +
  # fmt: skip
  annotate("segment",xend = 2000,y = 42500,x = 15000,yend = 42500,arrow = arrow(length = unit(0.15, "cm")),linewidth = 0.2,colour = "#999999") +
  inset_element(pd_nickel, left = 0.35, bottom = 0.02, right = 0.78, top = 0.32) +
  inset_element(pd_cobalt, left = 0.35, bottom = 0.68, right = 0.78, top = 0.98)
p_d_fig
# fmt: skip
ggsave("Figures/Fig2D.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)


# PANEL E - DESALINATION COST CURVE (NZE) -----------------------
runs <- list.files(
  "Results/Optimization/DesCostScenario/NZE/FI100",
  pattern = "Metrics.*",
  recursive = TRUE,
  full.names = TRUE
)
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
      Scenario == "DC025" ~ "$0.25/m3",
      Scenario == "DC05" ~ "$0.5/m3",
      Scenario == "DC1" ~ "$1.0/m3",
      Scenario == "DC15" ~ "$1.5/m3",
      Scenario == "DC2" ~ "$2.0/m3",
      Scenario == "DC25" ~ "$2.5/m3",
      Scenario == "DC5" ~ "$5.0/m3",
      Scenario == "DC10" ~ "$10/m3",
      TRUE ~ Scenario
    ) |>
      factor(levels = c("$0.25/m3", "$0.5/m3", "$1.0/m3", "$1.5/m3", "$2.0/m3", "$2.5/m3", "$5.0/m3", "$10/m3"))
  )

table(obj$Scenario)
table(obj$metric)
table(obj$Parameter)


df_des <- obj |>
  filter(!is.na(Scenario)) |>
  dplyr::select(-Units) |>
  filter(!str_detect(metric, "Water")) |>
  pivot_wider(names_from = Parameter, values_from = Value) |>
  mutate(
    Water_cons = Water / 1e3, # billion m3
    Cost = Cost / 1e3, # billion USD (discounted)
    Water_Impact = `Water impact` / 1e3 # billion m3 world-eq
  ) |>
  mutate(`Water impact` = NULL) |>
  arrange(Scenario, metric)

# Get % of water reduction between first point
base_water <- df_des |>
  filter(metric == "0%") |>
  rename(water_base = Water_Impact) |>
  dplyr::select(Scenario, water_base)

head(df_des)

df_des <- df_des |> dplyr::select(Scenario, metric, Cost, Water_Impact)


df_des <- df_des |> mutate(desCost = as.numeric(str_remove_all(Scenario, "\\$|\\/|m3")))
table(df_des$desCost)

# add NZE no desalianation demand
df_demand_aux <- df_demand |>
  filter(Scenario == "NZE") |>
  dplyr::select(Cost, Water_Impact, metric) |>
  mutate(Scenario = "NZE", desCost = 25, Scenario = "No Desalination")
df_des <- rbind(df_des, df_demand_aux)

# Contour polygons
ext_pts_c <- df_des |>
  group_by(desCost) |>
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

extra_l_c <- ext_pts_c |>
  rowwise() |>
  mutate(Water_Impact = list(seq(min(df_des$Water_Impact) * 0.9, x_left, length.out = 30)[-30])) |>
  tidyr::unnest(Water_Impact) |>
  mutate(Cost = y_left + s_left * (Water_Impact - x_left)) |>
  ungroup() |>
  select(desCost, Water_Impact, Cost)

extra_r_c <- ext_pts_c |>
  rowwise() |>
  mutate(Water_Impact = list(seq(x_right, max(df_des$Water_Impact) * 1.1, length.out = 30)[-1])) |>
  tidyr::unnest(Water_Impact) |>
  mutate(Cost = y_right + s_right * (Water_Impact - x_right)) |>
  ungroup() |>
  select(desCost, Water_Impact, Cost)

data_fig_ext_e <- bind_rows(df_des |> select(desCost, Water_Impact, Cost), extra_l_c, extra_r_c) |>
  arrange(desCost, Water_Impact)

# add fake curve below minimium to show polygon fill
data_fig_ext_e <- rbind(
  data_fig_ext_e,
  filter(data_fig_ext_e, desCost == 0.25) |> mutate(desCost = 0, Cost = Cost - 2000),
  filter(data_fig_ext_e, desCost == 25) |> mutate(desCost = 30, Cost = Cost + 10000)
)

scens_e <- sort(unique(data_fig_ext_e$desCost), decreasing = TRUE)

bands_e <- purrr::map_dfr(seq_len(length(scens_e) - 1), function(i) {
  s1 <- scens_e[i]
  s2 <- scens_e[i + 1]
  d1 <- filter(data_fig_ext_e, desCost == s1)
  d2 <- filter(data_fig_ext_e, desCost == s2)
  tibble(
    Water_Impact = c(d1$Water_Impact, rev(d2$Water_Impact)),
    Cost = c(d1$Cost, rev(d2$Cost)),
    band = paste0(s1, "-", s2),
    band_legend = s1
  )
})


data_fig_e <- df_des
df_opt_e <- df_des |> filter(metric == "0%", Scenario == "No Desalination")

write.csv(data_fig_a, "Figures/Data_Figures/Fig2e.csv", row.names = FALSE)


p_e <- ggplot(data_fig_e, aes(Water_Impact, Cost, col = Scenario)) +
  # geom_polygon(
  #   data = bands_e,
  #   aes(Water_Impact, Cost, group = band, fill = band_legend),
  #   alpha = 0.5,
  #   color = "white",
  #   linewidth = 0.1,
  #   inherit.aes = FALSE
  # ) +
  # scale_fill_gradientn(
  #   colours = rev(c("#EBCF2EFF", "#B4BF3AFF", "#88AB38FF", "#5E9432FF", "#3B7D31FF", "#225F2FFF", "#244422FF"))
  # ) +
  geom_vline(xintercept = df_opt_a$Water_Impact, linetype = "dashed", col = "#999999", linewidth = 0.3) +
  geom_hline(yintercept = df_opt_a$Cost, linetype = "dashed", col = "#999999", linewidth = 0.3) +
  geom_line(linewidth = 0.8) +
  # geom_point(size = 0.5) +
  geom_text(
    data = filter(data_fig_e, metric == "0%",!(Scenario %in% c("$2.0/m3","$0.25/m3","$1.5/m3"))),
    aes(label = Scenario),
    nudge_y = 60 * c(-0.85,-0.3,0,0.3,0.8,-1),
    nudge_x = 500 * c(0,0.7,0.8,0.7,0.5,-1.6),
    size = label_text * 5 / 14 * 0.8,
    hjust = 0.5,
    lineheight = 0.7
  ) +
  # fmt: skip
  annotate("text",x = df_opt_e$Water_Impact + 100,y = df_opt_e$Cost + 100,
    label = "Optimal Cost",col = "#999999",size = label_text * 5 / 14 * 0.8,hjust = 0.1,angle = 90) +
  geom_point(data = df_opt_e, col = "#999999", size = 0.6) +
  labs(
    x = expression("Freshwater Impact 2025-2050 (billion " ~ m^3 * "-eq)"),
    y = "Cost 2025-2050 (billion USD)",
    col = ""
  ) +
  scale_color_manual(values = rev(RColorBrewer::brewer.pal(9, "Spectral"))) +
  scale_y_continuous(
    labels = dollar_format(big.mark = ",", prefix = "$"),
    sec.axis = sec_axis(
      ~ (. - df_opt_e$Cost) / df_opt_e$Cost,
      name = "Change in Cost relative to Optimal Cost NZE (%)",
      labels = scales::percent
    )
  ) +
  scale_x_continuous(
    labels = scales::label_comma(),
    sec.axis = sec_axis(
      ~ (. - df_opt_e$Water_Impact) / df_opt_e$Water_Impact,
      name = "Change in Freshwater Impact relative to Optimal Cost NZE (%)",
      labels = scales::percent
    )
  ) +
  guides(color = "none", fill = "none") +
  coord_cartesian(
    xlim = c(min(data_fig_e$Water_Impact) * 0.95, max(data_fig_e$Water_Impact) * 1.05),
    ylim = c(min(data_fig_e$Cost) * 0.95, max(data_fig_e$Cost) * 1.05),
    expand = FALSE
  ) +
  theme_pb_large() +
  theme(
    legend.position = "none",
    axis.title.y.right = element_text(size = 7),
    axis.text.y.right = element_text(size = 7),
    axis.title.x.top = element_text(size = 7),
    axis.text.x.top = element_text(size = 7)
  )
p_e

# fmt: skip
ggsave("Figures/Fig2_Demand_Des.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)


# ============================================================
# PANEL F - MINERAL DECOMPOSITION (Des Cost at 0.5 m3) ------------
# ============================================================

data_fig_f <- mineral_decomp_CostDes |>
  mutate(metric = factor(metric, levels = metric_levels)) |>
  filter(Scenario == "$0.5/m3", metric != "No Water Constraint") |>
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

names(data_fig_f) <- names(data_fig_f) |> str_remove("perTon")

write.csv(data_fig_f, "Figures/Data_Figures/Fig2f_Mineral05CostDes.csv", row.names = FALSE)

p_f <- ggplot(data_fig_f, aes(Water, Cost, col = Mineral)) +
  geom_line() +
  # geom_point(size = 0.5) +
  geom_text(
    data = filter(data_fig_f, metric == "0%",!(Mineral %in% c("Cobalt","Nickel"))),
    aes(label = Mineral),
    nudge_y = 1000 * c(-1, -1),
    nudge_x = c(0,-1000),
    size = label_text * 5 / 14 * 0.8,
    hjust = 0.5
  ) +
  labs(x = expression("Freshwater Impact (" ~ m^3 * "-eq per ton)"), y = "Cost (USD per ton)", col = "") +
  scale_y_continuous(labels = dollar_format(big.mark = ",", prefix = "$")) +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_color_manual(values = minerals_colors) +
  # coord_cartesian(expand = F, xlim = c(0, 45000), ylim = c(0, 32000)) +
  theme_pb_large() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title.y.right = element_text(size = 5),
    axis.text.y.right = element_text(size = 5),
    axis.title.x.top = element_text(size = 5),
    axis.text.x.top = element_text(size = 5),
    strip.placement = "outside"
  )
p_f

# Zoom version cobalt and nickel
data_fig_f |> group_by(Mineral) |> reframe(max_w = max(Water), min_w = min(Water), max_c = max(Cost), min_c = min(Cost))
pf_nickel <- p_f %+%
  (data = filter(data_fig_f, Mineral == 'Nickel')) +
  geom_text(
    data = filter(data_fig_f, metric == "0%",Mineral == "Nickel"),
    aes(label = Mineral),
    nudge_y = 150,
    nudge_x = -100,
    size = label_text * 5 / 14 * 0.8,
    hjust = 0.5
  ) +
  coord_cartesian(xlim = c(340, 770), ylim = c(6910, 7190), expand = FALSE, clip = "on") +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = c(400, 500, 600, 700), labels = scales::label_comma()) +
  scale_y_continuous(breaks = c(7000, 7100), labels = dollar_format(big.mark = ",", prefix = "$")) +
  theme(
    axis.title = element_text(color = "#999999"),
    panel.border = element_rect(color = "#999999", fill = NA),
    axis.text = element_text(size = 4, color = "#999999"),
    plot.margin = margin(0, 0, 0, 0),
    panel.spacing = unit(0, "pt")
  )

pf_cobalt <- p_f %+%
  (data = filter(data_fig_f, Mineral == 'Cobalt')) +
  geom_text(
    data = filter(data_fig_f, metric == "0%",Mineral == "Cobalt"),
    aes(label = Mineral),
    nudge_y = 400,
    nudge_x = -40,
    size = label_text * 5 / 14 * 0.8,
    hjust = 0.5
  ) +
  coord_cartesian(xlim = c(650, 810), ylim = c(10400, 11300), expand = FALSE, clip = "on") +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = c(700, 800), labels = scales::label_comma()) +
  scale_y_continuous(breaks = c(10500, 11000), labels = dollar_format(big.mark = ",", prefix = "$")) +
  theme(
    axis.title = element_text(color = "#999999"),
    panel.border = element_rect(color = "#999999", fill = NA),
    axis.text = element_text(size = 4, color = "#999999"),
    plot.margin = margin(0, 0, 0, 0),
    panel.spacing = unit(0, "pt")
  )


library(patchwork)
p_f_fig <- p_f +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = "f",
    hjust = 1.2,
    vjust = 1.2,
    fontface = "bold",
    size = 14 * 5 / 14 * 0.8,
    colour = "black"
  ) +
  labs(title = bquote(bold("Desalination Cost $0.5 " ~ "per m"^3))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  # fmt: skip
  annotate("segment",xend = 2000,y = 7000,x = 13500,yend = 7000,arrow = arrow(length = unit(0.15, "cm")),linewidth = 0.2,colour = "#999999") +
  # fmt: skip
  annotate("segment",xend = 2000,y = 11000,x = 13500,yend = 11000,arrow = arrow(length = unit(0.15, "cm")),linewidth = 0.2,colour = "#999999") +
  inset_element(pf_nickel, left = 0.25, bottom = 0.07, right = 0.68, top = 0.37) +
  inset_element(pf_cobalt, left = 0.25, bottom = 0.37, right = 0.68, top = 0.67)
p_f_fig
# fmt: skip
ggsave("Figures/Fig2F.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)


# ============================================================
# ASSEMBLE FIGURE ---------------------------------------
# ============================================================
library(patchwork)
# fmt: skip
p_a_fig <- p_a + annotate("text",x = Inf, y = Inf,label = "a",hjust = 1.2, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black")+
  labs(title="Mineral Demand Levels")+theme(plot.title = element_text(hjust = 0.5,face = "bold"))
# fmt: skip
p_c_fig <- p_c + annotate("text",x = Inf, y = Inf,label = "c",hjust = 1.2, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black")+
  labs(title="Fish Index Levels")+theme(plot.title = element_text(hjust = 0.5))
# fmt: skip
p_e_fig <- p_e + annotate("text",x = Inf, y = Inf,label = "e",hjust = 1.2, vjust = 1.2,fontface = "bold",size = 14 * 5 / 14 * 0.8,colour = "black")+
  labs(title="Cost Desalination Levels")+theme(plot.title = element_text(hjust = 0.5))


fig2 <- (p_a_fig | p_b_fig) / (p_c_fig | p_d_fig) / (p_e_fig | p_f_fig) + plot_layout(guides = "collect")
fig2
ggsave("Figures/Figure2.png", fig2, units = "cm", dpi = 600, width = 8.8 * 2, height = 8.7 * 3)
ggsave("Figures/Figure2.svg", fig2, units = "cm", dpi = 600, width = 8.8 * 2, height = 8.7 * 3)

# EoF
