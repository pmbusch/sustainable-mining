# Comparison Map between cost degradation scenarios
# PBH January 2026

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
source('Scripts/00a-Common Variables.R', encoding = 'UTF-8')
(dict_scen <- tibble(Scenario = scens_selected, name = name_abbr))

demand <- read.csv("Parameters/Cu_Demand.csv")
deposit <- read.csv("Parameters/Cu_Deposit.csv")

url_results <- "Results/Optimization/Copper"

(runs <- list.files(url_results, recursive = T, full.names = TRUE) |>
  keep(~ str_detect(basename(.x), "^(Base.*|Water.*)$") && !str_detect(basename(.x), "_")))


source("Scripts/01-LoadOptimizationResults.R", encoding = "UTF-8")

table(df_results$name)
table(df_results$cost_deg)

data_fig <- df_results %>%
  filter(Scenario %in% c("Ambitious-Baseline-Baseline-Baseline-Baseline")) |> # reference scenario
  filter(cost_deg %in% c(0, 5)) |>
  left_join(deposit) %>%
  filter(t < 2051) %>%
  # filter(t==2050) %>% # uncomment for 2050 analysis
  mutate(water_m3 = ktons_extracted * water_footprint / 1e3) |> # million m3
  group_by(Name, cost_deg) %>%
  reframe(mtons = sum(ktons_extracted) / 1e3, water_m3 = sum(water_m3) / 1e3) |> # water to billion
  ungroup()

head(data_fig)
# pivot data wider
data_fig <- data_fig %>%
  pivot_wider(names_from = cost_deg, values_from = c(mtons, water_m3), names_prefix = "cost_deg_") |>
  mutate(
    # 5% cost vs 0% cost deg
    net = mtons_cost_deg_5 - mtons_cost_deg_0,
    net = if_else(near(net, 0), 0, net),
    net_water = water_m3_cost_deg_5 - water_m3_cost_deg_0,
    net_water = if_else(near(net_water, 0), 0, net_water),
    net_abs = abs(net),
    net_water_abs = abs(net_water),
    change = if_else(net_abs < 1, "No change", "Change") # less than 1 M tons in total cumulative production
  )

# Add lat long
data_fig <- data_fig %>% left_join(dplyr::select(deposit, Name, LATITUDE, LONGITUDE), by = "Name")

# Remove deposits no production
data_fig <- data_fig |> filter((mtons_cost_deg_0 + mtons_cost_deg_5) > 0)

# Save to plot
write.csv(data_fig, "Results/DataMap_5deg_0deg.csv", row.names = F)


## ACTUAL FIGURE IS MADE IN QGIS, simply loading this csv and adding a world basemap behing
# with appropiate labels, legends and so on.

# reorder for plotting
data_fig <- data_fig %>% arrange(net_abs)

range(data_fig$net)
map1 <- map_data('world')
p1 <- ggplot(data_fig) +
  # base map
  theme_minimal(8) +
  geom_polygon(data = map1, mapping = aes(x = long, y = lat, group = group), col = 'gray', fill = "white") +
  geom_point(aes(x = LONGITUDE, y = LATITUDE,size=mtons_cost_deg_5), alpha = 0.7,   shape  = 21,fill="darkgrey", colour = "black", stroke = 0.25,data=filter(data_fig,change=="No change")) + # no change
  geom_point(aes(x = LONGITUDE, y = LATITUDE,fill=net,size=mtons_cost_deg_5),alpha = 0.7,   shape  = 21, colour = "black", stroke = 0.25,data=filter(data_fig,change!="No change")) +
  coord_fixed(1.4, xlim = c(-140, 160), ylim = c(-60, 70)) +
  # coord_fixed(1.4, xlim = c(-75,-60), ylim=c(-40,-10))+ #Li Triangle
  scale_y_continuous(breaks = NULL, name = "") +
  scale_x_continuous(breaks = NULL, name = "") +
  scale_fill_gradientn(
    colours = rev(RColorBrewer::brewer.pal(8, "Spectral")),
    values = scales::rescale(c(min(data_fig$net, na.rm = TRUE), 0, max(data_fig$net, na.rm = TRUE))),
    labels = scales::label_comma(),
    breaks = c(-150, -100, -50, 0, 50)
  ) +
  scale_size_continuous(trans = "sqrt") +
  guides(
    fill = guide_colorbar(direction = "horizontal", barwidth = unit(6, "cm"), barheight = unit(0.25, "cm")),
    size = guide_legend(direction = "horizontal", nrow = 1, byrow = TRUE, title.position = "left")
  ) +
  labs(
    fill = "Variation in copper production (2025-2050, million tons)\n between scenarios: Cost Optimal vs 5% Cost Degradation",
    title = "(a) Copper Deposits",
    size = "Production in 5% Cost Degradation Scenario\n(2025-2050, million tons)"
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.5, 0.1),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 5),
    plot.margin = margin(1, 1, 1, 1),
    legend.key.height = unit(0.25, 'cm'),
    legend.key.width = unit(0.25, 'cm')
  )
p1

# fmt: skip
ggsave("Figures/Copper_variationMap.png", ggplot2::last_plot(),units = 'cm', dpi = 600, width = 8.7*3, height = 8.7*2)

data_fig |>
  filter(!near(net_abs, 0)) |>
  ggplot(aes(reorder(Name, net), net)) +
  geom_col() +
  coord_flip() +
  theme_bw(8) +
  theme(panel.grid = element_blank())

# Map of Water Scarcity -------------

# estimates new consumption per basin
head(df_results)

water_impact <- df_results |>
  filter(Scenario %in% c("Ambitious-Baseline-Baseline-Baseline-Baseline")) |> # reference scenario
  filter(cost_deg %in% c(0, 5)) |>
  left_join(deposit) %>%
  filter(t < 2051) %>%
  mutate(water_cons = ktons_extracted * water * 1e3) |> # m3 consumed (no impact factor)
  group_by(Basin_ID, cost_deg, t) %>%
  reframe(
    tons = sum(ktons_extracted) * 1e3,
    water_cons = sum(water_cons),
    aware_demand = mean(aware_demand),
    aware_available = mean(aware_available)
  ) |>
  group_by(Basin_ID, cost_deg) %>%
  # get maximum annual water consumption over the period (not cumulative)
  reframe(
    tons = max(tons),
    water_cons = max(water_cons),
    aware_demand = mean(aware_demand),
    aware_available = mean(aware_available)
  ) |>
  ungroup() |>
  mutate(
    share_consumed = if_else(near(aware_demand, 0), 0, water_cons / aware_demand),
    share_available = water_cons / aware_available,
    water_perTon = water_cons / tons
  ) |>
  arrange(desc(share_available))

water_impact

water_impact |> filter(Basin_ID == 61501)

# EoF
