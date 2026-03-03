# Shadow prices (or Lagrangian multipliers) for constraints in the optimal allocation model
# Indicate the marginal value change of the objective function (Cost OR freshwater impact) for active constraints
# Obtained directly from the optimization model output - by setting binary variables as fixed
# PBH Feb 2026

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
deposit <- read.csv("Parameters/Deposit.csv")

# Basin shadow prices ---------------
# USD savings per extra m3 of water allowed in each basin

(runs_sp <- list.files("Results/Optimization/DemandScenario", pattern = "Basin.*", recursive = T, full.names = TRUE))
sp <- do.call(
  rbind,
  lapply(runs_sp, function(folder_path) {
    transform(read.csv(folder_path), file_name = basename(folder_path), Scenario = basename(dirname(folder_path)))
  })
) |>
  rename(Year = t)
head(sp)
table(sp$file_name)

# Only shadow prices at cost optimal
sp <- sp |> filter(!str_detect(file_name, "Eps"))


## Time series ------------
sp_value <- sp |> filter(abs(shadow) > 0) |> filter(Scenario == "NZE") |> mutate(shadow = -shadow) # convert to savings
length(unique(sp_value$Basin_ID)) # 26

# Country by basin - pick the country with the largest resources in the deposits
basin_country <- deposit |>
  mutate(total_resources = resources_Copper + resources_Nickel + resources_Cobalt + resources_Lithium) |>
  group_by(Basin_ID, country) |>
  summarise(total_resources = sum(total_resources), .groups = "drop") |>
  group_by(Basin_ID) |>
  slice_max(total_resources, n = 1, with_ties = FALSE) |>
  ungroup()

sp_value |> left_join(basin_country) |> pull(country) |> unique() # 9 countries

dict_region <- read_excel("Inputs/Dictionaries/Dict_Countries_SP.xlsx", sheet = "Dict")

region_colors <- c(
  "CHL" = "#6a3d9a",
  "DRC" = "#4682b4",
  "PER" = "#8b4513",
  "IDN" = "#fdb462",
  "RUS" = "#756bb1",
  "USA" = "#c4dfbe",
  "CHN" = "#d74c5a",
  "ARG" = "#ff7f00",
  "MNG" = "#e31a1c",
  "PHL" = "#1f78b4",
  "BRA" = "#33a02c",
  "NCL" = "#b15928",
  "AUS" = "#fb9a99",
  "Europe" = "#2b8cbe",
  "MEX" = "#66c2a5",
  "MAR" = "#8dd3c7",
  "TZA" = "#ffffb3",
  "SDN" = "#bebada",
  "ZMB" = "#fb8072",
  "COL" = "#80b1d3",
  "VNM" = "#fdb462",
  "UZB" = "#1f78b4",
  "RoW" = "#4d4d4d"
)

# add ISO codes
sp_value <- sp_value |> left_join(basin_country) |> left_join(dict_region)


# # Pick 14 largest as basins
# sel_basins <- sp_value |> arrange(desc(shadow)) |> pull(Basin_ID) |> unique()
# sp_value$Basin_ID = factor(sp_value$Basin_ID, levels = sel_basins)
# colors_basins <- paletteer::paletteer_d("ggthemes::Classic_Cyclic", n = 13)
# # rest are greys
# colors_basins <- c(colors_basins, rep("#bdbdbd", length(levels(sp_value$Basin_ID)) - length(colors_basins)))
# names(colors_basins) <- levels(sp_value$Basin_ID)

# Undiscount things
optInputs <- read.csv("Results/Optimization/DemandScenario/SPS/OptimizationInputs.csv")
(r <- optInputs |> filter(Parameter == "Discount rate") |> pull(Value)) # 7%
sp_value <- sp_value |> mutate(shadow = shadow * (1 + r)^(Year - 2025))


# Pick some lines for labeling
sp_value_text <- sp_value |>
  filter(shadow < 300) |>
  group_by(ISO3) |>
  filter(Year == max(Year)) |>
  filter(shadow == max(shadow))

n_basins <- length(unique(sp_value$Basin_ID))

desalination_cost <- 0.5 # USD/m3
range(sp_value$shadow)
p_line <- ggplot(sp_value, aes(Year, shadow, col = ISO3, group = Basin_ID)) +
  geom_line(alpha = 0.8) +
  # facet_wrap(~Scenario) +
  geom_hline(yintercept = desalination_cost, linetype = "dashed") +
  geom_text(data=sp_value_text,aes(label=ISO3),size = 6 * 5 / 14 * 0.8,nudge_x=1,nudge_y=c(0,0,0,0,5,0,0,-5)) +
  # fmt: skip
  annotate("text",x=2030,y=300,label=paste0(n_basins," basins constrained by water availability"),color="black",size=7*5/14*0.8,hjust=0) +
  # fmt: skip
  annotate("text", x = 2032, y = -5, label = paste0("'Desalination cost: ' * " ,desalination_cost, " * ' USD/m'^3"), color = "black", size = 7*5/14*0.8,parse=T,hjust=0) +
  # scale_y_continuous(trans = "log10", labels = dollar_format(big.mark = " ", prefix = "$")) +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$")) + # removes one outlier
  coord_cartesian(xlim = c(2030, 2051), ylim = c(-5, 300)) +
  # scale_color_manual(values = colors_basins) +
  scale_color_manual(values = region_colors) +
  theme_pb_wide() +
  labs(x = "", y = "", title = expression("Avoided cost per extra " * m^3 * "of water allowed in basin")) +
  theme(legend.position = "none")
p_line

# fmt: skip
ggsave("Figures/Fig3_CostBasin.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)

## Bar plot showing locked resources in these basins

locked_res <- deposit |>
  filter(Basin_ID %in% unique(sp_value$Basin_ID)) |>
  left_join(dict_region) |>
  group_by(ISO3) |>
  reframe(
    resources_Copper = sum(resources_Copper) / 1e6,
    resources_Nickel = sum(resources_Nickel) / 1e6,
    resources_Cobalt = sum(resources_Cobalt) / 1e6,
    resources_Lithium = sum(resources_Lithium) / 1e6
  ) |>
  ungroup() |>
  pivot_longer(c(-ISO3), names_to = 'Mineral', values_to = 'mtons') |>
  mutate(
    Mineral = str_remove(Mineral, "resources_") |> factor(levels = rev(c("Copper", "Nickel", "Cobalt", "Lithium")))
  )

# labels
locked_res <- locked_res |> mutate(label = if_else(mtons > 100, ISO3, ""))

p_res <- ggplot(locked_res, aes(Mineral, mtons, fill = ISO3)) +
  geom_col(col="black",linewidth=0.1) +
  geom_text(aes(label=label), position = position_stack(vjust = 0.5), size = 6 * 5 / 14 * 0.8) +
  coord_flip(expand = F) +
  scale_fill_manual(values = region_colors) +
  labs(x = "", y = "", title = "Locked mineral resources in water constrained basins, in million tons") +
  theme_pb_small() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 4),
    axis.title = element_text(size = 4),
    plot.title = element_text(size = 4, hjust = 0.3, face = "plain")
  )
p_res

library(cowplot)
ggdraw() + draw_plot(p_line) + draw_plot(p_res, x = 0.15, y = 0.6, width = 0.5, height = 0.25)
# fmt: skip
ggsave("Figures/Fig3_CostBasin.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)


# Same figure but with Fish Index <70 ----------------

(runs_sp <- list.files("Results/Optimization/BioDScenario/NZE/", pattern = "Basin.*", recursive = T, full.names = TRUE))
sp <- do.call(
  rbind,
  lapply(runs_sp, function(folder_path) {
    transform(read.csv(folder_path), file_name = basename(folder_path), Scenario = basename(dirname(folder_path)))
  })
) |>
  rename(Year = t)
head(sp)
table(sp$file_name)

# Only shadow prices at cost optimal
sp <- sp |> filter(!str_detect(file_name, "Eps"))


## Time series ------------
# Filte by fihs index <70
sp_value <- sp |> filter(abs(shadow) > 0) |> filter(str_detect(Scenario, "70")) |> mutate(shadow = -shadow) # convert to savings
length(unique(sp_value$Basin_ID)) # 48

# Country by basin - pick the country with the largest resources in the deposits
basin_country <- deposit |>
  mutate(total_resources = resources_Copper + resources_Nickel + resources_Cobalt + resources_Lithium) |>
  group_by(Basin_ID, country) |>
  summarise(total_resources = sum(total_resources), .groups = "drop") |>
  group_by(Basin_ID) |>
  slice_max(total_resources, n = 1, with_ties = FALSE) |>
  ungroup()

sp_value |> left_join(basin_country) |> pull(country) |> unique() # 18 countries


# add ISO codes
sp_value <- sp_value |>
  left_join(basin_country) |>
  left_join(dict_region) |>
  mutate(ISO3 = if_else(ISO3 %in% names(region_colors), ISO3, "RoW") |> str_replace("COD", "DRC"))


# Undiscount things
optInputs <- read.csv("Results/Optimization/DemandScenario/SPS/OptimizationInputs.csv")
(r <- optInputs |> filter(Parameter == "Discount rate") |> pull(Value)) # 7%
sp_value <- sp_value |> mutate(shadow = shadow * (1 + r)^(Year - 2025))


# Pick some lines for labeling
sp_value_text <- sp_value |>
  filter(shadow < 2000) |>
  group_by(ISO3) |>
  filter(Year == max(Year)) |>
  filter(shadow == max(shadow))

n_basins <- length(unique(sp_value$Basin_ID))

desalination_cost <- 0.5 # USD/m3
range(sp_value$shadow)
p_line_basin <- ggplot(sp_value, aes(Year, shadow, col = ISO3, group = Basin_ID)) +
  geom_line(alpha = 0.8) +
  # facet_wrap(~Scenario) +
  geom_hline(yintercept = desalination_cost, linetype = "dashed") +
  geom_text_repel(data = sp_value_text, aes(label = ISO3), size = 6 * 5 / 14 * 0.8, nudge_x = 1) +
  # fmt: skip
  annotate("text",x=2030,y=1790,label=paste0(n_basins," basins constrained by water availability"),color="black",size=7*5/14*0.8,hjust=0) +
  # fmt: skip
  annotate("text", x = 2032, y = -30, label = paste0("'Desalination cost: ' * " ,desalination_cost, " * ' USD/m'^3"), color = "black", size = 7*5/14*0.8,parse=T,hjust=0) +
  # scale_y_continuous(trans = "log10", labels = dollar_format(big.mark = " ", prefix = "$")) +
  scale_y_continuous(labels = dollar_format(big.mark = ",", prefix = "$")) + # removes an outlier
  coord_cartesian(ylim = c(-30, 1800), xlim = c(2030, 2051)) +
  # scale_color_manual(values = colors_basins) +
  scale_color_manual(values = region_colors) +
  theme_pb_wide() +
  labs(
    x = "",
    y = "",
    title = expression("Avoided cost per extra " * m^3 * "of water allowed"),
    subtitle = "Including only basins with Fish Index < 70"
  ) +
  theme(legend.position = "none")
p_line_basin


## Bar plot showing locked resources in these basins

locked_res <- deposit |>
  filter(Basin_ID %in% unique(sp_value$Basin_ID)) |>
  left_join(dict_region) |>
  mutate(ISO3 = if_else(ISO3 %in% names(region_colors), ISO3, "RoW") |> str_replace("COD", "DRC")) |>
  group_by(ISO3) |>
  reframe(
    resources_Copper = sum(resources_Copper) / 1e6,
    resources_Nickel = sum(resources_Nickel) / 1e6,
    resources_Cobalt = sum(resources_Cobalt) / 1e6,
    resources_Lithium = sum(resources_Lithium) / 1e6
  ) |>
  ungroup() |>
  pivot_longer(c(-ISO3), names_to = 'Mineral', values_to = 'mtons') |>
  mutate(
    Mineral = str_remove(Mineral, "resources_") |> factor(levels = rev(c("Copper", "Nickel", "Cobalt", "Lithium")))
  )

# labels
locked_res <- locked_res |> mutate(label = if_else(mtons > 120, ISO3, ""))

p_res_basin <- ggplot(locked_res, aes(Mineral, mtons, fill = ISO3)) +
  geom_col(col="black",linewidth=0.1) +
  geom_text(aes(label=label), position = position_stack(vjust = 0.5), size = 6 * 5 / 14 * 0.8) +
  coord_flip(expand = F) +
  scale_fill_manual(values = region_colors) +
  labs(x = "", y = "", title = "Locked mineral resources in water constrained basins, in million tons") +
  theme_pb_small() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_text(size = 4),
    axis.title = element_text(size = 4),
    plot.title = element_text(size = 4, hjust = 0.3, face = "plain")
  )
p_res_basin

library(cowplot)
ggdraw() + draw_plot(p_line_basin) + draw_plot(p_res_basin, x = 0.15, y = 0.5, width = 0.5, height = 0.25)
# fmt: skip
ggsave("Figures/Fig3_CostBasin_FishIndex70.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)


## Basemap of basins from AWARE ------------
# sp_map <- sp |>
#   filter(Year == 2030) |>
#   filter(Scenario == "SPS") |>
#   mutate(shadow = if_else(near(shadow, 0), NA, shadow)) |>
#   mutate(shadow = -shadow) |>
#   arrange(desc(shadow))

unzip("Inputs/AWARE/AWARE20_Native_CFs_geospatial.kmz", exdir = "kmz_unzip")
k <- st_read("kmz_unzip/doc.kml")
cf_map <- st_make_valid(k) |> st_zm() |> st_cast("MULTIPOLYGON")
cf_map$Basin_ID <- as.numeric(str_remove(cf_map$Name, "CFs for Basin_ID "))
cf_map <- left_join(cf_map, sp, by = "Basin_ID")
cf_map <- cf_map |> filter(Basin_ID %in% sel_basins)
cf_map$Basin_ID <- factor(cf_map$Basin_ID)

map1 <- map_data('world')


# Bounding box to zoom into basins with data
bbox <- st_bbox(cf_map)
pad <- 5 # degrees of padding
pmap <- ggplot(cf_map) +
  # base map
  theme_minimal(8) +
  geom_polygon(data = map1, mapping = aes(x = long, y = lat, group = group), col = 'gray', fill = "white") +
  # Water stress map
  geom_sf(data = cf_map, aes(fill = Basin_ID), color = "grey30", linewidth = 0.1) +
  coord_sf(xlim = c(bbox["xmin"] - pad, bbox["xmax"] + pad), ylim = c(bbox["ymin"] - pad, bbox["ymax"] + pad)) +
  scale_fill_manual(values = colors_basins) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.border = element_blank()
  )
pmap

# South America inset map
pmap_sa <- ggplot(cf_map) +
  theme_minimal(8) +
  geom_polygon(data = map1, mapping = aes(x = long, y = lat, group = group), col = 'gray', fill = "white") +
  geom_sf(data = cf_map, aes(fill = Basin_ID), color = "grey30", linewidth = 0.1) +
  coord_sf(xlim = c(-75, -60), ylim = c(-40, -15)) +
  scale_fill_manual(values = colors_basins) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4)
  )
pmap_sa

library(cowplot)
plot_grid(pmap, pmap_sa, p_line, nrow = 1)

## total resources by basin -------------
minerals_colors <- c("Lithium" = "#00BFFF", "Copper" = "#2E8B57", "Nickel" = "#4D4D4D", "Cobalt" = "#8A2BE2")

deposit <- read.csv("Parameters/Deposit.csv")
deposit <- deposit |>
  filter(Basin_ID %in% sel_basins) |>
  group_by(Basin_ID) |>
  reframe(
    resources_Copper = sum(resources_Copper),
    resources_Nickel = sum(resources_Nickel),
    resources_Cobalt = sum(resources_Cobalt),
    resources_Lithium = sum(resources_Lithium)
  ) |>
  ungroup()
deposit <- deposit |>
  pivot_longer(c(-Basin_ID), names_to = 'Mineral', values_to = 'resources') |>
  mutate(Mineral = str_remove(Mineral, "resources_")) |>
  mutate(resources = resources / 1e6) # million tons

# Add continent info via dict_region
dict_region <- read_excel("Inputs/Dictionaries/Dict_Countries_SP.xlsx", sheet = "Dict")
basin_continent <- read.csv("Parameters/Deposit.csv") |>
  select(Basin_ID, country) |>
  distinct() |>
  left_join(dict_region, by = "country") |>
  select(Basin_ID, Continent) |>
  distinct()
deposit <- deposit |> left_join(basin_continent, by = "Basin_ID")

# Order basins by continent then by sel_basins order
deposit$Basin_ID <- factor(deposit$Basin_ID, levels = rev(sel_basins))

# Color by basin
deposit$X <- -1

p_bar <- ggplot(deposit, aes(y = Basin_ID, x = resources, fill = Mineral)) +
  geom_col() +
  geom_point(aes(x=X,col=Basin_ID),size=3,show.legend = FALSE) +
  scale_color_manual(values = colors_basins, guide = "none") +
  scale_fill_manual(values = minerals_colors) +
  ggforce::facet_col(~Continent, scales = "free_y", space = "free") +
  theme_pb_wide() +
  labs(y = "", x = "Contained Mineral resources per basin, in million tons") +
  theme(legend.position = c(0.7, 0.3), axis.text.y = element_blank(), axis.ticks.y = element_blank())
p_bar


plot_grid(
  plot_grid(plot_grid(pmap, pmap_sa, nrow = 1, rel_widths = c(0.8, 0.2)), p_bar, ncol = 1, rel_heights = c(.3, .7)),
  p_line,
  nrow = 1
)

# fmt: skip
ggsave("Figures/Basin_ShadowPrices.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*3, height = 8.7*2)

# Shadow price cost  by Mineral ---------
(runs_sp_cost <- list.files(
  "Results/Optimization/DemandScenario",
  pattern = "Demand*",
  recursive = T,
  full.names = TRUE
))

# Climate scenarios
runs_sp_cost_climate <- list.files(
  "Results/Optimization/ClimateScenario",
  pattern = "Demand*",
  recursive = T,
  full.names = TRUE
)

# Biodiversity scenarios
runs_sp_cost <- list.files(
  "Results/Optimization/BioDScenario/NZE/",
  pattern = "Demand*",
  recursive = T,
  full.names = TRUE
)


# get slack costs
(slack_costs <- optInputs |>
  filter(str_detect(Parameter, "Slack")) |>
  mutate(Parameter = str_remove(Parameter, "Slack cost ")))


# Join and filter for only at cost optimal
# runs_sp_cost <- c(runs_sp_cost, runs_sp_cost_climate)
runs_sp_cost <- runs_sp_cost[!str_detect(runs_sp_cost, "Eps")] # only cost optimal scenarios

sp_cost <- do.call(
  rbind,
  lapply(runs_sp_cost, function(folder_path) {
    transform(read.csv(folder_path), folder_path = folder_path, file_name = basename(folder_path))
  })
) |>
  rename(Year = t)
head(sp_cost)

# One demand scenario for now
sp_cost <- sp_cost |> mutate(Scenario = str_extract(folder_path, "APS|SPS|NZE"))
table(sp_cost$Scenario)
sp_cost <- sp_cost |> filter(Scenario == "NZE")

# # Get climate scenarios
# table(sp_cost$folder_path)
# sp_cost <- sp_cost |>
#   mutate(
#     ClimateScen = case_when(
#       str_detect(folder_path, "picontrol") ~ "Pre-industrial control",
#       str_detect(folder_path, "ssp126") ~ "SSP1-2.6",
#       str_detect(folder_path, "ssp370") ~ "SSP3-7.0",
#       str_detect(folder_path, "ssp585") ~ "SSP5-8.5",
#       TRUE ~ "No Climate Scenario"
#     ),
#     climateDriver = case_when(
#       str_detect(folder_path, "gfdl-esm4") ~ "gfdl-esm4",
#       str_detect(folder_path, "ipsl-cm6a-lr") ~ "ipsl-cm6a-lr",
#       str_detect(folder_path, "mpi-esm1-2-hr") ~ "mpi-esm1-2-hr",
#       str_detect(folder_path, "mri-esm2-0") ~ "mri-esm2-0",
#       str_detect(folder_path, "ukesm1-0-ll") ~ "ukesm1-0-ll",
#       T ~ "No Climate Scenario"
#     )
#   )
# table(sp_cost$ClimateScen)
# table(sp_cost$climateDriver)

# Get biodiversity scenarios
table(sp_cost$folder_path)
sp_cost <- sp_cost |>
  mutate(
    Scenario = case_when(
      str_detect(folder_path, "none") ~ "All Basins",
      str_detect(folder_path, "FI99") ~ "Fish Index < 100",
      str_detect(folder_path, "FI90") ~ "Fish Index < 90",
      str_detect(folder_path, "FI80") ~ "Fish Index < 80",
      str_detect(folder_path, "FI70") ~ "Fish Index < 70",
      str_detect(folder_path, "FI60") ~ "Fish Index < 60",
      str_detect(folder_path, "FI50") ~ "Fish Index < 50",
      T ~ "AAA"
    ) |>
      factor(
        levels = c(
          "All Basins",
          "Fish Index < 100",
          "Fish Index < 90",
          "Fish Index < 80",
          "Fish Index < 70",
          "Fish Index < 60",
          "Fish Index < 50"
        )
      )
  )
table(sp_cost$Scenario)

sp_cost <- sp_cost |>
  pivot_longer(c(sp_demand_cu, sp_demand_ni, sp_demand_co, sp_demand_li), names_to = 'Mineral', values_to = 'shadow') |>
  mutate(
    Mineral = case_when(
      str_detect(Mineral, "cu") ~ "Copper",
      str_detect(Mineral, "ni") ~ "Nickel",
      str_detect(Mineral, "co") ~ "Cobalt",
      str_detect(Mineral, "li") ~ "Lithium"
    ) |>
      factor(levels = c("Copper", "Nickel", "Cobalt", "Lithium"))
  ) |>
  mutate(shadow = -shadow * 1e3) # to USD per ton (model results are in million USD per kton)

# Undiscount them
sp_cost <- sp_cost |> mutate(shadow = shadow * (1 + r)^(Year - 2025))

# WHY SHADOW PRICES > SLACK COST sometimes?
# Shadow prices of the demand constraint can exceed the per-period slack cost
# because z[t] is a dynamic backlog stock (it carries over via z[t-1]). (DEMAND UNMENT cumulates to the next period)
# Increasing demand at time t propagates forward and may raise backlog
# (or force higher production) in multiple future periods.
# The dual therefore reflects the full intertemporal marginal cost,
# not just the one-period slack penalty.

sp_cost |>
  filter(Year > 2029) |> # ignore for noise at the beginning
  ggplot(aes(Year, shadow, col = Scenario, group = Scenario)) +
  geom_line() +
  facet_wrap(~Mineral, scales = "free") +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$"), limits = c(0, NA)) +
  theme_pb_wide() +
  labs(
    x = "",
    y = "",
    title = expression("Shadow Price of mineral demand constraints (USD per ton of mineral)"),
    col = "Basins included"
  ) +
  theme(legend.position = c(0.8, 0.9))

# fmt: skip
ggsave("Figures/Metal_ShadowPrices_fish.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)


# Shadow price Water by Mineral reduction (non-cost optimal solution) ---------
(runs_sp_waterSave <- list.files(
  "Results/Optimization/DemandScenario",
  pattern = "Demand*",
  recursive = T,
  full.names = TRUE
))

# Climate scenarios
runs_sp_waterSave_climate <- list.files(
  "Results/Optimization/ClimateScenario",
  pattern = "Demand*",
  recursive = T,
  full.names = TRUE
)

# Join and filter for only at waterSave optimal
runs_sp_waterSave <- c(runs_sp_waterSave, runs_sp_waterSave_climate)
runs_sp_waterSave <- runs_sp_waterSave[str_detect(runs_sp_waterSave, "Eps05")] # pick 5% cost

sp_waterSave <- do.call(
  rbind,
  lapply(runs_sp_waterSave, function(folder_path) {
    transform(read.csv(folder_path), folder_path = folder_path, file_name = basename(folder_path))
  })
) |>
  rename(Year = t)
head(sp_waterSave)

# One demand scenario for now
sp_waterSave <- sp_waterSave |> mutate(Scenario = str_extract(folder_path, "APS|SPS|NZE"))
table(sp_waterSave$Scenario)
sp_waterSave <- sp_waterSave |> filter(Scenario == "APS")

# Get climate scenarios
table(sp_waterSave$folder_path)
sp_waterSave <- sp_waterSave |>
  mutate(
    ClimateScen = case_when(
      str_detect(folder_path, "picontrol") ~ "Pre-industrial control",
      str_detect(folder_path, "ssp126") ~ "SSP1-2.6",
      str_detect(folder_path, "ssp370") ~ "SSP3-7.0",
      str_detect(folder_path, "ssp585") ~ "SSP5-8.5",
      TRUE ~ "No Climate Scenario"
    ),
    climateDriver = case_when(
      str_detect(folder_path, "gfdl-esm4") ~ "gfdl-esm4",
      str_detect(folder_path, "ipsl-cm6a-lr") ~ "ipsl-cm6a-lr",
      str_detect(folder_path, "mpi-esm1-2-hr") ~ "mpi-esm1-2-hr",
      str_detect(folder_path, "mri-esm2-0") ~ "mri-esm2-0",
      str_detect(folder_path, "ukesm1-0-ll") ~ "ukesm1-0-ll",
      T ~ "No Climate Scenario"
    )
  )
table(sp_waterSave$ClimateScen)
table(sp_waterSave$climateDriver)


sp_waterSave <- sp_waterSave |>
  pivot_longer(c(sp_demand_cu, sp_demand_ni, sp_demand_co, sp_demand_li), names_to = 'Mineral', values_to = 'shadow') |>
  mutate(
    Mineral = case_when(
      str_detect(Mineral, "cu") ~ "Copper",
      str_detect(Mineral, "ni") ~ "Nickel",
      str_detect(Mineral, "co") ~ "Cobalt",
      str_detect(Mineral, "li") ~ "Lithium"
    )
  ) |>
  mutate(shadow = -shadow * 1e3) # m3 per ton (model results are in million m3 per kton)


sp_waterSave <- sp_waterSave |> filter(climateDriver %in% c("No Climate Scenario", "gfdl-esm4"))

sp_waterSave <- sp_waterSave |> mutate(Climate = paste0(ClimateScen, " - ", climateDriver))

sp_waterSave |>
  filter(Year > 2029) |> # ignore for noise at the beginning
  ggplot(aes(Year, shadow, col = ClimateScen, group = Climate)) +
  geom_line() +
  facet_wrap(~Mineral, scales = "free") +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "")) +
  theme_pb_wide() +
  labs(
    x = "",
    y = "",
    col = "Climate Pathway",
    title = expression(
      "Shadow Price of mineral demand constraints (Freshwater impact in" ~ m^3 ~ "-eq saved per ton of mineral)"
    )
  ) +
  theme(legend.position = c(0.8, 0.6))

# fmt: skip
ggsave("Figures/Metal_ShadowPricesWater.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)
