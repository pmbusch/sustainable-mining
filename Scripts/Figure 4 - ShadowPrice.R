# Shadow prices (or Lagrangian multipliers) for constraints in the optimal allocation model
# Indicate the marginal value change of the objective function (Cost OR freshwater impact) for active constraints
# Obtained directly from the optimization model output - by setting binary variables as fixed
# PBH Feb 2026

source('Scripts/00-Libraries.R', encoding = 'UTF-8')


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
sp_value <- sp |> filter(abs(shadow) > 0) |> filter(Scenario == "SPS") |> mutate(shadow = -shadow) # convert to savings
length(unique(sp_value$Basin_ID)) # 28

# Pick 14 largest as basins
sel_basins <- sp_value |> arrange(desc(shadow)) |> pull(Basin_ID) |> unique()
sp_value$Basin_ID = factor(sp_value$Basin_ID, levels = sel_basins)
colors_basins <- paletteer::paletteer_d("ggthemes::Classic_Cyclic", n = 13)
# rest are greys
colors_basins <- c(colors_basins, rep("#bdbdbd", length(levels(sp_value$Basin_ID)) - length(colors_basins)))
names(colors_basins) <- levels(sp_value$Basin_ID)

# Undiscount things
optInputs <- read.csv("Results/Optimization/DemandScenario/SPS/OptimizationInputs.csv")
(r <- optInputs |> filter(Parameter == "Discount rate") |> pull(Value)) # 7%
sp_value <- sp_value |> mutate(shadow = shadow * (1 + r)^(Year - 2025))


desalination_cost <- 0.5 # USD/m3
p_line <- ggplot(sp_value, aes(Year, shadow, col = Basin_ID, group = Basin_ID)) +
  geom_line() +
  # facet_wrap(~Scenario) +
  geom_hline(yintercept = desalination_cost, linetype = "dashed") +
  # fmt: skip
  annotate("text", x = 2027, y = 0, label = paste0("'Desalination ~' * " ,desalination_cost, " * ' USD/m'^3"), color = "black", size = 7*5/14*0.8,parse=T,hjust=0) +
  # scale_y_continuous(trans = "log10", labels = dollar_format(big.mark = " ", prefix = "$")) +
  scale_y_continuous(limits = c(0, 150), labels = dollar_format(big.mark = " ", prefix = "$")) +
  scale_color_manual(values = colors_basins) +
  theme_pb_wide() +
  labs(x = "", y = "", title = expression("Avoided cost per extra " * m^3 * "of water allowed in basin")) +
  theme(legend.position = "none")
p_line

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
pmap <- ggplot(cf_map) +
  # base map
  theme_minimal(8) +
  geom_polygon(data = map1, mapping = aes(x = long, y = lat, group = group), col = 'gray', fill = "white") +
  # Water stress map
  geom_sf(data = cf_map, aes(fill = Basin_ID), color = "grey30", linewidth = 0.1) +
  coord_sf(xlim = c(-140, 160), ylim = c(-60, 70)) +
  # scale_fill_distiller(
  #   name = expression("Shadow Price basin (USD/" * m^3 * ")"),
  #   palette = "OrRd",
  #   na.value = "white",
  #   trans = "log10",
  #   direction = 1,
  #   breaks = seq(0, 30, 5),
  #   guide = guide_colorbar(direction = "horizontal", barwidth = unit(6, "cm"), barheight = unit(0.25, "cm"), order = 1)
  # ) +
  scale_fill_manual(values = colors_basins) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )
pmap


library(cowplot)
plot_grid(pmap, p_line, nrow = 1)

## total resources by basin -------------

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
deposit$Basin_ID <- factor(deposit$Basin_ID, levels = rev(sel_basins))

# Color by basin
deposit$X <- -1

p_bar <- ggplot(deposit, aes(Basin_ID, resources, fill = Mineral)) +
  geom_col() +
  geom_point(aes(y=X,col=Basin_ID),size=3,show.legend = FALSE) +
  # fmt: skip
  annotate("text", x = length(sel_basins), y = 10, label = "Basins", color = "black", size = 7*5/14*0.8,parse=T,hjust=0) +
  scale_color_manual(values = colors_basins, guide = "none") +
  coord_flip() +
  theme_pb_wide() +
  labs(x = "", y = "Contained Mineral resources per basin, in million tons") +
  theme(legend.position = c(0.7, 0.3), axis.text.y = element_blank(), axis.ticks.y = element_blank())
p_bar


plot_grid(plot_grid(pmap, p_bar, ncol = 1), p_line, nrow = 1)

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

# Join and filter for only at cost optimal
runs_sp_cost <- c(runs_sp_cost, runs_sp_cost_climate)
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
sp_cost <- sp_cost |> filter(Scenario == "APS")

# Get climate scenarios
table(sp_cost$folder_path)
sp_cost <- sp_cost |>
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
table(sp_cost$ClimateScen)
table(sp_cost$climateDriver)


sp_cost <- sp_cost |>
  pivot_longer(c(sp_demand_cu, sp_demand_ni, sp_demand_co, sp_demand_li), names_to = 'Mineral', values_to = 'shadow') |>
  mutate(
    Mineral = case_when(
      str_detect(Mineral, "cu") ~ "Copper",
      str_detect(Mineral, "ni") ~ "Nickel",
      str_detect(Mineral, "co") ~ "Cobalt",
      str_detect(Mineral, "li") ~ "Lithium"
    )
  ) |>
  mutate(shadow = -shadow * 1e3) # to USD per ton (model results are in million USD per kton)

# Undiscount them
sp_cost <- sp_cost |> mutate(shadow = shadow * (1 + r)^(Year - 2025))

sp_cost <- sp_cost |> mutate(Climate = paste0(ClimateScen, " - ", climateDriver))

# one for now
sp_cost <- sp_cost |> filter(climateDriver %in% c("No Climate Scenario", "gfdl-esm4"))

sp_cost |>
  filter(Year > 2029) |> # ignore for noise at the beginning
  ggplot(aes(Year, shadow, col = ClimateScen, group = Climate)) +
  geom_line() +
  facet_wrap(~Mineral, scales = "free") +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$"), limits = c(0, NA)) +
  theme_pb_wide() +
  labs(
    x = "",
    y = "",
    title = expression("Shadow Price of mineral demand constraints (USD per ton of mineral)"),
    col = "Climate Pathway"
  ) +
  theme(legend.position = c(0.8, 0.8))

# fmt: skip
ggsave("Figures/Metal_ShadowPrices.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)


# Shadow price Water by Mineral red  ---------
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
    title = expression("Shadow Price of mineral demand constraints (Freshwater impact in m3 saved per ton of mineral)")
  ) +
  theme(legend.position = c(0.8, 0.6))

# fmt: skip
ggsave("Figures/Metal_ShadowPricesWater.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)
