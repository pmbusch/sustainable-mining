# Trade off, based only on results
# PBH Nov 2025

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
source('Scripts/00a-Common Variables.R', encoding = 'UTF-8')
(dict_scen <- tibble(Scenario = scens_selected, name = name_abbr))

dict <- tibble(
  Scenario = c(
    "Ambitious-Baseline-Baseline-Baseline-Baseline",
    "Ambitious-Baseline-High Capacity-Baseline-Baseline_OtherLow",
    "Aggressive_Recycling"
  ),
  name = c("Baseline Demand", "High Demand", "Low Demand")
)


demand <- read.csv("Parameters/Cu_Demand.csv")
deposit <- read.csv("Parameters/Cu_Deposit.csv")

# Demand totals
(dem_tot <- demand |>
  left_join(dict) |>
  group_by(Scenario, name) |>
  reframe(mtons = sum(Demand) / 1e3) |>
  ungroup() |>
  mutate(label_dem = paste0(name, " ", round(mtons, 0), " Mt Cu")))


# Store results from optimization run
# (runs <- list.files("Results/Optimization/DemandScenario", pattern = "^Metric.*", recursive = T, full.names = TRUE))
(runs <- list.files("Results/Optimization/Copper", pattern = "^Metric.*", recursive = T, full.names = TRUE))

obj <- do.call(
  rbind,
  lapply(runs, function(folder_path) {
    transform(
      read.csv(folder_path),
      metric = basename(folder_path) |> str_remove_all("Metrics|\\.csv") |> as.numeric(),
      Scenario = basename(dirname(folder_path))
    )
  })
) |>
  mutate(metric = recode(metric, '0' = 0.5, '12' = 12.5), metric = if_else(is.na(metric), 0, metric)) |> # full cost
  left_join(dict)
# mutate(name = factor(name, name_abbr))

table(obj$Scenario)
table(obj$name)
table(obj$metric)
table(obj$Parameter)

df <- obj |>
  filter(!is.na(name)) |>
  pivot_wider(names_from = Parameter, values_from = Value) |>
  left_join(dem_tot) |>
  mutate(
    Water_cons = Water / 1e3, # in billion m3
    Cost = Cost / 1e3, # billion USD (discounted)
    Water_Impact = `Water impact` / 1e3, # billion m3 world equiv.
    lab_metric = case_when(
      metric == 0 ~ "Optimal Cost",
      metric == 0.5 ~ paste0("", metric, "% Cost Increase"),
      T ~ paste0("", metric, "%")
    )
  )

range(df$Water_Impact)


# note that slope is USD/m3 of water saved
# fit logaritmic model to data
# Y=a+b*log(X)
# Slope of Y: dY/dX = b/X

library(broom)
fits <- df %>%
  group_by(name) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(Cost ~ log(Water_Impact), data = .x)), coef = map(model, tidy))

summary(lm(Cost ~ log(Water_Impact), data = filter(df, name == "Baseline Demand")))

params <- fits %>%
  unnest(coef) %>%
  select(name, term, estimate) %>%
  tidyr::pivot_wider(names_from = term, values_from = estimate) %>%
  rename(a = `(Intercept)`, b = `log(Water_Impact)`)

# Slope at defined points
df <- df %>%
  left_join(params, by = "name") %>%
  mutate(cost_fitted = a + b * log(Water_Impact), slope = -b / Water_Impact) |> # negative slope: cost increases as Water_Impact decreases
  mutate(label_slope = if_else(metric %in% c(0.0, 4.0, 15.0), paste0("", round(slope, 2), " * ' USD/m'^3"), ""))


# Slope for demand reduction
df |>
  dplyr::select(name, metric, Cost, Water_Impact) |>
  mutate(name = str_remove(name, " Demand")) |>
  rename(Water = Water_Impact) |>
  pivot_wider(names_from = name, values_from = c(Cost, Water)) |>
  mutate(
    slope_High = (Cost_High - Cost_Baseline) / (Water_High - Water_Baseline),
    slope_Low = (Cost_Baseline - Cost_Low) / (Water_Baseline - Water_Low),
    slope_HighLow = (Cost_Low - Cost_High) / (Water_Low - Water_High)
  )

# per ton of cu
# df <- df |>
#   mutate(
#     Water = Water * 1e3 / mtons, # m3 per ton
#     Cost = Cost * 1e3 / mtons # USD per ton
#   )

# FIGURE - Pareto curves: non-dominated solutions
range(df$Water_Impact)
range(df$Cost)

desalination_cost <- 0.5 # USD per m3

ggplot(df, aes(Water_Impact, Cost, col = name)) +
  geom_line() +
  geom_point(size=0.5) +
  # fmt: skip
  geom_text_repel(data = filter(df, metric == 0),aes(label = label_dem),nudge_y = -10,size = 7 * 5 / 14 * 0.8,hjust = 0) +
  # fmt: skip
  geom_text(data=filter(df,str_detect(name,"High")), aes(label=lab_metric),col="#4d4d4d",nudge_y=15*c(1,1,-1,rep(1,9)), size=7*5/14*0.8,hjust=0) +
  # fmt: skip
  geom_text(data=filter(df,str_detect(name,"Baseline")), aes(label=label_slope),col="#4393c3",nudge_y=15, size=7*5/14*0.8,parse=T,hjust=0) +
  # fmt: skip
  geom_segment(x=3500,xend=3500-50/desalination_cost,y=1800,yend=1800+50, linetype="dashed", color="#bc80bd") +
  # fmt: skip
  annotate("text", x = 3600, y = 1820, label = paste0("'Desalination ~' * " ,desalination_cost, " * ' USD/m'^3"), color = "#bc80bd", size = 7*5/14*0.8,parse=T,hjust=0) +
  labs(x = expression("Total Freshwater Impact (billion " ~ m^3 ~ ")"), y = "Total Cost (billion USD)", col = "") +
  # labs(x = expression("Freshwater Impact (" ~ m^3 ~ " per ton Cu)"), y = "Cost (USD per ton Cu)", col = "") +
  # stat_function(fun = function(x) params[2, ]$a + params[2, ]$b * log(x), color = "blue", linewidth = 1) + # check log fit
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$")) +
  scale_x_continuous(labels = scales::label_comma(), limits = c(400, 5600)) +
  scale_color_manual(values = c("#378bc9", "#d94253", "#6c8364")) +
  theme_bw(8) +
  theme(panel.grid = element_blank(), legend.position = "none")

# ggsave("Figures/TradeOff.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7)
ggsave("Figures/Cu_TradeOff.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 1.2, height = 8.7)
# ggsave("Figures/Cu_TradeOff_perTon.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 1.2, height = 8.7)

# sketch ideas
ggplot(df, aes(mtons, Water, col = Cost)) +
  geom_point() +
  geom_text(data=filter(df,str_detect(name,"Low")), aes(label=lab_metric),col="darkgrey",nudge_x=2, size=7*5/14*0.8,hjust=0) +
  scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(8, "Spectral")), labels = scales::label_comma()) +
  labs(
    y = expression("Total Freshwater Impact (billion " ~ m^3 ~ ")"),
    col = "Total Cost (billion USD)",
    x = "Cumulative Demand [million tons Cu]"
  ) +
  guides(color = guide_colorbar(barwidth = unit(10, "cm"))) +
  theme_bw(8) +
  theme(legend.position = "bottom", panel.grid = element_blank())

ggsave("Figures/Cu_TradeOff_Demand.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7)

# Basin shadow prices ---------------
(runs_sp <- list.files("Results/Optimization/Copper", pattern = "Basin.*", recursive = T, full.names = TRUE))
sp <- do.call(
  rbind,
  lapply(runs_sp, function(folder_path) {
    transform(read.csv(folder_path), Scenario = basename(dirname(folder_path)))
  })
) |>
  left_join(dict) |>
  mutate(Year = t + 2024)
head(sp)

sp <- sp |>
  filter(Year == 2025) |>
  mutate(shadow = if_else(near(shadow, 0), NA, shadow)) |>
  arrange(desc(shadow)) |>
  mutate(shadow = -shadow)

# Basemap of basins from AWARE
unzip("Inputs/AWARE/AWARE20_Native_CFs_geospatial.kmz", exdir = "kmz_unzip")
k <- st_read("kmz_unzip/doc.kml")
cf_map <- st_make_valid(k) |> st_zm() |> st_cast("MULTIPOLYGON")
cf_map$Basin_ID <- as.numeric(str_remove(cf_map$Name, "CFs for Basin_ID "))
cf_map <- left_join(cf_map, sp, by = "Basin_ID")


map1 <- map_data('world')
p1 <- ggplot(cf_map) +
  # base map
  theme_minimal(8) +
  geom_polygon(data = map1, mapping = aes(x = long, y = lat, group = group), col = 'gray', fill = "white") +
  # Water stress map
  geom_sf(data = cf_map, aes(fill = shadow), color = "grey30", linewidth = 0.1) +
  coord_sf(xlim = c(-140, 160), ylim = c(-60, 70)) +
  scale_fill_distiller(
    name = expression("Shadow Price basin (USD/" * m^3 * ")"),
    palette = "OrRd",
    na.value = "white",
    direction = 1,
    breaks = seq(0, 30, 5),
    guide = guide_colorbar(direction = "horizontal", barwidth = unit(6, "cm"), barheight = unit(0.25, "cm"), order = 1)
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.5, 0.12),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )
p1

# EoF
