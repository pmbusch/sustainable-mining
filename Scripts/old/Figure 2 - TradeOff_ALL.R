# Figure 2 - Trade off Cost and Freshwater Impacts
# Pareto curve: every point in the curve is pareto optimal, cannot improve one objective (cost) without worsening the other (water impact)
# PBH Nov 2025

# LOAD -------------

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
source('Scripts/00a-Common Variables.R', encoding = 'UTF-8')
(dict_scen <- tibble(Scenario = Scenario, name = scen_name))


demand <- read.csv("Parameters/IEA_Demand.csv")
deposit <- read.csv("Parameters/Deposit.csv")

# Demand totals
(dem_tot <- demand |>
  pivot_longer(c(-Scenario, -Year), names_to = 'Mineral', values_to = 'Demand') |>
  group_by(Scenario, Mineral) |>
  reframe(mtons = sum(Demand) / 1e3) |>
  ungroup() |>
  mutate(label_dem = paste0(Scenario, " ", round(mtons, 0), " Mt")))


# a - BY DEMAND SCENARIOS  --------------------
# Store results from optimization run
(runs <- list.files("Results/Optimization/DemandScenario", pattern = "Metrics.*", recursive = T, full.names = TRUE))
# (runs <- list.files(
#   "Results/Optimization/ClimateScenario/APS/Deposit_ssp370_gfdl-esm4",
#   pattern = "Metrics.*",
#   recursive = T,
#   full.names = TRUE
# ))

obj <- do.call(
  rbind,
  lapply(runs, function(folder_path) {
    transform(read.csv(folder_path), file_name = basename(folder_path), Scenario = basename(dirname(folder_path)))
  })
)
head(obj)
unique(obj$file_name)
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
      str_detect(file_name, "Eps25") ~ "25%",
    ) |>
      # fmt: skip
      factor(levels = c("No Water Constraint", "0%", "0.5%", "1%", "2%", "3%", "4%", "5%", "6%", "8%", "10%", "12%", "15%", "20%", "25%"))
  )

table(obj$Scenario)
table(obj$metric)
table(obj$Parameter)

df <- obj |>
  filter(!is.na(Scenario)) |>
  pivot_wider(names_from = Parameter, values_from = Value) |>
  # left_join(dem_tot) |>
  mutate(
    Water_cons = Water / 1e3, # in billion m3
    Cost = Cost / 1e3, # billion USD (discounted)
    Water_Impact = `Water impact` / 1e3, # billion m3 world equiv.
    lab_metric = case_when(
      metric == "0%" ~ "Optimal Cost",
      metric == "0.5%" ~ paste0("", metric, " Cost Increase"),
      T ~ metric
    )
  ) |>
  mutate(`Water impact` = NULL)

range(df$Water_Impact)


# note that slope is USD/m3 of water saved
# fit logaritmic model to data
# Y=a+b*log(X)
# Slope of Y: dY/dX = b/X

library(broom)
fits <- df %>%
  group_by(Scenario) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(Cost ~ log(Water_Impact), data = .x)), coef = map(model, tidy))

summary(lm(Cost ~ log(Water_Impact), data = filter(df, Scenario == "SPS")))

params <- fits %>%
  unnest(coef) %>%
  select(Scenario, term, estimate) %>%
  tidyr::pivot_wider(names_from = term, values_from = estimate) %>%
  rename(a = `(Intercept)`, b = `log(Water_Impact)`)

# Slope at defined points
df <- df %>%
  left_join(params, by = "Scenario") %>%
  mutate(cost_fitted = a + b * log(Water_Impact), slope = -b / Water_Impact) |> # negative slope: cost increases as Water_Impact decreases
  mutate(
    label_slope = if_else(metric %in% c("0%", "5%", "10%", "25%"), paste0("", round(slope, 2), " * ' USD/m'^3"), "")
  )

# Better get slope as difference between points
df <- df |>
  arrange(Scenario, metric) |>
  mutate(slope = -(Cost - lag(Cost)) / (Water_Impact - lag(Water_Impact))) |>
  mutate(
    label_slope = if_else(metric %in% c("1%", "5%", "10%", "25%"), paste0("", round(slope, 2), " * ' USD/m'^3"), "")
  )

# Get % of water reduction between first point
base_water <- df |> filter(metric == "0%") |> rename(water_base = Water_Impact) |> dplyr::select(Scenario, water_base)
df <- df |>
  filter(!str_detect(metric, "Water")) |>
  left_join(base_water) |>
  mutate(water_red_pct = (Water_Impact - water_base) / water_base * 100) |>
  mutate(
    label_water_red = if_else(metric %in% c("0%", "1%", "3%", "4%", "8%"), "", paste0("", round(water_red_pct), "%")),
    label_water_red_full = if_else(
      metric %in% c("1%", "5%", "15%", "25%"),
      paste0("Cost: +", metric, " ~ Water: ", round(water_red_pct), "%"),
      ""
    )
  )


# Slope for demand reduction
df |>
  dplyr::select(Scenario, metric, Cost, Water_Impact) |>
  rename(Water = Water_Impact) |>
  pivot_wider(names_from = Scenario, values_from = c(Cost, Water)) |>
  mutate(
    slope_High = (Cost_NZE - Cost_APS) / (Water_NZE - Water_APS),
    slope_Low = (Cost_APS - Cost_SPS) / (Water_APS - Water_SPS),
    slope_HighLow = (Cost_SPS - Cost_NZE) / (Water_SPS - Water_NZE)
  )


# FIGURE - Pareto curves: non-dominated solutions

desalination_cost <- 0.5 # USD per m3
# pick closest point to slope
df_close <- df |> group_by(Scenario) |> slice_min(abs(slope - desalination_cost), n = 1)

# Scenario name
df <- df |>
  mutate(
    scen_name = case_when(
      Scenario == "APS" ~ "Announced Pledges Scenario",
      Scenario == "NZE" ~ "Net Zero Emissions\nDemand Scenario",
      Scenario == "SPS" ~ "Stated Policies Scenario",
      T ~ Scenario
    )
  )

data_fig <- df |> filter(!str_detect(metric, "Water"))
range(data_fig$Water_Impact)
range(data_fig$Cost)
unique(data_fig$Scenario)

# Contour regions interpolation
dem_total <- dem_tot |> group_by(Scenario) |> reframe(mtons = sum(mtons)) |> ungroup()

# Option - draw polygons between curves
# --- Prepare data ---
data_fig2 <- data_fig |> left_join(dem_total) |> arrange(desc(mtons), Water_Impact)

ext_pts <- data_fig2 |>
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

extra_l <- ext_pts |>
  rowwise() |>
  mutate(Water_Impact = list(seq(1e3, x_left, length.out = 30)[-30])) |>
  tidyr::unnest(Water_Impact) |>
  mutate(Cost = y_left + s_left * (Water_Impact - x_left)) |>
  ungroup() |>
  select(mtons, Water_Impact, Cost)

extra_r <- ext_pts |>
  rowwise() |>
  mutate(Water_Impact = list(seq(x_right, 8e3, length.out = 30)[-1])) |>
  tidyr::unnest(Water_Impact) |>
  mutate(Cost = y_right + s_right * (Water_Impact - x_right)) |>
  ungroup() |>
  select(mtons, Water_Impact, Cost)

data_fig_ext <- bind_rows(data_fig2 |> select(mtons, Water_Impact, Cost), extra_l, extra_r) |>
  arrange(mtons, Water_Impact)


# --- Build bands between curves ---
scens <- sort(unique(data_fig_ext$mtons), decreasing = TRUE)

bands <- map_dfr(seq_len(length(scens) - 1), function(i) {
  s1 <- scens[i]
  s2 <- scens[i + 1]
  d1 <- filter(data_fig_ext, mtons == s1)
  d2 <- filter(data_fig_ext, mtons == s2)
  tibble(
    Water_Impact = c(d1$Water_Impact, rev(d2$Water_Impact)),
    Cost = c(d1$Cost, rev(d2$Cost)),
    band = paste0(s1, "-", s2),
    band_legend = round(as.numeric(s1), 0)
  )
})
range(bands$band_legend)

data_fig_final <- data_fig |> filter(Scenario %in% c("APS", "SPS", "NZE"))
df_close_final <- df_close |> filter(Scenario %in% c("APS", "SPS", "NZE"))


# optimal at NZE
df_opt <- df |> filter(metric == "0%") |> filter(Scenario == "NZE")
df_slope_nze <- df |> filter(Scenario == "NZE") |> filter(metric == "25%")

ggplot(data_fig_final, aes(Water_Impact, Cost, col = Scenario)) +
  geom_polygon(
    data = bands,
    aes(Water_Impact, Cost, group = band, fill = band_legend),
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  scale_fill_gradientn(
    colours = rev(RColorBrewer::brewer.pal(11, "Spectral")),
    limits = c(800, 1500),
    breaks = c(800, 1500),
    labels = c("Low\nDemand", "High\nDemand"),
    name = ""
  ) +
  geom_vline(xintercept = df_opt$Water_Impact, linetype = "dashed", col = "darkgrey", linewidth = 0.3) +
  geom_hline(yintercept = df_opt$Cost, linetype = "dashed", col = "darkgrey", linewidth = 0.3) +
  geom_line(linewidth = 1) +
  geom_point(size=0.3) +
  # fmt: skip
  geom_text(data = filter(data_fig_final, metric == "0%"),aes(label = scen_name),nudge_y = -60*c(-1.5,2,-1),nudge_x=-500*c(1,2,0),size = 7 * 5 / 14 * 0.8,hjust = 0.5) +
  # fmt: skip
  # geom_text(data=filter(data_fig_final,str_detect(Scenario,"NZE")), aes(label=lab_metric),col="#4d4d4d",nudge_y=45*c(-1,1,1,rep(1,11)), size=7*5/14*0.8,hjust=0) +
  # fmt: skip
  # geom_text(data=filter(data_fig_final,str_detect(Scenario,"APS")), aes(label=label_water_red_full),col="#4d4d4d",nudge_y=45, size=7*5/14*0.8,hjust=0) +
  # fmt: skip
  # geom_text_repel(aes(label=label_water_red),col="darkblue",nudge_y=-45, size=7*5/14*0.8,hjust=0,fontface="italic") +
  # fmt: skip
  geom_text(data=filter(data_fig_final,str_detect(Scenario,"NZE")), aes(label=label_slope),col="#6c8364",nudge_y=45, size=7*5/14*0.8,parse=T,hjust=0) +
  # fmt: skip
  annotate("text", x=df_slope_nze$Water_Impact+1200,y=df_slope_nze$Cost+30,col="#6c8364",label="Implied Cost of Water Reduction",size=7*5/14*0.8,hjust=0) +
  geom_point(data=filter(data_fig_final,str_detect(Scenario,"NZE"),label_slope!=""),col="#6c8364",size=0.6) +
  # fmt: skip
  geom_segment(data=df_close_final,aes(x=Water_Impact+1000/desalination_cost/2,xend=df_close_final$Water_Impact-1000/desalination_cost/2,y=df_close_final$Cost-1000/2,yend=df_close_final$Cost+1000/2), linetype="dashed", color="#0072B2",linewidth=0.25) +
  geom_point(data=df_close_final,col="#0072B2",size=1) +
  # fmt: skip
  annotate("text", x = df_close_final$Water_Impact[2]+100, y = df_close_final$Cost[2]+20, label = paste0("'Desalination: ' * " ,desalination_cost, " * ' USD/m'^3"), color = "#0072B2", size = 7*5/14*0.8,parse=T,hjust=0) +
  # fmt: skip
  annotate("text", x = df_opt$Water_Impact+100, y = df_opt$Cost+100, label = "Optimal Cost", size = 7 * 5 / 14 * 0.8,hjust=0.1,angle=90) +
  geom_point(data=df_opt,col="black",size=0.6) +
  labs(
    x = expression("Total Freshwater Impact 2025-2050 (billion " ~ m^3 * "-eq)"),
    y = "Total Cost 2025-2050 (billion USD)",
    col = ""
  ) +
  # labs(x = expression("Freshwater Impact (" ~ m^3 ~ " per ton Cu)"), y = "Cost (USD per ton Cu)", col = "") +
  # stat_function(fun = function(x) params[2, ]$a + params[2, ]$b * log(x), color = "blue", linewidth = 1) + # check log fit
  scale_y_continuous(
    labels = dollar_format(big.mark = ",", prefix = "$"),
    sec.axis = sec_axis(
      ~ (. - df_opt$Cost) / df_opt$Cost,
      name = "Change in Cost relative to Optimal Cost NZE (%)",
      labels = scales::percent
    )
  ) +
  scale_x_continuous(
    labels = scales::label_comma(),
    sec.axis = sec_axis(
      ~ (. - df_opt$Water_Impact) / df_opt$Water_Impact,
      name = "Change in Freshwater Impact relative to Optimal Cost NZE (%)",
      labels = scales::percent
    )
  ) +
  scale_color_manual(values = demand_colors) +
  guides(color = "none") +
  coord_cartesian(
    xlim = c(min(data_fig_final$Water_Impact) * 0.95, max(data_fig_final$Water_Impact) * 1.05),
    ylim = c(min(data_fig_final$Cost) * 0.95, max(data_fig_final$Cost) * 1.05),
    expand = F
  ) +
  theme_bw(8) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title.y.right = element_text(size = 6),
    axis.text.y.right = element_text(size = 6),
    axis.title.x.top = element_text(size = 6),
    axis.text.x.top = element_text(size = 6)
  )

ggsave("Figures/Fig2_Demand.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)

# ggsave("Figures/TradeOff_perTon.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 1.2, height = 8.7)

# sketch ideas
# ggplot(df, aes(mtons, Water, col = Cost)) +
#   geom_point() +
#   geom_text(data=filter(df,str_detect(Scenario,"SPS")), aes(label=lab_metric),col="darkgrey",nudge_x=2, size=7*5/14*0.8,hjust=0) +
#   scale_color_gradientn(colours = rev(RColorBrewer::brewer.pal(8, "Spectral")), labels = scales::label_comma()) +
#   labs(
#     y = expression("Total Freshwater Impact (billion " ~ m^3 ~ ")"),
#     col = "Total Cost (billion USD)",
#     x = "Cumulative Demand [million tons Cu]"
#   ) +
#   guides(color = guide_colorbar(barwidth = unit(10, "cm"))) +
#   theme_bw(8) +
#   theme(legend.position = "bottom", panel.grid = element_blank())

# ggsave("Figures/Cu_TradeOff_Demand.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7)

# b - COUNTRY OF PRODUCTION BY WATER IMPACT -----------------

# Store results from optimization run
(runs <- list.files("Results/Optimization/DemandScenario", recursive = T, full.names = TRUE) |>
  (\(x) {
    x[(!str_detect(x, "SP_") & !str_detect(x, "Metrics") & !str_detect(x, "Slack") & !str_detect(x, "Inputs"))]
  })())


opt_results <- do.call(
  rbind,
  lapply(runs, function(folder_path) {
    transform(read.csv(folder_path), file_name = basename(folder_path), Scenario = basename(dirname(folder_path)))
  })
)
opt_results <- opt_results |> filter(ktons_extracted > 0 | capacity_added > 0 | mine_opened > 0) # reduce size
head(opt_results)
unique(opt_results$file_name)
opt_results <- opt_results |>
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
      str_detect(file_name, "Eps25") ~ "25%",
    ) |>
      # fmt: skip
      factor(levels = rev(c("No Water Constraint","0%","0.5%","1%","2%","3%","4%","5%","6%","8%","10%","12%","15%","20%","25%")))
  )
depositAll <- read.csv("Parameters/Deposit.csv")

deposit <- depositAll |>
  dplyr::select(
    ID,
    Name,
    grade_resource_Copper,
    grade_resource_Nickel,
    grade_resource_Cobalt,
    grade_resource_Lithium,
    recovery_rate_Copper,
    recovery_rate_Nickel,
    recovery_rate_Cobalt,
    recovery_rate_Lithium,
    country,
    water,
    Basin_ID
  )

prod <- opt_results |>
  left_join(deposit) |>
  mutate(
    copper = ktons_extracted / 1000 * grade_resource_Copper * recovery_rate_Copper / 100,
    nickel = ktons_extracted / 1000 * grade_resource_Nickel * recovery_rate_Nickel / 100,
    cobalt = ktons_extracted / 1000 * grade_resource_Cobalt * recovery_rate_Cobalt / 100,
    lithium = ktons_extracted / 1000 * grade_resource_Lithium * recovery_rate_Lithium / 100,
  ) |>
  group_by(Scenario, metric, country) |>
  reframe(Copper = sum(copper), Nickel = sum(nickel), Cobalt = sum(cobalt), Lithium = sum(lithium), ) |>
  filter(Copper + Nickel + Cobalt + Lithium > 0) |>
  ungroup()

## parenthesis - get fish index basin level (normalize from 0 to 100)
fish <- read.csv("Parameters/FW_FISH/FW_FISH_Basin_FishIndex_Global.csv")
water <- opt_results |>
  left_join(deposit) |>
  mutate(water = ktons_extracted * water / 1e3) |> # million m3
  group_by(Scenario, metric, Basin_ID) |>
  reframe(water = sum(water)) |>
  filter(water > 0) |>
  ungroup()
# global weighted by water cons fish index
water |>
  left_join(fish) |>
  group_by(Scenario, metric) |>
  reframe(avg_fish = weighted.mean(fish_index, water)) |>
  ungroup() |>
  ggplot(aes(metric, avg_fish, col = Scenario, group = Scenario)) +
  geom_line() +
  theme_pb_wide()

##

data_fig <- prod |> filter(Scenario == "NZE") |> filter(metric != "No Water Constraint")


# Long format
data_fig <- data_fig |>
  dplyr::select(Scenario, metric, country, Copper, Nickel, Cobalt, Lithium) |>
  pivot_longer(c(Copper, Nickel, Cobalt, Lithium), names_to = 'Mineral', values_to = 'total_metal') |>
  mutate(Mineral = factor(Mineral, levels = c("Copper", "Nickel", "Cobalt", "Lithium")))


# Total production by mineral and metric - COBALT is basically a co-product of Ni and Cu, not an active constraint
data_fig |>
  group_by(metric, Mineral) |>
  reframe(total_metal = sum(total_metal)) |>
  ungroup() |>
  pivot_wider(names_from = Mineral, values_from = total_metal)

# Key countries by mineral
data_fig |>
  filter(metric == "0%") |>
  group_by(Mineral) |>
  slice_max(order_by = total_metal, n = 10, with_ties = FALSE) |>
  summarise(Top10_countries = paste(country, collapse = ", "), .groups = "drop")

# Add slack as demand unmet
demand_long <- demand |>
  filter(Scenario == "NZE") |>
  pivot_longer(c(Copper, Nickel, Cobalt, Lithium), names_to = 'Mineral', values_to = 'total_demand') |>
  group_by(Mineral) |>
  reframe(total_demand = sum(total_demand) / 1e3) |> # in million tons
  ungroup()

unmet <- data_fig |>
  group_by(Scenario, metric, Mineral) |>
  reframe(total_metal = sum(total_metal)) |>
  ungroup() |>
  left_join(demand_long) |>
  mutate(unmet_demand = total_demand - total_metal) |>
  dplyr::select(-total_metal, -total_demand) |>
  filter(unmet_demand > 1e-3) |>
  rename(total_metal = unmet_demand) |>
  mutate(country = "Unmet Demand")


dict_region <- read_excel("Inputs/Dictionaries/Dict_Countries_SP.xlsx", sheet = "Dict")
data_fig <- data_fig |>
  rbind(unmet) |>
  left_join(dict_region) |>
  mutate(
    region = case_when(
      country == "Unmet Demand" ~ "Unmet Demand",
      Mineral == "Copper" &
        country %in%
          c("Chile", "Peru", "Indonesia", "Russia", "USA", "China", "Mongolia", "Dem. Rep. Congo", "Mexico") ~ ISO3,
      Mineral == "Nickel" &
        country %in% c("Indonesia", "Philippines", "Russia", "Australia", "New Caledonia", "Brazil") ~ ISO3,
      Mineral == "Cobalt" & country %in% c("Dem. Rep. Congo", "Indonesia") ~ ISO3,
      Mineral == "Lithium" &
        country %in% c("Chile", "Dem. Rep. Congo", "Argentina", "USA", "Australia", "Brazil") ~ ISO3,
      T ~ "RoW"
    ) |>
      str_replace("COD", "DRC")
  ) |>
  group_by(Scenario, metric, region, Mineral) |>
  reframe(total_metal = sum(total_metal)) |>
  ungroup() |>
  group_by(Scenario, metric, Mineral) |>
  mutate(share = total_metal / sum(total_metal), ) |>
  ungroup()

unique(data_fig$region)
region_colors <- c(
  "CHL" = "#6a3d9a",
  "DRC" = "#4682b4",
  "PER" = "#8b4513",
  "IDN" = "#fdb462",
  "RUS" = "#756bb1",
  "USA" = "#c4dfbe",
  "CAN" = "#e91a1c",
  "CHN" = "#d74c5a",
  "ARG" = "#ff7f00",
  "MNG" = "#e31a1c",
  "PHL" = "#1f78b4",
  "BRA" = "#33a02c",
  "NCL" = "#b15928",
  "AUS" = "#fb9a99",
  "Europe" = "#2b8cbe",
  "MEX" = "#66c2a5",
  "KAZ" = "#8c510a",
  "RoW" = "#4d4d4d",
  "Unmet Demand" = "#67000D80"
)

data_fig <- data_fig |> mutate(region = factor(region, levels = rev(names(region_colors))))

# add total water impact per scenario
impact <- df |> group_by(Scenario, metric) |> reframe(Water_Impact = mean(Water_Impact)) |> ungroup()

data_fig <- data_fig |> left_join(impact)


data_fig <- data_fig |> mutate(region_label = if_else(share > 0.01, as.character(region), ""))

# order by share region
order_col <- data_fig |>
  mutate(order_col = paste0(Mineral, region)) |>
  arrange(desc(share)) |>
  pull(order_col) |>
  unique()
# put RoW and demand unment at the end
order_col <- c(order_col[!str_detect(order_col, "RoW")], order_col[str_detect(order_col, "RoW")])
order_col <- c(order_col[!str_detect(order_col, "Unmet Demand")], order_col[str_detect(order_col, "Unmet Demand")])
data_fig <- data_fig |> mutate(order_col = paste0(Mineral, region) |> factor(levels = rev(order_col)))

ggplot(data_fig, aes(Water_Impact, total_metal, fill = region, group = order_col)) +
  geom_area(col="black",linewidth=0.1) +
  # geom_col(col="black",linewidth=0.1) +
  geom_text(data=filter(data_fig, metric == "0%"), aes(x=Water_Impact-100,label = region_label),hjust=1,, position = position_stack(vjust = 0.5), size = 7 * 5 / 14 * 0.8, col="white") +
  facet_wrap(~Mineral, ncol = 2, scales = "free") +
  scale_fill_manual(values = region_colors) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  coord_cartesian(expand = F) +
  labs(
    x = expression("Total Freshwater Impact (billion " ~ m^3 * "-eq)"),
    y = "",
    title = "2025-2050 Metal production, in million tons",
    subtitle = "Net Zero Emissions Demand Scenario"
  ) +
  theme_pb_wide() +
  theme(legend.position = "none", panel.spacing.x = unit(1.2, "lines"))

# fmt: skip
ggsave("Figures/Fig2_prod.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)


# BY FISH BIODIVESITY SCENARIO -----
# Fish biodiversity is a constraint on water available to extract

# Store results from optimization run
(runs <- list.files("Results/Optimization/BioDScenario/NZE/", pattern = "Metrics.*", recursive = T, full.names = TRUE))

# filter by driver gfdl-esm4
# runs <- runs[str_detect(runs, "gfdl-esm4")]

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
head(obj)
unique(obj$file_name)
unique(obj$Scenario)
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
      str_detect(file_name, "Eps25") ~ "25%",
    ) |>
      # fmt: skip
      factor(levels = c("No Water Constraint", "0%", "0.5%", "1%", "2%", "3%", "4%", "5%", "6%", "8%", "10%", "12%", "15%", "20%", "25%"))
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
      T ~ Scenario
    ) |>
      factor(
        levels = c(
          "All Basins",
          "Fish Index < 99.9",
          "Fish Index < 95",
          "Fish Index < 90",
          "Fish Index < 85",
          "Fish Index < 80",
          "Fish Index < 75",
          "Fish Index < 74",
          "Fish Index < 73",
          "Fish Index < 72",
          "Fish Index < 71",
          "Fish Index < 70",
          "Fish Index < 65",
          "Fish Index < 60",
          "Fish Index < 55",
          "Fish Index < 50"
        )
      )
  )

table(obj$Scenario)
table(obj$metric)
table(obj$Parameter)

df <- obj |>
  filter(!is.na(Scenario)) |>
  pivot_wider(names_from = Parameter, values_from = Value) |>
  # left_join(dem_tot) |>
  mutate(
    Water_cons = Water / 1e3, # in billion m3
    Cost = Cost / 1e3, # billion USD (discounted)
    Water_Impact = `Water impact` / 1e3, # billion m3 world equiv.
    lab_metric = case_when(
      metric == "0%" ~ "Optimal Cost",
      metric == "0.5%" ~ paste0("", metric, " Cost Increase"),
      T ~ metric
    )
  ) |>
  mutate(`Water impact` = NULL)
head(df)
range(df$Water_Impact)


# Slope as difference between points
df <- df |>
  arrange(Scenario, metric) |>
  mutate(slope = -(Cost - lag(Cost)) / (Water_Impact - lag(Water_Impact))) |>
  mutate(label_slope = if_else(metric %in% c("0.5%", "4%", "15%"), paste0("", round(slope, 2), " * ' USD/m'^3"), ""))

# Get % of water red between first point
base_water <- df |> filter(metric == "0%") |> rename(water_base = Water_Impact) |> dplyr::select(Scenario, water_base)
df <- df |>
  filter(!str_detect(metric, "Water")) |>
  left_join(base_water) |>
  mutate(water_red_pct = (Water_Impact - water_base) / water_base * 100) |>
  mutate(
    label_water_red = if_else(metric %in% c("0%", "1%", "3%", "4%", "8%"), "", paste0("", round(water_red_pct), "%"))
  )

# FIGURE - Pareto curves: non-dominated solutions

data_fig <- df |> filter(!str_detect(metric, "Water"))
range(data_fig$Water_Impact)
range(data_fig$Cost)

# Scenarios too similar
# data_fig <- data_fig |> filter(!str_detect(Scenario, "50|90"))

desalination_cost <- 0.5 # USD per m3
# pick closest point to slope
df_close <- data_fig |> group_by(Scenario) |> slice_min(abs(slope - desalination_cost), n = 1)

df_opt <- data_fig |> filter(metric == "0%") |> filter(Scenario == "All Basins")

# Contour regions interpolation
data_fig2 <- data_fig |>
  mutate(scen_val = case_when(Scenario == "All Basins" ~ 100, TRUE ~ as.numeric(str_extract(Scenario, "\\d+\\.?\\d*"))))

interp_grid <- akima::interp(
  x = data_fig2$Water_Impact,
  y = data_fig2$Cost,
  z = data_fig2$scen_val,
  duplicate = "mean",
  extrap = T
)

contour_df <- expand.grid(Water_Impact = interp_grid$x, Cost = interp_grid$y)
contour_df$z <- as.vector(interp_grid$z)

data_fig_final <- data_fig |> filter(str_detect(Scenario, "Basins|90|80|70|60|50"))
df_close_final <- df_close |> filter(str_detect(Scenario, "Basins|90|80|70|60|50"))

ggplot(data_fig_final, aes(Water_Impact, Cost, col = Scenario, group = Scenario)) +
  geom_contour_filled(data = contour_df, aes(Water_Impact, Cost, z = z), alpha = 0.5, inherit.aes = F) +
  scale_fill_manual(values = rev(c(RColorBrewer::brewer.pal(9, "Spectral"), "white")), name = "Fish Index\nThreshold") +
  geom_vline(xintercept = df_opt$Water_Impact, linetype = "dashed", linewidth = 0.3) +
  geom_hline(yintercept = df_opt$Cost, linetype = "dashed", linewidth = 0.3) +
  geom_line() +
  geom_point(size=0.5) +
  # fmt: skip
  geom_text(data = filter(data_fig_final, metric == "0%"),aes(label = Scenario),size = 7 * 5 / 14 * 0.8,hjust = 0,nudge_y=100*c(-1,1,1,-1,2.5,rep(1,1)),nudge_x=1000*c(0,0,0,-1.5,-1.8,rep(0,1))) +
  # fmt: skipx
  # geom_text(data=filter(data_fig,str_detect(Scenario,"SSP3-7.0")), aes(label=lab_metric),col="#4d4d4d",nudge_y=45*c(-1,1,1,rep(1,9)), size=7*5/14*0.8,hjust=0) +
  # fmt: skip
  # geom_text_repel(aes(label=label_water_red),col="darkblue",nudge_y=-45, size=7*5/14*0.8,hjust=0,fontface="italic") +
  # fmt: skip
  # geom_text(data=filter(data_fig,str_detect(Scenario,"SSP3-7.0")), aes(label=label_slope),col="#6c8364",nudge_y=45, size=7*5/14*0.8,parse=T,hjust=0) +
  # fmt: skip
  geom_segment(data=df_close_final,aes(x=Water_Impact+1000/desalination_cost/2,xend=df_close_final$Water_Impact-1000/desalination_cost/2,y=df_close_final$Cost-1000/2,yend=df_close_final$Cost+1000/2), linetype="dashed", color="#0072B2",linewidth=0.25) +
  geom_point(data=df_close_final,col="#0072B2",size=1) +
  # fmt: skip
  annotate("text", x = df_close_final$Water_Impact[3]+100, y = df_close_final$Cost[3]+150, label = paste0("'Desalination: ' * " ,desalination_cost, " * ' USD/m'^3"), color = "#0072B2", size = 7*5/14*0.8,parse=T,hjust=0) +
  labs(x = expression("Total Freshwater Impact (billion " ~ m^3 * "-eq)"), y = "Total Cost (billion USD)", col = "") +
  # labs(x = expression("Freshwater Impact (" ~ m^3 ~ " per ton Cu)"), y = "Cost (USD per ton Cu)", col = "") +
  # stat_function(fun = function(x) params[2, ]$a + params[2, ]$b * log(x), color = "blue", linewidth = 1) + # check log fit
  scale_y_continuous(
    labels = dollar_format(big.mark = ",", prefix = "$"),
    sec.axis = sec_axis(
      ~ (. - df_opt$Cost) / df_opt$Cost,
      name = "Change in Cost relative to Optimal Cost All Basins (%)",
      labels = scales::percent
    )
  ) +
  scale_x_continuous(
    labels = scales::label_comma(),
    sec.axis = sec_axis(
      ~ (. - df_opt$Water_Impact) / df_opt$Water_Impact,
      name = "Change in Freshwater Impact relative to Optimal Cost All Basins (%)",
      labels = scales::percent
    )
  ) +
  coord_cartesian(
    xlim = c(min(data_fig_final$Water_Impact), max((data_fig_final$Water_Impact))),
    ylim = c(min(data_fig_final$Cost), max((data_fig_final$Cost))),
    expand = F
  ) +
  # scale_colour_manual(values = rev(c("#EBCF2EFF", "#88AB38FF", "#5E9432FF", "#225F2FFF", "black"))) +
  guides(color = "none") +
  theme_pb_wide() +
  theme(
    panel.grid = element_blank(),
    # legend.position = "none",
    axis.title.y.right = element_text(size = 6),
    axis.text.y.right = element_text(size = 6),
    axis.title.x.top = element_text(size = 6),
    axis.text.x.top = element_text(size = 6)
  )

# ggsave("Figures/Fig2_Biodiversity.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)
# fmt: skip
ggsave("Figures/Fig2_Biodiversity_v2.png",ggplot2::last_plot(),units = 'cm',dpi = 600,width = 8.7 * 1.3,height = 8.7)

# PRODUCTION FISH INDEX SCENARIO  ---------------------------

# Store results from optimization run
(runs <- list.files("Results/Optimization/BioDScenario/NZE/", recursive = T, full.names = TRUE) |>
  (\(x) {
    x[(!str_detect(x, "SP_") & !str_detect(x, "Metrics") & !str_detect(x, "Slack") & !str_detect(x, "Inputs"))]
  })())


opt_results <- do.call(
  rbind,
  lapply(runs, function(folder_path) {
    transform(read.csv(folder_path), file_name = basename(folder_path), Scenario = basename(dirname(folder_path)))
  })
)
opt_results <- opt_results |> filter(ktons_extracted > 0 | capacity_added > 0 | mine_opened > 0) # reduce size
head(opt_results)
unique(opt_results$file_name)
table(opt_results$Scenario)

opt_results <- opt_results |>
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
      str_detect(file_name, "Eps25") ~ "25%",
    ) |>
      # fmt: skip
      factor(levels = rev(c("No Water Constraint","0%","0.5%","1%","2%","3%","4%","5%","6%","8%","10%","12%","15%","20%","25%")))
  ) |>
  mutate(
    Scenario = case_when(
      Scenario == "none" ~ "All Basins",
      Scenario == "FI99" ~ "Fish Index < 99.9",
      Scenario == "FI90" ~ "Fish Index < 90",
      Scenario == "FI80" ~ "Fish Index < 80",
      Scenario == "FI70" ~ "Fish Index < 70",
      Scenario == "FI60" ~ "Fish Index < 60",
      Scenario == "FI50" ~ "Fish Index < 50",
      T ~ Scenario
    ) |>
      factor(
        levels = c(
          "All Basins",
          "Fish Index < 99.9",
          "Fish Index < 90",
          "Fish Index < 80",
          "Fish Index < 70",
          "Fish Index < 60",
          "Fish Index < 50"
        )
      )
  )

# DEPOSIT already downloaded above

prod <- opt_results |>
  left_join(deposit) |>
  mutate(
    copper = ktons_extracted / 1000 * grade_resource_Copper * recovery_rate_Copper / 100,
    nickel = ktons_extracted / 1000 * grade_resource_Nickel * recovery_rate_Nickel / 100,
    cobalt = ktons_extracted / 1000 * grade_resource_Cobalt * recovery_rate_Cobalt / 100,
    lithium = ktons_extracted / 1000 * grade_resource_Lithium * recovery_rate_Lithium / 100,
  ) |>
  group_by(Scenario, metric, country) |>
  reframe(Copper = sum(copper), Nickel = sum(nickel), Cobalt = sum(cobalt), Lithium = sum(lithium), ) |>
  filter(Copper + Nickel + Cobalt + Lithium > 0) |>
  ungroup()


# Filter by Fish Index
table(prod$Scenario)
data_fig <- prod |> filter(str_detect(Scenario, "70")) |> filter(metric != "No Water Constraint")


# Long format
data_fig <- data_fig |>
  dplyr::select(Scenario, metric, country, Copper, Nickel, Cobalt, Lithium) |>
  pivot_longer(c(Copper, Nickel, Cobalt, Lithium), names_to = 'Mineral', values_to = 'total_metal') |>
  mutate(Mineral = factor(Mineral, levels = c("Copper", "Nickel", "Cobalt", "Lithium")))

# Add slack as demand unmet
demand_long <- demand |>
  filter(Scenario == "NZE") |>
  pivot_longer(c(Copper, Nickel, Cobalt, Lithium), names_to = 'Mineral', values_to = 'total_demand') |>
  group_by(Mineral) |>
  reframe(total_demand = sum(total_demand) / 1e3) |> # in million tons
  ungroup()

unmet <- data_fig |>
  group_by(Scenario, metric, Mineral) |>
  reframe(total_metal = sum(total_metal)) |>
  ungroup() |>
  left_join(demand_long) |>
  mutate(unmet_demand = total_demand - total_metal) |>
  dplyr::select(-total_metal, -total_demand) |>
  filter(unmet_demand > 1e-3) |>
  rename(total_metal = unmet_demand) |>
  mutate(country = "Unmet Demand")

# Key countries by mineral
data_fig |>
  filter(metric == "0%") |>
  group_by(Mineral) |>
  slice_max(order_by = total_metal, n = 10, with_ties = FALSE) |>
  summarise(Top10_countries = paste(country, collapse = ", "), .groups = "drop")


# Dict region already loaded
data_fig <- data_fig |>
  rbind(unmet) |>
  left_join(dict_region) |>
  mutate(
    region = case_when(
      country == "Unmet Demand" ~ "Unmet Demand",
      # fmt: skip
      Mineral == "Copper" & country %in% c("Chile", "Peru", "Indonesia", "Russia", "USA", "China", "Mongolia", "Dem. Rep. Congo", "Mexico","Kazakhstan") ~ ISO3,
      Mineral == "Nickel" &
        country %in%
          c("Indonesia", "Philippines", "Russia", "Australia", "New Caledonia", "Brazil", "USA", "Canada") ~ ISO3,
      Mineral == "Cobalt" & country %in% c("Dem. Rep. Congo", "Indonesia", "Australia", "USA", "Philippines") ~ ISO3,
      Mineral == "Lithium" &
        country %in%
          c("Chile", "Dem. Rep. Congo", "Argentina", "USA", "Australia", "Brazil", "Canada", "Mexico") ~ ISO3,
      T ~ "RoW"
    ) |>
      str_replace("COD", "DRC")
  ) |>
  group_by(Scenario, metric, region, Mineral) |>
  reframe(total_metal = sum(total_metal)) |>
  ungroup() |>
  group_by(Scenario, metric, Mineral) |>
  mutate(share = total_metal / sum(total_metal), ) |>
  ungroup()

data_fig <- data_fig |> mutate(region = factor(region, levels = rev(names(region_colors))))

# add total water impact per scenario
impact <- df |> group_by(Scenario, metric) |> reframe(Water_Impact = mean(Water_Impact)) |> ungroup()

data_fig <- data_fig |> left_join(impact)


data_fig <- data_fig |> mutate(region_label = if_else(share > 0.01, as.character(region), ""))

# order by share region
order_col <- data_fig |>
  mutate(order_col = paste0(Mineral, region)) |>
  arrange(desc(share)) |>
  pull(order_col) |>
  unique()
# put RoW and Unmet Demand at the end
order_col <- c(order_col[!str_detect(order_col, "RoW")], order_col[str_detect(order_col, "RoW")])
order_col <- c(order_col[!str_detect(order_col, "Unmet Demand")], order_col[str_detect(order_col, "Unmet Demand")])
data_fig <- data_fig |> mutate(order_col = paste0(Mineral, region) |> factor(levels = rev(order_col)))

ggplot(data_fig, aes(Water_Impact, total_metal, fill = region, group = order_col)) +
  geom_area(col="black",linewidth=0.1) +
  # geom_col(col="black",linewidth=0.1) +
  geom_text(data=filter(data_fig, metric == "0%"), aes(x=Water_Impact-100,label = region_label),hjust=1,, position = position_stack(vjust = 0.5), size = 7 * 5 / 14 * 0.8, col="white") +
  facet_wrap(~Mineral, ncol = 2, scales = "free") +
  scale_fill_manual(values = region_colors) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma, breaks = seq(6000, 9000, 1000)) +
  coord_cartesian(expand = F) +
  labs(
    x = expression("Total Freshwater Impact (billion " ~ m^3 * "-eq)"),
    y = "",
    title = "2025-2050 Metal production, in million tons",
    subtitle = "Including only basins with Fish Index < 70"
  ) +
  theme_pb_wide() +
  theme(legend.position = "none", panel.spacing.x = unit(1.2, "lines"))

# fmt: skip
ggsave("Figures/Fig2_prod_biodiversity.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)


# BY CLIMATE SCENARIO -----

# Store results from optimization run
(runs <- list.files(
  "Results/Optimization/ClimateScenario/NZE/",
  pattern = "Metrics.*",
  recursive = T,
  full.names = TRUE
))

# filter by driver gfdl-esm4
# runs <- runs[str_detect(runs, "gfdl-esm4")]

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
head(obj)
unique(obj$file_name)
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
      str_detect(file_name, "Eps25") ~ "25%",
    ) |>
      # fmt: skip
      factor(levels = c("No Water Constraint", "0%", "0.5%", "1%", "2%", "3%", "4%", "5%", "6%", "8%", "10%", "12%", "15%", "20%", "25%"))
  )
# mutate(
#   Scenario = case_when(
#     str_detect(folder_path, "picontrol") ~ "Pre-industrial control",
#     str_detect(folder_path, "ssp126") ~ "SSP1-2.6",
#     str_detect(folder_path, "ssp370") ~ "SSP3-7.0",
#     str_detect(folder_path, "ssp585") ~ "SSP5-8.5",
#     TRUE ~ "No Climate Scenario"
#   )
# ) |>
# mutate(
#   climateDriver = case_when(
#     str_detect(folder_path, "gfdl-esm4") ~ "gfdl-esm4",
#     str_detect(folder_path, "ipsl-cm6a-lr") ~ "ipsl-cm6a-lr",
#     str_detect(folder_path, "mpi-esm1-2-hr") ~ "mpi-esm1-2-hr",
#     str_detect(folder_path, "mri-esm2-0") ~ "mri-esm2-0",
#     str_detect(folder_path, "ukesm1-0-ll") ~ "ukesm1-0-ll",
#     T ~ "No Climate Scenario"
#   )
# )

table(obj$Scenario)
table(obj$climateDriver)
table(obj$metric)
table(obj$Parameter)
obj$climateDriver = "gfdl-esm4" # dummy to reuse old code

df <- obj |>
  filter(!is.na(Scenario)) |>
  pivot_wider(names_from = Parameter, values_from = Value) |>
  # left_join(dem_tot) |>
  mutate(
    Water_cons = Water / 1e3, # in billion m3
    Cost = Cost / 1e3, # billion USD (discounted)
    Water_Impact = `Water impact` / 1e3, # billion m3 world equiv.
    lab_metric = case_when(
      metric == "0%" ~ "Optimal Cost",
      metric == "0.5%" ~ paste0("", metric, " Cost Increase"),
      T ~ metric
    )
  ) |>
  mutate(`Water impact` = NULL)
head(df)
range(df$Water_Impact)


# Slope as difference between points
df <- df |>
  arrange(Scenario, metric) |>
  mutate(slope = -(Cost - lag(Cost)) / (Water_Impact - lag(Water_Impact))) |>
  mutate(label_slope = if_else(metric %in% c("0.5%", "4%", "15%"), paste0("", round(slope, 2), " * ' USD/m'^3"), ""))

# Get % of water red between first point
base_water <- df |>
  filter(metric == "0%") |>
  rename(water_base = Water_Impact) |>
  dplyr::select(Scenario, climateDriver, water_base)
df <- df |>
  filter(!str_detect(metric, "Water")) |>
  left_join(base_water) |>
  mutate(water_red_pct = (Water_Impact - water_base) / water_base * 100) |>
  mutate(
    label_water_red = if_else(metric %in% c("0%", "1%", "3%", "4%", "8%"), "", paste0("", round(water_red_pct), "%"))
  )

# FIGURE - Pareto curves: non-dominated solutions

desalination_cost <- 0.5 # USD per m3
# pick closest point to slope
df_close <- df |> group_by(Scenario) |> slice_min(abs(slope - desalination_cost), n = 1)


data_fig <- df |> filter(!str_detect(metric, "Water")) |> mutate(group_key = paste0(Scenario, climateDriver))
range(data_fig$Water_Impact)
range(data_fig$Cost)

data_fig <- data_fig |> filter(Scenario != "Pre-industrial control")

ggplot(data_fig, aes(Water_Impact, Cost, col = Scenario, group = group_key)) +
  geom_line() +
  geom_point(size=0.5) +
  # fmt: skip
  geom_text_repel(data = filter(data_fig, metric == "0%",climateDriver=="gfdl-esm4"),aes(label = Scenario),size = 7 * 5 / 14 * 0.8,hjust = 0, direction = "y",
  nudge_x = 200) +
  # fmt: skip
  # geom_text(data=filter(data_fig,str_detect(Scenario,"SSP3-7.0")), aes(label=lab_metric),col="#4d4d4d",nudge_y=45*c(-1,1,1,rep(1,9)), size=7*5/14*0.8,hjust=0) +
  # fmt: skip
  # geom_text_repel(aes(label=label_water_red),col="darkblue",nudge_y=-45, size=7*5/14*0.8,hjust=0,fontface="italic") +
  # fmt: skip
  # geom_text(data=filter(data_fig,str_detect(Scenario,"SSP3-7.0")), aes(label=label_slope),col="#6c8364",nudge_y=45, size=7*5/14*0.8,parse=T,hjust=0) +
  # fmt: skip
  # geom_segment(data=df_close,aes(x=Water_Impact+1000/desalination_cost/2,xend=df_close$Water_Impact-1000/desalination_cost/2,y=df_close$Cost-1000/2,yend=df_close$Cost+1000/2), linetype="dashed", color="#bc80bd") +
  # fmt: skip
  # annotate("text", x = df_close$Water_Impact[2]-200, y = df_close$Cost[2]+400, label = paste0("'Desalination ~' * " ,desalination_cost, " * ' USD/m'^3"), color = "#bc80bd", size = 7*5/14*0.8,parse=T,hjust=0) +
  labs(x = expression("Total Freshwater Impact (billion " ~ m^3 ~ "-eq)"), y = "Total Cost (billion USD)", col = "") +
  # labs(x = expression("Freshwater Impact (" ~ m^3 ~ " per ton Cu)"), y = "Cost (USD per ton Cu)", col = "") +
  # stat_function(fun = function(x) params[2, ]$a + params[2, ]$b * log(x), color = "blue", linewidth = 1) + # check log fit
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$")) +
  scale_x_continuous(labels = scales::label_comma()) +
  coord_cartesian(xlim = c(2100, 9500)) +
  # scale_color_manual(values = c("#378bc9", "#d94253", "#6c8364")) +
  theme_bw(8) +
  theme(panel.grid = element_blank(), legend.position = "none")

ggsave("Figures/Fig2_Climate.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)
# ggsave("Figures/TradeOff_perTon.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 1.2, height = 8.7)

# BY MINERAL ----------------------
# Need to split costs and water allocation at deposit level, based on revenue share at each deposit

# Get total mineral production - total metal production previously loaded

# First get share of costs per deposit
prices <- read.csv("Parameters/MineralPrices.csv") # 2021-2025 avg price per ton

# revenue shares (depends only on grade, recovery rate and price) - NO RESULT FROM THE model
revShare <- deposit |>
  mutate(
    # fmt: skip
    copper_revenue = grade_resource_Copper * recovery_rate_Copper / 100 * prices[prices$Mineral == "Copper", ]$price_avg,
    # fmt: skip
    nickel_revenue = grade_resource_Nickel * recovery_rate_Nickel / 100 * prices[prices$Mineral == "Nickel", ]$price_avg,
    # fmt: skip
    cobalt_revenue = grade_resource_Cobalt * recovery_rate_Cobalt / 100 * prices[prices$Mineral == "Cobalt", ]$price_avg,
    # fmt: skip
    lithium_revenue = grade_resource_Lithium * recovery_rate_Lithium / 100 * prices[prices$Mineral == "Lithium", ]$price_avg
  ) |>
  mutate(total_revenue = copper_revenue + nickel_revenue + cobalt_revenue + lithium_revenue) |>
  mutate(
    copper_share = copper_revenue / total_revenue,
    nickel_share = nickel_revenue / total_revenue,
    cobalt_share = cobalt_revenue / total_revenue,
    lithium_share = lithium_revenue / total_revenue
  ) |>
  dplyr::select(ID, Name, copper_share, nickel_share, cobalt_share, lithium_share)

# get total production by deposit
data_fig_d <- opt_results |>
  left_join(deposit) |>
  mutate(
    copper = ktons_extracted / 1000 * grade_resource_Copper * recovery_rate_Copper / 100,
    nickel = ktons_extracted / 1000 * grade_resource_Nickel * recovery_rate_Nickel / 100,
    cobalt = ktons_extracted / 1000 * grade_resource_Cobalt * recovery_rate_Cobalt / 100,
    lithium = ktons_extracted / 1000 * grade_resource_Lithium * recovery_rate_Lithium / 100
  )

# calculate costs and water impacts
optInputs <- read.csv("Results/Optimization/DemandScenario/SPS/OptimizationInputs.csv")
(r <- optInputs |> filter(Parameter == "Discount rate") |> pull(Value)) # 7%
depositCosts <- depositAll |> dplyr::select(ID, Name, OPEX_ore, CAPEX_opening, CAPEX_exp, share_NiCoCu, water_footprint)

# residual value
mine_life <- 15
fraction_not_recovered <- 0.2

data_fig_d <- data_fig_d |>
  left_join(depositCosts) |>
  # ALL THIS CODE JUST TO GET REMAINING FRACTION
  mutate(
    years_to_end = 2050 - t,
    remaining_life = mine_life - years_to_end,
    frac = pmin(pmax(remaining_life / mine_life, 0), 1),
    CAPEX_opening_adj = CAPEX_opening - (1 - fraction_not_recovered) * CAPEX_opening * frac,
    CAPEX_exp_adj = CAPEX_exp - (1 - fraction_not_recovered) * CAPEX_exp * frac
  ) |>
  mutate(
    # fmt: skip
    costs = (ktons_extracted * OPEX_ore / 1e3 + mine_opened * CAPEX_opening_adj + capacity_added * CAPEX_exp_adj / 1e3) * share_NiCoCu/1e3, # billion USD
    costs = costs / (1 + r)^(t - 2025), # discount them
    water = ktons_extracted * water_footprint / 1e6 # to billion m3
  ) |>
  select(-years_to_end, -remaining_life, -frac) |>
  # Allocate costs at each deposit based on revenue share
  left_join(revShare) |>
  mutate(
    copper_cost = copper_share * costs,
    nickel_cost = nickel_share * costs,
    cobalt_cost = cobalt_share * costs,
    lithium_cost = lithium_share * costs,
    copper_water = copper_share * water,
    nickel_water = nickel_share * water,
    cobalt_water = cobalt_share * water,
    lithium_water = lithium_share * water
  ) |>
  group_by(Scenario, metric) |>
  reframe(
    Copper = sum(copper), # million tons
    Nickel = sum(nickel),
    Cobalt = sum(cobalt),
    Lithium = sum(lithium),
    copper_cost = sum(copper_cost), # billion USD
    nickel_cost = sum(nickel_cost),
    cobalt_cost = sum(cobalt_cost),
    lithium_cost = sum(lithium_cost),
    costs = sum(costs), # billion USD
    copper_water = sum(copper_water), # billion m3
    nickel_water = sum(nickel_water),
    cobalt_water = sum(cobalt_water),
    lithium_water = sum(lithium_water),
    water = sum(water) # billion m3
  ) |>
  ungroup()


# Add slack costs
(runs_slack <- list.files("Results/Optimization/DemandScenario", pattern = "Slack.*", recursive = T, full.names = TRUE))
slack <- do.call(
  rbind,
  lapply(runs_slack, function(folder_path) {
    transform(read.csv(folder_path), file_name = basename(folder_path), Scenario = basename(dirname(folder_path)))
  })
)

slack <- slack |>
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
      str_detect(file_name, "Eps25") ~ "25%",
    ) |>
      # fmt: skip
      factor(levels = c("No Water Constraint", "0%", "0.5%", "1%", "2%", "3%", "4%", "5%", "6%", "8%", "10%", "12%", "15%", "20%", "25%"))
  )

slack_costs <- slack |>
  mutate(
    slack_cu = slack_cu * optInputs[optInputs$Parameter == "Slack cost Copper", ]$Value / (1 + r)^(t - 2025),
    slack_ni = slack_ni * optInputs[optInputs$Parameter == "Slack cost Nickel", ]$Value / (1 + r)^(t - 2025),
    slack_co = slack_co * optInputs[optInputs$Parameter == "Slack cost Cobalt", ]$Value / (1 + r)^(t - 2025),
    slack_li = slack_li * optInputs[optInputs$Parameter == "Slack cost Lithium", ]$Value / (1 + r)^(t - 2025)
  ) |>
  group_by(Scenario, metric) |>
  reframe(
    slack_cu = sum(slack_cu) / 1e3, # billion USD
    slack_ni = sum(slack_ni) / 1e3,
    slack_co = sum(slack_co) / 1e3,
    slack_li = sum(slack_li) / 1e3
  ) |>
  ungroup()

# add to data
data_fig_d <- data_fig_d |>
  left_join(slack_costs) |>
  mutate(
    copper_cost = copper_cost + slack_cu,
    nickel_cost = nickel_cost + slack_ni,
    cobalt_cost = cobalt_cost + slack_co,
    lithium_cost = lithium_cost + slack_li,
    costs = costs + slack_cu + slack_ni + slack_co + slack_li
  )

# Get values per ton
data_fig_d <- data_fig_d |>
  mutate(
    Copper_CostperTon = copper_cost * 1e3 / (Copper), # USD per ton
    Nickel_CostperTon = nickel_cost * 1e3 / (Nickel),
    Cobalt_CostperTon = cobalt_cost * 1e3 / (Cobalt),
    Lithium_CostperTon = lithium_cost * 1e3 / (Lithium),
    Copper_WaterperTon = copper_water * 1e3 / (Copper), # m3 per ton
    Nickel_WaterperTon = nickel_water * 1e3 / (Nickel),
    Cobalt_WaterperTon = cobalt_water * 1e3 / (Cobalt),
    Lithium_WaterperTon = lithium_water * 1e3 / (Lithium)
  )

# Gather for figure
data_fig_d_long <- data_fig_d |>
  dplyr::select(Scenario, metric, contains("perTon")) |>
  pivot_longer(cols = -c(Scenario, metric), names_to = c("Mineral", "Type"), names_sep = "_", values_to = "Value") |>
  pivot_wider(names_from = Type, values_from = Value) |>
  mutate(
    lab_metric = case_when(
      metric == "0%" ~ "Optimal Cost",
      metric == "0.5%" ~ paste0("", metric, " Cost Increase"),
      T ~ metric
    )
  )
names(data_fig_d_long) <- names(data_fig_d_long) |> str_remove("perTon")


data_fig_d_long <- data_fig_d_long |> filter(Scenario == "NZE") |> filter(metric != "No Water Constraint")
table(data_fig_d_long$metric)


# either with log scale or facet...
ggplot(data_fig_d_long, aes(Water, Cost, col = Mineral)) +
  geom_line() +
  geom_point(size=0.5) +
  # facet_wrap(~Mineral, scales = "free") +
  # fmt: skip
  geom_text(data = filter(data_fig_d_long, metric == "0%"),aes(label = Mineral),nudge_y = -500*c(1,1,1,1),nudge_x=0,size = 7 * 5 / 14 * 0.8,hjust = 0.5) +
  labs(x = expression("Freshwater Impact (" ~ m^3 * "-eq per ton)"), y = "Cost (USD per ton)", col = "") +
  scale_y_continuous(
    labels = dollar_format(big.mark = ",", prefix = "$")
    # breaks = c(1000, 10000, 100000)
  ) +
  scale_x_continuous(labels = scales::label_comma()) +
  # coord_cartesian(xlim = c(1700, 8100)) +
  scale_color_manual(values = minerals_colors) +
  theme_pb_wide() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title.y.right = element_text(size = 5),
    axis.text.y.right = element_text(size = 5),
    axis.title.x.top = element_text(size = 5),
    axis.text.x.top = element_text(size = 5),
    strip.placement = "outside"
  )

# fmt: skip
ggsave("Figures/Fig2_Mineral.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7, height = 8.7)

# Secondary axis using patchwork
# secondary axis range
max_metrics <- data_fig_d_long |>
  filter(metric == "0%") |>
  rename(maxCost = Cost, maxWater = Water) |>
  dplyr::select(Mineral, maxCost, maxWater)

make_plot <- function(mineral_name) {
  df_m <- data_fig_d_long |> dplyr::filter(Mineral == mineral_name)
  mm_m <- max_metrics |> dplyr::filter(Mineral == mineral_name)

  ggplot(df_m, aes(Water, Cost, col = Mineral)) +
    geom_line() +
    geom_point(size = 0.5) +
    labs(
      title = mineral_name,
      x = expression("Freshwater Impact (" ~ m^3 * "-eq per ton)"),
      y = "Cost (USD per ton)",
      col = ""
    ) +
    scale_y_continuous(
      labels = dollar_format(big.mark = ",", prefix = "$"),
      sec.axis = sec_axis(
        ~ (. - mm_m$maxCost) / mm_m$maxCost,
        name = "Change in Cost relative to Optimal Cost (%)",
        labels = scales::percent
      )
    ) +
    scale_x_continuous(
      labels = scales::label_comma(),
      sec.axis = sec_axis(
        ~ (. - mm_m$maxWater) / mm_m$maxWater,
        name = "Change in Freshwater Impact relative to Optimal Cost (%)",
        labels = scales::percent
      )
    ) +
    scale_color_manual(values = minerals_colors) +
    theme_pb_wide() +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid = element_blank(),
      legend.position = "none",
      axis.title.y.right = element_text(size = 5),
      axis.text.y.right = element_text(size = 5),
      axis.title.x.top = element_text(size = 5),
      axis.text.x.top = element_text(size = 5)
    )
}

library(patchwork)

minerals <- c("Copper", "Nickel", "Cobalt", "Lithium")

plots <- lapply(minerals, make_plot)

wrap_plots(plots, ncol = 2) + plot_layout(axes = "collect")

# fmt: skip
ggsave("Figures/Fig2_Mineral_facet.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*1.5, height = 8.7)

# EoF
