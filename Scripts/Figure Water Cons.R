# Water Results analysis

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
source('Scripts/00a-Common Variables.R', encoding = 'UTF-8')
(dict_scen <- tibble(Scenario = scens_selected, name = name_abbr))


# Files with results
url_results <- "Results/Optimization/DemandScenario"
url_results <- "Results/Optimization/Copper"

(runs <- list.files(url_results, recursive = T, full.names = TRUE) |>
  keep(~ str_detect(basename(.x), "^(Base.*|Water.*)$") && !str_detect(basename(.x), "_")))


source("Scripts/01-LoadOptimizationResults.R", encoding = "UTF-8")

data_fig <- df_results %>%
  left_join(deposit) %>%
  filter(t < 2051) %>%
  # filter(t==2050) %>% # uncomment for 2050 analysis
  mutate(
    water_m3 = ktons_extracted * water_footprint / 1e3 # million m3
  ) |>
  mutate(name = Scenario) |>
  group_by(name, cost_deg, t) %>%
  reframe(mtons = sum(ktons_extracted) / 1e3, water_m3 = sum(water_m3) / 1e3) |> # water to billion
  ungroup()

# Pick key scenarios
unique(data_fig$name)

dict <- tibble(
  name = c(
    "Ambitious-Baseline-Baseline-Baseline-Baseline",
    "Ambitious-Baseline-High Capacity-Baseline-Baseline",
    "Ambitious-Baseline-Baseline-Baseline-Enhanced recycling"
  ),
  abbr = c("Baseline", "High Demand", "Low Demand")
)

data_fig <- data_fig |>
  left_join(dict) |>
  filter(!is.na(abbr)) |>
  mutate(key = paste0(cost_deg, "_", abbr)) |>
  # filter(key %in% c("Ref._0", "Large LIB_0", "Small LIB_0", "Ref._1", "Ref._5"))
  # filter(name %in% c("Ref.", "Large LIB", "Small LIB"), cost_deg %in% c(0, 1, 5))
  filter(cost_deg %in% c(0, 1, 5))


ggplot(data_fig, aes(t, water_m3, col = abbr, group = key)) +
  geom_line() +
  geom_text(data=filter(data_fig,t==2050,cost_deg==0), aes(label=abbr), nudge_x = 0.5, size=7*5/14*0.8,hjust=0) +
  annotate("text", x = 2025.5, y = 150, label = "0% Cost deg.", size = 8 * 5 / 14 * 0.8, hjust = 0) +
  annotate("text", x = 2025.5, y = 105, label = "1% Cost deg.", size = 8 * 5 / 14 * 0.8, hjust = 0) +
  annotate("text", x = 2025.5, y = 72, label = "5% cost deg.", size = 8 * 5 / 14 * 0.8, hjust = 0) +
  scale_color_manual(values = c("Baseline" = "#000000", "High Demand" = "#D62728", "Low Demand" = "#2CA02C")) +
  theme_bw() +
  coord_cartesian(expand = F) +
  xlim(2025, 2053) +
  ylim(0, 190) +
  labs(x = "Year", y = expression("Freshwater Impact [billion " ~ m^3 * ~" per year]"), col = "") +
  theme(legend.position = "none", panel.grid = element_blank())

# url_save <- "Figures/water_ts.png"
url_save <- "Figures/Cu_water_ts.png"
ggsave(url_save, ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7)


data_fig2 <- data_fig %>% group_by(key) %>% reframe(mtons = sum(mtons) / 1e3, water = sum(water)) |> ungroup()

data_fig2 |> ggplot(aes(key, water, fill = key)) + geom_col() + theme_bw() + coord_flip()

data_fig2 |> ggplot(aes(mtons, water, col = key)) + geom_point(size=3) + theme_bw()

# EoF
