# ============================================================
# SI Figure - Climate scenario trade-offs
# Standalone figure
# PBH Mar 2026
# ============================================================

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/00a-Common Variables.R", encoding = "UTF-8")


# fmt: skip
metric_levels <- c("No Water Constraint","0%","0.5%","1%","2%","3%","4%","5%","6%","8%","10%","12%","15%","20%","25%")

runs <- list.files(
  "Results/Optimization/ClimateScenario/NZE/",
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
      Scenario = basename(dirname(folder_path)),
      file_name = basename(folder_path)
    )
  })
)

table(obj$Scenario)

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

df <- obj |>
  filter(!is.na(Scenario)) |>
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

base_water <- df |> filter(metric == "0%") |> rename(water_base = Water_Impact) |> dplyr::select(Scenario, water_base)

df <- df |>
  filter(!str_detect(metric, "Water")) |>
  left_join(base_water, by = c("Scenario")) |>
  mutate(water_red_pct = (Water_Impact - water_base) / water_base * 100) |>
  mutate(label_water_red = if_else(metric %in% c("0%", "1%", "3%", "4%", "8%"), "", paste0(round(water_red_pct), "%")))

desalination_cost <- 0.5
df_close <- df |> group_by(Scenario) |> slice_min(abs(slope - desalination_cost), n = 1) |> ungroup()


p_climate <- ggplot(df, aes(Water_Impact, Cost, col = Scenario, group = Scenario)) +
  geom_line() +
  geom_point(size = 0.5) +
  geom_text_repel(
    data = filter(df, metric == "0%"),
    aes(label = Scenario),
    size = 7 * 5 / 14 * 0.8,
    hjust = 0,
    direction = "y",
    nudge_x = 200
  ) +
  # geom_text_repel(aes(label=label_water_red),col="darkblue",nudge_y=-45, size=7*5/14*0.8,hjust=0,fontface="italic") +
  labs(x = expression("Total Freshwater Impact (billion " ~ m^3 * "-eq)"), y = "Total Cost (billion USD)", col = "") +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$")) +
  scale_x_continuous(labels = scales::label_comma()) +
  coord_cartesian(xlim = c(2100, 9500)) +
  theme_bw(8) +
  theme(panel.grid = element_blank(), legend.position = "none")
p_climate

# fmt: skip
ggsave("Figures/SI/Fig2_Climate.png",p_climate,units = "cm",dpi = 600,width = 8.7,height = 8.7)
