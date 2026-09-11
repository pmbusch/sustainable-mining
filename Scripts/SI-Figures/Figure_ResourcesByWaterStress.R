# Cumulative Resource Analysis by Water Stress
# Purpose: Visualize how mineral resources accumulate across water stress levels
# Shows both absolute cumulative resources and percentage of total resources
# PBH Feb 2026

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
source("Scripts/00a-Common Variables.R", encoding = "UTF-8")

# Load deposit data ----
deposits <- read_csv("Parameters/Deposit.csv")

# Calculate water stress ----
# Stress = water demand / (demand + available water)
deposits <- deposits |>
  mutate(gross_available = aware_available + aware_demand) |>
  mutate(water_stress = if_else(gross_available <= 0, 1, aware_demand / gross_available)) |>
  mutate(water_stress = if_else(aware_demand < 0, 0, water_stress)) |>
  mutate(water_stress = pmin(water_stress, 1)) # cap at 100%
range(deposits$water_stress, na.rm = TRUE) # check range of water stress values

# to million tons
deposits <- deposits |>
  mutate(resources_Copper = resources_Copper / 1e6) |>
  mutate(resources_Nickel = resources_Nickel / 1e6) |>
  mutate(resources_Cobalt = resources_Cobalt / 1e6) |>
  mutate(resources_Lithium = resources_Lithium / 1e6)


# Minerals to analyze
minerals <- c("Copper", "Nickel", "Cobalt", "Lithium")

deposits |>
  reframe(
    total_Copper = sum(resources_Copper, na.rm = TRUE),
    total_Nickel = sum(resources_Nickel, na.rm = TRUE),
    total_Cobalt = sum(resources_Cobalt, na.rm = TRUE),
    total_Lithium = sum(resources_Lithium, na.rm = TRUE)
  )

# Prepare data for all minerals ----
mineral_data_all <- map_dfr(unique(minerals), function(mineral) {
  col <- paste0("resources_", mineral)

  deposits |>
    filter(!is.na(.data[[col]]), !is.na(water_stress)) |>
    group_by(Basin_ID) |>
    summarise(water_stress = first(water_stress), resources = sum(.data[[col]]), .groups = "drop") |>
    arrange(water_stress) |>
    mutate(
      cumulative_resources = cumsum(resources),
      pct_resources = 100 * cumulative_resources / sum(resources),
      mineral = mineral
    )
})

mineral_data_all <- mineral_data_all |>
  group_by(mineral) |>
  arrange(water_stress, .by_group = TRUE) |>
  ungroup() |>
  mutate(mineral = factor(mineral, levels = c("Copper", "Nickel", "Cobalt", "Lithium")))

# Generate combined faceted plot ----

# with secondary axis
library(ggh4x)

totals <- mineral_data_all %>%
  group_by(mineral) %>%
  summarise(total = max(cumulative_resources, na.rm = TRUE), .groups = "drop")

# Build a list of scales, one per facet
y_scales <- lapply(totals$total, function(tot) {
  scale_y_continuous(
    labels = scales::comma_format(),
    expand = c(0, 0),
    sec.axis = sec_axis(~ . / tot * 100, name = "Share of total (%)", labels = scales::percent_format(scale = 1, accuracy = 1))
  )
})

ggplot(mineral_data_all, aes(x = water_stress, ymin = 0, ymax = cumulative_resources)) +
  geom_ribbon(aes(fill = mineral), alpha = 0.7, color = "black", linewidth = 0.3) +
  facet_wrap(~mineral, scales = "free", ncol = 2) +
  facetted_pos_scales(y = y_scales) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  scale_fill_manual(values = minerals_colors) +
  labs(x = "Basin Water Stress", y = "Cumulative Resources (million tonnes)") +
  theme_pb_large() +
  theme(
    legend.position = "none",
    axis.title.y.right = element_text(color = "#525252"),
    axis.text.y.right = element_text(color = "#525252")
  )


# Save plot ----
# fmt: skip
ggsave("Figures/ExtData-Figures/Resource_WaterStress.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)
# fmt: skip
ggsave("Figures/ExtData-Figures/Resource_WaterStress.svg",ggplot2::last_plot(),units = 'cm',dpi = 600,width = 8.7 * 2,height = 8.7 * 2)
group_svg_layers("Figures/ExtData-Figures/Resource_WaterStress.svg")


# FISH INDEX -----------
# Same plot for fish index

# Prepare data for all minerals ----
mineral_data_fish <- map_dfr(unique(minerals), function(mineral) {
  col <- paste0("resources_", mineral)

  deposits |>
    filter(!is.na(.data[[col]]), !is.na(fish_index)) |>
    group_by(Basin_ID) |>
    summarise(fish_index = first(fish_index), resources = sum(.data[[col]]), .groups = "drop") |>
    arrange(fish_index) |>
    mutate(
      cumulative_resources = cumsum(resources),
      pct_resources = 100 * cumulative_resources / sum(resources),
      mineral = mineral
    )
})

mineral_data_fish <- mineral_data_fish |>
  group_by(mineral) |>
  arrange(fish_index, .by_group = TRUE) |>
  ungroup() |>
  mutate(mineral = factor(mineral, levels = c("Copper", "Nickel", "Cobalt", "Lithium")))

# Generate combined faceted plot ----

totals <- mineral_data_fish %>%
  group_by(mineral) %>%
  summarise(total = max(cumulative_resources, na.rm = TRUE), .groups = "drop")

# Build a list of scales, one per facet
y_scales <- lapply(totals$total, function(tot) {
  scale_y_continuous(
    labels = scales::comma_format(),
    expand = c(0, 0),
    sec.axis = sec_axis(~ . / tot * 100, name = "Share of total (%)", labels = scales::percent_format(scale = 1, accuracy = 1))
  )
})

ggplot(mineral_data_fish, aes(x = fish_index, ymin = 0, ymax = cumulative_resources)) +
  geom_ribbon(aes(fill = mineral), alpha = 0.7, color = "black", linewidth = 0.3) +
  facet_wrap(~mineral, scales = "free", ncol = 2) +
  facetted_pos_scales(y = y_scales) +
  scale_fill_manual(values = minerals_colors) +
  labs(x = "Fish Biodiversity Index", y = "Cumulative Resources (million tonnes)") +
  theme_pb_large() +
  coord_cartesian(expand = F) +
  theme(
    legend.position = "none",
    axis.title.y.right = element_text(color = "#525252"),
    axis.text.y.right = element_text(color = "#525252")
  )


# Save plot ----
# fmt: skip
ggsave("Figures/ExtData-Figures/Resource_FishIndex.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)
# fmt: skip
ggsave("Figures/ExtData-Figures/Resource_FishIndex.svg",ggplot2::last_plot(),units = 'cm',dpi = 600,width = 8.7 * 2,height = 8.7 * 2)
group_svg_layers("Figures/ExtData-Figures/Resource_FishIndex.svg")
