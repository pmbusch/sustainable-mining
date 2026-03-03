# Comparison of freshwater impact vs fish biodiversity index at deposit level
# PBH Feb 2026

source('Scripts/00-Libraries.R', encoding = 'UTF-8')
source("Scripts/00a-Common Variables.R", encoding = "UTF-8")

deposits <- read_csv("Parameters/Deposit.csv")
fish <- read_csv("Parameters/FW_FISH/FW_FISH_Basin_FishIndex_Global.csv")

deposits <- deposits |> filter(!is.na(Basin_ID)) |> left_join(fish, by = "Basin_ID")
deposits$resources_ore <- deposits$resources_ore / 1e6

# deposits long
deposits_long <- deposits |>
  dplyr::select(
    ID,
    Basin_ID,
    country,
    water_footprint,
    fish_index,
    resources_Copper,
    resources_Nickel,
    resources_Cobalt,
    resources_Lithium
  ) |>
  pivot_longer(
    c(resources_Copper, resources_Nickel, resources_Cobalt, resources_Lithium),
    names_to = 'Mineral',
    values_to = 'mtons'
  ) |>
  mutate(mtons = mtons / 1e6, Mineral = str_remove(Mineral, "resources_")) |>
  mutate(Mineral = factor(Mineral, levels = c("Copper", "Nickel", "Cobalt", "Lithium")))

# Figure -----------
ggplot(deposits_long, aes(water_footprint, fish_index, size = mtons, col = Mineral)) +
  geom_point(alpha=.8) +
  annotate("text", x = 0.6, y = 0.001, label = "Each dot is a deposit", hjust = 1.1, vjust = -0.5, size = 3) +
  scale_color_manual(values = minerals_colors) +
  scale_x_log10(
    labels = function(x) {
      s <- scales::comma(x, accuracy = 0.01)
      sub("\\.$", "", sub("0+$", "", s))
    },
    name = expression("Freshwater Impact [" ~ m^3 * ~"world-eq / ton]")
  ) +
  scale_y_log10(
    labels = function(x) {
      s <- scales::comma(x, accuracy = 0.01)
      sub("\\.$", "", sub("0+$", "", s))
    },
    name = "Fish Biodiversity Index",
    breaks = c(0.01, 0.1, 1, 10, 100)
  ) +
  scale_size_continuous(
    labels = function(x) {
      s <- scales::comma(x, accuracy = 0.01)
      sub("\\.$", "", sub("0+$", "", s))
    },
    name = "Resources, million tons",
    trans = 'sqrt',
    breaks = c(0.1, 1, 10, 25, 50, 100, 150),
    range = c(0.3, 6)
  ) +
  theme_pb_wide() +
  theme(
    legend.position = c(0.9, 0.8),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.key.height = unit(0.3, "cm")
  )

# fmt: skip
ggsave("Figures/Deposit/Aware_Fish.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7*2, height = 8.7*2)

# 2d cumulative curves -------------

library(data.table)

dt <- as.data.table(deposits_long)

# bin to reduce resolution (adjust n for speed/precision)
n <- 100

dt[,
  w_bin := cut(
    water_footprint,
    breaks = unique(quantile(water_footprint, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)),
    include.lowest = TRUE
  ),
  by = Mineral
]

dt[,
  f_bin := cut(
    fish_index,
    breaks = unique(quantile(fish_index, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)),
    include.lowest = TRUE
  ),
  by = Mineral
]

grid <- dt[, .(mtons = sum(mtons)), by = .(Mineral, w_bin, f_bin)]

setorder(grid, Mineral, w_bin, f_bin)
grid[, cum_mtons := cumsum(mtons), by = .(Mineral, w_bin)]
grid[, cum_mtons := cumsum(cum_mtons), by = Mineral]
grid[, cum_share := cum_mtons / sum(mtons), by = Mineral]

ggplot(grid, aes(as.numeric(w_bin), as.numeric(f_bin), fill = cum_share)) +
  geom_tile() +
  geom_contour(aes(z = cum_share), breaks = c(.5, .8, .9), color = "black", linewidth = 0.3) +
  facet_wrap(~Mineral) +
  scale_fill_viridis_c(labels = scales::percent) +
  labs(x = "Water footprint (quantile bin)", y = "Fish index (quantile bin)", fill = "Cum. resources")


# Bar plot with quadrants ----------
deposits_long |>
  group_by(Mineral) |>
  mutate(
    w_high = water_footprint >= quantile(water_footprint, .8),
    f_high = fish_index >= quantile(fish_index, .8),
    quadrant = case_when(
      !w_high & !f_high ~ "Low water / Low fish",
      !w_high & f_high ~ "Low water / High fish",
      w_high & !f_high ~ "High water / Low fish",
      w_high & f_high ~ "High water / High fish"
    )
  ) |>
  ungroup() |>
  summarise(share = sum(mtons) / sum(mtons), .by = c(Mineral, quadrant)) |>
  ggplot(aes(quadrant, share, fill = quadrant)) +
  geom_col() +
  facet_wrap(~Mineral) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = "Share of resources")
