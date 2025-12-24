# Levelized cost vs Water impact
# PBH Dec 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Copper ------------

cu <- read.csv("Parameters/Cu_Deposit.csv")


range(cu$aware_cf)
range(cu$water_footprint)
range(cu$water)

# Levelized cost of extraction
# Assume 20 mine life, 2% depletion rate (for expansion), 80% of resources available
# opening is in million usd
# expansion is in usd per tpa
mine_life <- 20
prod_rate <- 0.04
cu <- cu |>
  mutate(
    prod = resources * prod_rate * 0.8,
    prod = if_else(cap2025 > prod, cap2025, prod - cap2025), # existing capacity
    total_extraction = prod * mine_life,
    total_extraction = if_else(total_extraction > resources * 0.8, resources * 0.8, total_extraction),
    level_cost = OPEX + (CAPEX_opening * 1e6 + CAPEX_exp * prod) / total_extraction,
    status_open = if_else(status == "Production", "Open", "On folder")
  )

# FIX: mine opening pro rated to total extraction over 20 mine life
# capex is additional to existing capacity

cu |>
  mutate(resources = resources / 1e6) |>
  filter(resources > 0.5) |>
  ggplot(aes(water_footprint, level_cost, col = status_open)) +
  geom_point(aes(size=resources),alpha=0.7,stroke = 0) +
  # scale_color_gradient2(low = "darkblue",midpoint=0.1,mid="grey", high = "darkred",
  #                       trans="log10",breaks=legend_breaks,labels=legend_breaks) +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$"), limits = c(0, 13) * 1e3) +
  # scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$")) +
  # scale_x_continuous(labels = comma) +
  # xlim(0, 2000) +
  # xlim(0, 60e3) +
  scale_x_log10(labels = comma) +
  scale_size_continuous(breaks = c(10, 50, 100)) +
  coord_cartesian(expand = F) +
  # scale_color_manual(values = c("Open Pit" = "#8B5742", "Underground" = "#8B7D7B")) +
  scale_color_manual(values = c("Open" = "#8B5742", "On folder" = "#8B7D7B")) +
  labs(
    x = expression("Unitary Water Footprint [" ~ m^3 * ~"per ton Cu]"),
    y = "Levelized Extraction Costs [$USD per ton Cu]",
    fill = "",
    col = "Mine Type",
    size = "Total Resource [million tons Cu]",
    tag = "(a)"
    # title = "160 Lithium deposits"
  ) +
  theme_bw(8) +
  guides(col = guide_legend(nrow = 1), size = guide_legend(nrow = 1)) +
  theme(
    panel.grid = element_blank(),
    plot.tag = element_text(face = "bold"),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box = "horizontal",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.4, "cm"),
    legend.position = c(0.4, 0.09),
    legend.box.background = element_rect(colour = "black")
  )

ggsave("Figures/CU_water_impact.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)

# Histograms
cu |>
  mutate(water_cat = if_else(water_footprint < 500, "Low", "High")) |>
  ggplot(aes(OPEX, fill = water_cat)) +
  # geom_histogram(position = "dodge")
  geom_density(alpha = .6)


cu |>
  group_by(country) |>
  mutate(water_avg = mean(water_footprint)) |>
  ungroup() |>
  ggplot(aes(reorder(country, water_avg), water_footprint)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(col=OPEX),position = position_jitter(width = 0.2), alpha=0.3) +
  coord_flip() +
  # scale_y_continuous(labels = comma, limits = c(0, 1e3)) +
  scale_y_continuous(labels = comma, limits = c(0, 50e3)) +
  scale_color_gradient(low = "darkblue", high = "darkred", labels = scales::label_comma(suffix = "$")) +
  labs(y = expression("Unitary Water Footprint [" ~ m^3 * ~"per ton Cu]"), x = "", col = "OPEX\n[$USD per ton Cu]")

ggplot(cu, aes(water, water_footprint)) + geom_point() + xlim(0, 1e3) + ylim(0, 50e3)
cu |>
  group_by(country) |>
  reframe(water = sum(water), water_footprint = sum(water_footprint)) |>
  ungroup() |>
  mutate(avgCF = water_footprint / water) |>
  arrange(desc(avgCF))

## Cost Curve -----------

cu$resources_curve <- cu$resources * 0.8 / 1e6


cu2 <- cu %>%
  arrange(level_cost) %>% # order bars
  arrange(desc(status_open)) |> # order by oepn or not
  mutate(xmin = lag(cumsum(resources_curve), default = 0), xmax = xmin + resources_curve)

range_wf <- range(cu2$water_footprint)

# cumulative
cu_demand <- tibble(Scenario = c("Reference", "Large LIB", "Small LIB", "Recycling"), cu_mton = c(727, 772, 699, 690))

ggplot(cu2) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = level_cost, fill = water_footprint),
    color = "grey30",
    linewidth = 0.01
  ) +
  # cumulative demand
  geom_point(
    data = data.frame(x = c(727,50), y = c(500,12000)),
    aes(x, y),
    shape = 8,        # star
    size  = 2,
    color = "black",
    inherit.aes = FALSE) +
  annotate("text", x = 80, y = 12000, label = "Cumulative Demand", size = 8 * 5 / 14 * 0.8, hjust = 0) +
  annotate("text", x = 500, y = 7500, label = "Open Deposits", size = 8 * 5 / 14 * 0.8, hjust = 0) +
  annotate("text", x = 1500, y = 7500, label = "On folder", size = 8 * 5 / 14 * 0.8, hjust = 0) +
  scale_fill_gradientn(
    colours = rev(RColorBrewer::brewer.pal(8, "Spectral")),
    trans = "log10",
    labels = scales::label_comma()
  ) +
  scale_y_continuous(labels = dollar_format(big.mark = " ", prefix = "$"), limits = c(0, 13) * 1e3) +
  scale_x_continuous(labels = scales::label_comma(), limits = c(0, 2050)) +
  labs(
    x = "Copper Resources [million tons Cu]",
    y = "Levelized Extraction Costs [$USD per ton Cu]",
    fill = expression("Unitary Water Footprint [" ~ m^3 * ~"per ton Cu]")
  ) +
  coord_cartesian(expand = F, ylim = c(0, 13) * 1e3) +
  theme_minimal(8) +
  guides(fill = guide_colorbar(barwidth = unit(10, "cm"))) +
  theme(legend.position = "bottom", panel.grid = element_blank())

ggsave("Figures/cu-costCurve.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 8.7 * 2, height = 8.7)

# EoF
