# Water stress map with mineral deposits
# PBH Jan 2026

source("Scripts/00-Libraries.R", encoding = "UTF-8")
library(patchwork)
source("Scripts/00a-Common Variables.R", encoding = "UTF-8")

# LOAD -----------

## Basemap of water stress ----------
unzip("Inputs/AWARE/AWARE20_Native_CFs_geospatial.kmz", exdir = "kmz_unzip")
k <- st_read("kmz_unzip/doc.kml")
cf_map <- st_make_valid(k) |> st_zm() |> st_cast("MULTIPOLYGON")
cf_map$Basin_ID <- as.numeric(str_remove(cf_map$Name, "CFs for Basin_ID "))
aware_basin <- read.csv("Parameters/AWARE_Basin_Stress.csv")
cf_map <- left_join(cf_map, aware_basin, by = "Basin_ID")
head(cf_map)

# For Figure SI - Instead of water stress, add fish biodiversity index
fish <- read.csv("Parameters/FW_FISH/FW_FISH_Basin_FishIndex_Global.csv")

cf_map <- cf_map |> left_join(fish, by = "Basin_ID")
ggplot(cf_map, aes(fish_index)) + stat_ecdf() + xlim(0, 3.5) # vast majority under 3


## Mineral deposits with water use ----------
deps <- read.csv("Parameters/Deposits_Map.csv")
deps <- deps |>
  mutate(resources = resources / 1e6) |>
  mutate(Mineral = factor(Mineral, levels = c("Copper", "Nickel", "Cobalt", "Lithium"))) %>%
  arrange((Mineral))
table(deps$Mineral)
range(deps$resources)
ggplot(deps, aes(x = resources)) + geom_histogram(bins = 500)

# FIGURE ------------

# limit stress to 100%
cf_map <- cf_map |> mutate(stress = pmin(stress, 1)) # cap at 100% to avoid outliers dominating the color scale
skimr::skim(cf_map$stress)
skimr::skim(cf_map$fish_index)

map1 <- map_data('world')
p1 <- ggplot(deps) +
  # base map
  theme_minimal(8) +
  geom_polygon(data = map1, mapping = aes(x = long, y = lat, group = group), col = 'gray', fill = "white") +
  # Water stress map
  # geom_sf(data = cf_map, aes(fill = stress), color = "grey30", linewidth = 0.1) +
  geom_sf(data = cf_map, aes(fill = fish_index), color = "grey30", linewidth = 0.1) + # comment/uncomment for fish biodiversity
  # fmt: skip
  # scale_fill_distiller(
  scale_fill_gradientn(
    # name="Water Stress",na.value = "white", labels = function(x) ifelse(x >= 1, ">100%", scales::percent(x)), trans = "sqrt",
    name="Fish Biodiversity Index",na.value = "white", # FISH BIO    
  # colours = c("white", "#FEE08B", "#D73027"),values  = rescale(c(0, 0.5, 3.5)), 
  colours = c("white", "#EBCF2EFF", "#244422FF"),values  = rescale(c(0, 10, 100)),  # FISH BIO
 guide = guide_colorbar(direction = "horizontal",
                         barwidth = unit(6, "cm"),
                         barheight = unit(0.25, "cm"),
                         order = 1)) +
  ggnewscale::new_scale_fill() +
  # Deposits
  scale_fill_manual(values = minerals_colors, guide = guide_legend(direction = "horizontal", nrow = 1)) +
  coord_sf(xlim = c(-140, 160), ylim = c(-60, 70)) +
  scale_y_continuous(breaks = NULL, name = "") +
  scale_x_continuous(breaks = NULL, name = "") +
  scale_size_continuous(
    trans = "sqrt",
    breaks = c(0.1, 1, 10, 25, 50, 100, 150),
    range = c(0.3, 3.5), # reduce size of painted points
    guide = guide_legend(direction = "horizontal", nrow = 1, byrow = TRUE, title.position = "left", order = 2)
  ) +
  labs(title = "(a) Deposits (main mineral)", size = "Resources, million tons") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.5, 0.11),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 6),
    legend.box.spacing = unit(0, "cm"),
    legend.margin = margin(2, 12, 2, 2),
    legend.spacing.y = unit(1.5, "mm"),
    legend.box = "vertical",
    plot.margin = margin(1, 1, 1, 1),
    legend.key.height = unit(0.25, 'cm'),
    legend.key.width = unit(0.25, 'cm'),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )
p1

# Insets now
mk_inset <- function(xlim, ylim, tag, data_) {
  p1 +
    geom_point(data=data_,aes(x = LONGITUDE, y = LATITUDE,size=resources,fill=Mineral),alpha = 0.7,  shape  = 21, colour = "black", stroke = 0.25) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme(legend.position = "none") +
    labs(title = tag)
}

# mk_inset <- function(xlim, ylim, tag, data_) {
#   p_in <- p1 +
#     geom_point(data=data_,aes(x = LONGITUDE, y = LATITUDE,size=resources,fill=Mineral),alpha = 0.7,  shape  = 21, colour = "black", stroke = 0.25) +
#     coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
#     guides(fill = "none", color = "none") +
#     # theme(legend.position = "none") +
#     labs(title = tag)

#   leg_size <- cowplot::get_legend(p_in + guides(fill = "none"))
#   p_in +
#     theme(legend.position = "none") +
#     patchwork::inset_element(leg_size, left = 0.72, bottom = 0.12, right = 0.98, top = 0.48)
# }

# fmt: skip
# p_copper1 <- mk_inset(c(-130, -100), c(25, 55), "(b) Copper (incl. co-products)", filter(deps, Mineral == "Copper"))
# p_copper2 <- mk_inset(c(-85, -65), c(-35, 5), "", filter(deps, Mineral == "Copper"))
# p_lithium <- mk_inset(c(-75, -65), c(-30, -13), "(c) Lithium", filter(deps, Mineral == "Lithium"))
# p_cobalt <- mk_inset(c(20, 32), c(-15, -5), "(d) Cobalt (incl. co-products)", filter(deps, Mineral == "Cobalt"))
# p_nickel <- mk_inset(c(110, 155), c(-44, 20), "(e) Nickel (incl. co-products)", filter(deps, Mineral == "Nickel"))

# Choose equal area rectangles
p_copper <- mk_inset(c(-82, -42), c(-35, 5), "(b) Copper (incl. co-products)", filter(deps, Mineral == "Copper"))
p_lithium <- mk_inset(c(-75, -35), c(-40, 0), "(c) Lithium", filter(deps, Mineral == "Lithium"))
p_cobalt <- mk_inset(c(10, 50), c(-35, 5), "(d) Cobalt (incl. co-products)", filter(deps, Mineral == "Cobalt"))
p_nickel <- mk_inset(c(113, 153), c(-35, 5), "(e) Nickel (incl. co-products)", filter(deps, Mineral == "Nickel"))


boxes_sf <- tibble(
  tag = c("b", "c", "d", "e"),
  Mineral = c("Copper", "Lithium", "Cobalt", "Nickel"),
  xmin = c(-82, -75, 10, 113),
  xmax = c(-42, -35, 50, 153),
  ymin = c(-35, -40, -35, -35),
  ymax = c(5, 0, 5, 5)
) %>%
  rowwise() %>%
  mutate(geometry = st_as_sfc(st_bbox(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), crs = st_crs(cf_map)))) %>%
  ungroup() %>%
  st_as_sf()

p_big <- p1 +
  geom_point(data=filter(deps,PRIMARY_COMMODITY==Mineral),aes(x = LONGITUDE, y = LATITUDE,size=resources,fill=Mineral),alpha = 0.7,  shape  = 21, colour = "black", stroke = 0.25) +
  geom_sf(data = boxes_sf, fill = NA, aes(color = Mineral), linewidth = 0.5, linetype = "dashed") +
  scale_color_manual(values = minerals_colors, guide = "none") +
  coord_sf(xlim = c(-140, 160), ylim = c(-60, 70))


# Total resources and demand
demand <- read.csv("Parameters/IEA_Demand.csv")
demand <- demand |>
  filter(Scenario == "SPS") |>
  pivot_longer(c(Copper, Nickel, Cobalt, Lithium), names_to = 'Mineral', values_to = 'Demand') |>
  group_by(Mineral) |>
  reframe(Demand = sum(Demand) / 1e3) |>
  ungroup() # to million tons
p_demand <- deps |>
  group_by(Mineral) |>
  summarise(Resources = sum(resources)) |>
  left_join(demand) |>
  mutate(Mineral = factor(Mineral, levels = rev(c("Copper", "Nickel", "Cobalt", "Lithium")))) %>%
  ggplot(aes(y = Mineral)) +
  geom_segment(aes(x = Demand, xend = Resources, yend = Mineral), linewidth = 0.6, colour = "grey60") +
  geom_point(aes(x = Resources, colour = Mineral), size = 2) +
  geom_point(aes(x = Demand), size = 1.5, shape = 21, fill = "white") +
  geom_text(aes(x = Resources, label = Mineral), hjust = 0, size = 1.5,nudge_y=-0.5) +
  geom_text(aes(x = Resources,label = ifelse(Mineral == "Nickel",paste0("Resources: ", scales::comma(Resources)),scales::comma(Resources))),nudge_y = 0.5, hjust = -0.1, size = 1.5) +
  geom_text(aes(x = Demand,label = ifelse(Mineral == "Nickel",paste0("2025–2050 Demand: ", scales::comma(Demand)),scales::comma(Demand))),nudge_y = 0.5, hjust = 1.1, size = 1.5) +
  scale_x_log10(limits = c(5, 5e3), labels = scales::comma) +
  scale_color_manual(values = minerals_colors, guide = "none") +
  labs(x = "", y = "", title = "") +
  theme_minimal(base_size = 6) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank(),
    plot.margin = margin(2, 2, 2, 2),
    plot.background = element_rect(colour = "black", fill = "white", linewidth = 0.3)
  )
# p_demand

# p_big1 <- p_big + patchwork::inset_element(p_demand, left = 0.01, bottom = 0.01, right = 0.205, top = 0.25)

p_big / wrap_plots(list(p_copper, p_lithium, p_cobalt, p_nickel), nrow = 1) + plot_layout(heights = c(2, 1))


# fmt: skip
ggsave("Figures/Figure1.png", ggplot2::last_plot(),units = 'cm', dpi = 1200, width = 8.7*3, height = 8.7*2)
# ggsave("Figures/Figure1.svg", ggplot2::last_plot(), units = 'cm', dpi = 1200, width = 8.7 * 3, height = 8.7 * 2)
# ggsave("Figures/Figure1.pdf", ggplot2::last_plot(), units = 'cm', dpi = 1200, width = 8.7 * 3, height = 8.7 * 2)

ggsave("Figures/Figure1_Fish.png", ggplot2::last_plot(), units = 'cm', dpi = 1200, width = 8.7 * 3, height = 8.7 * 2)

## Version 2 - Facets --------

p_cu <- p1 +
  geom_point(data=filter(deps,Mineral=="Copper"),aes(x = LONGITUDE, y = LATITUDE,size=resources),fill=minerals_colors["Copper"],alpha = 0.7,  shape  = 21, colour = "black", stroke = 0.25) +
  coord_sf(xlim = c(-140, 160), ylim = c(-60, 70)) +
  labs(title = "Copper", tag = "(a)") +
  theme(plot.margin = margin(1, 1, 0, 1))
p_ni <- p1 +
  geom_point(data=filter(deps,Mineral=="Nickel"),aes(x = LONGITUDE, y = LATITUDE,size=resources),fill=minerals_colors["Nickel"],alpha = 0.7,  shape  = 21, colour = "black", stroke = 0.25) +
  coord_sf(xlim = c(-140, 160), ylim = c(-60, 70)) +
  labs(title = "Nickel", tag = "(b)") +
  theme(plot.margin = margin(1, 1, 0, 1))
p_co <- p1 +
  geom_point(data=filter(deps,Mineral=="Cobalt"),aes(x = LONGITUDE, y = LATITUDE,size=resources),fill=minerals_colors["Cobalt"],alpha = 0.7,  shape  = 21, colour = "black", stroke = 0.25) +
  coord_sf(xlim = c(-140, 160), ylim = c(-60, 70)) +
  labs(title = "Cobalt", tag = "(c)") +
  theme(plot.margin = margin(1, 1, 0, 1))
p_li <- p1 +
  geom_point(data=filter(deps,Mineral=="Lithium"),aes(x = LONGITUDE, y = LATITUDE,size=resources),fill=minerals_colors["Lithium"],alpha = 0.7,  shape  = 21, colour = "black", stroke = 0.25) +
  coord_sf(xlim = c(-140, 160), ylim = c(-60, 70)) +
  labs(title = "Lithium", tag = "(d)") +
  theme(plot.margin = margin(1, 1, 0, 1))


# Assemble grid with shared legend at bottom
plot_grid <- cowplot::plot_grid(p_cu, p_ni, p_co, p_li, nrow = 2)
plot_grid

ggsave("Figures/Figure1_v2.png", ggplot2::last_plot(), units = 'cm', dpi = 1200, width = 8.7 * 4, height = 8.7 * 2)

# EoF
