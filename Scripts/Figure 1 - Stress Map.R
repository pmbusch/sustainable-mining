# Water stress map with mineral deposits
# PBH Jan 2026

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# LOAD -----------

## Basemap of water stress ----------
unzip("Inputs/AWARE/AWARE20_Native_CFs_geospatial.kmz", exdir = "kmz_unzip")
k <- st_read("kmz_unzip/doc.kml")
cf_map <- st_make_valid(k) |> st_zm() |> st_cast("MULTIPOLYGON")
cf_map$Basin_ID <- as.numeric(str_remove(cf_map$Name, "CFs for Basin_ID "))
aware_basin <- read.csv("Parameters/AWARE_Basin_Stress.csv")
cf_map <- left_join(cf_map, aware_basin, by = "Basin_ID")
head(cf_map)

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

map1 <- map_data('world')
p1 <- ggplot(deps) +
  # base map
  theme_minimal(8) +
  geom_polygon(data = map1, mapping = aes(x = long, y = lat, group = group), col = 'gray', fill = "white") +
  # Water stress map
  geom_sf(data = cf_map, aes(fill = stress), color = "grey30", linewidth = 0.1) +
  # fmt: skip
  # scale_fill_distiller(
  scale_fill_gradientn(
    name="Water Stress",na.value = "white", labels = function(x) ifelse(x >= 1, ">100%", scales::percent(x)), trans = "sqrt",
  # palette = "OrRd", direction = 1,
  colours = c("white", "#FEE08B", "#D73027"),values  = rescale(c(0, 0.5, 3.5)), 
 guide = guide_colorbar(direction = "horizontal",
                         barwidth = unit(6, "cm"),
                         barheight = unit(0.25, "cm"),
                         order = 1)) +
  ggnewscale::new_scale_fill() +
  # Deposits
  scale_fill_manual(
    values = c("Lithium" = "#fb9a99", "Copper" = "#33a02c", "Nickel" = "#525252", "Cobalt" = "#6a3d9a"),
    guide = guide_legend(direction = "horizontal", nrow = 1)
  ) +
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
    legend.position = c(0.5, 0.12),
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
library(patchwork)
mk_inset <- function(xlim, ylim, tag, data_) {
  p1 +
    geom_point(data=data_,aes(x = LONGITUDE, y = LATITUDE,size=resources,fill=Mineral),alpha = 0.7,  shape  = 21, colour = "black", stroke = 0.25) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme(legend.position = "none") +
    labs(title = tag)
}

# fmt: skip
# p_copper1 <- mk_inset(c(-130, -100), c(25, 55), "(b) Copper (incl. co-products)", filter(deps, Mineral == "Copper"))
# p_copper2 <- mk_inset(c(-85, -65), c(-35, 5), "", filter(deps, Mineral == "Copper"))
# p_lithium <- mk_inset(c(-75, -65), c(-30, -13), "(c) Lithium", filter(deps, Mineral == "Lithium"))
# p_cobalt <- mk_inset(c(20, 32), c(-15, -5), "(d) Cobalt (incl. co-products)", filter(deps, Mineral == "Cobalt"))
# p_nickel <- mk_inset(c(110, 155), c(-44, 20), "(e) Nickel (incl. co-products)", filter(deps, Mineral == "Nickel"))

# Choose equal area rectangles
p_copper <- mk_inset(c(-90, -50), c(-35, 5), "", filter(deps, Mineral == "Copper"))
p_lithium <- mk_inset(c(-90, -50), c(-40, 0), "(c) Lithium", filter(deps, Mineral == "Lithium"))
p_cobalt <- mk_inset(c(10, 50), c(-20, 20), "(d) Cobalt (incl. co-products)", filter(deps, Mineral == "Cobalt"))
p_nickel <- mk_inset(c(115, 155), c(-35, 5), "(e) Nickel (incl. co-products)", filter(deps, Mineral == "Nickel"))


boxes_sf <- tibble(
  tag = c("b", "c", "d", "e"),
  Mineral = c("Copper", "Lithium", "Cobalt", "Nickel"),
  xmin = c(-90, -90, 10, 115),
  xmax = c(-50, -50, 50, 155),
  ymin = c(-35, -40, -20, -35),
  ymax = c(5, 0, 20, 5)
) %>%
  rowwise() %>%
  mutate(geometry = st_as_sfc(st_bbox(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), crs = st_crs(cf_map)))) %>%
  ungroup() %>%
  st_as_sf()

p_big <- p1 +
  geom_point(data=filter(deps,PRIMARY_COMMODITY==Mineral),aes(x = LONGITUDE, y = LATITUDE,size=resources,fill=Mineral),alpha = 0.7,  shape  = 21, colour = "black", stroke = 0.25) +
  geom_sf(data = boxes_sf, fill = NA, aes(color = Mineral), linewidth = 0.5, linetype = "dashed") +
  scale_color_manual(
    values = c("Lithium" = "#fb9a99", "Copper" = "#33a02c", "Nickel" = "#525252", "Cobalt" = "#6a3d9a"),
    guide = "none"
  ) +
  coord_sf(xlim = c(-140, 160), ylim = c(-60, 70))

p_big / wrap_plots(list(p_copper, p_lithium, p_cobalt, p_nickel), nrow = 1) + plot_layout(heights = c(2, 1))

# fmt: skip
ggsave("Figures/Figure1.png", ggplot2::last_plot(),units = 'cm', dpi = 1200, width = 8.7*3, height = 8.7*2)
# ggsave("Figures/Figure1.svg", ggplot2::last_plot(), units = 'cm', dpi = 1200, width = 8.7 * 3, height = 8.7 * 2)
# ggsave("Figures/Figure1.pdf", ggplot2::last_plot(), units = 'cm', dpi = 1200, width = 8.7 * 3, height = 8.7 * 2)

## Version 2 - Facets --------

p_cu <- p1 +
  geom_point(data=filter(deps,Mineral=="Copper"),aes(x = LONGITUDE, y = LATITUDE,size=resources),fill="#33a02c",alpha = 0.7,  shape  = 21, colour = "black", stroke = 0.25) +
  coord_sf(xlim = c(-140, 160), ylim = c(-60, 70)) +
  labs(title = "Copper", tag = "(a)") +
  theme(plot.margin = margin(1, 1, 0, 1))
p_ni <- p1 +
  geom_point(data=filter(deps,Mineral=="Nickel"),aes(x = LONGITUDE, y = LATITUDE,size=resources),fill="#525252",alpha = 0.7,  shape  = 21, colour = "black", stroke = 0.25) +
  coord_sf(xlim = c(-140, 160), ylim = c(-60, 70)) +
  labs(title = "Nickel", tag = "(b)") +
  theme(plot.margin = margin(1, 1, 0, 1))
p_co <- p1 +
  geom_point(data=filter(deps,Mineral=="Cobalt"),aes(x = LONGITUDE, y = LATITUDE,size=resources),fill="#6a3d9a",alpha = 0.7,  shape  = 21, colour = "black", stroke = 0.25) +
  coord_sf(xlim = c(-140, 160), ylim = c(-60, 70)) +
  labs(title = "Cobalt", tag = "(c)") +
  theme(plot.margin = margin(1, 1, 0, 1))
p_li <- p1 +
  geom_point(data=filter(deps,Mineral=="Lithium"),aes(x = LONGITUDE, y = LATITUDE,size=resources),fill="#fb9a99",alpha = 0.7,  shape  = 21, colour = "black", stroke = 0.25) +
  coord_sf(xlim = c(-140, 160), ylim = c(-60, 70)) +
  labs(title = "Lithium", tag = "(d)") +
  theme(plot.margin = margin(1, 1, 0, 1))


# Assemble grid with shared legend at bottom
plot_grid <- cowplot::plot_grid(p_cu, p_ni, p_co, p_li, nrow = 2)
plot_grid

ggsave("Figures/Figure1_v2.png", ggplot2::last_plot(), units = 'cm', dpi = 1200, width = 8.7 * 4, height = 8.7 * 2)

# EoF
