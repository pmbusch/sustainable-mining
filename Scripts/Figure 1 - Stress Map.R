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
cu <- read.csv("Parameters/Cu_Deposit.csv") |> mutate(Mineral = "Copper")
li <- read.csv("Parameters/Deposit_water.csv") |>
  rename(LATITUDE = Latitude, LONGITUDE = Longitude, resources = all_resource) |>
  mutate(Mineral = "Lithium")

# join
deps <- rbind(
  dplyr::select(cu, LATITUDE, LONGITUDE, resources, Mineral),
  dplyr::select(li, LATITUDE, LONGITUDE, resources, Mineral)
) |>
  mutate(resources = resources / 1e6) |>
  arrange((Mineral))
table(deps$Mineral)
range(deps$resources)

map1 <- map_data('world')
p1 <- ggplot(deps) +
  # base map
  theme_minimal(8) +
  geom_polygon(data = map1, mapping = aes(x = long, y = lat, group = group), col = 'gray', fill = "white") +
  # Water stress map
  geom_sf(data = cf_map, aes(fill = stress), color = "grey30", linewidth = 0.1) +
  # fmt: skip
  scale_fill_distiller(name="Water Stress",palette = "OrRd", na.value = "white", direction = 1, labels = scales::percent, trans = "sqrt",
 guide = guide_colorbar(direction = "horizontal",
                         barwidth = unit(6, "cm"),
                         barheight = unit(0.25, "cm"),
                         order = 1)) +
  ggnewscale::new_scale_fill() +
  # Deposits
  geom_point(aes(x = LONGITUDE, y = LATITUDE,size=resources,fill=Mineral),alpha = 0.7,  shape  = 21, colour = "black", stroke = 0.25) +
  scale_fill_manual(
    values = c("Lithium" = "#1f78b4", "Copper" = "#33a02c"),
    guide = guide_legend(direction = "horizontal", nrow = 1)
  ) +
  coord_sf(xlim = c(-140, 160), ylim = c(-60, 70)) +
  scale_y_continuous(breaks = NULL, name = "") +
  scale_x_continuous(breaks = NULL, name = "") +
  scale_size_continuous(
    trans = "sqrt",
    breaks = c(1, 10, 25, 50, 100, 150),
    range = c(1, 3.5), # reduce size of painted points
    guide = guide_legend(direction = "horizontal", nrow = 1, byrow = TRUE, title.position = "left", order = 2)
  ) +
  labs(title = "(a)", size = "Resources, million tons") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.5, 0.12),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 6),
    legend.box.spacing = unit(0, "cm"),
    legend.spacing.y = unit(0.5, "mm"),
    legend.box = "vertical",
    plot.margin = margin(1, 1, 1, 1),
    legend.key.height = unit(0.25, 'cm'),
    legend.key.width = unit(0.25, 'cm'),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )
p1

# Insets now
library(patchwork)
mk_inset <- function(xlim, ylim, tag = NULL) {
  p1 + coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) + theme(legend.position = "none") + labs(title = tag)
}

p_northAmerica <- mk_inset(c(-120, -90), c(25, 45), "(b) North America")
p_southAmerica <- mk_inset(c(-85, -65), c(-35, 0), "(c) South America")
p_Africa <- mk_inset(c(20, 40), c(-20, 0), "(d) Africa")
p_Australia <- mk_inset(c(110, 155), c(-45, -10), "(e) Australia")

boxes_sf <- tibble(
  tag = c("b", "c", "d", "e"),
  xmin = c(-120, -85, 20, 110),
  xmax = c(-90, -65, 40, 155),
  ymin = c(25, -35, -20, -45),
  ymax = c(45, 0, 0, -10)
) %>%
  rowwise() %>%
  mutate(geometry = st_as_sfc(st_bbox(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), crs = st_crs(cf_map)))) %>%
  ungroup() %>%
  st_as_sf()

p_big <- p1 +
  geom_sf(data = boxes_sf, fill = NA, color = "black", linewidth = 0.5, linetype = "dashed") +
  coord_sf(xlim = c(-140, 160), ylim = c(-60, 70))

p_big /
  wrap_plots(list(p_northAmerica, p_southAmerica, p_Africa, p_Australia), nrow = 1) +
  plot_layout(heights = c(2, 1))

# fmt: skip
ggsave("Figures/Figure1.png", ggplot2::last_plot(),units = 'cm', dpi = 1200, width = 8.7*3, height = 8.7*2)
# ggsave("Figures/Figure1.svg", ggplot2::last_plot(), units = 'cm', dpi = 1200, width = 8.7 * 3, height = 8.7 * 2)
# ggsave("Figures/Figure1.pdf", ggplot2::last_plot(), units = 'cm', dpi = 1200, width = 8.7 * 3, height = 8.7 * 2)
