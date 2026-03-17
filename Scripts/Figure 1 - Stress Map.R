# Water stress map with mineral deposits
# PBH Jan 2026

source("Scripts/00-Libraries.R", encoding = "UTF-8")
library(patchwork)
library(scales)
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

# Correlation ore grade and water scarcity ---------
deposits <- read.csv("Parameters/Deposit.csv")
deposits <- left_join(deposits, dplyr::select(aware_basin, Basin_ID, stress), by = "Basin_ID")
names(deposits)

deps_corr <- dplyr::select(
  deposits,
  aware_cf,
  stress,
  aware_available,
  resources_Copper,
  resources_Nickel,
  resources_Cobalt,
  resources_Lithium,
  grade_resource_Copper,
  grade_resource_Nickel,
  grade_resource_Cobalt,
  grade_resource_Lithium
)

cor(deps_corr, use = "complete.obs")
# GGally::ggpairs(deps_corr)

### Find boxes with highest amount of resources -------------
library(terra)
# raster grid (1°)
r <- rast(xmin = -180, xmax = 180, ymin = -90, ymax = 90, resolution = 1)
# 39x39 to plot a 40x40
w <- matrix(1, 39, 39)
boxes <- deps |>
  group_split(Mineral) |>
  lapply(function(d) {
    v <- vect(d, geom = c("LONGITUDE", "LATITUDE"), crs = "EPSG:4326")

    r_res <- rasterize(v, r, field = "resources", fun = "sum", background = 0)

    r_sum <- focal(r_res, w = w, fun = sum, na.policy = "omit", fillvalue = 0)

    m <- which.max(values(r_sum))
    xy <- xyFromCell(r_sum, m)

    tibble(
      Mineral = unique(d$Mineral),
      lon_center = xy[1],
      lat_center = xy[2],
      xmin = xy[1] - 20,
      xmax = xy[1] + 20,
      ymin = xy[2] - 20,
      ymax = xy[2] + 20
    )
  }) |>
  bind_rows()
boxes


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
  # geom_sf(data = cf_map, aes(fill = stress), color = "grey30", linewidth = 0.05) +
  geom_sf(data = cf_map, aes(fill = fish_index), color = "grey30", linewidth = 0.05) + # comment/uncomment for fish biodiversity
  scale_fill_gradientn(
    name = "Water Stress",
    labels = function(x) ifelse(x >= 1, ">100%", scales::percent(x)),
    colours = c("white", "#ffe7a599", "#d8362999"),
    values = scales::rescale(c(0, 0.5, 3.5)),
    na.value = "white",
    trans = "sqrt",
    # Uncomment for Fish Index figure (SI)
    # name = "Fish Biodiversity Index", # FISH BIO
    # colours = c("white", "#EBCF2E99", "#24442299"),
    # values = scales::rescale(c(0, 10, 100)), # FISH BIO
    guide = guide_colorbar(
      direction = "horizontal",
      barwidth = unit(6, "cm"),
      barheight = unit(0.25, "cm"),
      title.position = "top",
      title.hjust = 0.5, # center title
      order = 1
    )
  ) +
  ggnewscale::new_scale_fill() +
  # Deposits
  scale_fill_manual(values = minerals_colors, guide = "none") +
  coord_sf(xlim = c(-155, 165), ylim = c(-52, 70)) +
  scale_y_continuous(breaks = NULL, name = "") +
  scale_x_continuous(breaks = NULL, name = "") +
  scale_size_continuous(
    trans = "sqrt",
    breaks = c(0.1, 1, 25, 100),
    labels = c("0.1", "1", "25", "100"),
    range = c(0.3, 3.5), # reduce size of painted points
    guide = guide_legend(
      direction = "horizontal",
      nrow = 1,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0.5,
      order = 2
    ),
    name = "Resources (Mt)"
  ) +
  labs(title = "All minerals") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.55, 0.09),
    legend.background = element_blank(),
    legend.text = element_text(size = 8),
    legend.box.spacing = unit(0, "cm"),
    legend.margin = margin(2, 12, 2, 2),
    legend.spacing.y = unit(1.5, "mm"),
    legend.box = "horizontal",
    plot.margin = margin(1, 1, 1, 1),
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
    legend.key.height = unit(0.25, 'cm'),
    legend.key.width = unit(0.25, 'cm'),
    legend.key = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )
p1

# Insets now
mk_inset <- function(xlim, ylim, tag, data_, letter) {
  p1 +
    geom_point(data=data_,aes(x = LONGITUDE, y = LATITUDE,size=resources,fill=Mineral),alpha = 0.7,  shape  = 21, colour = "black", stroke = 0.15) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10, hjust = 0.5, face = "bold", color = minerals_colors[tag]),
      panel.border = element_rect(color = paste0(minerals_colors[tag], "B3"), fill = NA, linewidth = 1.5),
      plot.margin = margin(0, 0, 0, 0),
      panel.spacing = unit(0, "pt")
    ) +
    # fmt: skip
    annotate("text",x = -Inf, y = -Inf,label = letter,hjust = -0.5, vjust = -0.5,fontface = "bold",size = 5,colour = "black") +
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

# Choose equal area rectangles
(x <- boxes)
# fmt: skip
p_copper <- mk_inset(c(x[1,]$xmin, x[1,]$xmax), c(x[1,]$ymin,x[1,]$ymax), "Copper", filter(deps, Mineral == "Copper"),"b")
# fmt: skip
p_nickel <- mk_inset(c(x[2,]$xmin, x[2,]$xmax), c(x[2,]$ymin,x[2,]$ymax), "Nickel", filter(deps, Mineral == "Nickel"),"e")
# fmt: skip
p_cobalt <- mk_inset(c(x[3,]$xmin, x[3,]$xmax), c(x[3,]$ymin,x[3,]$ymax), "Cobalt", filter(deps, Mineral == "Cobalt"),"c")
# fmt: skip
p_lithium <- mk_inset(c(x[4,]$xmin, x[4,]$xmax), c(x[4,]$ymin,x[4,]$ymax), "Lithium", filter(deps, Mineral == "Lithium"),"d")


boxes_sf <- boxes |>
  dplyr::select(-lon_center, -lat_center) |>
  dplyr::slice(c(1, 4, 3, 2)) |> # re order
  mutate(tag = c("b", "c", "d", "e")) |>
  rowwise() %>%
  mutate(geometry = st_as_sfc(st_bbox(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), crs = st_crs(cf_map)))) %>%
  ungroup() %>%
  st_as_sf()

p_big <- p1 +
  geom_point(data=filter(deps,PRIMARY_COMMODITY==Mineral),aes(x = LONGITUDE, y = LATITUDE,size=resources,fill=Mineral),alpha = 0.7,  shape  = 21, colour = "black", stroke = 0.15) +
  geom_sf(data = boxes_sf, fill = NA, aes(color = Mineral), linewidth = 0.5, alpha = 0.7) +
  geom_text(data = filter(boxes_sf,Mineral!="Lithium"),fontface = "bold", size = 10 * 5 / 14 * 0.8,
            aes(x = xmin, y = ymin, label = Mineral,color=Mineral),
            hjust = 0, vjust = 1.5) +
  geom_text(data = filter(boxes_sf,Mineral=="Lithium"),fontface = "bold", size = 10 * 5 / 14 * 0.8,
            aes(x = xmax, y = ymax, label = Mineral,color=Mineral),
            hjust = 1, vjust = -0.5) +
  # fmt: skip
  annotate("text",x = -Inf, y = -Inf,label = "a",hjust = -0.5, vjust = -0.5,fontface = "bold",size = 5,colour = "black") +
  scale_color_manual(values = minerals_colors, guide = "none") +
  coord_sf(xlim = c(-155, 165), ylim = c(-52, 70))
p_big

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

p_big / wrap_plots(list(p_copper, p_cobalt, p_lithium, p_nickel), nrow = 1) + plot_layout(heights = c(0.61, 0.39))


# fmt: skip
ggsave("Figures/Figure1.png", ggplot2::last_plot(),units = 'cm', dpi = 1200, width = 8.7*3, height = 8.7*2)
ggsave("Figures/Figure1.svg", ggplot2::last_plot(), units = 'cm', dpi = 1200, width = 8.7 * 3, height = 8.7 * 2)
ggsave("Figures/Figure1.pdf", ggplot2::last_plot(), units = 'cm', dpi = 1200, width = 8.7 * 3, height = 8.7 * 2)

# Uncomment for Fish figure (SI)
# ggsave("Figures/Figure1_Fish.png", ggplot2::last_plot(), units = 'cm', dpi = 1200, width = 8.7 * 3, height = 8.7 * 2)
# ggsave("Figures/Figure1_Fish.svg", ggplot2::last_plot(), units = 'cm', dpi = 1200, width = 8.7 * 3, height = 8.7 * 2)

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
