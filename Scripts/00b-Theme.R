# A compact custom ggplot2 theme based on common settings
# Usage:
#   library(ggplot2)
#   source("R/theme_pmb.R")
#   ggplot(...) + ... + theme_pmb("small")   # for 8 x 8 cm figures
#   ggplot(...) + ... + theme_pmb("wide")    # for 18 x 8 cm figures
#
# Also see recommended geom and ggsave settings below.
theme_pb <- function(
  preset = c("small", "wide", "largeFont"),
  base_family = "",
  text_col = "#222222",
  panel_border_col = "black"
) {
  preset <- match.arg(preset)
  # base sizes calibrated to figure sizes
  # - small (8 x 8 cm): base_size = 8
  # - wide  (18 x 8 cm): base_size = 10
  base_size <- switch(preset, small = 7, wide = 8, largeFont = 10)

  # Derived sizes
  sz_plot_title <- base_size + 1.5 # bold main title
  sz_axis_title <- base_size - 1 # axis titles (often small in your figures)
  sz_axis_text <- base_size - 1.5 # axis tick labels
  sz_legend_text <- base_size - 1.5
  sz_strip_text <- base_size - 1.2
  sz_plot_tag <- base_size + 0.5

  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # general text color / family
      text = ggplot2::element_text(colour = text_col, family = base_family),

      # Titles and tags
      plot.title = ggplot2::element_text(size = sz_plot_title, face = "bold", colour = text_col, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = base_size, hjust = 0.5),
      plot.caption = ggplot2::element_text(size = base_size - 2, hjust = 1, colour = text_col, lineheight = 0.9),
      plot.tag = ggplot2::element_text(size = sz_plot_tag, face = "bold"),

      # Panels and grids
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", colour = panel_border_col),
      panel.border = ggplot2::element_rect(colour = panel_border_col, fill = NA),

      # Axis
      axis.title = ggplot2::element_text(size = sz_axis_title, colour = text_col),
      axis.text = ggplot2::element_text(size = sz_axis_text, colour = text_col),
      axis.ticks = ggplot2::element_line(size = 0.35, colour = "black"),

      # Facets / strips
      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      strip.text = ggplot2::element_text(
        size = sz_strip_text,
        colour = text_col,
        margin = ggplot2::margin(t = 2, b = 2),
        face = "bold"
      ),

      # Legend
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key.size = ggplot2::unit(0.35, "cm"),
      legend.text = ggplot2::element_text(size = sz_legend_text, colour = text_col),
      legend.title = ggplot2::element_text(size = sz_legend_text),
      legend.box.spacing = unit(0.1, "cm"),
      legend.key.height = unit(0.25, 'cm'),
      legend.key.width = unit(0.25, 'cm'),

      # Margins (match common manual plot.margin tweaks)
      plot.margin = ggplot2::margin(t = 4, r = 6, b = 4, l = 4, unit = "pt")
    )
}

# Convenience presets
theme_pb_small <- function(...) theme_pb(preset = "small", ...)
theme_pb_wide <- function(...) theme_pb(preset = "wide", ...)
theme_pb_large <- function(...) theme_pb(preset = "largeFont", ...)
