## Load all required libraries to use
## Common file to run from multiple scripts
## PBH Feb 2023

# Library -----
list_libraries <- c(
  "tidyverse",
  "tidyverse",
  "readr",
  "readxl",
  "ggplot2",
  "data.table",
  "dplyr",
  "gridExtra",
  "glmnet",
  "openxlsx",
  "reshape2",
  "scales",
  "RColorBrewer",
  "sf",
  "ggrepel",
  "paletteer"
) # maps

# Install libraries if they are not present
# UNCOMMENT THE CODE TO INSTALL LIBRARIES THE FIRST TIME
# new_libraries <- list_libraries[!(list_libraries %in% installed.packages()[,"Package"])]
# lapply(new_libraries, install.packages)
# rm(new_libraries)

lapply(list_libraries, require, character.only = TRUE)

rm(list_libraries)

source("Scripts/00b-Theme.R", encoding = "UTF-8")

# Functions -----
# load all required functions automatically
file.sources = list.files("Scripts/00-Functions", pattern = "*.R$", full.names = TRUE, ignore.case = TRUE)
sapply(file.sources, source, .GlobalEnv)
rm(file.sources)

# Common function to save SVG files with text without the auto-text size autoformatting
clean_svg <- function(filename, output = filename) {
  svg_content <- readLines(filename, warn = FALSE)
  # Handle both single and double quotes
  svg_content <- gsub('textLength="[^"]*"', '', svg_content)
  svg_content <- gsub("textLength='[^']*'", '', svg_content)
  svg_content <- gsub('lengthAdjust="[^"]*"', '', svg_content)
  svg_content <- gsub("lengthAdjust='[^']*'", '', svg_content)
  writeLines(svg_content, output)
}

group_svg_layers <- function(input, output = input) {
  doc <- xml2::read_xml(input)
  xml2::xml_set_attr(xml2::xml_root(doc), "xmlns:inkscape", "http://www.inkscape.org/namespaces/inkscape")
  root_g <- xml2::xml_find_first(doc, "/*[local-name()='svg']/*[local-name()='g'][@class='svglite']")

  classify <- function(node) {
    tag <- xml2::xml_name(node)
    sty <- xml2::xml_attr(node, "style")
    if (is.na(sty)) {
      sty <- ""
    }
    switch(
      tag,
      text = "Labels",
      polygon = "Data",
      circle = "Data",
      path = "Data",
      rect = "Grid",
      line = "Grid",
      polyline = {
        if (grepl("stroke-dasharray", sty)) {
          "Grid"
        } else {
          n <- length(strsplit(trimws(xml2::xml_attr(node, "points")), "\\s+")[[1]])
          if (n <= 2) "Grid" else "Data"
        }
      },
      NA_character_
    )
  }

  elems <- xml2::xml_find_all(root_g, "./*[local-name()='g']/*[not(local-name()='g')]")
  cats <- vapply(elems, classify, character(1))

  wrappers <- list()
  for (cat in c("Grid", "Data", "Labels")) {
    w <- xml2::xml_add_child(root_g, "g") # inside g597
    xml2::xml_set_attr(w, "id", cat)
    xml2::xml_set_attr(w, "inkscape:label", cat)
    xml2::xml_set_attr(w, "inkscape:groupmode", "layer")
    wrappers[[cat]] <- w
  }

  for (i in seq_along(elems)) {
    if (is.na(cats[i])) {
      next
    }
    xml2::xml_add_child(wrappers[[cats[i]]], elems[[i]])
    xml2::xml_remove(elems[[i]])
  }

  empties <- xml2::xml_find_all(root_g, "./*[local-name()='g'][not(*)][not(@inkscape:label)]")
  xml2::xml_remove(empties)

  xml2::write_xml(doc, output)
  clean_svg(output)
  invisible(output)
}

# EoF
