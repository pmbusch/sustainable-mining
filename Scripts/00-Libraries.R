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

# EoF
