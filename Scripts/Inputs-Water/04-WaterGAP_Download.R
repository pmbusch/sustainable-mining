# ============================================================================ #
# WaterGAP Data Download Script
#
# Downloads monthly resolution WaterGAP spatial data from ISIMIP3b
# Parses filenames to extract metadata and filters for monthly data only
# https://doi.org/10.48364/ISIMIP.230418.7
# ISIMIP3b Simulation Data from the Global Water Sector
#
# Author: Pablo Busch
# Date: Feb 2026
# ============================================================================ #

# Load required packages ----
source('Scripts/00-Libraries.R', encoding = 'UTF-8')
library(curl)

# Set paths ----
input_dir <- "Inputs/AWARE/WaterGAP"
output_dir <- "Inputs/AWARE/WaterGAP/downloaded"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Read file list ----
filelist_path <- file.path(input_dir, "filelist.txt")
urls <- read_lines(filelist_path)

# Parse filenames and extract metadata ----
# Filename structure: {model}_{climate_model}_{forcing}_{scenario}_{socioeconomic}_{variant}_{variable}_{scale}_{resolution}_{period_start}_{period_end}.nc

parse_watergap_filename <- function(url) {
  filename <- basename(url)
  filename_no_ext <- str_remove(filename, "\\.nc$")
  parts <- str_split(filename_no_ext, "_")[[1]]

  if (length(parts) >= 10) {
    data <- tibble(
      url = url,
      filename = filename,
      model = parts[1],
      climate_model = parts[2],
      forcing = parts[3],
      scenario = parts[4],
      socioeconomic = parts[5],
      variant = parts[6],
      variable = parts[7],
      scale = parts[8],
      resolution = parts[9],
      period_start = as.integer(parts[10]),
      period_end = as.integer(parts[11])
    )
  } else {
    data <- tibble(
      url = url,
      filename = filename,
      model = NA_character_,
      climate_model = NA_character_,
      forcing = NA_character_,
      scenario = NA_character_,
      socioeconomic = NA_character_,
      variant = NA_character_,
      variable = NA_character_,
      scale = NA_character_,
      resolution = NA_character_,
      period_start = NA_integer_,
      period_end = NA_integer_
    )
  }
  return(data)
}

file_metadata <- map_df(urls, parse_watergap_filename)

# Add metadata columns ----
file_metadata <- file_metadata %>%
  mutate(
    period_length = period_end - period_start + 1,
    is_full_century = (period_start == 2015 & period_end == 2100),
    scenario_label = case_when(
      scenario == "picontrol" ~ "Pre-industrial control",
      scenario == "ssp126" ~ "SSP1-2.6 (Low emissions)",
      scenario == "ssp370" ~ "SSP3-7.0 (Medium-high emissions)",
      scenario == "ssp585" ~ "SSP5-8.5 (High emissions)",
      TRUE ~ scenario
    ),
    variable_label = case_when(
      variable == "atotuse" ~ "Actual consumptive water use",
      variable == "dis" ~ "Streamflow (discharge)",
      variable == "qtot" ~ "Total runoff from land",
      TRUE ~ variable
    ),
    variable_unit = case_when(
      variable == "atotuse" ~ "kg/m2/s",
      variable == "dis" ~ "m³/s",
      variable == "qtot" ~ "kg/m2/s",
      TRUE ~ NA_character_
    )
  )

# Save metadata ----
saveRDS(file_metadata, file.path(input_dir, "file_metadata.rds"))
write_csv(file_metadata, file.path(input_dir, "file_metadata.csv"))

# Filter for monthly resolution only ----
files_to_download <- file_metadata %>% filter(resolution == "monthly")

# Download files ----
# TAKES LONG TIME, BIG FILES EACH, and about 60 of them
download_watergap_file <- function(url, dest_dir = output_dir, overwrite = FALSE) {
  filename <- basename(url)
  dest_path <- file.path(dest_dir, filename)

  if (file.exists(dest_path) && !overwrite) {
    return(invisible(TRUE))
  }

  tryCatch(
    {
      curl_download(url, dest_path, quiet = FALSE)
      return(invisible(TRUE))
    },
    error = function(e) {
      warning(glue("Error downloading {filename}: {e$message}"))
      return(invisible(FALSE))
    }
  )
}

# Download all monthly files
results <- map_lgl(files_to_download$url, ~ download_watergap_file(.x, output_dir, overwrite = FALSE))

# EoF
