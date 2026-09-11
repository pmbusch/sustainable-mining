# =============================================================================
# Figure 4 — Data Preparation: AWARE CF Draw Severity Summary
#
# Purpose:
#   The raw aware_draw index (0-4999, see Scripts/Sensitivity/01-Sampling.R) has no
#   physical ordering and ~2 observations per level across 10,000 samples, so it
#   carries almost no learnable signal for the Figure 4 SHAP model. This script
#   reduces each draw to one continuous, physically meaningful value instead: the
#   water-consumption-weighted mean AWARE CF across basins that contain deposits.
#
# Output:
#   Parameters/AWARE_Stochastic_CFs/draw_summary.csv — aware_draw, aware_cf_severity
#
# Dependencies: Scripts/Inputs-Water/04-AWARE_Stochastic.R must already have been run
#               (reads its per-draw output files, does not regenerate them)
#
# Author:  Pablo Busch
# Date:    2026
# =============================================================================

source('Scripts/00-Libraries.R', encoding = 'UTF-8')

PATH_DRAWS <- "Parameters/AWARE_Stochastic_CFs"
PATH_DEPOSIT <- "Parameters/Deposit.csv"
N_DRAWS <- 5000

# -----------------------------------------------------------------------------
# 1. Basin weights (water_cons_2025 is already basin-level, duplicated across
#    every deposit row sharing a basin — see Scripts/Inputs-Water/03-AWARE.R)
# -----------------------------------------------------------------------------

basin_weights <- read_csv(PATH_DEPOSIT, show_col_types = FALSE) %>%
  distinct(Basin_ID, water_cons_2025)

draw_files <- file.path(PATH_DRAWS, sprintf("draw_%04d.csv", 0:(N_DRAWS - 1)))

# Basin order is identical across all draw files (written from the same basin_ids
# vector in 04-AWARE_Stochastic.R), so build the weight vector once
basin_ids <- fread(draw_files[1])$Basin_ID
weight_vec <- basin_weights$water_cons_2025[match(basin_ids, basin_weights$Basin_ID)]
weight_vec <- replace_na(weight_vec, 0)

# -----------------------------------------------------------------------------
# 2. Weighted mean CF per draw
# -----------------------------------------------------------------------------

cat(sprintf("Summarising %d AWARE CF draws...\n", N_DRAWS))

severity <- vapply(draw_files, function(f) weighted.mean(fread(f)$aware_cf, w = weight_vec), numeric(1))

draw_summary <- tibble(aware_draw = 0:(N_DRAWS - 1), aware_cf_severity = unname(severity))

write_csv(draw_summary, file.path(PATH_DRAWS, "draw_summary.csv"))
cat(sprintf("Wrote draw-level CF severity summary: %s\n", file.path(PATH_DRAWS, "draw_summary.csv")))

# EoF
