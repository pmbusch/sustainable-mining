# =============================================================================
# LHS SAMPLING SCRIPT — GLOBAL SENSITIVITY ANALYSIS
# Mining Supply Optimisation Model — Water Impact Drivers
#
# Purpose:
#   Generate a compact master sample table (N x K matrix) using Latin Hypercube
#   Sampling. Each row represents one independent scenario. The optimisation
#   model reads one row at a time and reconstructs all inputs internally.
#
# Output:
#   lhs_samples.csv — one row per sample, one column per sampled dimension
#
# Parameter groups:
#   1. Demand scalars       (3 dims)  — applied to IEA_Demand.csv at runtime
#   2. Recovery rates       (4 dims)  — one draw per mineral, all deposits
#   3. Max depletion rate   (4 dims)  — one draw per primary mineral group
#   4. Deposit-level params (varies)  — one U(0,1) quantile per
#                                       {param x mineral x mine_type_group}
#
# Deposit-level parameters sampled: OPEX_ore, CAPEX_opening, CAPEX_exp, water
# Mine-type groups: brine | brine_DLE | other
# Minerals: Copper, Nickel, Cobalt, Lithium
#
# Author:  [your name]
# Date:    2025
# Seed:    42 (fixed for reproducibility)
# =============================================================================

# -----------------------------------------------------------------------------
# 0. DEPENDENCIES
# -----------------------------------------------------------------------------

library(lhs) # randomLHS()
library(randtoolbox)
source('Scripts/00-Libraries.R', encoding = 'UTF-8')


# -----------------------------------------------------------------------------
# 1. CONFIGURATION ----------------------------------
# -----------------------------------------------------------------------------

SEED <- 24032026
N_SAMPLES <- 500 # increase later as needed
MINERALS <- c("Copper", "Nickel", "Cobalt", "Lithium")

# Mine-type grouping: raw values in mine_type column -> group label
MINE_TYPE_MAP <- c(
  "Brine" = "brine",
  "Brine DLE" = "brine_DLE",
  "Clay" = "other_lithium",
  "Combined" = "other",
  "Hard Rock" = "other_lithium",
  "Open Pit" = "",
  "Other" = "other",
  "Underground" = "other"
)
MINE_TYPE_GROUPS_LI <- c("brine", "brine_DLE", "other_lithium")

# Deposit-level parameters with their _low / _high column suffixes
DEPOSIT_PARAMS <- list(
  opex = list(col = "OPEX_ore", low = "OPEX_ore_low", high = "OPEX_ore_high"),
  capex_open = list(col = "CAPEX_opening", low = "CAPEX_opening_low", high = "CAPEX_opening_high"),
  capex_exp = list(col = "CAPEX_exp", low = "CAPEX_exp_low", high = "CAPEX_exp_high"),
  water = list(col = "water", low = "water_low", high = "water_high")
)

# File paths
PATH_DEPOSIT <- "Parameters/Deposit.csv" # ~1000-row deposit DB
PATH_OUTPUT <- "Parameters/samples.csv"


# -----------------------------------------------------------------------------
# 2. LOAD DEPOSIT DATABASE --------------------------------------------
# -----------------------------------------------------------------------------

cat("Loading deposit database...\n")
db <- read_csv(PATH_DEPOSIT, show_col_types = FALSE)

# Assign primary mineral per deposit: argmax of grade_resource_* (NA-safe)
grade_cols <- paste0("grade_resource_", MINERALS)
db <- db %>%
  mutate(
    primary_mineral = MINERALS[apply(select(., all_of(grade_cols)), 1, function(x) {
      if (all(is.na(x))) {
        return(NA_integer_)
      }
      which.max(x)
    })]
  )

# Assign mine-type group
db <- db %>% mutate(mine_type_group = recode(mine_type, !!!MINE_TYPE_MAP, .default = "other"))

cat(sprintf("  Deposits loaded: %d\n", nrow(db)))
cat("  Primary mineral distribution:\n")
print(table(db$primary_mineral, useNA = "ifany"))
cat("  Mine-type group distribution:\n")
print(table(db$mine_type_group, useNA = "ifany"))


# -----------------------------------------------------------------------------
# 3. DEFINE PARAMETER DIMENSIONS FOR LHS --------------------------------------------
#
#   Each entry in `param_defs` is one LHS dimension:
#     name  : column name in output CSV
#     min   : lower bound for qunif()
#     max   : upper bound for qunif()
#
#   For deposit-level draws the bounds are always 0–1 (quantile), so the model
#   applies: value = low + q * (high - low) row-by-row at runtime.
# -----------------------------------------------------------------------------

param_defs <- list()

## --- 3a. Demand scalars -------------------------------------------------------
param_defs[["demand_level"]] <- list(min = 0, max = 2) # 0=SPS, 1=APS, 2=NZE
param_defs[["share_LFP"]] <- list(min = 0.3, max = 0.9)
param_defs[["ni_co_ratio"]] <- list(min = 6, max = 12)
param_defs[["desal_cost"]] <- list(min = 0.25, max = 1.5)
param_defs[["fish_threshold"]] <- list(min = 60, max = 100)

## --- 3b. Recovery rates (one draw per mineral, applied to all deposits) -------
#   Bounds: median of {recovery_rate_{mineral}_low/high} across all deposits
#   Rationale: the single draw is a global multiplier, so representative central
#   bounds are used. The model scales each deposit's base rate by the draw.

for (m in MINERALS) {
  low_col <- paste0("recovery_rate_", m, "_low")
  high_col <- paste0("recovery_rate_", m, "_high")

  if (!low_col %in% names(db) || !high_col %in% names(db)) {
    cat(sprintf("  WARNING: recovery rate bounds missing for %s — skipping\n", m))
    next
  }

  # Use median bounds across all deposits with non-NA values for this mineral
  rows <- db %>% filter(primary_mineral == m)
  if (nrow(rows) == 0) {
    rows <- db
  } # fallback to all rows if no primary match

  lo <- median(rows[[low_col]], na.rm = TRUE)
  hi <- median(rows[[high_col]], na.rm = TRUE)

  param_defs[[paste0("recovery_", m)]] <- list(min = lo, max = hi)
}

## --- 3c. Max depletion rate --------------------------------------------------
# Cu, Ni, Co: one draw per mineral (no mine-type split)
# Li: one draw per mine-type group (brine, brine_DLE, other_lithium)

for (m in c("Copper", "Nickel", "Cobalt")) {
  rows <- db %>% filter(primary_mineral == m)
  if (nrow(rows) == 0 || !"max_depletion_rate_low" %in% names(db) || !"max_depletion_rate_high" %in% names(db)) {
    next
  }
  lo <- median(rows$max_depletion_rate_low, na.rm = TRUE)
  hi <- median(rows$max_depletion_rate_high, na.rm = TRUE)
  if (is.na(lo) || is.na(hi)) {
    next
  }
  param_defs[[paste0("depletion_", m)]] <- list(min = lo, max = hi)
}

for (mt in MINE_TYPE_GROUPS_LI) {
  rows <- db %>% filter(primary_mineral == "Lithium", mine_type_group == mt)
  if (nrow(rows) == 0 || !"max_depletion_rate_low" %in% names(db) || !"max_depletion_rate_high" %in% names(db)) {
    next
  }
  lo <- median(rows$max_depletion_rate_low, na.rm = TRUE)
  hi <- median(rows$max_depletion_rate_high, na.rm = TRUE)
  if (is.na(lo) || is.na(hi)) {
    next
  }
  param_defs[[paste0("depletion_Lithium_", mt)]] <- list(min = lo, max = hi)
}

## --- 3d. Deposit-level params: one U(0,1) per {param x mineral x mine_type} --
# Cu, Ni, Co: one draw per mineral (no mine-type split)
# Li: one draw per mine-type group

for (param_key in names(DEPOSIT_PARAMS)) {
  for (m in c("Copper", "Nickel", "Cobalt")) {
    param_defs[[paste0(param_key, "_", m)]] <- list(min = 0, max = 1)
  }
  for (mt in MINE_TYPE_GROUPS_LI) {
    param_defs[[paste0(param_key, "_Lithium_", mt)]] <- list(min = 0, max = 1)
  }
}

K <- length(param_defs)
cat(sprintf("\nTotal LHS dimensions: %d\n", K))
cat(sprintf("Sample size:          %d\n", N_SAMPLES))
cat(sprintf("Approximate coverage: %.1f samples per dimension\n", N_SAMPLES / K))


# -----------------------------------------------------------------------------
# 4. GENERATE LHS MATRIX --------------------------------------------
# -----------------------------------------------------------------------------

library(qrng)
set.seed(SEED)
# lhs_matrix <- randomLHS(N_SAMPLES, K) # N x K matrix, values in (0,1)
sobol_matrix <- sobol(n = N_SAMPLES, d = K, randomize = "digital.shift", seed = SEED)

# Quick uniformity check on a few dimensions
apply(sobol_matrix[, 1:5], 2, summary)

# Map each LHS column to its parameter bounds via qunif()
sample_df <- as.data.frame(matrix(NA_real_, nrow = N_SAMPLES, ncol = K))
colnames(sample_df) <- names(param_defs)

for (i in seq_along(param_defs)) {
  p <- param_defs[[i]]
  sample_df[[i]] <- qunif(sobol_matrix[, i], min = p$min, max = p$max)
}

# Add sample ID
sample_df <- sample_df %>% mutate(sample_id = seq_len(N_SAMPLES), .before = 1)


# -----------------------------------------------------------------------------
# 5. VALIDATION CHECKS
# -----------------------------------------------------------------------------

cat("\nValidation summary:\n")

# Check no NAs in output
n_na <- sum(is.na(sample_df))
if (n_na > 0) {
  warning(sprintf("  %d NA values in sample table — check bound derivation above", n_na))
} else {
  cat("  No NA values\n")
}

# Check all columns within bounds
out_of_bounds <- sapply(names(param_defs), function(nm) {
  p <- param_defs[[nm]]
  any(sample_df[[nm]] < p$min | sample_df[[nm]] > p$max, na.rm = TRUE)
})
if (any(out_of_bounds)) {
  warning("  Out-of-bounds values in: ", paste(names(which(out_of_bounds)), collapse = ", "))
} else {
  cat("  All values within declared bounds\n")
}

# Marginal distribution check: mean should be close to midpoint for each param
cat("  Spot-check means vs midpoints (first 6 demand/recovery params):\n")
check_cols <- head(names(param_defs), 6)
for (nm in check_cols) {
  p <- param_defs[[nm]]
  mid <- (p$min + p$max) / 2
  actual <- mean(sample_df[[nm]], na.rm = TRUE)
  cat(sprintf("    %-30s  midpoint=%.4f  sample_mean=%.4f\n", nm, mid, actual))
}


# -----------------------------------------------------------------------------
# 6. WRITE OUTPUT
# -----------------------------------------------------------------------------

write_csv(sample_df, PATH_OUTPUT)

cat(sprintf("\nOutput written: %s\n", PATH_OUTPUT))
cat(sprintf("Dimensions:     %d rows x %d columns (incl. sample_id)\n", nrow(sample_df), ncol(sample_df)))
cat("\nColumn groups:\n")
cat(sprintf("  sample_id                   : 1\n"))
cat(sprintf("  demand scalars              : 3\n"))
cat(sprintf("  recovery rates              : %d\n", sum(startsWith(names(sample_df), "recovery_"))))
cat(sprintf("  depletion rates             : %d\n", sum(startsWith(names(sample_df), "depletion_"))))
cat(sprintf(
  "  deposit quantile draws      : %d\n",
  sum(names(sample_df) %in% grep("^(opex|capex|water)_", names(sample_df), value = TRUE))
))
cat("\nDone.\n")

# EoF
