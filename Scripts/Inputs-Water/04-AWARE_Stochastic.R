# AWARE 2.0 Stochastic CF Preprocessing
# Restructures the raw per-basin Monte Carlo AWARE2.0 CF ensemble (9406 basin files x
# 5000 draws each) into per-draw files (5000 files x 9406 basins), so the optimization
# model only needs to open one small file per Monte Carlo sample instead of one file
# per basin (9406 files) on every run.
# Source: Seitfudem, Berger & Boulay (2026), Zenodo https://doi.org/10.5281/zenodo.19637310
# PBH Aug 2026

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Not included in the GitHub repo (~6 GB zip). Download from the Zenodo record above
# and unzip into Inputs/AWARE/Stochastic/ - see README.md "Data Availability".
PATH_STOCHASTIC_CF <- "Inputs/AWARE/Stochastic/CFs"
PATH_DEPOSIT <- "Parameters/Deposit.csv"
PATH_OUTPUT_DIR <- "Parameters/AWARE_Stochastic_CFs"
N_DRAWS <- 5000 # random scenarios provided by Seitfudem et al.


# ----------------------------
# 1. Read per-basin stochastic CF files (annual value only)
# ----------------------------

files <- list.files(PATH_STOCHASTIC_CF, pattern = "^Monte_Carlo_results_CFs_.*\\.csv$", full.names = TRUE)
cat(sprintf("Found %d basin files\n", length(files)))

basin_ids <- basename(files) |> str_remove("^Monte_Carlo_results_CFs_") |> str_remove("\\.csv$") |> as.numeric()

draw_cols <- as.character(0:(N_DRAWS - 1))

# Each basin file has 12 monthly rows + one Annual_arithm_average row; the model uses a
# single annual CF per basin (same convention as the deterministic AWARE20_Native_CFs data)
read_annual_cf <- function(f) {
  dt <- fread(f, header = TRUE)
  row <- dt[month == "Annual_arithm_average", ..draw_cols]
  as.numeric(row[1])
}

cf_list <- lapply(files, read_annual_cf)
cat("Finished reading all basin files\n")

# Guard against any malformed file silently misaligning the basin x draw matrix below
bad_len <- lengths(cf_list) != N_DRAWS
if (any(bad_len)) {
  stop(sprintf(
    "%d file(s) do not have %d draw columns: %s",
    sum(bad_len),
    N_DRAWS,
    paste(basename(files)[bad_len], collapse = ", ")
  ))
}


# ----------------------------
# 2. Assemble basin x draw matrix
# ----------------------------

cf_matrix <- matrix(unlist(cf_list, use.names = FALSE), nrow = length(cf_list), ncol = N_DRAWS, byrow = TRUE)
rownames(cf_matrix) <- basin_ids
colnames(cf_matrix) <- draw_cols

cat(sprintf("Assembled CF matrix: %d basins x %d draws\n", nrow(cf_matrix), ncol(cf_matrix)))


# ----------------------------
# 3. Validate basin IDs against deposit database
# ----------------------------

deposit_basins <- unique(read_csv(PATH_DEPOSIT, show_col_types = FALSE)$Basin_ID)
missing_basins <- setdiff(deposit_basins, basin_ids)
if (length(missing_basins) > 0) {
  stop(sprintf(
    "%d deposit basin(s) missing from stochastic CF set: %s",
    length(missing_basins),
    paste(missing_basins, collapse = ", ")
  ))
}
cat(sprintf(
  "All %d deposit basins found in stochastic CF set (%d basins total)\n",
  length(deposit_basins),
  length(basin_ids)
))


# ----------------------------
# 4. Write one file per draw
# ----------------------------

dir.create(PATH_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

for (d in 0:(N_DRAWS - 1)) {
  out <- data.table(Basin_ID = basin_ids, aware_cf = cf_matrix[, as.character(d)])
  fwrite(out, sprintf("%s/draw_%04d.csv", PATH_OUTPUT_DIR, d))
  if (d %% 500 == 0) {
    cat(sprintf("  Wrote draw %d/%d\n", d, N_DRAWS - 1))
  }
}

cat(sprintf("\nDone. Wrote %d files to %s\n", N_DRAWS, PATH_OUTPUT_DIR))

# EoF
