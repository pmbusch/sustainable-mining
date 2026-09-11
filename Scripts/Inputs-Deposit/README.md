# Deposit Input Data Processing Scripts

## Overview

Builds the mineral deposit database (Cu, Ni, Co, Li) from S&P Global (Capital IQ Pro) exports: reserves, resources, grades, production capacity, and costs per deposit.

**These raw exports (`Inputs/SP/`) are proprietary and not included in the GitHub repo.** They require a valid S&P Global license. See the root `README.md` "Data Availability" section for the exact export parameters used, so a licensed user can reproduce the same data and re-run this pipeline.

## Workflow

Scripts should be run in order:

1. **`01-NiCuCo_Deposits.R`** - Loads and processes S&P exports for copper, nickel, and cobalt (co-product-aware, since many deposits produce more than one of these). Writes `Parameters/Intermediate/CuNiCo_Deposit_SP.csv`.
2. **`02-Li_Deposits.R`** - Loads and processes the S&P export for lithium (treated as primary product only, no co-production). Writes `Parameters/Intermediate/Li_Deposit_SP.csv`.
3. **`03-JoinDeposits_Map.R`** - Merges the two outputs above into `Parameters/Intermediate/All_Deposit_SP.csv` and a long-format `Parameters/Deposits_Map.csv` (used by the Figure 1 map). This is then joined with water data in `Scripts/Inputs-Water/03-AWARE.R` to produce the final `Parameters/Deposit.csv`.
4. **`99-CoProducts_Venn.R`** *(optional)* - Venn diagram of co-product overlap across minerals; a diagnostic figure, not a pipeline data step.
