# Water Input Data Processing Scripts

## Overview

Scripts for preparing water scarcity characterization factors (CFs) for the sustainable mining model. Integrates AWARE 2.0 baseline data with WaterGAP2-2e projections from ISIMIP3b to produce spatially-explicit, scenario-dependent CFs at basin level, plus a freshwater fish biodiversity overlap index.

Some inputs used here (IUCN fish range data, AWARE Monte Carlo CFs, WaterGAP NetCDFs) are too large to include on GitHub - see the root `README.md` "Data Availability" section for download instructions before running steps 2 and 5-9.

## Workflow

Scripts should be run in order:

### 1. Deposit Water Data
- **`01a-Cu_Water.R`** - Processes water consumption data for copper deposits
- **`01b-Li_Water.R`** - Processes water consumption data for lithium deposits

### 2. Fish Biodiversity
- **`02-FishBiodiversity.R`** - Computes a basin-level freshwater fish biodiversity overlap index from IUCN species range data (requires downloading `Inputs/FW_FISH/`, see root README)

### 3. AWARE Baseline
- **`03-AWARE.R`** - Loads AWARE 2.0 baseline CFs, joins deposits to basins, computes deposit-level water footprints. Produces the master `Parameters/Deposit.csv` used by nearly every downstream figure and the optimization model.

### 4. AWARE Monte Carlo Uncertainty
- **`04-AWARE_Stochastic.R`** - Restructures the raw per-basin AWARE 2.0 Monte Carlo CF ensemble (requires downloading `Inputs/AWARE/Stochastic/`, see root README) into per-draw files for the sensitivity analysis.
