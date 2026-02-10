# Water Input Data Processing Scripts

## Overview

Scripts for preparing water scarcity characterization factors (CFs) for the sustainable mining model. Integrates AWARE 2.0 baseline data with WaterGAP2-2e projections from ISIMIP3b to produce spatially-explicit, scenario-dependent CFs at basin level.

## Workflow

Scripts should be run in order:

### 1. Deposit Water Data
- **`01a-Cu_Water.R`** - Processes water consumption data for copper deposits
- **`01b-Li_Water.R`** - Processes water consumption data for lithium deposits

### 2. AWARE Baseline
- **`02-AWARE.R`** - Loads AWARE 2.0 baseline CFs, joins deposits to basins, computes deposit-level water footprints

### 3. WaterGAP Download
- **`03-WaterGAP_Download.R`** - Downloads WaterGAP2-2e monthly outputs from ISIMIP for all scenarios, climate models, and variables

### 4. Grid-Basin Lookup
- **`04a-Create_Grid_Basin_Lookup.R`** - Spatial intersection between WaterGAP 0.5-degree grid cells and AWARE basin polygons. Computes area-weighted overlap fractions so grid cells split across multiple basins are allocated proportionally. Run once.
- **`04-Create_Grid_Basin_Lookup_UPSTREAM.R`** - Alternative outlet-based mapping using DDM30 stream network (not used in main pipeline)

### 5. Basin Aggregation
- **`05-WaterGAP_Basin.R`** - Loads raw .nc files for qtot and atotuse, filters 2025-2050, converts units (kg/m2/s to m3/month), and aggregates grid-level data to basin level using the overlap lookup table. Loops over each scenario x climate_model and saves per-combination intermediate CSVs.

### 6. Characterization Factors
- **`06-WaterGAP_CF.R`** - Calculates AWARE-style CFs from basin-level qtot and atotuse. Scales EWR by monthly discharge ratio, computes monthly AMD and CFs, then aggregates to yearly (CF = mean, availability and demand = sum). Outputs basin-level CFs per scenario x climate_model x year.

### 7. Deposit-Level Water Scenarios
- **`07-Deposit_WaterScenarios.R`** - Joins the deposit database (`Parameters/Deposit.csv`) with basin-level CF projections (`basin_cf_data_5yr.csv`) on Basin_ID. Pivots periods wide so each deposit keeps one row, with columns `cf_2025–2030`, `availability_m3_yr_2025–2030`, etc. Saves one file per scenario x climate_model to `Parameters/WaterScenarios/`.

