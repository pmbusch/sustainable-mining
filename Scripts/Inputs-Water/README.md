# Water Input Data Processing Scripts

## Overview

This folder contains scripts for loading, processing, and preparing water scarcity data for the sustainable mining model. The scripts integrate multiple data sources to calculate spatially-explicit water scarcity characterization factors (CFs) for different climate scenarios and time periods.

The method for CF calculations is based on the AWARE LCA method (https://wulca-waterlca.org/aware/input-data-watergap/). The projections for future water availability and demand are based on the WaterGAP2-2e model outputs from the ISIMIP3b project (https://doi.org/10.48364/ISIMIP.230418.7).

## Workflow

The scripts should be run in the following order:

### 1. Deposit-Specific Water Data
- **`01a-Cu_Water.R`** - Processes water use data for copper deposits
- **`01b-Li_Water.R`** - Processes water use data for lithium deposits

### 2. AWARE Baseline Data
- **`02-AWARE.R`** - Loads and processes AWARE (Available WAter REmaining) baseline characterization factors
- **`03-AWARE_Proj.R`** - Projects AWARE data to future scenarios

### 3. WaterGAP Future Projections
- **`04-WaterGAP_Download.R`** - Downloads WaterGAP2-2e climate model output files from ISIMIP database
  - Parses filelist to extract metadata (climate model, scenario, variable, time period)
  - Downloads only monthly resolution data
  - Saves files to `Inputs/AWARE/WaterGAP/downloaded/`

- **`05-WaterGAP_Process.R`** - Processes downloaded WaterGAP netCDF files
  - Loads netCDF files in chunks by scenario-variable combination (memory-efficient)
  - Filters for 2025-2050 time period
  - Joins with continental area data for unit conversions
  - Converts units: qtot/atotuse (kg/m²/s → m³/month), dis (m³/s → m³/month)
  - Ensembles climate models (median, Q25, Q75)
  - Saves processed data to `Parameters/WaterGAP/`

- **`05b-Create_Grid_Basin_Lookup.R`** - Creates grid-basin spatial lookup table
  - Pre-computes spatial intersection between WaterGAP grid cells and AWARE basins
  - Calculates overlap areas and fractions in equal-area projection
  - Saves reusable lookup table (CSV) for fast basin aggregation
  - Only needs to be run once

- **`06b-WaterGAP_BasinAgg.R`** - Aggregates gridded data to basin level
  - Loads all ensemble files (all scenarios and variables)
  - Joins with grid-basin lookup table for fast spatial aggregation
  - Aggregates to basin level using area-weighted overlap
  - Saves intermediate basin-level data (CSV)

- **`06c-WaterGAP_CF.R`** - Calculates characterization factors
  - Loads basin-level aggregated data from 06b
  - Joins with AWARE reference data (ActAvail, EWR)
  - Scales Environmental Water Requirements (EWR) to projected conditions
  - Calculates Available Minus Demand (AMD) = (discharge - EWR - water use) / area
  - Computes world weighted average AMD
  - Calculates characterization factors: CF = AMD / AMD_world
  - Limits CF to range [0.1, 100]
  - Saves final CF data and summary statistics


## Required Data Sources

### 1. AWARE Data
- **Source**: AWARE characterization factors
- **Location**: `Inputs/AWARE/`
- **Files needed**:
  - `AWARE20_Native_CFs_geospatial.kmz` - Basin polygons with baseline CFs
  - `AWARE20_Intermediate_Variables.xlsx` - Reference data with sheets:
    - `ActAvail_1990_2019` - Historical actual availability by basin and month
    - `EWR` - Environmental Water Requirements by basin and month

### 2. WaterGAP Data (ISIMIP3b)
- **Source**: ISIMIP Repository (https://files.isimip.org/)
- **Location**: `Inputs/AWARE/WaterGAP/`
- **Files needed**:
  - `filelist.txt` - List of all available WaterGAP files to download
  - `watergap22e_gswp3-w5e5_continentalarea_histsoc_static.nc` - Grid cell areas (m²)
  - Climate model outputs (downloaded by script 04):
    - **Climate models**: gfdl-esm4, ipsl-cm6a-lr, mpi-esm1-2-hr, ukesm1-0-ll, mri-esm2-0
    - **Scenarios**: picontrol, ssp126, ssp370, ssp585
    - **Variables**:
      - `dis` - Streamflow/discharge (m³/s)
      - `atotuse` - Actual consumptive water use (kg/m²/s)
      - `qtot` - Total runoff from land (kg/m²/s)
    - **Resolution**: Monthly, 2015-2100


## Notes

- **Memory Management**: Script 05 processes data in chunks to avoid memory issues when loading large netCDF files
- **Performance Optimization**: Script 05b pre-computes spatial intersections to avoid slow spatial joins in later steps
- **Modular Workflow**: Basin aggregation (06b) and CF calculations (06c) are separated for flexibility and easier debugging

