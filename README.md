# Reducing water stress from mining battery materials

Replication materials for Busch et al. 2025 Reducing water stress from mining battery materials *(submitted)*.

The following code and data inputs allows for the reproduction of all the tables, figures and calculations made in the article, both in the main body and supplementary information.

If you identify any error in the source code or have any further suggestions please contact Pablo Busch at pmbusch@uc.cl.

# Organization

* **Inputs**: Original data inputs used in the analysis. Some large/proprietary sources are not included - see "Data Availability" below.
* **Parameters**: Processed data derived from Inputs, used to run the analysis or re-create figures.
* **Scripts**: All code to process the data, run models and create figures.
* **Results**: Aggregated model/analysis outputs stored to recreate tables and figures.
* **Figures**: Figures of the article main body and supplementary information.

# Pipeline

Scripts run in this order:

1. **`Scripts/Inputs-Demand/`** - IEA Critical Minerals demand scenarios
2. **`Scripts/Inputs-Deposit/`** - S&P Global deposit database (Cu/Ni/Co/Li: reserves, grades, costs)
3. **`Scripts/Inputs-Water/`** - AWARE water-stress characterization factors and fish biodiversity index
4. **`Scripts/Optimization/`** (Julia + Gurobi) - Mixed integer linear programming (MILP) model minimizing cost subject to demand/resource/water constraints, with a Pareto (epsilon-constraint) exploration of cost vs. water-scarcity impact
5. **`Scripts/Sensitivity/`** - Random sampling over model inputs
6. **`Figure*_PrepareData*.R`** (top-level `Scripts/`) - Aggregates optimization results into figure-ready data
7. **`Figure*.R`** (top-level `Scripts/`), **`Scripts/SI-Figures/`**, **`Scripts/Other-Figures/`** - Final figures

# Data Availability

## Proprietary deposit data (not included)

`Inputs/SP/` (raw exports) and the derived `Parameters/Deposit.csv`, `Parameters/Deposits_Map.csv`, `Parameters/Intermediate/*_SP.csv` are excluded from this repository, since they are built from **S&P Global (Capital IQ Pro)** data under a license that does not permit redistribution.

To regenerate them, a user with their own S&P Global license should export deposit-level data for Copper, Nickel, Cobalt, and Lithium with the same fields used in `Scripts/Inputs-Deposit/01-NiCuCo_Deposits.R` and `02-Li_Deposits.R` (e.g. `PROP_NAME`, `PROP_ID`, `COUNTRY_NAME`, `TOTAL_CASH_COST_LB`, `TOTAL_CASH_COST_TONNE`, `CONTAINED_RESV_PCT_TONNE`, `GRD_RESV_PCT_TONNE`, `CONTAINED_R_AND_R_PCT_TONNE`, `R_AND_R_ORE_TONNAGE`, `COMMODITY_PRODUCTION_TONNE_BY_PERIOD`, `PRODUCTION_CAPACITY_TONNE`, `HEAD_GRD_PCT`, `PAID_METAL_PRODUCED_KILOTONNES`, `COMMODITY_PRICE_LB`), place the `.xls` exports in `Inputs/SP/`, and run `Scripts/Inputs-Deposit/01 → 02 → 03` followed by `Scripts/Inputs-Water/03-AWARE.R`.

## Large external datasets (not included)

| Dataset | Size | Source | Used by |
|---|---|---|---|
| IUCN Red List freshwater fish range shapefiles - FISHES | ~2.5 GB (zipped) | [IUCN spatial data portal](https://www.iucnredlist.org/resources/spatial-data-download) (free account required) - unzip into `Inputs/FW_FISH/` | `Scripts/Inputs-Water/02-FishBiodiversity.R` |
| AWARE 2.0 Monte Carlo CF ensemble | ~6 GB (zipped) | Seitfudem, Berger & Boulay (2026), [Zenodo](https://doi.org/10.5281/zenodo.19637310) - unzip into `Inputs/AWARE/Stochastic/` | `Scripts/Inputs-Water/04-AWARE_Stochastic.R` |

Once downloaded, all intermediate/processed files derived from these (`Parameters/FW_FISH/`, `Parameters/AWARE_Stochastic_CFs/`) regenerate automatically by re-running the corresponding scripts, and are themselves excluded from GitHub as regenerable outputs.

# Instructions

Users can run all the code for replication using the `sustainable-mining.Rproj` file, or by setting their own working directory and running scripts independently.

## Runtime

Each individual optimization scenario run takes roughly 60 seconds; the full scenario sweeps take longer:

* Figures reading results (`Figure1_StressMap.R`, `Figure2_ParetoCurves.R`, `Figure3_Country.R`, `Figure4_VariableImportance.R`) run in a few minutes each.
* Re-running the demand-scenario sweep (11 scenarios, feeds Figures 2 and 3) takes ~30 minutes.
* Re-running the full sensitivity sweep (10,000 simulation runs, feeds Figure 4) takes several hours.

# Software required

The script code was developed with **R** software version 4.4.1.

The optimization code was developed with **julia** version 1.11.0, and using Gurobi as solver. Instruction to get a license from Gurobi can be found [here](https://www.gurobi.com/solutions/licensing/). Alternatively, the optimization scripts can be adapted to run with another standard optimization solver.

The R code requires the following packages (this list was audited against every current, non-archived script - `old/` folders may use a few additional packages not needed for the main pipeline):
```
install.packages(c(
  "tidyverse", "readr", "readxl", "ggplot2", "data.table", "dplyr", "gridExtra",
  "reshape2", "scales", "RColorBrewer", "sf", "ggrepel", "terra", "ggh4x",
  "zoo", "patchwork", "leaflet", "ggVennDiagram", "geomtextpath", "curl",
  "Hmisc", "rpart", "ranger", "cowplot", "ncdf4", "lubridate", "CFtime",
  "glue", "lightgbm", "ggdensity", "lhs", "randtoolbox", "qrng", "janitor"
), dependencies = T)
```
The julia code requires the following packages: *CSV*,*DataFrames*,*JuMP*,*Gurobi*,*LinearAlgebra*. The following command can install these dependencies:
```
using Pkg
Pkg.add("CSV")
Pkg.add("DataFrames")
Pkg.add("JuMP")
Pkg.add("Gurobi")
Pkg.add("LinearAlgebra")
```

The model has only been tested using OS Windows 10 and 11, but it should work on Mac and Linux as well using **R** and **julia**.

# License
This project is covered under the **MIT License**
