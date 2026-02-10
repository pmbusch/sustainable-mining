# Optimization Scripts

## Overview

Mixed-integer linear programming (MILP) model for optimizing mineral supply (Cu, Ni, Co, Li) across deposits and time periods (2025-2050). Minimizes discounted cost while meeting demand, respecting resource constraints, and optionally enforcing basin-level water availability limits. A Pareto exploration phase trades off cost for reduced water scarcity impact.

Solver: Gurobi (via JuMP).

## Scripts

### Main model
- **`Optimization_Multi.jl`** - Core optimization function `runOptimization()`. Builds and solves the MILP model with all constraints, then optionally runs the multi-objective Pareto exploration (epsilon-constraint on cost, minimizing water scarcity impact). See `ModelFormulation.tex` for the mathematical formulation.

### Scenario runners
- **`01-DemandScenarios.jl`** - Loops over IEA demand scenarios (e.g., NZE, APS, STEPS) and runs the optimization for each. No water constraints applied (baseline climate).
- **`02-ClimateScenarios.jl`** - Loops over demand scenarios x climate scenario files (`Parameters/WaterScenarios/`). Each combination applies scenario-specific water availability and water footprint data.

### Helper functions
- **`SaveFunction.jl`** - Extracts decision variable values and model metrics (cost, water, slack, mines opened) from a solved model and writes CSVs to `Results/Optimization/`.
- **`ShadowPriceFunction.jl`** - Copies the model, relaxes integrality, fixes binaries, and extracts shadow prices for basin water constraints and demand constraints.
- **`ClimateScenarioFunction.jl`** - Loads climate scenario data from `Parameters/WaterScenarios/` to create time-indexed `water_footprint[d,t]` and `aware_available[basin,t]` matrices. Falls back to static AWARE values when no climate scenario is specified.

## Inputs

| File | Description |
|------|-------------|
| `Parameters/Deposit.csv` | Deposit database with resources, grades, costs, water, Basin_ID |
| `Parameters/IEA_Demand.csv` | Mineral demand scenarios by year |
| `Parameters/WaterScenarios/*.csv` | Per-deposit water footprint and availability by period |
| `Parameters/basin_to_deposits_upstream.csv` | Basin-to-deposit mapping including upstream basins |

## Outputs

Results saved to `Results/Optimization/<folder>/`:
- `Base.csv` - Extraction, capacity added, mine opening decisions
- `Base_Slack.csv` - Unmet demand (slack) per mineral per year
- `Base_Metrics.csv` - Total cost, water consumption, water impact, mines opened
- `SP_Basin.csv` - Shadow prices for basin water constraints
- `SP_Demand.csv` - Shadow prices for mineral demand constraints
- `Water_EpsXX.csv` - Pareto solutions at various cost degradation levels
- `OptimizationInputs.csv` - Run parameters (discount rate, slack costs, etc.)
