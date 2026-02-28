# Run Optimization Model with demand and deposits paramters created previously
# Calls a user-defined function to run an optimization model
# Has loops to run all the desired scenarios.
# PBH Dec 2025

using CSV
using DataFrames
using JuMP
using Gurobi
using LinearAlgebra


# Load built-in optimization function
# other potential path: Scripts/Supply Model/Optimization/
include("Optimization_Multi.jl")

# Load data
depositAll = DataFrame(CSV.File("Parameters/Cu_Deposit.csv"))
demandAll = DataFrame(CSV.File("Parameters/Cu_Demand.csv"))


# Single Run - DEBUG
demandBase = filter(row -> row.Scenario == "Ambitious-Baseline-Baseline-Baseline-Baseline", demandAll)
deposittest = DataFrame(CSV.File("Parameters/Cu_Deposit.csv"))
# Copper high price: 13,500 USD per metric ton, set 50% as slack cost
runOptimization(demandBase,depositAll,"TestCu";bigM_cost=14000*1.5/1e3,multiobjective=false)

# DEMAND SCENARIOS
# Extract unique scenarios
unique_scenarios = unique(demandAll.Scenario)
for scen in unique_scenarios
    println(scen)
    # Filter scenario
    demand_scen = filter(row -> row.Scenario == scen, demandAll)
    # runOptimization(demand_scen,depositAll,"DemandScenario/Copper/$scen";bigM_cost=14000*1.5/1e3)
end

# End of File