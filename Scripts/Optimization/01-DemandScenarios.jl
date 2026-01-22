# Run Optimization Model with demand and deposits paramters created previously
# Calls a user-defined function to run an optimization model
# Has loops to run all the desired scenarios.
# PBH March 2024

using CSV
using DataFrames
using JuMP
using Gurobi
using LinearAlgebra


# Load built-in optimization function
# other potential path: Scripts/Supply Model/Optimization/
include("Optimization_MGA.jl")

# Load data
depositAll = DataFrame(CSV.File("Parameters/Deposit_water.csv"))
demandAll = DataFrame(CSV.File("Parameters/Demand.csv"))


# Single Run - DEBUG
# demandBase = filter(row -> row.Scenario == "Ambitious-Baseline-Baseline-Baseline-Baseline", demandAll)
#deposittest = DataFrame(CSV.File("Parameters/Deposit_water.csv"))
# runOptimization(demandBase,depositAll,"Test")

# DEMAND SCENARIOS
# Extract unique scenarios
unique_scenarios = unique(demandAll.Scenario)
for scen in unique_scenarios
    println(scen)
    # Filter scenario
    demand_scen = filter(row -> row.Scenario == scen, demandAll)
    runOptimization(demand_scen,depositAll,"DemandScenario/$scen")
end

# End of File