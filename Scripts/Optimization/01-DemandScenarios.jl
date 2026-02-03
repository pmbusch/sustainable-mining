# Run Optimization Model with demand and deposits paramters created previously
# Calls a user-defined function to run an optimization model
# Has loops to run all the desired scenarios.
# Last updated: PBH Jan 2026

using CSV
using DataFrames
using JuMP
using Gurobi
using LinearAlgebra

# Load built-in optimization function
# other potential path: Scripts/Supply Model/Optimization/
include("Optimization_Multi.jl")

# Load data
depositAll = DataFrame(CSV.File("Parameters/Deposit.csv"))
demandAll = DataFrame(CSV.File("Parameters/IEA_Demand.csv"))

# Single Run - DEBUG
# demandBase = filter(row -> row.Scenario == "NZE", demandAll)
# runOptimization(demandBase, depositAll, "TestOre")

# DEMAND SCENARIOS
# Extract unique scenarios
unique_scenarios = unique(demandAll.Scenario)
for scen in unique_scenarios
    println(scen)
    # Filter scenario
    demand_scen = filter(row -> row.Scenario == scen, demandAll)
    runOptimization(demand_scen, depositAll, "DemandScenario/$scen")
end

# End of File