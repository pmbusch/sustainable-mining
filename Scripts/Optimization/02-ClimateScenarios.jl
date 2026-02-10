# Run Optimization Model across all climate scenarios
# Iterates over all files in Parameters/WaterScenarios/
# Last updated: PBH Feb 2026

using CSV
using DataFrames
using JuMP
using Gurobi
using LinearAlgebra

# Load built-in optimization function
include("Optimization_Multi.jl")

# Load data
depositAll = DataFrame(CSV.File("Parameters/Deposit.csv"))
demandAll = DataFrame(CSV.File("Parameters/IEA_Demand.csv"))

# DEMAND x CLIMATE SCENARIOS
unique_scenarios = unique(demandAll.Scenario)

climate_files = readdir("Parameters/WaterScenarios")
climate_files = filter(f -> endswith(f, ".csv"), climate_files)

for scen in unique_scenarios
    demand_scen = filter(row -> row.Scenario == scen, demandAll)
    for cf in climate_files
        climate_name = replace(cf, ".csv" => "")
        println("Demand: $scen | Climate: $climate_name")
        runOptimization(demand_scen, depositAll, "ClimateScenario/$scen/$climate_name"; climate_scenario=cf)
    end
end

# End of File
