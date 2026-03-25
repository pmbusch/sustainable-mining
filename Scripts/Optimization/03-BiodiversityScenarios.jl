# Run Optimization Model across demand and biodiversity scenarios
# Last updated: PBH Feb 2026

using CSV
using DataFrames
using JuMP
using Gurobi
using LinearAlgebra

# Load optimization function
include("Optimization_Multi.jl")

# Load data
depositAll = DataFrame(CSV.File("Parameters/Deposit.csv"))
demandAll = DataFrame(CSV.File("Parameters/IEA_Demand.csv"))

# Demand scenarios
# unique_scenarios = unique(demandAll.Scenario)
unique_scenarios = ["NZE"]

# Biodiversity scenarios
# FI: Fish index threshold
biod_limits = [
    ("none", 100.0), ("FI99", 99.9), ("FI90", 90.0), ("FI80", 80.0), ("FI70", 70.0), ("FI60", 60.0), ("FI50", 50.0)
]

biod_limits = [
    ("none", 100.0),
    ("FI99", 99.9),
    ("FI95", 95.0),
    ("FI90", 90.0),
    ("FI85", 85.0),
    ("FI80", 80.0),
    ("FI75", 75.0),
    ("FI70", 70.0),
    ("FI65", 65.0),
    ("FI60", 60.0),
    ("FI55", 55.0),
    ("FI50", 50.0),
]

# Run combinations
for scen in unique_scenarios
    demand_scen = filter(row -> row.Scenario == scen, demandAll)
    for (biod_name, biod_limit) in biod_limits
        println("Demand: $scen | Biodiversity: $biod_name")
        runOptimization(demand_scen, depositAll, "BioDScenario/$scen/$biod_name"; fishBiodiversity_limit=biod_limit)
    end
end

# End of file