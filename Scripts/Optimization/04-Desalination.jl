# Run Optimization Model across demand and biodiversity and WATER DESALINATION COST scenarios
# Last updated: PBH March 2026

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

# Desalination cost scenarios
des_costs = [
    ("DC025", 0.25),
    ("DC05", 0.5),
    ("DC1", 1.0),
    ("DC15", 1.5),
    ("DC2", 2.0),
    ("DC25", 2.5),
    ("DC5", 5.0),
    ("DC10", 10.0),
]

# Biodiversity scenarios
biod_limits = [("FI100", 100.0), ("FI90", 90.0), ("FI80", 80.0), ("FI70", 70.0)]

# Run combinations
for scen in unique_scenarios
    demand_scen = filter(row -> row.Scenario == scen, demandAll)
    for (dc_name, des_cost) in des_costs
        for (biod_name, biod_limit) in biod_limits
            println("Demand: $scen | Biod: $biod_name | DesCost: $dc_name")

            runOptimization(
                demand_scen,
                depositAll,
                "DesCostScenario/$scen/$biod_name/$dc_name";
                fishBiodiversity_limit=biod_limit,
                cost_water_des=des_cost,
            )
        end
    end
end