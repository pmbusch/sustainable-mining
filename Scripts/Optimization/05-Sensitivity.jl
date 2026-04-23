# Run Optimization Model sensitivity analysis samples analysis
# Last updated: PBH March 2026

using CSV
using DataFrames
using JuMP
using Gurobi
using LinearAlgebra

const GRB_ENV = Gurobi.Env()
GRBsetintparam(GRB_ENV, "Threads", 4)

# Load built-in optimization function
include("Optimization_Multi.jl")

# Load data once outside the loop
depositAll = DataFrame(CSV.File("Parameters/Deposit.csv"))
demandAll = DataFrame(CSV.File("Parameters/IEA_Demand.csv"))
include("LoadSampleFunction.jl")

# Run specific samples not run on SLURM batch job for any reason
for sample_id in [6838,6932,6885]
    println("\n========================================")
    println("Running sample $sample_id / 10")
    println("========================================")

    # Fresh copy of deposit for each sample — avoids draws accumulating across iterations
    deposit_copy = copy(depositAll)

    # Reconstruct demand and deposit from sample row
    demand, deposit, cost_water_des, fish_threshold = Base.invokelatest(load_sample, sample_id, deposit_copy)

    # Run optimisation — save results in folder named by sample number
    save_folder = "Samples2/" * lpad(string(sample_id), 4, '0')
    runOptimization(
        demand,
        deposit,
        save_folder;
        cost_water_des=cost_water_des,
        fishBiodiversity_limit=fish_threshold,
        fast_solve=true,
    )

    println("  Sample $sample_id complete — results in Results/Optimization/$save_folder")
end

println("\nAll samples complete.")