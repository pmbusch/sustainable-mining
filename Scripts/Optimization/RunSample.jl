# RunSample.jl — runs a contiguous block of samples sequentially
# Called by SLURM array: julia RunSample.jl <batch_id>
# Batch 1 → samples 1–50, Batch 2 → samples 51–100, etc.
# Last updated: PBH March 2026

using CSV
using DataFrames
using JuMP
using Gurobi
using LinearAlgebra

include("Optimization_Multi.jl")

# -----------------------------------------------------------------------------
# CONFIGURATION
# -----------------------------------------------------------------------------
SAMPLES_PER_BATCH = 50

# Read batch index from SLURM array task ID
batch_id = parse(Int, ARGS[1])
id_start = (batch_id - 1) * SAMPLES_PER_BATCH + 1
id_end = batch_id * SAMPLES_PER_BATCH
println("Batch $batch_id — samples $id_start to $id_end")

# Load base data once per batch job (not reloaded for every sample)
depositAll = DataFrame(CSV.File("Parameters/Deposit.csv"))
demandAll = DataFrame(CSV.File("Parameters/IEA_Demand.csv"))
include("LoadSampleFunction.jl")

# -----------------------------------------------------------------------------
# LOOP OVER SAMPLES IN THIS BATCH
# -----------------------------------------------------------------------------
for sample_id in id_start:id_end
    println("\n  Sample $sample_id starting...")

    for (water_des_available, fish_strict) in [(0, 0), (0, 1), (1, 0), (1, 1)]
        deposit_copy = copy(depositAll)
        demand, deposit, cost_water_des, fish_threshold = Base.invokelatest(load_sample, sample_id, deposit_copy)

        # Apply binary conditions
        cost_water_des_run = water_des_available == 1 ? cost_water_des : 1000.0
        fish_threshold_run = fish_strict == 1 ? 100.0 : fish_threshold

        des_name = water_des_available == 1 ? "Desalination" : "NoDesalination"
        fish_name = fish_strict == 1 ? "StrictFish" : "BaselineFish"
        save_folder = "Samples/" * des_name * "_" * fish_name * "/" * lpad(string(sample_id), 3, '0')

        runOptimization(
            demand,
            deposit,
            save_folder;
            cost_water_des=cost_water_des_run,
            fishBiodiversity_limit=fish_threshold_run,
            fast_solve=true,
        )
    end
end

println("\nBatch $batch_id complete.")