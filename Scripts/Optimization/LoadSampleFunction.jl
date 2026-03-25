# =============================================================================
# LoadSample.jl — Reconstruct model inputs from one LHS sample row
#
# Purpose:
#   Given a sample index, load the corresponding row from the LHS sample table
#   and reconstruct two model inputs:
#     1. demand   :: DataFrame  — time series 2025–2050 with columns
#                                 [Year, Copper, Nickel, Cobalt, Lithium] (ktons)
#     2. deposit  :: DataFrame  — full deposit database with sampled columns
#                                 overwritten in-place
#   Also returns:
#     cost_water_des :: Float64 — desalination cost scalar (USD/m3) for the
#                                 keyword argument in runOptimization()
#
# Depends on:
#   CSV, DataFrames  (already used in Optimization_Multi.jl)
#
# File paths (adjust to project structure):
#   PATH_SAMPLES  — lhs_samples.csv produced by lhs_sampling.R
#   PATH_DEMAND   — Parameters/IEA_Demand.csv
#   PATH_DEPOSIT  — deposit database CSV
# =============================================================================

using CSV
using DataFrames
using Statistics

# -----------------------------------------------------------------------------
# FILE PATHS — adjust to your project layout
# -----------------------------------------------------------------------------

const PATH_SAMPLES = "Parameters/samples.csv"

# Scenario order in IEA_Demand.csv — low to high
const SCENARIOS = ["SPS", "APS", "NZE"]

# Minerals relevant to recovery rate and depletion sampling
const MINERALS = ["Copper", "Nickel", "Cobalt", "Lithium"]

# Mine-type grouping — must match lhs_sampling.R
const MINE_TYPE_MAP = Dict(
    "Brine" => "brine",
    "Brine DLE" => "brine_DLE",
    "Clay" => "other_lithium",
    "Combined" => "other",
    "Hard Rock" => "other_lithium",
    "Open Pit" => "other",
    "Other" => "other",
    "Underground" => "other",
)

# Deposit-level parameters: sample column prefix => (base_col, low_col, high_col)
const DEPOSIT_PARAMS = Dict(
    "opex" => ("OPEX_ore", "OPEX_ore_low", "OPEX_ore_high"),
    "capex_open" => ("CAPEX_opening", "CAPEX_opening_low", "CAPEX_opening_high"),
    "capex_exp" => ("CAPEX_exp", "CAPEX_exp_low", "CAPEX_exp_high"),
    "water" => ("water", "water_low", "water_high"),
)

# Load things once, to avoid repeated load in every run
samples = CSV.read(PATH_SAMPLES, DataFrame)
# Extract the three scenario tables (all years present in each)
dem_sps = filter(r -> r.Scenario == "SPS", demandAll)
dem_aps = filter(r -> r.Scenario == "APS", demandAll)
dem_nze = filter(r -> r.Scenario == "NZE", demandAll)
# Sort by year to ensure alignment
sort!(dem_sps, :Year)
sort!(dem_aps, :Year)
sort!(dem_nze, :Year)
years = dem_sps.Year   # shared year vector

# =============================================================================
# MAIN FUNCTION
# =============================================================================

function load_sample(sample_index::Int, deposit; save_deposit::Bool=false)

    # -------------------------------------------------------------------------
    # 1. LOAD SAMPLE ROW
    # -------------------------------------------------------------------------

    if sample_index < 1 || sample_index > nrow(samples)
        error("sample_index $sample_index out of range (1–$(nrow(samples)))")
    end

    s = samples[sample_index, :]   # NamedTuple-like row; access as s.column_name

    # -------------------------------------------------------------------------
    # 2. RECONSTRUCT DEMAND TIME SERIES
    #
    #   demand_level ∈ [0, 2] interpolates across three IEA scenarios:
    #     0.0        → SPS  only
    #     0.0–1.0    → weighted blend of SPS and APS
    #     1.0        → APS  only
    #     1.0–2.0    → weighted blend of APS and NZE
    #     2.0        → NZE  only
    #
    #   Steps applied in order:
    #     a) Interpolate total demand across scenarios
    #     b) Re-scale ev_Nickel and ev_Cobalt for new share_LFP
    #     c) Redistribute total (ev_Nickel + ev_Cobalt) by ni_co_ratio
    #     d) Recompute total Nickel = non-ev + new ev_Nickel
    # -------------------------------------------------------------------------

    # Scalar draws
    dl = s.demand_level   # ∈ [0, 2]
    lfp_new = s.share_LFP      # ∈ [0.3, 0.9]
    nc_ratio = s.ni_co_ratio    # ∈ [6, 12]

    # --- 2a. Interpolate across scenarios ------------------------------------
    # Blend SPS→APS for dl ∈ [0,1], APS→NZE for dl ∈ [1,2]

    function blend_col(col::Symbol)
        if dl <= 1.0
            w = dl                    # weight on APS (0 = pure SPS, 1 = pure APS)
            return (1 - w) .* dem_sps[!, col] .+ w .* dem_aps[!, col]
        else
            w = dl - 1.0              # weight on NZE (0 = pure APS, 1 = pure NZE)
            return (1 - w) .* dem_aps[!, col] .+ w .* dem_nze[!, col]
        end
    end

    # Blend all relevant columns
    share_LFP_base = blend_col(:share_LFP)   # original LFP share (time series)
    ev_Cobalt = blend_col(:ev_Cobalt)
    ev_Copper = blend_col(:ev_Copper)
    ev_Lithium = blend_col(:ev_Lithium)
    ev_Nickel = blend_col(:ev_Nickel)
    total_Cobalt = blend_col(:Cobalt)
    total_Copper = blend_col(:Copper)
    total_Lithium = blend_col(:Lithium)
    total_Nickel = blend_col(:Nickel)

    # --- 2b. Re-scale ev_Nickel and ev_Cobalt for new share_LFP --------------
    # LFP batteries use no Ni or Co. Higher LFP share reduces ev demand for both.
    # Scale factor = original_share / new_share (capped to avoid division by zero)
    lfp_scale = share_LFP_base ./ max.(lfp_new, 0.01)   # element-wise, per year
    ev_Nickel = ev_Nickel .* lfp_scale
    ev_Cobalt = ev_Cobalt .* lfp_scale

    # --- 2c. Redistribute (ev_Ni + ev_Co) by ni_co_ratio --------------------
    # ni_co_ratio = Nickel / Cobalt in the EV demand mix
    # Total pool is conserved; only the split changes
    ev_NiCo_total = ev_Nickel .+ ev_Cobalt
    ev_Nickel_new = ev_NiCo_total .* nc_ratio ./ (nc_ratio + 1.0)
    ev_Cobalt_new = ev_NiCo_total .* 1.0 ./ (nc_ratio + 1.0)

    # --- 2d. Recompute total Nickel and Cobalt (non-ev part unchanged) -------
    non_ev_Nickel = total_Nickel .- ev_Nickel   # non-EV nickel demand (unchanged)
    non_ev_Cobalt = total_Cobalt .- ev_Cobalt

    final_Nickel = non_ev_Nickel .+ ev_Nickel_new
    final_Cobalt = non_ev_Cobalt .+ ev_Cobalt_new

    # Assemble demand DataFrame (one row per year, ktons — matches optimization input)
    demand = DataFrame(;
        Year=years,
        Copper=total_Copper,    # Copper not affected by LFP or NiCo ratio
        Nickel=final_Nickel,
        Cobalt=final_Cobalt,
        Lithium=total_Lithium,   # Lithium not affected by LFP or NiCo ratio
    )

    # -------------------------------------------------------------------------
    # 3. RECONSTRUCT DEPOSIT DATABASE
    #
    #   Three types of adjustment, applied to a copy of the base database:
    #
    #   a) Recovery rates — one global draw per mineral, applied to all rows
    #   b) Max depletion rate — one global draw per primary mineral group
    #   c) Deposit-level params (OPEX, CAPEX, water) — one U(0,1) quantile draw
    #      per {param × mineral × mine_type_group}, interpolated between _low/_high
    # -------------------------------------------------------------------------

    # Assign primary mineral per deposit: argmax of grade_resource_* (NA-safe)
    grade_cols = ["grade_resource_" * m for m in MINERALS]
    function primary_mineral(row)
        vals = [ismissing(row[c]) ? NaN : Float64(row[c]) for c in grade_cols]
        best = argmax(vals)
        return all(isnan, vals) ? missing : MINERALS[best]
    end
    deposit[!, :primary_mineral] = [primary_mineral(row) for row in eachrow(deposit)]

    # Assign mine-type group
    deposit[!, :mine_type_group] = [
        get(MINE_TYPE_MAP, ismissing(r.mine_type) ? "" : r.mine_type, "other") for r in eachrow(deposit)
    ]

    # --- 3a. Recovery rates --------------------------------------------------
    # Sample column: recovery_{Mineral} — already mapped to physical units in R
    # Apply directly to every row (global draw per mineral)

    for m in MINERALS
        col = "recovery_rate_" * m
        sample_col = "recovery_" * m
        if hasproperty(s, Symbol(sample_col)) && col in names(deposit)
            deposit[!, col] .= s[Symbol(sample_col)]
        end
    end

    # --- 3b. Max depletion rate ----------------------------------------------
    # Cu, Ni, Co: one draw per mineral (no mine-type split)
    # Li: one draw per mine-type group (brine, brine_DLE, other_lithium)

    for m in ["Copper", "Nickel", "Cobalt"]
        sample_col = Symbol("depletion_" * m)
        if !hasproperty(s, sample_col)
            continue
        end
        mask = .!ismissing.(deposit.primary_mineral) .& (deposit.primary_mineral .== m)
        deposit[mask, :max_depletion_rate] .= s[sample_col]
    end

    for mt in ["brine", "brine_DLE", "other_lithium"]
        sample_col = Symbol("depletion_Lithium_" * mt)
        if !hasproperty(s, sample_col)
            continue
        end
        mask = (
            .!ismissing.(deposit.primary_mineral) .&
            (deposit.primary_mineral .== "Lithium") .&
            (deposit.mine_type_group .== mt)
        )
        deposit[mask, :max_depletion_rate] .= s[sample_col]
    end

    # --- 3c. Deposit-level params (OPEX, CAPEX, water) -----------------------
    # Sample column: {param_key}_{Mineral}_{mine_type_group} ∈ [0, 1]
    # Reconstructed value = low + q * (high - low), applied per matching row

    for (param_key, (base_col, low_col, high_col)) in DEPOSIT_PARAMS
        if !(base_col in names(deposit) && low_col in names(deposit) && high_col in names(deposit))
            @warn "Skipping $param_key — columns not found in deposit database"
            continue
        end

        # Cu, Ni, Co: one draw per mineral
        for m in ["Copper", "Nickel", "Cobalt"]
            sample_col = Symbol(param_key * "_" * m)
            if !hasproperty(s, sample_col)
                continue
            end
            q = s[sample_col]
            mask = .!ismissing.(deposit.primary_mineral) .& (deposit.primary_mineral .== m)
            if sum(mask) == 0
                continue
            end
            lo = deposit[mask, low_col]
            hi = deposit[mask, high_col]
            deposit[mask, base_col] = lo .+ q .* (hi .- lo)
        end

        # Li: one draw per mine-type group
        for mt in ["brine", "brine_DLE", "other_lithium"]
            sample_col = Symbol(param_key * "_Lithium_" * mt)
            if !hasproperty(s, sample_col)
                continue
            end
            q = s[sample_col]
            mask = (
                .!ismissing.(deposit.primary_mineral) .&
                (deposit.primary_mineral .== "Lithium") .&
                (deposit.mine_type_group .== mt)
            )
            if sum(mask) == 0
                continue
            end
            lo = deposit[mask, low_col]
            hi = deposit[mask, high_col]
            deposit[mask, base_col] = lo .+ q .* (hi .- lo)
        end
    end

    # Drop helper columns before passing to optimization
    select!(deposit, Not([:primary_mineral, :mine_type_group]))

    # Recompute water_footprint from updated water and deposit-level aware_cf
    deposit[!, :water_footprint] = deposit[!, :aware_cf] .* deposit[!, :water]
    # -------------------------------------------------------------------------
    # 4. DESALINATION COST SCALAR
    #
    #   Sampled as a multiplier ∈ [0.25, 1.5] relative to the base cost.
    #   Pass the returned value as the keyword argument to runOptimization().
    # -------------------------------------------------------------------------

    cost_water_des = s.desal_cost

    # -------------------------------------------------------------------------
    # 5. RETURN
    # -------------------------------------------------------------------------

    println("  Demand rows:   $(nrow(demand))  ($(minimum(demand.Year))–$(maximum(demand.Year)))")
    println("  Deposit rows:  $(nrow(deposit))")
    println("  Desal cost:    $(round(cost_water_des, digits=1)) USD/m3")
    println("  Fish threshold: $(round(s.fish_threshold, digits=1))")

    if save_deposit
        path = "Parameters/Sample_Runs/deposit_sample_$(lpad(sample_index, 3, '0')).csv"
        CSV.write(path, deposit)
    end

    return demand, deposit, cost_water_des, s.fish_threshold
end