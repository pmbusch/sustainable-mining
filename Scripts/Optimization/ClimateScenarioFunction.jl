# Load climate scenario data to create time-indexed water_footprint and aware_available
# If climate_scenario is "none", replicates static values across all time periods
# Otherwise reads from Parameters/WaterScenarios/<climate_scenario>.csv
using CSV, DataFrames, Statistics

function load_climate_scenario(deposit, climate_scenario, d_size, t_size)
    years = 2025:(2024 + t_size)

    if climate_scenario == "none"
        # Replicate static values across time (same as original non-indexed behavior)
        wf_static = deposit[!, :water_footprint] ./ 1e3 #  divide by 1e6 to million m3, multiply by 1e3 to get to kton ore processed
        water_footprint_dt = repeat(wf_static, 1, t_size)

        aa_static = combine(groupby(deposit, :Basin_ID), :aware_available => mean => :aware_available)
        aa_static.aware_available ./= 1e6 # million m3
        aa_static.aware_available .= max.(aa_static.aware_available, 0.0)
        aware_available_dt = Dict((r.Basin_ID, t) => r.aware_available for r in eachrow(aa_static) for t in 1:t_size)

        return water_footprint_dt, aware_available_dt
    end

    # Read climate scenario CSV
    scenario_df = CSV.read(joinpath("Parameters", "WaterScenarios", climate_scenario), DataFrame)

    # Build matrices [d, t]
    water_footprint_dt = zeros(d_size, t_size)
    aware_available_raw = zeros(nrow(scenario_df), t_size)

    # Parse period columns and fill matrices
    wf_cols = [c for c in names(scenario_df) if startswith(c, "water_footprint_")]
    aa_cols = [c for c in names(scenario_df) if startswith(c, "aware_available_")]

    for col in wf_cols
        parts = split(col, "_")
        y_end = parse(Int, parts[end])
        y_start = parse(Int, parts[end - 1])
        for (ti, y) in enumerate(years)
            if y >= y_start && (y < y_end || (y == y_end && y_end == last(years)))
                water_footprint_dt[:, ti] .= scenario_df[!, col] ./ 1e3 # divide by 1e6 to million m3, multiply by 1e3 to get to kton ore processed
            end
        end
    end

    for col in aa_cols
        parts = split(col, "_")
        y_end = parse(Int, parts[end])
        y_start = parse(Int, parts[end - 1])
        for (ti, y) in enumerate(years)
            if y >= y_start && (y < y_end || (y == y_end && y_end == last(years)))
                aware_available_raw[:, ti] .= scenario_df[!, col]
            end
        end
    end

    # Convert to million m3 and floor negatives to zero
    aware_available_raw ./= 1e6
    aware_available_raw .= max.(aware_available_raw, 0.0)

    # Aggregate aware_available by basin (mean across deposits per basin, per time period)
    basin_ids = Int.(scenario_df[!, :Basin_ID])
    unique_basins = unique(basin_ids)
    aware_available_dt = Dict{Tuple{Int,Int},Float64}()
    for b in unique_basins
        mask = basin_ids .== b
        for ti in 1:t_size
            aware_available_dt[(b, ti)] = mean(aware_available_raw[mask, ti])
        end
    end

    return water_footprint_dt, aware_available_dt
end
