# Optimization with multiobjective function to explore water impact reductions per small increases in total cost
# Method builds paretto curves
# Also akin to MGA: Model to Generate Alternatives

using CSV
using DataFrames
using JuMP
using Gurobi
using LinearAlgebra
using Statistics

include("SaveFunction.jl")  # load save function
include("ShadowPriceFunction.jl")  # load shadow price function
include("ClimateScenarioFunction.jl")  # load climate scenario function

# Optimization Run Function
# Inputs:
# - Demand 
# - Deposits parameters
# - Save folder 
# - discount rate (default 7%)
# - use an hyperbolic discount rate (default is false)
# - run multiobjective to explore solutions with some cost degradation
# - climate scenario to use for water constraints (availability) and freshwater impacts (default "none")
# - cost to not met demand (slack) - historic high price for each mineral by 50% more (only lithium price is orignally in per tons LCE)
function runOptimization(
    demand,
    deposit,
    saveFolder;
    discount_rate=0.07,
    hyperbolic=false,
    multiobjective=true,
    climate_scenario="none",
    bigM_cost_Li=68000 * 1.5 * 5.323 / 1e3,
    bigM_cost_Cu=14000 * 1.5 / 1e3,
    bigM_cost_Ni=48000 * 1.5 / 1e3,
    bigM_cost_Co=82000 * 1.5 / 1e3,
)
    d_size = size(deposit, 1)
    t_size = size(demand, 1)

    # Extract necessary columns
    # Demand in ktons
    demand_cu = demand[!, :Copper] # already in ktons
    demand_ni = demand[!, :Nickel]
    demand_co = demand[!, :Cobalt]
    demand_li = demand[!, :Lithium] # in ktons Li

    # Name
    deposit_name = deposit[!, :Name]
    deposit_id = deposit[!, :ID]

    # Recovery rates
    recovery_rate_cu = deposit[!, :recovery_rate_Copper]
    recovery_rate_ni = deposit[!, :recovery_rate_Nickel]
    recovery_rate_co = deposit[!, :recovery_rate_Cobalt]
    recovery_rate_li = deposit[!, :recovery_rate_Lithium]

    # Reserves and resources by mineral
    resources_cu = deposit[!, :resources_Copper] .* recovery_rate_cu ./ 1e3 # to ktons
    grade_cu = deposit[!, :grade_resource_Copper] ./ 100 # to %
    resources_ni = deposit[!, :resources_Nickel] .* recovery_rate_ni ./ 1e3 # to ktons
    grade_ni = deposit[!, :grade_resource_Nickel] ./ 100 # to %
    resources_co = deposit[!, :resources_Cobalt] .* recovery_rate_co ./ 1e3 # to ktons
    grade_co = deposit[!, :grade_resource_Cobalt] ./ 100 # to %
    resources_li = deposit[!, :resources_Lithium] .* recovery_rate_li ./ 1e3 # to ktons 
    grade_li = deposit[!, :grade_resource_Lithium] ./ 100 # to

    resources_ore = deposit[!, :resources_ore] ./ 1e3 # to ktons

    # Dynamics
    cap2025 = deposit[!, :cap2025] ./ 1e3 # to ktons

    max_depletion_rate = deposit[!, :max_depletion_rate] # to %
    max_prod_rate = resources_ore .* max_depletion_rate # 4% depletion rate, kton per year (2% for brine evaporation)
    max_prod_rate = max.(max_prod_rate, cap2025) # some small mines have really high depletion rate
    max_ramp_up = max_prod_rate ./ 4 # 4 years ramp up
    min_prod_rate = max_prod_rate ./ 4

    # Costs, convert all to million USD
    cost_extraction = deposit[!, :OPEX_ore] ./ 1e3 # divide by 1e6 to million USD, multiply by 1e3 to get to kton per ore processed
    cost_expansion = deposit[!, :CAPEX_exp] ./ 1e3 # same as extraction
    cost_opening = deposit[!, :CAPEX_opening] # million usd

    # Allocate cost only for battery minerals: Li, Cu, Ni, Cobalt
    share_NiCoCu = deposit[!, :share_NiCoCu]
    cost_extraction = cost_extraction .* share_NiCoCu
    cost_expansion = cost_expansion .* share_NiCoCu
    cost_opening = cost_opening .* share_NiCoCu

    # Water parameters
    water_cons = deposit[!, :water] ./ 1e3 # water consumption, converted to million m3 per kton ore processed

    # Load time-indexed water_footprint[d,t] and aware_available[basin][t] from climate scenario
    water_footprint, aware_available = load_climate_scenario(deposit, depositAll, climate_scenario, d_size, t_size)

    # Map of contained deposits (including in upstream basins) for each basin
    bd = CSV.read("Parameters/basin_to_deposits_upstream.csv", DataFrame)
    # --- Map deposit_id -> row index in `deposit` (so it matches x[i,t]) ---
    id2idx = Dict(deposit.ID[i] => i for i in 1:nrow(deposit))

    # --- Basin -> Vector{Int} of deposit row indices (incl. upstream) ---
    deposits_in_basin = Dict(Int(first(v.Basin_ID)) => [id2idx[id] for id in v.ID] for v in groupby(bd, :Basin_ID))

    aware_basins = unique([k[1] for k in keys(aware_available)])
    basins = sort(intersect(collect(keys(deposits_in_basin)), aware_basins))

    # Set big M values
    bigM_extract = maximum(max_prod_rate)

    # Base capacity 
    prod_rate = cap2025

    # Discount rates for costs
    if hyperbolic
        discounter = 1 .+ discount_rate .* (0:(size(demand, 1) - 1)) # hyperbolic discount rate
    else
        discounter = (1 .+ discount_rate) .^ (0:(size(demand, 1) - 1))
    end

    cost_extraction = cost_extraction .* (1 ./ discounter')
    cost_opening = cost_opening .* (1 ./ discounter')
    cost_expansion = cost_expansion .* (1 ./ discounter')

    # Salvage or terminal values for infrastructure development
    mine_life = 15.0
    fraction_notRecovered = 0.2 # cost not recoverd for terminal life

    years = 2025:2050
    years_to_end = 2050 .- years
    remaining_life = mine_life .- years_to_end
    frac = clamp.(remaining_life ./ mine_life, 0.0, 1.0)
    # Linear reduction in the CAPEX to account for salvage values
    salvage_credit_open = (1 .- fraction_notRecovered) .* cost_opening .* reshape(frac, 1, :)
    cost_opening = cost_opening .- salvage_credit_open
    # Same for expansion
    salvage_cap = (1 .- fraction_notRecovered) .* cost_expansion .* reshape(frac, 1, :)
    cost_expansion = cost_expansion .- salvage_cap

    # Avoid expansion of certain mines with no info
    status = deposit[!, :status]
    delay_years = deposit[!, :delay_years] # delay in expansion
    for i in 1:size(cost_expansion, 1)
        if delay_years[i] > 0
            cost_expansion[i, 1:delay_years[i]] .= 1e9 # Not possible to expand, given the delay in years   
        end
    end

    # Big M effect, should be reduced towards the future?
    bigM_cost_Cu = bigM_cost_Cu .* (1 ./ discounter')
    bigM_cost_Ni = bigM_cost_Ni .* (1 ./ discounter')
    bigM_cost_Co = bigM_cost_Co .* (1 ./ discounter')
    bigM_cost_Li = bigM_cost_Li .* (1 ./ discounter')

    # Create optimization model
    model = Model(Gurobi.Optimizer)

    # Decision variables
    @variable(model, x[1:d_size, 1:t_size] >= 0)  # Extraction
    @variable(model, y[1:d_size, 1:t_size] >= 0)  # Additional capacity
    @variable(model, w[1:d_size, 1:t_size], Bin)  # Open or not
    @variable(model, z_cu[1:t_size] >= 0)  # Slack to match balance
    @variable(model, z_ni[1:t_size] >= 0)  # Slack to match balance
    @variable(model, z_co[1:t_size] >= 0)  # Slack to match balance
    @variable(model, z_li[1:t_size] >= 0)  # Slack to match balance

    # Fix deposits already open 
    for d in 1:d_size
        if status[d] == "Production"
            fix(w[d, 1], 1; force=true) # open at year 1
        end
        # DOES NOT WORK FOR YEAR 3 AS CURRENTLY CONSTRAINT C4 FAILS (prod_rate and max_prod_rate are not time indexed)
        # if status[d] == "Development"
        #     fix(w[d, 3], 1; force=true) # open at year 3
        # end
    end

    mines_alreadyOpen = count(s -> s == "Production" || s == "Development", status)

    # Define expression called multiple times in the function (abstraction)
    # Cost expression for objective function
    @expression(
        model,
        cost_expr,
        sum(
            cost_extraction[d, t] * x[d, t] + cost_expansion[d, t] * y[d, t] + cost_opening[d, t] * w[d, t] for
            d in 1:d_size, t in 1:t_size
        ) + sum(
            bigM_cost_Cu[t] * z_cu[t] +
            bigM_cost_Ni[t] * z_ni[t] +
            bigM_cost_Co[t] * z_co[t] +
            bigM_cost_Li[t] * z_li[t] for t in 1:t_size
        )
    )
    # Water consumption and impact
    @expression(model, water_expr, sum(water_cons[d] * x[d, t] for d in 1:d_size, t in 1:t_size))
    @expression(model, water_impact_expr, sum(water_footprint[d, t] * x[d, t] for d in 1:d_size, t in 1:t_size))
    # Slack cost
    @expression(
        model,
        slack_cost_expr,
        sum(
            bigM_cost_Cu[t] * z_cu[t] +
            bigM_cost_Ni[t] * z_ni[t] +
            bigM_cost_Co[t] * z_co[t] +
            bigM_cost_Li[t] * z_li[t] for t in 1:t_size
        )
    )
    # Mines opeded
    @expression(model, mines_opened_expr, sum(w[d, t] for d in 1:d_size, t in 1:t_size)) - mines_alreadyOpen

    # Objective function
    @objective(model, Min, cost_expr)

    # Constraints
    # Met demand
    @constraint(
        model,
        c1_cu[t in 1:t_size],
        sum(x[d, t] * grade_cu[d] * recovery_rate_cu[d] for d in 1:d_size) + z_cu[t] >=
            demand_cu[t] + (t > 1 ? z_cu[t - 1] : 0)
    )
    @constraint(
        model,
        c1_ni[t in 1:t_size],
        sum(x[d, t] * grade_ni[d] * recovery_rate_ni[d] for d in 1:d_size) + z_ni[t] >=
            demand_ni[t] + (t > 1 ? z_ni[t - 1] : 0)
    )
    @constraint(
        model,
        c1_co[t in 1:t_size],
        sum(x[d, t] * grade_co[d] * recovery_rate_co[d] for d in 1:d_size) + z_co[t] >=
            demand_co[t] + (t > 1 ? z_co[t - 1] : 0)
    )
    @constraint(
        model,
        c1_li[t in 1:t_size],
        sum(x[d, t] * grade_li[d] * recovery_rate_li[d] for d in 1:d_size) + z_li[t] >=
            demand_li[t] + (t > 1 ? z_li[t - 1] : 0)
    )
    # Extraction less than available production capacity
    @constraint(model, c2[d in 1:d_size, t in 1:t_size], x[d, t] <= sum(y[d, t1] for t1 in 1:t) + cap2025[d])
    # Max depletion of resources
    @constraint(model, c3_ore[d in 1:d_size], sum(x[d, t] for t in 1:t_size) <= resources_ore[d])
    @constraint(
        model,
        c3_cu[d in 1:d_size],
        sum(x[d, t] * grade_cu[d] * recovery_rate_cu[d] for t in 1:t_size) <= resources_cu[d]
    )
    @constraint(
        model,
        c3_ni[d in 1:d_size],
        sum(x[d, t] * grade_ni[d] * recovery_rate_ni[d] for t in 1:t_size) <= resources_ni[d]
    )
    @constraint(
        model,
        c3_co[d in 1:d_size],
        sum(x[d, t] * grade_co[d] * recovery_rate_co[d] for t in 1:t_size) <= resources_co[d]
    )
    @constraint(
        model,
        c3_li[d in 1:d_size],
        sum(x[d, t] * grade_li[d] * recovery_rate_li[d] for t in 1:t_size) <= resources_li[d]
    )
    # Max production rate only on open mines
    @constraint(
        model,
        c4[d in 1:d_size, t in 1:t_size],
        sum(y[d, t1] for t1 in 1:t) + prod_rate[d] <= sum(w[d, t1] for t1 in 1:t) * max_prod_rate[d]
    )
    # Open mine only once
    @constraint(model, c5[d in 1:d_size], sum(w[d, t] for t in 1:t_size) <= 1)
    # Max Ramp up
    @constraint(model, c6[d in 1:d_size, t in 1:t_size], y[d, t] <= max_ramp_up[d])

    # Solve with no water constraint first
    optimize!(model)

    # create directory if not there
    if !isdir("Results/Optimization/" * saveFolder)
        mkpath("Results/Optimization/" * saveFolder)
    end

    # Save results
    save_results_from_model!(
        model; sr_saveFolder=saveFolder, sr_Optname="NoWaterConstraint", sr_ids=deposit_id, sr_names=deposit_name
    )

    # Water constraint Water available per basin (includes consumption in upstream basins)
    @constraint(
        model,
        c7[b in basins, t in 1:t_size],
        sum(x[i, t] * water_cons[i] for i in deposits_in_basin[b]) <= aware_available[b, t]
    )

    # Save results prior to MGA for water
    optimize!(model)

    # store just for MGA run (if not code breaks)
    z_values_cu = value.(z_cu)
    z_values_ni = value.(z_ni)
    z_values_co = value.(z_co)
    z_values_li = value.(z_li)

    # Save optimization parameters - common for all the runs inside the function loop
    url_file = "Results/Optimization/" * saveFolder * "/OptimizationInputs.csv"
    inputs_text = DataFrame(
        [
            ("Discount rate", discount_rate),
            ("Time vector size", t_size),
            ("Deposit vector size", d_size),
            ("Slack cost Copper", bigM_cost_Cu[1]),
            ("Slack cost Nickel", bigM_cost_Ni[1]),
            ("Slack cost Cobalt", bigM_cost_Co[1]),
            ("Slack cost Lithium", bigM_cost_Li[1]),
        ],
        [:Parameter, :Value],
    )
    CSV.write(url_file, inputs_text)

    save_results_from_model!(
        model; sr_saveFolder=saveFolder, sr_Optname="Base", sr_ids=deposit_id, sr_names=deposit_name
    )

    # Get shadow prices (dual variables)
    save_shadow_prices_from_model!(model; sr_saveFolder=saveFolder, sr_Optname="", sr_basins=basins)

    # Run multiobjective to minimize water impact
    if (multiobjective)
        # MGA water 
        opt_val = objective_value(model)

        # fix slack values (to prevent no meeting further demand)
        for idx in eachindex(z_values_cu)
            fix(z_cu[idx], z_values_cu[idx]; force=true)
            fix(z_ni[idx], z_values_ni[idx]; force=true)
            fix(z_co[idx], z_values_co[idx]; force=true)
            fix(z_li[idx], z_values_li[idx]; force=true)
        end

        # new objective
        @objective(model, Min, sum(water_footprint[d, t] * x[d, t] for d in 1:d_size, t in 1:t_size))

        # constraint 
        cost_con = @constraint(model, cost_expr <= 1.15 * opt_val)  # create once

        # cost degradation
        for epsilon_cost in [0.15, 0.125, 0.1, 0.08, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01, 0.005]
            # for epsilon_cost in [0.1, 0.05, 0.03, 0.01]
            # Cost constraint
            set_normalized_rhs(cost_con, (1 + epsilon_cost) * opt_val) # updated
            optimize!(model)

            save_results_from_model!(
                model;
                sr_saveFolder=saveFolder,
                sr_Optname="Water_Eps$(lpad(string(round(Int, 100 * epsilon_cost)), 2, '0'))",
                sr_ids=deposit_id,
                sr_names=deposit_name,
            )

            # Get shadow prices (dual variables)
            save_shadow_prices_from_model!(
                model;
                sr_saveFolder=saveFolder,
                sr_Optname="Water_Eps$(lpad(string(round(Int, 100 * epsilon_cost)), 2, '0'))",
                sr_basins=basins,
            )
        end
    end
end