# Optimization with Methods to Generate Alternatives

using CSV
using DataFrames
using JuMP
using Gurobi
using LinearAlgebra

# Optimization Run Function
# Inputs:
# - Demand 
# - Deposits parameters
# - Save folder 
# - discount rate (default 7%)
# - cost to not met demand (slack) - historic high price: 68000 USD per LCE
# - use an hyperbolic discount rate (default is false)
function runOptimization(demand,deposit,saveFolder;discount_rate = 0.07,bigM_cost = 100000*5.323/1e3,hyperbolic=false)
    
    d_size = size(deposit, 1) 
    t_size = size(demand, 1)
    
    # Extract necessary columns
    # Demand in ktons
    demand = demand[!, :Demand] # already in ktons
    # Name
    deposit_name = deposit[!, :Name]
    # Reserves
    reserve = deposit[!, :reserves] ./ 1e3 # to ktons
    resources = deposit[!, :resources] .* 0.8 ./ 1e3 # to ktons, 80 % recovery rate
    # Dynamics
    cap2025 = deposit[!, :cap2025] ./ 1e3 # to ktons
    
    max_prod_rate = resources .* 0.04 # 4% depletion rate, kton per year
    max_prod_rate = max.(max_prod_rate, cap2025) # some small mines have really high depletion rate
    max_ramp_up = max_prod_rate ./ 4 # 4 years ramp up
    min_prod_rate = max_prod_rate ./ 4 
    
    # Costs, convert all to million USD
    cost_extraction = deposit[!, :OPEX] ./ 1e3 # divide by 1e6 to million USD, multiply by 1e3 to get to kton
    cost_expansion = deposit[!, :CAPEX_exp] ./ 1e3 # same as extraction
    cost_opening = deposit[!, :CAPEX_opening] # million usd
    
    # Water footprint
    water_footprint = deposit[!, :water_footprint] ./ 1e3 # divide by 1e6 to million m3, multiply by 1e3 to get to kton
    
    # Set big M values
    bigM_extract = maximum(max_prod_rate)
    
    # Base capacity 
    prod_rate = cap2025
    
    # Discount rates for costs
    if hyperbolic
        discounter = 1 .+ discount_rate .*(0:size(demand, 1) - 1) # hyperbolic discount rate
    else
        discounter = (1 .+ discount_rate) .^(0:size(demand, 1) - 1)
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
    salvage_cap =  (1 .- fraction_notRecovered) .* cost_expansion .* reshape(frac, 1, :)
    cost_expansion = cost_expansion .- salvage_cap

    # Avoid expansion of certain mines with no info
    status = deposit[!, :status]
    delay_years = deposit[!, :delay_years] # delay in expansion
    for i in 1:size(cost_expansion, 1)
        if delay_years[i] > 0
            cost_expansion[i, 1:delay_years[i]] .= 1e6 # Not possible to expand, given the delay in years   
        end
    end
    
    # Big M effect, should be reduced towards the future?
    bigM_cost = bigM_cost .* (1 ./ discounter') 
    
    # Create optimization model
    model = Model(Gurobi.Optimizer)
    
    # Decision variables
    @variable(model, x[1:d_size, 1:t_size] >= 0)  # Extraction
    @variable(model, y[1:d_size, 1:t_size] >= 0)  # Additional capacity
    @variable(model, w[1:d_size, 1:t_size], Bin)  # Open or not
    @variable(model, z[1:t_size] >= 0)  # Slack to match balance
    
    # Fix deposits already open 
    for d in 1:d_size
        if status[d] == "Producing"
            fix(w[d,1], 1; force=true) # open at year 1
        end
        if status[d] == "Construction"
            fix(w[d,3], 1; force=true)
        end
    end


    # Objective function
    @objective(model, Min, sum(cost_extraction[d, t] * x[d, t] +
    cost_expansion[d, t] * y[d, t] +
    cost_opening[d, t] * w[d, t] for d in 1:d_size, t in 1:t_size) +
    sum(bigM_cost[t] * z[t] for t in 1:t_size))
    
    # Constraints
    # Extraction less than available production capacity
    @constraint(model, c1[d in 1:d_size, t in 1:t_size], x[d, t] <= sum(y[d, t1] for t1 in 1:t) + cap2025[d])
    # Met demand
    @constraint(model, c2[t in 1:t_size], sum(x[d, t] for d in 1:d_size) + z[t] >= demand[t]+(t > 1 ? z[t-1] : 0))
    # Max depletion of resources
    @constraint(model, c3[d in 1:d_size], sum(x[d, t] for t in 1:t_size) <= resources[d])
    # Max production rate only on open mines
    @constraint(model, c6[d in 1:d_size, t in 1:t_size], sum(y[d, t1] for t1 in 1:t) + prod_rate[d] <= sum(w[d, t1] for t1 in 1:t) * max_prod_rate[d])
    # Open mine only once
    @constraint(model, c7[d in 1:d_size], sum(w[d, t] for t in 1:t_size) <= 1)
    # Max Ramp up
    @constraint(model, c8[d in 1:d_size, t in 1:t_size], y[d, t] <= max_ramp_up[d])
    
    # Save results prior to MGA for water
    optimize!(model)
    
    # create directory if not there
    if !isdir("Results/Optimization/"* saveFolder)
        mkpath("Results/Optimization/"* saveFolder)
    end
    
    # Save optimization parameters
    url_file = "Results/Optimization/"* saveFolder *"/OptimizationInputs.csv"
    inputs_text = DataFrame([
        ("Discount rate", discount_rate),
        ("Slack cost", bigM_cost[1]),
        ("Time vector size", t_size),
        ("Deposit vector size", d_size)
        ], [:Parameter, :Value])
    CSV.write(url_file, inputs_text)
    
    # Save decision variables
    # vectorize
    x_values  = [value(x[d,t])  for d in 1:d_size, t in 1:t_size]
    y_values  = [value(y[d,t])  for d in 1:d_size, t in 1:t_size]
    w_values  = [value(w[d,t])  for d in 1:d_size, t in 1:t_size]
    z_values  = [value(z[t])    for t in 1:t_size]

   # start year 
    years = 2025:(2024 + t_size)

    df_results = DataFrame(
        d = repeat(deposit_name, outer = t_size),
        t = repeat(years,inner = d_size),
        ktons_extracted = vec(x_values),
        capacity_added  = vec(y_values),
        mine_opened     = vec(w_values)
    )

    # CSV save
    url_results = "Results/Optimization/" * saveFolder * "/Base.csv"
    CSV.write(url_results, df_results)

    # Slack
    df_z = DataFrame(variable="demand_unmet",t = 2025:(t_size+2024),value = vec(z_values))
    url_file = "Results/Optimization/"* saveFolder *"/Base_slack.csv"
    CSV.write(url_file, df_z)

    # Save cost and water
    cost = sum(cost_extraction[d,t] * x_values[d,t] +
            cost_expansion[d,t]  * y_values[d,t] +
            cost_opening[d,t]    * w_values[d,t] for d = 1:d_size, t = 1:t_size)+
            sum(bigM_cost[t] * z_values[t] for t in 1:t_size)

    water = sum(water_footprint[d] * x_values[d, t] for d in 1:d_size, t in 1:t_size)

    mines_opened = sum(w_values[d,t] for d = 1:d_size, t = 1:t_size)
    url_file = "Results/Optimization/"* saveFolder *"/Metrics.csv"
    inputs_text = DataFrame([
        ("Cost", cost),
        ("Water", water),
        ("Openings", mines_opened),
        ], [:Parameter, :Value])
    CSV.write(url_file, inputs_text)

    # MGA water 
    opt_val = objective_value(model)
    # new objective
    @objective(model, Min,sum(water_footprint[d] * x[d, t] for d in 1:d_size, t in 1:t_size))

    # cost degradation
    for epsilon_cost in [0.1, 0.05, 0.03, 0.01]
        # Cost constraint
        @constraint(model,
            sum(cost_extraction[d,t] * x[d,t] +
                cost_expansion[d,t]  * y[d,t] +
                cost_opening[d,t]    * w[d,t]
                for d = 1:d_size, t = 1:t_size)+
                    sum(bigM_cost[t] * z[t] for t in 1:t_size) <= 
                    (1 + epsilon_cost) * opt_val
        )
 
        optimize!(model)

        # Save decision variables
        # vectorize
        x_values  = [value(x[d,t])  for d in 1:d_size, t in 1:t_size]
        y_values  = [value(y[d,t])  for d in 1:d_size, t in 1:t_size]
        w_values  = [value(w[d,t])  for d in 1:d_size, t in 1:t_size]
        z_values  = [value(z[t])    for t in 1:t_size]

    # start year 
        years = 2025:(2024 + t_size)

        df_results = DataFrame(
            d = repeat(deposit_name, outer = t_size),
            t = repeat(years,inner = d_size),
            ktons_extracted = vec(x_values),
            capacity_added  = vec(y_values),
            mine_opened     = vec(w_values)
        )

        # CSV save
        url_results = "Results/Optimization/" * saveFolder * "/Water" * string(round(Int,epsilon_cost* 100)) * ".csv"
        CSV.write(url_results, df_results)

        # Slack
        df_z = DataFrame(variable="demand_unmet",t = 2025:(t_size+2024),value = vec(z_values))
        url_file = "Results/Optimization/"* saveFolder *"/Water"* string(round(Int,epsilon_cost* 100))*"_slack.csv"
        CSV.write(url_file, df_z)

        # Save cost and water
        cost = sum(cost_extraction[d,t] * x_values[d,t] +
                cost_expansion[d,t]  * y_values[d,t] +
                cost_opening[d,t]    * w_values[d,t] for d = 1:d_size, t = 1:t_size)+
                sum(bigM_cost[t] * z_values[t] for t in 1:t_size)

        water = sum(water_footprint[d] * x_values[d, t] for d in 1:d_size, t in 1:t_size)

        mines_opened = sum(w_values[d,t] for d = 1:d_size, t = 1:t_size)
        url_file = "Results/Optimization/"* saveFolder *"/Metrics"* string(round(Int,epsilon_cost*100))*".csv"
        inputs_text = DataFrame([
            ("Cost", cost),
            ("Water", water),
            ("Openings", mines_opened),
            ], [:Parameter, :Value])
        CSV.write(url_file, inputs_text)
    end 
end