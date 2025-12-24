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
function runOptimization(demand,deposit,saveFolder,discount_rate = 0.07,bigM_cost = 100000*5.323/1e3,hyperbolic=false)
    
    d_size = size(deposit, 1) 
    t_size = size(demand, 1)
    
    # Extract necessary columns
    # Demand in ktons
    demand = demand[!, :Demand]
    # Name
    deposit_name = deposit[!, :Deposit_Name]
    # Reserves
    reserve = deposit[!, :reserve]
    resource_demostrated = deposit[!, :resource_demostrated]
    resource_inferred = deposit[!, :resource_inferred]
    # Dynamics
    prod_rate2022 = deposit[!, :prod_rate2022]
    prod_rate2023 = deposit[!, :prod_rate2023]
    prod_rate2025 = deposit[!, :prod_rate2025]
    prod_rate2030 = deposit[!, :prod_rate2030]
    
    max_prod_rate = deposit[!, :max_prod_rate]
    max_ramp_up = deposit[!, :max_ramp_up]
    min_prod_rate = max_prod_rate ./ 4 
    # Costs, all in million USD
    cost_extraction1 = deposit[!, :cost1] ./ 1e3 # divide by 1e6 to million USD, multiply by 1e3 to get to kton
    cost_extraction2 = deposit[!, :cost2] ./ 1e3
    cost_extraction3 = deposit[!, :cost3] ./ 1e3
    cost_expansion = deposit[!, :cost_expansion] ./ 1e3 # same as extraction
    cost_opening = deposit[!, :cost_opening] ./ 1e6 # million usd
    # Non Monetary Factors
    edb = deposit[!, :edb] # 0 to 100, 0 is better and 100 is worse
    
    # Water
    water1 = deposit[!, :water1]
    water2 = deposit[!, :water2]
    water3 = deposit[!, :water3]
    # Water stress coefficient
    waterRisk_coef = deposit[!, :waterRisk_coef]

    # Water stress [availability]
    ratio_demand_avai = deposit[!, :ratio_demand_avai]

    # Set big M values
    bigM_extract = maximum(max_prod_rate)
    
    # Planned capacity over time, towards 2030
    prod_rate = zeros(d_size, t_size)
    
    # Fill the matrix for the specific years
    prod_rate[:, 1] .= prod_rate2022          # Year 2022
    prod_rate[:, 2] .= prod_rate2023          # Year 2023
    prod_rate[:, 4] .= prod_rate2025          # Year 2025
    prod_rate[:, 9:end] .= prod_rate2030      # Years 2030 to 2070
    
    # Linear interpolation for 2024 (index 3)
    prod_rate[:, 3] .= (prod_rate2023 .+ prod_rate2025) ./ 2
    
    # Linear interpolation for 2026 to 2029 (indices 5 to 8)
    for i in 1:4
        t = i / 5
        prod_rate[:, 4 + i] .= (1 - t) .* prod_rate2025 .+ t .* prod_rate2030
    end
    
    # Discount rates for costs
    if hyperbolic
        discounter = 1 .+ discount_rate .*(0:size(demand, 1) - 1) # hyperbolic discount rate
    else
        discounter = (1 .+ discount_rate) .^(0:size(demand, 1) - 1)
    end

    cost_extraction1 = cost_extraction1 .* (1 ./ discounter')
    cost_extraction2 = cost_extraction2 .* (1 ./ discounter')
    cost_extraction3 = cost_extraction3 .* (1 ./ discounter')
    cost_opening = cost_opening .* (1 ./ discounter')
    cost_expansion = cost_expansion .* (1 ./ discounter')
    
    # Salvage or terminal values for infrastructure development
    mine_life = 15
    fraction_notRecovered = 0.2 # cost not reduced

    remaining_life = mine_life .- (2050 .- 2022:2050)
    frac = max.(remaining_life ./ mine_life, 0.0)
    # Linear reduction in the CAPEX to account for salvage values
    salvage_credit_open = (1 .- fraction_notRecovered) .* cost_opening .* reshape(frac, 1, :)
    cost_opening = cost_opening .- salvage_credit_open
    # Same for expansion
    salvage_cap =  (1 .- fraction_notRecovered) .* cost_expansion .* reshape(frac, 1, :)
    cost_expansion = cost_expansion .- salvage_cap

    # Avoid expansion of certain mines with no info
    status_delay = deposit[!, :Status_Delay]    
    status = deposit[!, :Status]
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
    @variable(model, x2[1:d_size, 1:t_size] >= 0)  # Stepwise cost
    @variable(model, x3[1:d_size, 1:t_size] >= 0) 
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
    @objective(model, Min, sum(cost_extraction1[d, t] * x[d, t] +
    cost_extraction2[d, t] * x2[d, t] +  # Stepwise cost
    cost_extraction3[d, t] * x3[d, t] + 
    cost_expansion[d, t] * y[d, t] +
    cost_opening[d, t] * w[d, t] for d in 1:d_size, t in 1:t_size) +
    sum(bigM_cost[t] * z[t] for t in 1:t_size))
    
    # Constraints
    # Extraction less than available production capacity
    @constraint(model, c1[d in 1:d_size, t in 1:t_size], x[d, t] + x2[d, t] +x3[d,t] <= sum(y[d, t1] for t1 in 1:t) + prod_rate[d,t])
    # Met demand
    @constraint(model, c2[t in 1:t_size], sum(x[d, t] + x2[d, t] +x3[d,t] for d in 1:d_size) + z[t] >= demand[t]+(t > 1 ? z[t-1] : 0))
    # Max depletion of reserves, 3 stages
    @constraint(model, c3[d in 1:d_size], sum(x[d, t] for t in 1:t_size) <= reserve[d])
    @constraint(model, c4[d in 1:d_size], sum(x2[d, t] for t in 1:t_size) <= resource_demostrated[d])
    @constraint(model, c5[d in 1:d_size], sum(x3[d, t] for t in 1:t_size) <= resource_inferred[d])        
    # Max production rate only on open mines
    @constraint(model, c6[d in 1:d_size, t in 1:t_size], sum(y[d, t1] for t1 in 1:t) + prod_rate[d,t] <= sum(w[d, t1] for t1 in 1:t) * max_prod_rate[d])
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
    x2_values = [value(x2[d,t]) for d in 1:d_size, t in 1:t_size]
    x3_values = [value(x3[d,t]) for d in 1:d_size, t in 1:t_size]
    y_values  = [value(y[d,t])  for d in 1:d_size, t in 1:t_size]
    w_values  = [value(w[d,t])  for d in 1:d_size, t in 1:t_size]
    z_values  = [value(z[t])    for t in 1:t_size]

   # start year 
    years = 2022:(2021 + t_size)

    df_results = DataFrame(
        d = repeat(deposit_name, outer = t_size),
        t = repeat(years,inner = d_size),
        tons_extracted1 = vec(x_values),
        tons_extracted2 = vec(x2_values),
        tons_extracted3 = vec(x3_values),
        capacity_added  = vec(y_values),
        mine_opened     = vec(w_values)
    )

    # CSV save
    url_results = "Results/Optimization/" * saveFolder * "/Base.csv"
    CSV.write(url_results, df_results)

    # Slack
    df_z = DataFrame(variable="demand_unmet",t = 2022:(t_size+2021),value = vec(z_values))
    url_file = "Results/Optimization/"* saveFolder *"/Base_slack.csv"
    CSV.write(url_file, df_z)

    # Save cost and water
    cost = sum(cost_extraction1[d,t] * x_values[d,t] +
            cost_extraction2[d,t] * x2_values[d,t] +
            cost_extraction3[d,t] * x3_values[d,t] +
            cost_expansion[d,t]  * y_values[d,t] +
            cost_opening[d,t]    * w_values[d,t] for d = 1:d_size, t = 1:t_size)+
            sum(bigM_cost[t] * z_values[t] for t in 1:t_size)

    water = sum(water1[d] * x_values[d, t] * waterRisk_coef[d] +
        water2[d] * x2_values[d, t] * waterRisk_coef[d] +  
        water3[d] * x3_values[d, t] * waterRisk_coef[d]  for d in 1:d_size, t in 1:t_size)

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
    @objective(model, Min,sum(water1[d] * x[d, t] * waterRisk_coef[d] +
        water2[d] * x2[d, t] * waterRisk_coef[d] +  
        water3[d] * x3[d, t] * waterRisk_coef[d]  for d in 1:d_size, t in 1:t_size))

    # cost degradation
    for epsilon_cost in [0.1, 0.05, 0.03, 0.01]
        # Cost constraint
        @constraint(model,
            sum(cost_extraction1[d,t] * x[d,t] +
                cost_extraction2[d,t] * x2[d,t] +
                cost_extraction3[d,t] * x3[d,t] +
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
        x2_values = [value(x2[d,t]) for d in 1:d_size, t in 1:t_size]
        x3_values = [value(x3[d,t]) for d in 1:d_size, t in 1:t_size]
        y_values  = [value(y[d,t])  for d in 1:d_size, t in 1:t_size]
        w_values  = [value(w[d,t])  for d in 1:d_size, t in 1:t_size]
        z_values  = [value(z[t])    for t in 1:t_size]

    # start year 
        years = 2022:(2021 + t_size)

        df_results = DataFrame(
            d = repeat(deposit_name, outer = t_size),
            t = repeat(years,inner = d_size),
            tons_extracted1 = vec(x_values),
            tons_extracted2 = vec(x2_values),
            tons_extracted3 = vec(x3_values),
            capacity_added  = vec(y_values),
            mine_opened     = vec(w_values)
        )

        # CSV save
        url_results = "Results/Optimization/" * saveFolder * "/Water" * string(round(Int,epsilon_cost* 100)) * ".csv"
        CSV.write(url_results, df_results)

        # Slack
        df_z = DataFrame(variable="demand_unmet",t = 2022:(t_size+2021),value = vec(z_values))
        url_file = "Results/Optimization/"* saveFolder *"/Water"* string(round(Int,epsilon_cost* 100))*"_slack.csv"
        CSV.write(url_file, df_z)

        # Save cost and water
        cost = sum(cost_extraction1[d,t] * x_values[d,t] +
                cost_extraction2[d,t] * x2_values[d,t] +
                cost_extraction3[d,t] * x3_values[d,t] +
                cost_expansion[d,t]  * y_values[d,t] +
                cost_opening[d,t]    * w_values[d,t] for d = 1:d_size, t = 1:t_size)+
                sum(bigM_cost[t] * z_values[t] for t in 1:t_size)

        water = sum(water1[d] * x_values[d, t] * waterRisk_coef[d] +
            water2[d] * x2_values[d, t] * waterRisk_coef[d] +  
            water3[d] * x3_values[d, t] * waterRisk_coef[d]  for d in 1:d_size, t in 1:t_size)

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