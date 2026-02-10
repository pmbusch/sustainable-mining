# Common function to linearize a problem, solve it, extract the duals (shadow prices) and save them

using JuMP, Gurobi, DataFrames, CSV

function save_shadow_prices_from_model!(
    model::JuMP.Model;
    sr_saveFolder::AbstractString,
    sr_Optname::AbstractString,
    sr_basins,              # e.g., basins vector/iterator of basin IDs
)
    outdir = joinpath("Results", "Optimization", sr_saveFolder)
    print(outdir)

    w = model[:w] # Binary variables
    c7 = model[:c7] # Basin capacity constraints
    c1_cu = model[:c1_cu] # Mineral demand constraint
    c1_ni = model[:c1_ni]
    c1_co = model[:c1_co]
    c1_li = model[:c1_li]

    d_size, t_size = size(w)
    years = 2025:(2024 + t_size)

    m_dual, ref = copy_model(model)
    relax_integrality(m_dual)

    # Fix binaries to original solution (on the copied model)
    for idx in eachindex(w)
        fix(ref[w[idx]], value(w[idx]); force=true)
    end

    set_optimizer(m_dual, Gurobi.Optimizer)
    optimize!(m_dual)

    # Basin shadow prices
    df_basin = DataFrame(; Basin_ID=Int[], t=Int[], shadow=Float64[])
    for b in sr_basins, t in 1:t_size
        con = ref[c7[b, t]]
        push!(df_basin, (Int(b), Int(t) + 2024, shadow_price(con)))
    end
    CSV.write(joinpath(outdir, "SP_Basin" * sr_Optname * ".csv"), df_basin)

    # Demand shadow prices
    df_dem = DataFrame(;
        t=Int[], sp_demand_cu=Float64[], sp_demand_ni=Float64[], sp_demand_co=Float64[], sp_demand_li=Float64[]
    )
    for t in 1:t_size
        push!(
            df_dem,
            (
                Int(t) + 2024,
                shadow_price(ref[c1_cu[t]]),
                shadow_price(ref[c1_ni[t]]),
                shadow_price(ref[c1_co[t]]),
                shadow_price(ref[c1_li[t]]),
            ),
        )
    end
    CSV.write(joinpath(outdir, "SP_Demand" * sr_Optname * ".csv"), df_dem)

    return nothing
end
