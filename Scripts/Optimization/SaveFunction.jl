# Common function to abstract code do to multiple results savings
using JuMP, DataFrames, CSV

function save_results_from_model!(
    sr_model::JuMP.Model;
    sr_saveFolder::AbstractString,
    sr_Optname::AbstractString,
    sr_ids::AbstractVector,
    sr_names::AbstractVector{<:AbstractString},
)
    outdir = joinpath("Results", "Optimization", sr_saveFolder)

    # Get references of variables and expressions inside the model
    x = sr_model[:x]
    y = sr_model[:y]
    w = sr_model[:w]
    z_cu = sr_model[:z_cu]
    z_ni = sr_model[:z_ni]
    z_co = sr_model[:z_co]
    z_li = sr_model[:z_li]
    w_des = sr_model[:w_des]

    cost_expr = sr_model[:cost_expr] #in million USD
    water_expr = sr_model[:water_expr] # in million m3
    water_impact_expr = sr_model[:water_impact_expr] # in million m3 world-eq
    slack_cost_expr = sr_model[:slack_cost_expr] # in million USD
    mines_opened_expr = sr_model[:mines_opened_expr] # in number of mines opened

    x_values = value.(x)
    y_values = value.(y)
    w_values = value.(w)
    w_des_values = value.(w_des)
    d_size, t_size = size(x_values)

    years = 2025:(2024 + t_size)

    # --- Base ---
    df_base = DataFrame(;
        Name=repeat(sr_names; outer=t_size),
        ID=repeat(sr_ids; outer=t_size),
        t=repeat(years; inner=d_size),
        ktons_extracted=vec(x_values),
        capacity_added_ktpa=vec(y_values),
        mine_opened=vec(w_values),
        water_desalinated_million_m3=vec(w_des_values),
    )
    CSV.write(joinpath(outdir, sr_Optname * ".csv"), df_base)

    # --- Slack ---
    df_slack = DataFrame(;
        variable="demand_unmet",
        t=years,
        slack_cu=value.(z_cu),
        slack_ni=value.(z_ni),
        slack_co=value.(z_co),
        slack_li=value.(z_li),
    )
    CSV.write(joinpath(outdir, sr_Optname * "_Slack.csv"), df_slack)

    cost = value(cost_expr)
    water = value(water_expr)
    water_impact = value(water_impact_expr)
    slack_cost = value(slack_cost_expr)
    mines_opened = value(mines_opened_expr)

    df_metrics = DataFrame(;
        Parameter=["Cost", "Slack cost", "Additional Mine Openings", "Water", "Water impact"],
        Value=[cost, slack_cost, mines_opened, water, water_impact],
        Units=["Million USD", "Million USD", "Number of mines", "Million m3", "Million m3 world-eq"],
    )
    CSV.write(joinpath(outdir, sr_Optname * "_Metrics.csv"), df_metrics)

    return nothing
end