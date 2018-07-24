using FileIO, DataFrames, Lazy, DataFramesMeta, Optim, CSVFiles, Distributions;
gr4jul = DataFrame(load("data/gr4julia.csv"))

team1 = gr4jul[:HomeTeam]
team2 = gr4jul[:AwayTeam]
goals = gr4jul[:hg]

# create a likelihood function
function create_negloglik_fn(goals, attack_order, defense_order)
    return function(haad)
        -sum(logpdf.(Poisson.(max.(haad[1:16][attack_order] .- haad[17:32][defense_order],0)),goals))
    end
end

#function ha_ad_est(team1, team2, goals)
t12g = DataFrame(team1 = team1, team2 = team2, goals = goals)
sort!(t12g, cols=[:team1,:team2])

t1g = @> begin
    t12g
    @by(:team1, attack = mean(:goals))
    @orderby(:team1)
end

t1g[:attack_order] = 1:16
t2g = DataFrame(team2 = t1g[:team1], defense = 0, defense_order = 1:16)

t12gad = @> begin
    t12g
    join(t1g, on=:team1)
    join(t2g, on=:team2)
    @orderby(:team1,:team2)
end

attack_order = t12gad[:attack_order]
defense_order = t12gad[:defense_order]

haad = vcat(t1g[:attack], repeat([0], inner=16))

negloglik_goals = create_negloglik_fn(t12gad[:goals],t12gad[:attack_order], t12gad[:defense_order]);

res = optimize(negloglik_goals, haad, Optim.Options(iterations=Int64(1e5)))

Optim.minimizer(res)
#end

@time res = ha_ad_est(gr4jul[:HomeTeam],gr4jul[:AwayTeam], gr4jul[:hg])

attack_est = @> begin
    gr4jul
    @by(:HomeTeam, attack = 0)
    @orderby(:HomeTeam)
end
attack_est[:attack] = res[1:16];

defense_est = @> begin
    gr4jul
    @by(:HomeTeam, defense = 0)
    @orderby(:HomeTeam)
end
defense_est[:defense] = res[17:32]

ad_est = join(attack_est, defense_est, on=:HomeTeam)
sort!(ad_est,cols=:attack, rev = true)
print(ad_est)

sort!(ad_est,cols=:defense, rev = true)
print(ad_est)
