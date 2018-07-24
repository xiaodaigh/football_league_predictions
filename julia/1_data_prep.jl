include("0a_setup.jl");
include("0c_load_data.jl");


fixtures[:HomeTeam] = string.(fixtures[:HomeTeam])
fixtures[:AwayTeam] = string.(fixtures[:AwayTeam])

fixtures_latest = @where(fixtures, :season .== "1718")
sort!(fixtures_latest, cols=[:HomeTeam,:AwayTeam])

# create the minimal datasets for merging
fixtures_latest1 = fixtures_latest[[:HomeTeam,:AwayTeam,:FTHG]];



# estimate the attack strength
ha_est = @> begin
    fixtures_latest
    @by(:HomeTeam, home_attack = mean(:FTHG))
    @orderby(:HomeTeam)
end


ad_est = @> begin
    fixtures_latest
    @by(:AwayTeam, away_defense = 0.0)
    @orderby(:AwayTeam)
end

function loglikgoalsfn(goals, base)
    return function(adj)
        sum(logpdf.(Poisson.(max.(base .- adj,0)), goals))
    end
end

# function loglikgoals(goals, attack, defense)
#     sum(logpdf.(Poisson.(max.(attack .- defense,0)), goals))
# end

res = loglikgoals(fixtures_latest[:FTHG], repeat(ha_est[:home_attack], inner = 19), deleteat!(repeat(ad_est[:away_defense], outer = 20), (1:20) .+ (0:20:380)))
res

ha_init = ha_est[:home_attack]
goals=fixtures_latest1[:FTHG]


function op_ha_ad(goals, ha_init)
    ha_old = ha_init
    ad_old = repeat([0.0], inner=20)

    #res = loglikgoals(goals, repeat(ha_old, inner = 19), deleteat!(repeat(ad_old, outer = 20), (1:20) .+ (0:20:380)))

    # to optimise for defense, start with the attack values
    loglikfn = loglikgoalsfn(goals, repeat(ha_old, inner = 19))
    res = optimize(loglikfn, deleteat!(repeat(ad_old, outer = 20), (1:20) .+ (0:20:380)))
    ad_new = Optim.minimizer(res)


    loglikfn2 = loglikgoalsfn(goals, repeat(ha_old, inner = 19))
    optimize(loglikfn, deleteat!(repeat(ad_old, outer = 20), (1:20) .+ (0:20:380)))

    while(0 != sum(abs.(ha_old .- ha_est[:home_attack]) + abs.(ad_old .- ad_est[:away_defense])))
        ha_old = ha_est[:home_attack]
        ad_old = ad_est[:away_defense]
        optimres = optimize(negloglik_ad, ad_est[:away_defense],Optim.Options(iterations=10_000))
        ad_est[:away_defense] = Optim.minimizer(optimres)
        optimres = optimize(negloglik_ha, ha_est[:home_attack],Optim.Options(iterations=10_000))
        ha_est[:home_attack] = Optim.minimizer(optimres)

        println(sum(abs.(ha_old .- ha_est[:home_attack]) + abs.(ad_old .- ad_est[:away_defense])))
    end
end





function negloglik_ad(x)
    ad_est[:away_defense] = x;
    fixtures_latest2 = join(fixtures_latest1, ha_est, on = :HomeTeam)
    fixtures_latest2 = join(fixtures_latest2, ad_est, on = :AwayTeam)
    res = @> begin
        fixtures_latest2
        @transform(loglik = logpdf.(Poisson.(max.(:home_attack .- :away_defense,0)), :FTHG))
        #@transform(loglik = :FTHG.*log.(:home_attack .- :away_defense) .- (:home_attack .- :away_defense))
    end
    -sum(res[:loglik])
end

function negloglik_ha(x)
    ha_est[:home_attack] = x;
    fixtures_latest2 = join(fixtures_latest1, ha_est, on = :HomeTeam)
    fixtures_latest2 = join(fixtures_latest2, ad_est, on = :AwayTeam)
    res = @> begin
        fixtures_latest2
        @transform(loglik = logpdf.(Poisson.(max.(:home_attack .- :away_defense,0)), :FTHG))
        #@transform(loglik = :FTHG.*log.(:home_attack .- :away_defense) .- (:home_attack .- :away_defense))
    end
    -sum(res[:loglik])
end

ad_est[:away_defense] = zeros(20)
ha_old = ha_est[:home_attack].*0
ad_old = ad_est[:away_defense].*0

while(0 != sum(abs.(ha_old .- ha_est[:home_attack]) + abs.(ad_old .- ad_est[:away_defense])))
    ha_old = ha_est[:home_attack]
    ad_old = ad_est[:away_defense]
    optimres = optimize(negloglik_ad, ad_est[:away_defense],Optim.Options(iterations=10_000))
    ad_est[:away_defense] = Optim.minimizer(optimres)
    optimres = optimize(negloglik_ha, ha_est[:home_attack],Optim.Options(iterations=10_000))
    ha_est[:home_attack] = Optim.minimizer(optimres)

    println(sum(abs.(ha_old .- ha_est[:home_attack]) + abs.(ad_old .- ad_est[:away_defense])))
end

#rename!(ha_est,:HomeTeam=>:team)
#rename!(ad_est,:AwayTeam=>:team)

#ha_ad_est = join(ha_est, ad_est, on=:team)


fixtures_latest2 = join(fixtures_latest1, ha_est, on = :HomeTeam)
fixtures_latest2 = join(fixtures_latest2, ad_est, on = :AwayTeam)
res = @> begin
    fixtures_latest2
    @transform(loglik = logpdf.(Poisson.(max.(:home_attack .- :away_defense,0)), :FTHG))
    #@transform(loglik = :FTHG.*log.(:home_attack .- :away_defense) .- (:home_attack .- :away_defense))
end
