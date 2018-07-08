clubelo[:From][1]


fixtures_latest = @where(fixtures, :season .== "1718")

# estimate the attack strength

using Distributions, DataFramesMeta, Lazy


home_attack_estimate = @> begin
    fixtures_latest
    @by(:HomeTeam, meanhg = mean(:FTHG))
    @orderby(:HomeTeam)
end

# create a minimal dataset for merging
fixtures_latest1 = fixtures_latest[[:HomeTeam,:AwayTeam,:FTHG]]

fixtures_latest2 = join(fixtures_latest1, home_attack_estimate, on = :HomeTeam)

pdf(Poisson(2.5), 0)