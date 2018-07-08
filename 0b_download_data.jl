season_start = 1993:2017
seasons = lpad.(mod.(season_start,100),2,0) .* lpad.(mod.(season_start.+1,100),2,0)
urls = "http://www.football-data.co.uk/mmz4281/" .* seasons .* "/E0.csv"

@rput urls
R"""
library(data.table)
fixtures = rbindlist(lapply(urls, read.csv), fill=T, use.names=T)
fst::write_fst(fixtures,"fixtures.fst")
"""
# R"""
# fixtures = fst::read_fst("fixtures.fst")
# """;
@rget fixtures;

# get rid of empty rows
fixtures = @where(fixtures, length.(string.(:Date)) .!= 0);
FileIO.save("fixtures.jld","fixtures",fixtures)

# minor data cleansing
fixtures[:HomeTeam] = [ht=="Middlesboro"?"Middlesbrough":ht for ht in fixtures[:HomeTeam]]
fixtures[:HomeTeam] = [ht=="Nott'm Forest"?"Forest":ht for ht in fixtures[:HomeTeam]]

fixtures[:AwayTeam] = [ht=="Middlesboro"?"Middlesbrough":ht for ht in fixtures[:AwayTeam]]
fixtures[:AwayTeam] = [ht=="Nott'm Forest"?"Forest":ht for ht in fixtures[:AwayTeam]]

unique(fixtures[:HomeTeam]) |> sort
unique(fixtures[:AwayTeam]) |> sort

# download Elo data from clubelo.com
@time clubelo = vcat(readcsvurl.("http://api.clubelo.com/" .* replace.((fixtures[:HomeTeam] |> unique), " "=>""))...)

FileIO.save("clubelo.jld","clubelo",clubelo)