season_start = 1993:2017
seasons = lpad.(mod.(season_start,100),2,0) .* lpad.(mod.(season_start.+1,100),2,0)
urls = "http://www.football-data.co.uk/mmz4281/" .* seasons .* "/E0.csv"

@rput urls;
@rput seasons;
R"""
library(data.table)
fixtures = rbindlist(lapply(seasons, function(season) {
    url = paste0("http://www.football-data.co.uk/mmz4281/" , season , "/E0.csv")
    res = read.csv(url)
    setDT(res)
    res[,season:=season]
}), fill=T, use.names=T)
setDT(fixtures)
fixtures[,Date:=as.character(Date)]
fixtures = fixtures[Date != ""]
fixtures[nchar(Date) == 8, date:=as.Date(Date,"%d/%m/%y")]
fixtures[nchar(Date) == 10, date:=as.Date(Date,"%d/%m/%Y")]
fst::write_fst(fixtures,"data/fixtures.fst")
"""
@rget fixtures;

# get rid of empty rows
fixtures = @where(fixtures, length.(string.(:Date)) .!= 0);

if false
    FileIO.save("data/fixtures.jld","fixtures",fixtures)
end

# minor data cleansing
fixtures[:HomeTeam] = [ht=="Middlesboro"?"Middlesbrough":ht for ht in fixtures[:HomeTeam]]
fixtures[:HomeTeam] = [ht=="Nott'm Forest"?"Forest":ht for ht in fixtures[:HomeTeam]]

fixtures[:AwayTeam] = [ht=="Middlesboro"?"Middlesbrough":ht for ht in fixtures[:AwayTeam]]
fixtures[:AwayTeam] = [ht=="Nott'm Forest"?"Forest":ht for ht in fixtures[:AwayTeam]]

unique(fixtures[:HomeTeam]) |> sort
unique(fixtures[:AwayTeam]) |> sort

# download Elo data from clubelo.com
@time clubelo = vcat(readcsvurl.("http://api.clubelo.com/" .* replace.((fixtures[:HomeTeam] |> unique), " "=>""))...)

FileIO.save("data/clubelo.jld","clubelo",clubelo)