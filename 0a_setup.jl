ENV["R_HOME"]=""
ENV["PATH"]="C:/Program Files/R/R-3.5.1/bin/x64/"

using HTTP, DataFrames, RCall, DataFramesMeta, uCSV, FileIO

# ENV["R_HOME"]=""
# ENV["PATH"]="C:/Program Files/R/R-3.5.0/bin"
# Pkg.build("RCall")
#readcsvurl(urls[1])
#@time a = vcat.(readcsvurl.(urls))

function readcsvurl(url)
    print(url)
    DataFrame(uCSV.read(IOBuffer(HTTP.get(url).body), delim=',', header=1))
    #res = HTTP.get(url);
    #mycsv = readcsv(res.body,header=true)
    #mycsv1 = DataFrame(mycsv[1], Symbol.(squeeze(mycsv[2],1)); makeunique=true)
end

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