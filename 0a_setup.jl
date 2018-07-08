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
