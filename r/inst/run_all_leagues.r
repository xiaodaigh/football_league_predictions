library(rvest)
library(data.table)
library(tidyr)
library(stringr)
source("r/R/footystats_functions.r")

urls = list(
  csl_url = "https://footystats.org/china/chinese-super-league/matches",
  ireland1_url = "https://footystats.org/republic-of-ireland/premier-division/matches",
  allsvenskn_url = "https://footystats.org/sweden/allsvenskan/matches",
  brazil1_url = "https://footystats.org/brazil/serie-a/matches",
  eliteserien_url = "https://footystats.org/norway/eliteserien/matches",
  #epl1516_url = "https://footystats.org/england/premier-league/2015-2016/matches",
  finland1_url = "https://footystats.org/finland/veikkausliiga/matches",
  mls_url = "https://footystats.org/usa/mls/matches",
  superettan_url = "https://footystats.org/sweden/superettan/matches",
  japan1_url = "https://footystats.org/japan/j1-league/matches",
  colombia1_url = "https://footystats.org/colombia/categoria-primera-a/matches",
  denmark1_url = "https://footystats.org/denmark/superliga/matches"
)

outpath = "data/"
locked = c(rep(F,length(leagues)-1),T)
abc = data.table(league=leagues, url=urls %>% unlist, locked = locked)
write.csv(abc,"r/init/leagues.csv")



leagues = c("csl","ireland1","sweden1","brazil1","norway1","finland1","mls","sweden2","japan1","colombia1","denamrk1")


res = get_pred_haad(urls$csl_url,file.path(outpath,"csl" %>% paste0(".rds")), nsim=10000)
res[[3]]

#get_pred_haad(urls$csl_url,file.path("r/shiny","csl" %>% paste0(".rds")))  

mapply(get_pred_haad, urls, file.path(outpath, leagues %>% paste0(".rds")), locked)


get_pred_haad(urls$colombia1_url, file.path(outpath, "colombia1"))
  