# read_all_seasons
library(data.table)
library(stringr)
library(fst)

ss = 93:(93+25) %% 100

sss = paste0(
  str_pad(ss[-length(ss)],2,pad="0"),
  str_pad(ss[-1],2,pad="0"))

system.time(epl_fixtures <- lapply(sss, function(sss) {
  print(sss)
  m = read.csv(sprintf("http://www.football-data.co.uk/mmz4281/%s/E0.csv",sss))
  setDT(m)
  m[,season:=sss]
  m
  }))

epl_fixtures <- rbindlist(epl_fixtures,use.names = T, fill = T)[!is.na(HomeTeam) & HomeTeam != "",]
epl_fixtures[HomeTeam=="Nott'm Forest", HomeTeam:="Forest"]
epl_fixtures[HomeTeam=="Middlesboro", HomeTeam:="Middlesbrough"]
epl_fixtures[AwayTeam=="Nott'm Forest", AwayTeam:="Forest"]
epl_fixtures[AwayTeam=="Middlesboro", AwayTeam:="Middlesbrough"]
fst::write_fst(epl_fixtures,"data/epl_fixtures.fst")

