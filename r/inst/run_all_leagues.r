library(rvest)
library(data.table)
library(tidyr)
library(stringr)
library(purrr)
library(fst)
library(dplyr)
source("r/R/footystats_functions.r")

outpath = "data/"

all_leagues = fread("r/inst/leagues.csv")[league != "japan1"]
all_leagues = all_leagues[league == "switzerland1"]

if(F) {
  res = get_pred_haad(all_leagues[1,url],file.path(outpath,"csl" %>% paste0(".rds")), nsim=10000);
  res[[3]]
}

allres = mapply(get_pred_haad, all_leagues$url, file.path(outpath, all_leagues$league %>% paste0(".rds")), locked = all_leagues$locked, nsim = 1000, SIMPLIFY = F)
allres1 = map(allres, pluck(3)) %>% rbindlist
allres2 = map(allres, pluck(1)) %>% rbindlist

allres2[,max(rank)]

allres2[rank==14,1000/.N,team]

#allres1 = allres[seq(3,88,8)] %>% rbindlist
View(allres1)
fst::write_fst(allres1, "data/league_winners_leader%s" %>% 
                 sprintf(Sys.time() %>% as.character %>% str_replace_all(":","_")))


all_leagues[10,get_pred_haad(url,"data/denmark1.rds",locked=T)]


# load the chinese league -------------------------------------------------
data_path = "D:/git/betting_arbi/data/output/matches"
league2dl = "csl"
matches  = dir(data_path) %>% 
  Filter(function(x) substr(x,1,3) == league2dl, .) %>% 
  sort %>% 
  pluck(length(.)) %>% 
  file.path(data_path, .) %>% 
  read_fst(as.data.table=T) %>% 
  group_by(HomeTeam, AwayTeam) %>% 
  summarise(odds_1 = max(odds_1), odds_2 = max(odds_2), odds_x = max(odds_x))

all_leagues[league == league2dl, get_pred_haad(url, file.path(outpath,league), locked=locked, match_predictions = matches)]
