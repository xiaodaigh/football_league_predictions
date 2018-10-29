library(rvest)
library(data.table)
library(tidyr)
library(stringr)
library(purrr)
library(fst)
library(dplyr)
source("r/R/footystats_functions.r")

outpath = "data/"

all_leagues = fread("r/inst/leagues.csv")[!league %in%  c("japan1","colombia1","mls","ireland2","denmark1")]
#all_leagues = all_leagues[8,]
all_leagues = all_leagues[league == "csl"]

#all_leagues[league=="czech1", get_pred_haad(url,"cezh_test",locked=F)]

#get_pred_haad(all_leagues$url,"spain_tmp",locked=F)

allres = mapply(get_pred_haad, all_leagues$url, file.path(outpath, all_leagues$league %>% paste0(".rds")), locked = all_leagues$locked, nsim = 1000, SIMPLIFY = F)
allres1 = map(allres, pluck(3)) %>% rbindlist
a = allres[[1]][[1]]
View(allres1)
fst::write_fst(allres1, "data/league_winners_leader%s" %>% 
                 sprintf(Sys.time() %>% as.character %>% str_replace_all(":","_")))

bet365_names = c("Bournemouth", "Arsenal",         "Brighton",        "Burnley",        
                 "Cardiff",         "Chelsea",         "Crystal Palace",  "Everton",        
                 "Fulham",          "Huddersfield",    "Leicester",       "Liverpool",      
                  "Man City",        "Man Utd",         "Newcastle",       "Southampton",    
                  "Tottenham",       "Watford",         "West Ham",        "Wolverhampton")
footystats_names = c("AFC Bournemouth","Arsenal" ,                "Brighton & Hove Albion" ,
                     "Burnley"  ,               "Cardiff City"  ,          "Chelsea"     ,           
                     "Crystal Palace"  ,        "Everton"       ,          "Fulham"   ,              
                     "Huddersfield Town"   ,    "Leicester City"  ,        "Liverpool"  ,            
                     "Manchester City"     ,    "Manchester United"    ,   "Newcastle United" ,      
                      "Southampton"        ,     "Tottenham Hotspur" ,      "Watford" ,               
                     "West Ham United"    ,     "Wolverhampton Wanderers")

# load the chinese league -------------------------------------------------
if(F) {
  data_path = "D:/git/betting_arbi/data/output/matches"
  league2dl = "epl"
  matches  = dir(data_path) %>% 
    Filter(function(x) substr(x,1,nchar(league2dl)) == league2dl, .) %>% 
    sort %>% 
    pluck(length(.)) %>% 
    file.path(data_path, .) %>% 
    read_fst(as.data.table=T) %>% 
    group_by(HomeTeam, AwayTeam) %>% 
    summarise(odds_1 = max(odds_1), odds_2 = max(odds_2), odds_x = max(odds_x))
  
  x = match(matches$HomeTeam, bet365_names)
  matches$HomeTeam = ifelse(is.na(x),matches$HomeTeam, footystats_names[x])
  
  x = match(matches$AwayTeam, bet365_names)
  matches$AwayTeam = ifelse(is.na(x),matches$AwayTeam, footystats_names[x])
  
  # last season
  #gr3 = get_footystats("https://footystats.org/england/premier-league/2017-2018/matches")
  #last_ha_ad_aa_hd = est_ha_ad_aa_hd(gr3)
  
  res = NULL
  #all_leagues[league == league2dl, {res <<-get_pred_haad(url, file.path(outpath,league %>% paste0(".rds")), locked=locked, match_predictions = matches, nsim = 1000, init_haad = last_ha_ad_aa_hd)}]
  all_leagues[league == league2dl, {res <<-get_pred_haad(url, file.path(outpath,league %>% paste0(".rds")), locked=locked, match_predictions = matches, nsim = 1000)}]
  res[[3]]
}

