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
  superettan_url = "https://footystats.org/sweden/superettan/matches"
  )

outpath = "data/"
nsim = 1000


url = "https://footystats.org/england/premier-league/2017-2018/matches"
system.time(rbyr <- round_by_round_haad(url, "epl1718_round_by_round.rds"))

rbyr[,unique(team)] %>% setdiff(fnl_tbl$team)
setdiff(fnl_tbl$team,rbyr[,unique(team)])

# create actual table
gr3 = get_footystats(url)
fnl_tbl = with(gr3, compute_pts(HomeTeam, AwayTeam, hg, ag))
print(fnl_tbl[order(rank)])
setnames(fnl_tbl, "rank","actual_rank")
setnames(fnl_tbl, "pts","actual_pts")

rbyr171 = merge(rbyr, fnl_tbl, by = "team")
nsim = rbyr171[,.N,round][,mean(N)]/rbyr17[,dplyr::n_distinct(team)]

round_by_round_result_elo = simulate_round_by_round_elo("1718")

round_by_round_result_elo1 = copy(round_by_round_result_elo)
round_by_round_result_elo1[,team:=as.character(team)]
round_by_round_result_elo1[,team_orig := team]

system.time(round_by_round_result_elo1[,team:=to_footystats_names(team_orig)])
round_by_round_result_elo1 = merge(round_by_round_result_elo1, fnl_tbl, by="team")



# what is the prediction based on rank ------------------------------------
# if we use the rank at each round to predict the outcome
round = 1
rank_pred = lapply(1:37, function(round) {
  m3remove = 1:(380 - round*10)
  res = with(gr3[-(m3remove),], compute_pts(HomeTeam, AwayTeam, hg, ag))
  res[,round:=round]
  res
}) %>% rbindlist



rank_pred = merge(rank_pred, fnl_tbl, by ="team")
# compare approaches ------------------------------------------------------

fromn=18
ton=20

compare_haad_elo <- function(fromn, ton) {
  one2four = rbyr171[rank %in% fromn:ton & actual_rank %in% fromn:ton, .(haad = .N/nsim/length(fromn:ton)), round]
  one2four2 = round_by_round_result_elo1[rank %in% fromn:ton & actual_rank %in% fromn:ton, .(elo= .N/nsim/length(fromn:ton)), round]
  one2four4 = rank_pred[rank %in% fromn:ton & actual_rank %in% fromn:ton, .(rank_pred = .N/length(fromn:ton)), round]
  
  one2four3 = merge(one2four, one2four2, by = "round", all.y = T, all.x=T) %>% 
    merge(one2four4, by = "round", all.y = T, all.x=T)
  
  one2four3 %>% 
    gather(key=method, value = value, - round) %>% 
    ggplot + 
    geom_line(aes(x=round, y = value, colour = method))
}

compare_haad_elo(1,1)
compare_haad_elo(1,4)
compare_haad_elo(1,6)
compare_haad_elo(7,10)
compare_haad_elo(11,20)
compare_haad_elo(18,20)

rbyr171[rank %in% fromn:ton & actual_rank %in% fromn:ton,.N,.(round,team)]



# without city ------------------------------------------------------------
round_by_round_result_elo1[team!="Man City", best_rank_wo_city := min(rank), rseed]

round_by_round_result_elo1[team!="Man City", sum(rank == best_rank_wo_city)]



