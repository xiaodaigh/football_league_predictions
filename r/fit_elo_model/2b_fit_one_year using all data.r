# simulate a season using ELO ---------------------------------------------
# xt = unique(epl_fixtures_w_elo[,HomeTeam] %>% as.character %>% sort)
xt1 = data.table(team=c("Man City","Liverpool","Tottenham","Man United","Chelsea","Arsenal","Everton","Leicester",
                        "Crystal Palace","Burnley","West Ham","Bournemouth","Newcastle","Southampton","Fulham"
                        ,"Brighton","Watford", "Wolves","Cardiff","Huddersfield"))

xt2 = expand.grid(xt1$team,xt1$team)
setDT(xt2)
setnames(xt2,names(xt2),c("HomeTeam","AwayTeam"))
xt2[,date:=Sys.Date()]
xt2[,date1:=date]

setkey(xt2,HomeTeam)

#fst::write_fst(elos,"data/elos.fst")

setnames(xt2,"HomeTeam","Club")
elos2 = foverlaps(xt2,elos1,
                  by.x=c("Club","date","date1"),
                  by.y=c("Club","from","to"))


setnames(elos2,c("Club","Elo","AwayTeam"),c("HomeTeam","HomeElo","Club"))

elos3 = foverlaps(elos2,elos1,
                  by.x=c("Club","date","date1"),
                  by.y=c("Club","from","to"))
setnames(elos3,c("Club","Elo"),c("AwayTeam","AwayElo"))

xt3 = elos3[,.(HomeTeam,AwayTeam,HomeElo,AwayElo)]
xt3 = xt3[HomeTeam != AwayTeam]
xt3[,dr:=HomeElo - AwayElo]
xt3[,drneg:=pmin(dr,0)]
xt3[,drpos:=pmax(dr,0)]

xt3[,elo_prob_h:=predict(model_elo_h_fnl, xt3)]
xt3[,elo_prob_a:=predict(model_elo_a_fnl, xt3)]
xt3[,elo_prob_d:=1 - elo_prob_h - elo_prob_a]

ntries = 10000
elo_sim = future.apply::future_lapply(1:ntries, function(rseed) {
  set.seed(rseed)
  xt3[,rpick:=runif(.N)]
  xt3[rpick <= elo_prob_h,hpts:=3]
  xt3[rpick <= elo_prob_h,apts:=0]
  xt3[rpick > elo_prob_h,hpts:=1]
  xt3[rpick > elo_prob_h,apts:=1]
  xt3[rpick > elo_prob_h+elo_prob_d,hpts:=0]
  xt3[rpick > elo_prob_h+elo_prob_d,apts:=3]
  
  xt4 = rbindlist(list(
    xt3[,sum(hpts),.(team=HomeTeam)]
    ,xt3[,sum(apts),.(team=AwayTeam)]))
  xt4=xt4[,sum(V1),team]
  xt4[order(V1,decreasing = T),rank:=1:20]
  xt4[,rseed:=rseed]
  xt4
})

elo_sim = rbindlist(elo_sim)
fst::write_fst(elo_sim,"elo_sim.fst")
fst::write_fst(elo_sim,"r/shiny/elo_sim.fst")

elo_sim[,.N/ntries,.(team,rank)] %>% spread(key=rank,value=V1)


# best wo city ------elo_sim------------------------------------------------------
elo_sim[team != "Man City", min_rank_wo_city := min(rank), rseed]

elo_sim[team != "Man City",10000/sum(min_rank_wo_city==rank), team]




# elo sim: winner ---------------------------------------------------------
elo_sim[rank==1,.(`Predicted Odds`=ntries/.N),team][order(`Predicted Odds`)]

# elo sim:top 4 -----------------------------------------------------------
elo_sim[rank<=4,.(`Predicted Odds`=ntries/.N),team][order(`Predicted Odds`)]

# elo sim:top 6 -----------------------------------------------------------
elo_sim[rank<=6,.(`Predicted Odds`=ntries/.N),team][order(`Predicted Odds`)]

# elo sim:relegated -----------------------------------------------------------
elo_sim[rank>=18,.(`Predicted Odds`=ntries/.N),team][order(`Predicted Odds`)]

# elo sim:not relegated -----------------------------------------------------------
elo_sim[rank<18,.(`Predicted Odds`=ntries/.N),team][order(`Predicted Odds`)]

# elo sim:top10 -----------------------------------------------------------
elo_sim[rank<=10,.(`Predicted Odds`=ntries/.N),team][order(`Predicted Odds`)]

# elo sim:bottom10 -----------------------------------------------------------
elo_sim[rank>=11,.(`Predicted Odds`=ntries/.N),team][order(`Predicted Odds`)]

# elo sim:not in top4 -----------------------------------------------------------
elo_sim[rank>4,.(`Predicted Odds`=ntries/.N),team][order(`Predicted Odds`)]
elo_sim[team=="Arsenal",.(`Predicted Odds`=ntries/.N),.(rank5 = pmin(rank,5))][order(rank5)]
elo_sim[team=="Tottenham",.(`Predicted Odds`=ntries/.N),.(rank5 = pmin(rank,5))][order(rank5)]


# elo sim: winner ---------------------------------------------------------
elo_sim[rank==1,.(`Predicted Odds`=ntries/.N),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]

# elo sim:top 4 -----------------------------------------------------------
elo_sim[rank<=4,.(`Predicted Odds`=ntries/.N),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]

# elo sim:top 6 -----------------------------------------------------------
elo_sim[rank<=6,.(`Predicted Odds`=ntries/.N),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]

# elo sim:relegated -----------------------------------------------------------
elo_sim[rank>=18,.(`Predicted Odds`=ntries/.N),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]

# elo sim:not relegated -----------------------------------------------------------
elo_sim[rank<18,.(`Predicted Odds`=ntries/.N),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]

# elo sim:top10 -----------------------------------------------------------
elo_sim[rank<=10,.(`Predicted Odds`=ntries/.N),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]

# elo sim:bottom10 -----------------------------------------------------------
elo_sim[rank>=11,.(`Predicted Odds`=ntries/.N),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]

# elo sim:not in top4 -----------------------------------------------------------
elo_sim[rank>4,.(`Predicted Odds`=ntries/.N),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
elo_sim[team=="Arsenal",.(`Predicted Odds`=ntries/.N),.(rank5 = pmin(rank,5))][order(rank5)]
elo_sim[team=="Tottenham",.(`Predicted Odds`=ntries/.N),.(rank5 = pmin(rank,5))][order(rank5)]
