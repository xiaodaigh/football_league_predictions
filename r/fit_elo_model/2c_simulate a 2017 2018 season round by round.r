source("r/fit_elo_model/2a_data prep to simulate a season.r")
season2simulate = "1617"

simulate_round_by_round_elo = function(season2simulate) {
  xt3_tmp = epl_fixtures_w_elo[season==season2simulate,.(HomeTeam,AwayTeam,HomeElo,AwayElo, FTHG, FTAG, date)]
  xt3_tmp = xt3_tmp[HomeTeam != AwayTeam]
  
  setkey(xt3_tmp, date)
  
  round = 1
  system.time(round_by_round_result_elo <- lapply(1:37, function(round) {
    print(round)
    pt = proc.time()
    # take the first elo and percolate down
    matches_in_round = ((round-1)*10 + 1):(10*round)
    
    # take the elo from those matches form that round, and impute eleo from that onwards
    # with the elo from that round
    tmp = rbindlist(list(
      xt3_tmp[order(date),][matches_in_round,.(Elo = HomeElo, team = HomeTeam)]
      ,xt3_tmp[order(date),][matches_in_round,.(Elo = AwayElo, team = AwayTeam)]))
    
    xt3 = merge(
      xt3_tmp[-(1:(round*10)),.(date, HomeTeam, AwayTeam)],
      tmp[,.(HomeTeam = team, HomeElo = Elo)],
      by = "HomeTeam") %>% 
      merge(
        tmp[,.(AwayTeam = team, AwayElo = Elo)]
        , by = "AwayTeam"
      )
    
    xt3[,dr:=HomeElo - AwayElo]
    xt3[,drneg:=pmin(dr,0)]
    xt3[,drpos:=pmax(dr,0)]
    
    
    xt3[,elo_prob_h:=predict(model_elo_h_fnl, xt3)]
    xt3[,elo_prob_a:=predict(model_elo_a_fnl, xt3)]
    xt3[,elo_prob_d:=1 - elo_prob_h - elo_prob_a]
    setkey(xt3,date)
    
    ntries = 1000
    rseed = 1
    xt_already_happened = xt3_tmp[1:(round*10),]
    xt_already_happened1 = with(xt_already_happened, compute_pts(HomeTeam, AwayTeam, FTHG, FTAG))
    
    
    elo_sim = lapply(1:ntries, function(rseed) {
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
        ,xt3[,sum(apts),.(team=AwayTeam)]
        ,xt_already_happened1[,sum(pts),team]
      ))
      
      xt4=xt4[,.(pts = sum(V1)),team]
      
      xt4[order(pts,decreasing = T),rank:=1:20]
      xt4[,rseed:=rseed]
      xt4
    })
    
    elo_sim = rbindlist(elo_sim)
    elo_sim[,round:=round]
    print(timetaken(pt))
    
    print(elo_sim[rank%in% 1:4, .N/nsim, team][order(V1,decreasing=T)])
    elo_sim
  }))
  round_by_round_result_elo <- rbindlist(round_by_round_result_elo)
  fst::write_fst(round_by_round_result_elo, paste0("data/round_by_round_result_elo",".fst"))
}


if(F) {
  fst::write_fst(round_by_round_result_elo,"r/shiny/elo_sim.fst")
  
  #elo_sim[,.N/ntries,.(team,rank)] %>% spread(key=rank,value=V1)
  
  # elo sim: winner ---------------------------------------------------------
  round_by_round_result_elo[rank==1,.(`Predicted Odds`=ntries/.N),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
  
  # elo sim:top 4 -----------------------------------------------------------
  round_by_round_result_elo[rank<=4,.(`Predicted Odds`=ntries/.N),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
  
  # elo sim:top 6 -----------------------------------------------------------
  round_by_round_result_elo[rank<=6,.(`Predicted Odds`=ntries/.N),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
  
  # elo sim:relegated -----------------------------------------------------------
  round_by_round_result_elo[rank>=18,.(`Predicted Odds`=ntries/.N),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
  
  # elo sim:not relegated -----------------------------------------------------------
  round_by_round_result_elo[rank<18,.(`Predicted Odds`=ntries/.N),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
  
  # elo sim:top10 -----------------------------------------------------------
  round_by_round_result_elo[rank<=10,.(`Predicted Odds`=ntries/.N),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
  
  # elo sim:bottom10 -----------------------------------------------------------
  round_by_round_result_elo[rank>=11,.(`Predicted Odds`=ntries/.N),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
  
  # elo sim:not in top4 -----------------------------------------------------------
  round_by_round_result_elo[rank>4,.(`Predicted Odds`=ntries/.N),team][`Predicted Odds`>=1.01][order(`Predicted Odds`)]
  round_by_round_result_elo[team=="Arsenal",.(`Predicted Odds`=ntries/.N),.(rank5 = pmin(rank,5))][order(rank5)]
  round_by_round_result_elo[team=="Tottenham",.(`Predicted Odds`=ntries/.N),.(rank5 = pmin(rank,5))][order(rank5)]
}
