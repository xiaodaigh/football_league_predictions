#' Estimate home attack/away defense/away attack/home defense given a data.table
#' @export
est_ha_ad_aa_hd <- function(gr4, nteams = gr4[,dplyr::n_distinct(HomeTeam)]) {
  ha = gr4[order(HomeTeam),.(ha = mean(hg)),HomeTeam]
  ad = gr4[order(AwayTeam),.(ad = 0),AwayTeam]
  
  # system.time(res_ha_ad <- optim(c(ha$ha, rep(0,nteams)), function(v) {
  #   ha[,ha:=v[1:nteams]]
  #   ad[,ad:=v[(nteams+1):(2*nteams)]]
  #   
  #   gr4a = merge(gr4, ha, by="HomeTeam") %>% 
  #     merge(ad,by ="AwayTeam")
  #   
  #   -gr4a[,dpois(hg, ha  - ad, log = T) %>% sum]
  # }, method= "Nelder-Mead", control = list(maxit=1e5)))
  
  setkey(gr4, HomeTeam, AwayTeam)
  res_ha_ad = haad_est(gr4$hg, gr4$HomeTeam, gr4$AwayTeam)
  
  ha[,ha:=res_ha_ad$par[1:nteams]]
  ad[,ad:=res_ha_ad$par[(nteams+1):(2*nteams)]]
  
  ha[order(ha,decreasing = T)]
  ad[order(ad,decreasing = T)]
  
  # away attack and home defense --------------------------------------------
  aa = gr4[order(AwayTeam),.(aa = mean(ag)),AwayTeam]
  hd = gr4[order(HomeTeam),.(hd = 0),HomeTeam]
  
  # v = c(aa$aa, rep(0,nteams))
  # system.time(res_aa_hd <- optim(c(aa$aa, rep(0,nteams)), function(v) {
  #   aa[,aa:=v[1:nteams]]
  #   hd[,hd:=v[(nteams+1):(2*nteams)]]
  #   
  #   gr4a = merge(gr4, aa, by="AwayTeam") %>% 
  #     merge(hd,by ="HomeTeam")
  #   
  #   -gr4a[,dpois(ag, aa  - hd, log = T) %>% sum]
  # }, control = list(maxit=1e5)))
  setkey(gr4,AwayTeam, HomeTeam)
  res_aa_hd = haad_est(gr4$ag, gr4$AwayTeam, gr4$HomeTeam)
  
  aa[,aa:=res_aa_hd$par[1:nteams]]
  hd[,hd:=res_aa_hd$par[(nteams+1):(2*nteams)]]
  
  aa[order(aa,decreasing = T)]
  hd[order(hd,decreasing = T)]
  
  
  # team strengths ----------------------------------------------------------
  tstrength = ha %>% 
    merge(hd,by="HomeTeam") %>% 
    merge(aa[,.(HomeTeam = AwayTeam, aa)],by="HomeTeam") %>% 
    merge(ad[,.(HomeTeam = AwayTeam, ad)],by="HomeTeam")
  
  tstrength[order(ha, decreasing = T)]
  tstrength[order(hd, decreasing = T)]
  
  list(ha=ha,ad=ad,aa=aa,hd=hd)
}

#' Simulate a season using the haad method
#' @export
simulate_season_haad <- function(gr3, ha, ad, aa, hd, nsim=1000) {
  gr3b = gr3 %>%
    merge(ha,by="HomeTeam") %>% 
    merge(hd,by="HomeTeam") %>% 
    merge(aa,by="AwayTeam") %>% 
    merge(ad,by="AwayTeam")
  
  system.time(res1000 <- replicate(nsim, {
    gr3b1 = copy(gr3b[HomeTeam!=AwayTeam])
    gr3b1[is.na(hg),hg:=sapply(ha-ad,function(haad) rpois(1,haad))]
    gr3b1[is.na(hg), hg := 0]
    
    gr3b1[is.na(ag),ag:=sapply(aa-hd,function(aahd) rpois(1,aahd))]
    gr3b1[is.na(ag),ag := 0]
    
    # compute points ----------------------------------------------------------
    gr3b1[hg > ag,hpts:=3]
    gr3b1[hg > ag,apts:=0]
    gr3b1[hg == ag,hpts:=1]
    gr3b1[hg == ag,apts:=1]
    gr3b1[hg < ag,hpts:=0]
    gr3b1[hg < ag,apts:=3]
    
    gr3b1a = rbindlist(
      list(
        gr3b1[,sum(hpts),.(team=HomeTeam)]
        ,gr3b1[,sum(apts),.(team=AwayTeam)]
      ))
    
    gr3b1a1 = gr3b1a[,.(pts = sum(V1)),team]
    gr3b1a1[order(pts,decreasing = T), rank:=1:.N]
    setkey(gr3b1a1,rank)
    gr3b1a1
  }, simplify = F)) #77 seconds
  
  res1000a = rbindlist(res1000)
  res1000a
}

#' Download from footystats.org and format the table
get_footystats <- function(url) {
  tdist <- read_html(url)
  gr_old = tdist %>%
    html_nodes("table.matches-table.inactive-matches")
  
  xx = tdist %>% html_nodes("span[itemprop='name']") %>% html_text
  xx =  xx[-(1:3)]
  
  scr_line = tdist %>% 
    html_nodes("table.matches-table:not(.inactive-matches) span.bold.ft-score") %>% 
    html_text
  
  gr_new = lapply(gr_old, function(gr_old) {
    gr = gr_old %>%
      html_table(header = TRUE);gr
    
    if(F) {
      ncol(gr)
      # column 7 and beyond just ignore
      gr[,7]
    }
    
    gr = gr[,1:6]
    names(gr) <- c("datetime","HomeTeam","scoreline_odds","AwayTeam", "tot_g","ht_scoreline")
    setDT(gr)
    gr[substr(HomeTeam, nchar(HomeTeam)-3, nchar(HomeTeam)) == "Odds",HomeTeam:=sapply(HomeTeam, function(HomeTeam) {
      nc = nchar(HomeTeam)
      substr(HomeTeam,1,nc-4)
    })]
    
    gr[,scoreline:=str_extract(scoreline_odds, regex("[:digit:] - [:digit:]"))]
    gr
  }) %>% rbindlist(use.names=T,fill=T)
  
  gr = gr_new
  nteams = dplyr::n_distinct(gr_new$HomeTeam)
  
  setDT(gr)
  gr[,HomeTeam:=as.character(HomeTeam)]
  gr[,AwayTeam:=as.character(AwayTeam)]
  #setkey(gr,HomeTeam,AwayTeam)
  
  gr1 = gr[,str_split(scoreline,"-|–") %>% (function(l) {
    res = lapply(l,function(ll) {
      data.table(hg=ll[1] %>% as.integer, ag =ll[2] %>% as.integer)
    })
    res %>% rbindlist(use.names = T, fill=T)
  })]
  
  gr3tmp = cbind(gr, gr1)
  
  if(length(xx) != 0) {
    gr_current = 
      data.table(
        HomeTeam = xx[c(T,F)]
        , AwayTeam = xx[c(F,T)]
        , scoreline = c(rep("",length(xx)/2-length(scr_line)), scr_line))
    
    gr_current1 = gr_current[,str_split(scoreline,"-|–") %>% (function(l) {
      res = lapply(l,function(ll) {
        data.table(hg=ll[1] %>% as.integer, ag =ll[2] %>% as.integer)
      })
      res %>% rbindlist(use.names = T, fill=T)
    })]
    
    gr_current2 = cbind(gr_current, gr_current1) 
    gr3 = rbindlist(list(gr_current2, gr3tmp),use.names = T, fill = T)
  } else {
    gr3 = gr3tmp
  }
  
  # good
  gr3[,.N]
  gr3[is.na(hg),.N,scoreline]
  gr3[is.na(ag),.N,scoreline]
  
  gr3[,.N,.(HomeTeam,ok=is.na(hg))] %>% spread(key=ok, value =N)
  gr3[,.N,.(AwayTeam,ok=is.na(hg))] %>% spread(key=ok, value =N)
  gr3
}

#' Download the data from footystats, estimate the haad and simultae the rest of the season
get_pred_haad = function(url, saveas, nsim=1000) {
  print(url)
  gr3 = get_footystats(url)
  
  gr4 = gr3[!is.na(hg),.(HomeTeam,AwayTeam,hg,ag)]
  # look at it --------------------------------------------------------------
  gr4[,mean(hg),HomeTeam][order(V1, decreasing = T)]
  gr4[,mean(ag),AwayTeam][order(V1, decreasing = T)]
  
  # home attacka nd away defense --------------------------------------------
  ha_ad_aa_hd = est_ha_ad_aa_hd(gr4)
  
  ha = ha_ad_aa_hd$ha
  ad = ha_ad_aa_hd$ad
  aa = ha_ad_aa_hd$aa
  hd = ha_ad_aa_hd$hd
  
  # simulate the rest of the season -----------------------------------------
  res1000a = simulate_season_haad(gr3, ha, ad, aa, hd)
  
  resA = res1000a[
    ,.(min(pts), mean(pts) %>% round(0), max(pts)
       , max(rank), median(rank) %>% round(0), min(rank)), team][order(V2,decreasing = T)]
  
  resB = res1000a[rank==1,.((nsim/.N) %>% round(2),(.N/nsim) %>% round(2)),team][order(V1)]
  
  resABC = list(res1000a, resA, resB, gr3, ha=ha ,ad=ad,aa=aa,ad=ad)
  saveRDS(resABC, file=saveas)
  print(resABC)
  resABC
}


#' Create a function that accepts a vector haad where the first 1/2 of the values are the attacking value and the sceond half are the
#' defensive values
#' @param goals An integer vector containing the goals scored by team1 vs team2
#' @param attack_order How to distribute the first half of haad (attacks) so that we don't have to merge so that it's more efficient
#' @param defense_order How to distribute the second half of haad (defense) so that we don't have to merge so that it's more efficient
#' @nteam nteams The number of teams; 2*nteams should equal the length of the haad argument in the output function
#' @export
create_negloglik_fn = function(goals, attack_order, defense_order, nteams) {
  function(haad){
    -sum(dpois(goals, pmax(0.00,haad[1:nteams][attack_order] - haad[(nteams+1):(nteams*2)][defense_order]), log = T))
  }
}

#' Estimate attack and defense under the haad system
#' @param goals An integer vector containing the goals scored by team1 vs team2
#' @param team1 A vector denoting the team that scored the goals
#' @param team2 A vector containing the defending team
#' @export
haad_est = function(goals, team1, team2) {
  gt1t2 = data.table(goals = goals, team1=team1, team2=team2)
  setkey(gt1t2,team1,team2)
  
  ha_dt = gt1t2[order(team1),.(a = mean(goals), attack_order=.GRP), team1]
  ad_dt = gt1t2[order(team2),.(d = 0, defense_order=.GRP), team2]
  
  gt1t2a = merge(gt1t2
                 ,ha_dt, by = "team1") %>% 
    merge(ad_dt, by = "team2")
  
  negloglik_fn = create_negloglik_fn(gt1t2a$goals, gt1t2a$attack_order, gt1t2a$defense_order, dplyr::n_distinct(team1))
  
  haad = c(ha_dt[,a], ad_dt[,d])
  
  system.time(heheres <- optim(haad, negloglik_fn, control = list(maxit=1e5)))
  heheres
}



# original code used to derive footystatsnames ----------------------------
if(F) {
  setdiff(round_by_round_result_elo[,unique(team)], rbyr17[,unique(team)])
  x = round_by_round_result_elo[round==37,mean(pts),team][order(V1),team]
  y = rbyr[,unique(team)]
  xy = mapply(function(x,y) sprintf("else if (team == '%s') return('%s')", x,y), x, y)
  names(xy) = NULL
  xy
  yx=to_footystats_names(x,T)
  x[sapply(yx, is.null)]
  yx=to_footystats_names(x)
  
}

#' Convert other team names to footstat names
#' @output
to_footystats_names <- function(teams, debug=F) {
  res = sapply(teams, function(team) {
    if (team == 'West Brom') return('West Bromwich Albion') 
    else if (team == 'Swansea') return('Swansea City')             
    else if (team == 'Stoke') return('Stoke City')             
    else if (team == 'Southampton') return('Southampton')        
    else if (team == 'Huddersfield') return('Huddersfield Town') 
    else if (team == 'Brighton') return('Brighton & Hove Albion')
    else if (team == 'Watford') return('Watford')                
    else if (team == 'West Ham') return('West Ham United')       
    else if (team == 'Crystal Palace') return('Crystal Palace')  
    else if (team == 'Newcastle') return('Newcastle United')     
    else if (team == 'Bournemouth') return('AFC Bournemouth')    
    else if (team == 'Leicester') return('Leicester City')       
    else if (team == 'Everton') return('Everton')                
    else if (team == 'Burnley') return('Burnley')                
    else if (team == 'Arsenal') return('Arsenal')                
    else if (team == 'Chelsea') return('Chelsea')                
    else if (team == 'Liverpool') return('Liverpool')            
    else if (team == 'Tottenham') return('Tottenham Hotspur')    
    else if (team == 'Man United') return('Manchester United')   
    else if (team == 'Man City') return('Manchester City')
    else if (team == 'Sunderland') return('Manchester City')
    else if (team == 'Middlesbrough') return('Middlesbrough')
    else if (team == 'Hull') return('Hull City')
  })
  if(!debug) {
    stopifnot(all(!sapply(res,is.null)))
  }

  res
}

#' Simulate a completed season using haad
#' @export
round_by_round_haad <- function(url, outfile) {
  gr3 = get_footystats(url)
  nteams = gr3[,dplyr::n_distinct(HomeTeam)]
  
  m2remove_inc = (nteams/2L)
  m2remove_max = nrow(gr3[!is.na(hg)]) - m2remove_inc*4L
  
  system.time(rbyr17 <- lapply(seq(m2remove_max,m2remove_inc,by=-m2remove_inc), function(m2remove) {
    #gr4 = gr3[!is.na(hg),.(HomeTeam,AwayTeam,hg,ag)]
    round = ((nrow(gr3)-m2remove)/m2remove_inc) %>% round(0)
    print(paste0("round: ", round))
    gr4 = copy(gr3)[!is.na(hg),][-(1:m2remove),]
    # look at it --------------------------------------------------------------
    gr4[,mean(hg,na.rm=T),HomeTeam][order(V1, decreasing = T)]
    gr4[,mean(ag,na.rm=T),AwayTeam][order(V1, decreasing = T)]
    
    if(gr4[,dplyr::n_distinct(HomeTeam)] != gr4[,dplyr::n_distinct(AwayTeam)]) {
      return(data.table())
    }
    # home attack and away defense --------------------------------------------
    system.time(ha_ad_aa_hd <- est_ha_ad_aa_hd(gr4))
    
    ha = ha_ad_aa_hd$ha
    ad = ha_ad_aa_hd$ad
    aa = ha_ad_aa_hd$aa
    hd = ha_ad_aa_hd$hd
    
    # simulate the rest of the season -----------------------------------------
    gr3copy = copy(gr3)
    gr3copy[1:m2remove,hg:=NA]
    gr3copy[1:m2remove,ag:=NA]
    res1000a = simulate_season_haad(gr3copy, ha, ad, aa, hd, nsim=nsim)
    
    resA = res1000a[
      ,.(min(pts), mean(pts) %>% round(0), max(pts)
         , max(rank), median(rank) %>% round(0), min(rank)), team][order(V2,decreasing = T)]
    
    resB = res1000a[rank %in% 1,.((nsim/.N) %>% round(2),(.N/nsim) %>% round(2)),team][order(V1)]
    
    print(resB)
    
    res1000a[,round:=round]
    res1000a
  }))
  rbyr17 = rbyr17 %>% rbindlist(use.names = T, fill = T)
  fst::write_fst(rbyr17,file.path(outpath,outfile))
  rbyr17
}