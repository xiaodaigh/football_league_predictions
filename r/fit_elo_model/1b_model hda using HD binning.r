# this didn't quite work that well
gethprob = function(rseed, elos_dev_set, sample_pct = 0.9) {
  set.seed(rseed)
  elos3h = elos_dev_set[runif(.N) < sample_pct,.(sum(FTR %in% c("H")),.N),dr][order(dr,decreasing = T),]
  elos3h1 = copy(elos3h)
  nextcps = NULL
  nextcp = elos3h[,which.max(cumsum(V1)/cumsum(N))]
  while(nextcp < nrow(elos3h)) {
    nextcps <- c(nextcps, nextcp)
    elos3h <- elos3h[-(1:nextcp),]
    nextcp = elos3h[,which.max(cumsum(V1)/cumsum(N))]
    nextcp = nextcp[length(nextcp)]
  }
  
  cps = c(-Inf,sort(elos3h1[cumsum(nextcps),dr]),Inf)
  probh = elos3h1[,.(sum(V1)/sum(N)),
                  cut(dr,cps,include.lowest=T,right=F)][order(cut,decreasing=F)]
  probh[,low:=cps[-length(cps)]]
  probh[,high:=cps[-1]]
  probh
  #probh$V1[elo_scoreset[,cut(dr,cps, include.lowest =T, right=F)]]
}

# use all to do one -------------------------------------------------------
elos2_3 = mergeelo(mall1,load_path = F)
system.time(probh_pre_avg4 <- lapply(1:100, gethprob, elos_dev_set = elos2_3))

obj = copy(probh_pre_avg4)

predicth = function(obj, dr) {
  res = lapply(obj, function(obj1) {
    cps = c(obj1$low, Inf)
    obj1[order(cut),V1][cut(dr,cps,right=T)]
  })
  
  Reduce(`+`, res)/length(res)
}

elos2_3[,probh:=predicth(probh_pre_avg4, dr)]
elos2_3s = elos2_3[season=="1718",.(HomeElo, AwayElo, probh,1/B365H+1/B365D,FTR)]

elos2_3s[,mylik:=ifelse(FTR=="A",1-probh,probh)]
elos2_3s[,theirlik:=ifelse(FTR=="A",1-V4,V4)]

elos2_3s[,sum(log(mylik)),cut(HomeElo-AwayElo,pretty(HomeElo-AwayElo))]
elos2_3s[,log(theirlik) %>% sum,cut(HomeElo-AwayElo,pretty(HomeElo-AwayElo))]

elos2_3s[order(probh/V4,decreasing=T)]
elos2_3[,bprop:= B365H*(B365D - probh)/probh/(B365D-B365H)]

plot(lowess(elos2_3[,.(HomeElo-AwayElo,FTR%in%c("D","H"))]))


elos2_3



library(magrittr)
library(dplyr)
library(tidyr)
elodiffbin = elos2_3[, c(-Inf,quantile(HomeElo-AwayElo,seq(0.01,0.99,by=0.01)),Inf) %>% 
                       unique %>% sort]

hehe = elos2_3[,.N,.(FTR, elobin = cut(HomeElo-AwayElo,elodiffbin))]

hehe[,pct:=N/sum(N),elobin]

hehe %>%
  select(FTR,elobin,pct) %>% 
  spread(key=FTR,value=pct)


elos3_strategy_3a = elos2_3[probh>odds_implied_probh,][
  order(dr,decreasing = T),.(dr,probh,odds_implied_probh,B365H,FTR)]
elos3_strategy_3a[,ret:=cumsum(B365H*(FTR=="H"))-1:.N]




# mseas = function(sss, scr_threshold=100) {
#   elos3_3 = mergeelo(mall1[season==sss,], load_path=F)
#   elos3_3[,probh:=predicth(probh_pre_avg4,dr)]
#   elos3_strategy_3b = elos3_3[probh>odds_implied_probh & dr > scr_threshold,][
#     order(dr,decreasing = T),.(date,dr,probh,odds_implied_probh,B365H,FTR)]
#   elos3_strategy_3b[,ret:=cumsum(B365H*(FTR=="H"))-1:.N]
#   
#   #lines(elos3_strategy_3b[,.(dr,ret)],col="red")
#   elos3_strategy_3b
# }
# 
# # plot(elos3_strategy_3a[,.(dr,ret)],type="l")
# # abline(h=0)
# 
# sapply(c(100,200,300,400,-100,-200,-300,-400), function(scr_threshold) {
#   res = lapply(sss, mseas, scr_threshold=scr_threshold)
#   # unlist(lapply(res,function(dta) dta[,.N]))
#   # unlist(lapply(res,function(dta) dta[,sum(B365H*(FTR=="H")) - .N]))
#   res1 = rbindlist(res)
#   #res1[,.(.N, sum(B365H*(FTR=="H")), sum(B365H*(FTR=="H")) - .N),year(date)]
#   #res1[,.(.N, sum(B365H*(FTR=="H")), sum(B365H*(FTR=="H")) - .N)]
#   if(scr_threshold==100) {
#     plot(res1[order(date),.(date,cumsum(B365H*(FTR=="H"))-1:.N)],type="l")
#   } else{
#     lines(res1[order(date),.(date,cumsum(B365H*(FTR=="H"))-1:.N)])
#   }
# })

# backtest on all ---------------------------------------------------------
elos3_3 = mergeelo(mall1, load_path=F)
elos3_3[,probh:=predicth(probh_pre_avg4,dr)]

elos3_strategy_3b = elos3_3[probh>odds_implied_probh & dr > 100,][
  order(dr,decreasing = T),.(date,dr,probh,odds_implied_probh,B365H,FTR)]
elos3_strategy_3b[order(date),ret:=cumsum(B365H*(FTR=="H"))-1:.N]
plot(elos3_strategy_3b[order(date),.(date,ret)],type="l",col="red",ylim=c(-50,100))


hehe = function(scrt) {
  elos3_strategy_3b = elos3_3[probh>odds_implied_probh & dr > scrt,][
    order(dr,decreasing = T),.(date,dr,probh,odds_implied_probh,B365H,FTR)]
  elos3_strategy_3b[order(date),ret:=cumsum(B365H*(FTR=="H"))-1:.N]
  lines(elos3_strategy_3b[order(date),.(date,ret)],lty=(scrt/100)+5)
  browser()
}

plot(elos3_strategy_3b[order(date),.(date,ret)],type="l",col="red",ylim=c(-50,100))
abline(h=0,col="blue")
sapply(seq(-400,400,by=100), hehe)

library(ggplot2)
elos3_3[year(ssdate)>1995,.N,.(FTR,ssdate)] %>% 
  ggplot + 
  geom_line(aes(x=ssdate,y=N, colour=FTR))

elos3_3[FTR=="H",plikelihood:=probh]
elos3_3[FTR!="H",plikelihood:=1-probh]



