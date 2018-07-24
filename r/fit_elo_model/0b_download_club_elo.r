library(data.table)
library(magrittr)
library(stringr)

# download Elo data from clubelo.com --------------------------------------
# took about 1.5 mins
system.time(elos <- lapply(setdiff(unique(epl_fixtures$HomeTeam),"Middlesboro"), function(HomeTeam) {
  if(HomeTeam == "Nott'm Forest") {
    HomeTeam = "Forest"
  }
  read.csv(paste0("http://api.clubelo.com/", 
                  str_replace_all(HomeTeam," ","")))
}) %>% rbindlist)

elos[is.na(Elo),unique(Club)]
fst::write_fst(elos,"data/elos.fst")