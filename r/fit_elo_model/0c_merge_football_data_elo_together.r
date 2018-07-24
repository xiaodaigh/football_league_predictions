library(data.table)
epl_fixtures = fst::read_fst("data/epl_fixtures.fst", as.data.table = T)

# create a date as the Date variable is a string --------------------------
epl_fixtures[,date_char:=as.character(Date)]
epl_fixtures[nchar(date_char) == 10, date:=as.Date(date_char,"%d/%m/%Y")]
epl_fixtures[nchar(date_char) == 8,  date:=as.Date(date_char,"%d/%m/%y")]

# merge football-data and clubelo together ------------------------------------------------------
elos = fst::read_fst("data/elos.fst",as.data.table = T)
elos[,from:=as.Date(From,"%Y-%m-%d")]
elos[,to:=as.Date(To,"%Y-%m-%d")]
elos1 = elos[,.(Club, Elo, from, to)]
setkey(elos1,Club,from,to)

# merge on the elos -------------------------------------------------------
# create another date variable for use with foverlaps
epl_fixtures[,date1:=date]
setkey(epl_fixtures,HomeTeam)
  
setnames(epl_fixtures,"HomeTeam","Club")

epl_fixtures_w_elo = foverlaps(epl_fixtures,elos1,
                    by.x=c("Club","date","date1"),
                    by.y=c("Club","from","to"))

setnames(epl_fixtures_w_elo,c("Club","Elo","AwayTeam"),c("HomeTeam","HomeElo","Club"))

epl_fixtures_w_elo = foverlaps(epl_fixtures_w_elo,elos1,
                by.x=c("Club","date","date1"),
                by.y=c("Club","from","to"))
setnames(epl_fixtures_w_elo, c("Club","Elo"),c("AwayTeam","AwayElo"))


epl_fixtures_w_elo[,dr:=HomeElo-AwayElo]
epl_fixtures_w_elo[,drneg:=pmin(dr,0)]
epl_fixtures_w_elo[,drpos:=pmax(dr,0)]


fst::write_fst(epl_fixtures_w_elo,"data/epl_fixtures_w_elo.fst")
