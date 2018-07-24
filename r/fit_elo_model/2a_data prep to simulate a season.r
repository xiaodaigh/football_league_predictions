source("r/fit_elo_model/0aa_setup.r")
source("r/fit_elo_model/0c_merge_football_data_elo_together.r")

# load the models ---------------------------------------------------------
model_elo_h_fnl = readRDS("model_elo_h_fnl.rds")
model_elo_a_fnl = readRDS("model_elo_a_fnl.rds")

epl_fixtures_w_elo[,elo_prob_h:=predict(model_elo_h_fnl,epl_fixtures_w_elo)]
epl_fixtures_w_elo[,elo_prob_a:=predict(model_elo_a_fnl,epl_fixtures_w_elo)]
epl_fixtures_w_elo[,elo_prob_d:=1 - elo_prob_h - elo_prob_a]



