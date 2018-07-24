epl_fixtures_w_elo = fst::read_fst("data/epl_fixtures_w_elo.fst", as.data.table = T)

# checking for abnormalit uisng season as a variable ----------------------
summary(glm((FTR=="H")~season, data=epl_fixtures_w_elo, family=binomial))
# it can be seen that season1516 and season9394 have unusual home ground advantage

summary(glm((FTR=="A")~season, data=epl_fixtures_w_elo, family=binomial))
# nothing too unusual

summary(glm((FTR=="D")~season, data=epl_fixtures_w_elo, family=binomial))
# nothing too unusual

# after some trial and error a 4th order polynomial model is chosen
epl_fixtures_w_elo[,drneg:=pmin(dr,0)]
epl_fixtures_w_elo[,drpos:=pmax(dr,0)]

# after some trail and error arrived at this model for H
summary(model_elo_h1 <- glm(formula = (FTR == "H") ~ drneg + drpos, data = epl_fixtures_w_elo, family=binomial))
summary(model_elo_h2 <- glm(formula = (FTR == "H") ~ drneg + drpos + I(drpos^2), data = epl_fixtures_w_elo))

summary(model_elo_a1 <- glm(formula = (FTR == "A") ~ drneg + drpos, data = epl_fixtures_w_elo, family=binomial))
summary(model_elo_a2 <- glm(formula = (FTR == "A") ~ drneg + drpos, data = epl_fixtures_w_elo))

epl_fixtures_w_elo[,test_model_ph1:=predict(model_elo_h1,epl_fixtures_w_elo,type="response")]
epl_fixtures_w_elo[,test_model_ph2:=predict(model_elo_h2,epl_fixtures_w_elo,type="response")]

test_model_ph1_lik = epl_fixtures_w_elo[,ifelse(FTR=="H",test_model_ph1,1-test_model_ph1) %>% log %>% sum]
test_model_ph2_lik = epl_fixtures_w_elo[,ifelse(FTR=="H",test_model_ph2,1-test_model_ph2) %>% log %>% sum]

# if logistic model better?
test_model_ph1_lik > test_model_ph2_lik # FALSE


epl_fixtures_w_elo[,test_model_pa1:=predict(model_elo_a1,epl_fixtures_w_elo,type="response")]
epl_fixtures_w_elo[,test_model_pa2:=predict(model_elo_a2,epl_fixtures_w_elo,type="response")]

test_model_pa1_lik = epl_fixtures_w_elo[,ifelse(FTR=="A",test_model_pa1,1-test_model_pa1) %>% log %>% sum]
test_model_pa2_lik = epl_fixtures_w_elo[,ifelse(FTR=="A",test_model_pa2,1-test_model_pa2) %>% log %>% sum]

# if logistic model better?
test_model_ph1_lik > test_model_ph2_lik # FALSE

# ZJ: inconclusion the straight linear model is better at predicting
model_elo_h_fnl = model_elo_h2
model_elo_a_fnl = model_elo_a2

saveRDS(model_elo_h_fnl, "model_elo_h_fnl.rds")
saveRDS(model_elo_a_fnl, "model_elo_a_fnl.rds")
