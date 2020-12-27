source("scripts/cp_time_of_throw.R")
# df: df_cp_throw_filt

library(skimr)
library(BART)

str(df_cp_throw_filt)

df_cp_throw_filt <- df_cp_throw_filt %>% 
  as.data.frame() %>% 
  select(-c(penalty_codes, penalty_jersey_numbers, offense_formation, 
            defenders_in_the_box, type_dropback, pre_snap_home_score,
            pre_snap_visitor_score, n_scores, game_clock, absolute_yardline_number)) 

# changing outcome to 0-1
# changing possession team to factor
# changing variables to numeric, logical or factor
# dropping all NA's
df_cp_throw <- df_cp_throw_filt %>% 
  mutate(
    pass_result = case_when(
      pass_result == "C" ~ 1, 
      pass_result == "I" ~ 0),
    target_height = as.numeric(target_height), 
    qb_hit = as.factor(qb_hit),
    is_leading = as.factor(is_leading),
    roof = as.factor(roof)) %>% 
  drop_na()

# prepping data for BART
drop_cols <- c("game_id", "play_id", "target_nfl_id", "pass_result", "cp", 
               "possession_team", "home_team_abbr", "visitor_team_abbr", 
               "air_yards", "play_description", "play_type", "yardline_side", 
               "yardline_number", "personnel_o", "personnel_d", 
               "offense_play_result", "play_result", "epa", 
               "is_defensive_pi", "game_date", "game_time_eastern", "week",
               "is_spike", "is_tip", "is_throw_away", "is_penalty")
               

y <- df_cp_throw$pass_result
x <- df_cp_throw[,!colnames(df_cp_throw) %in% drop_cols]

# BART
bart_fit1 <- lbart(x.train = x, 
                   y.train = y, 
                   sparse = TRUE, 
                   ndpost = 500, 
                   nskip = 2500, 
                   keepevery = 5, 
                   printevery = 500)

saveRDS(bart_fit1, file = "data/BART_time_of_throw/bart_fit1.RDS")


bart_fit2 <- lbart(x.train = x, 
                    y.train = y, 
                    sparse = TRUE, 
                    ndpost = 500, 
                    nskip = 2500, 
                    keepevery = 5, 
                    printevery = 500)

saveRDS(bart_fit2, file = "data/BART_time_of_throw/bart_fit2.RDS")

# assess the convergence 
library(coda)
rhat <- gelman.diag(mcmc.list(mcmc(bart_fit1$prob.train), mcmc(bart_fit2$prob.train)), multivariate = FALSE)
ess <- effectiveSize(mcmc.list(mcmc(bart_fit1$prob.train), mcmc(bart_fit2$prob.train)))

save(rhat, ess, file = "data/BART_time_of_throw/bart_fit_diags.RData")

# Posterior means of probability and credible intervals 

prob_train1 <- bart_fit1$prob.train
prob_train2 <- bart_fit2$prob.train
prob_train <- rbind(prob_train1, prob_train2)

# posterior means
prob_means <- apply(prob_train, mean, MAR=2)

# 95% CI
ci.fun <- function(a){
  c(quantile(a,.025),quantile(a,.975))
}

prob.ci = apply(prob_train, 2, ci.fun)

low_bound = prob.ci[1,] 
upp_bound = prob.ci[2,]

trained_plus_phat_time_of_throw <- cbind(df_cp_throw, prob_means, low_bound, upp_bound)

save(trained_plus_phat_time_of_throw, file = "data/BART_time_of_throw/trained_plus_phat_time_of_throw.RData")
# probabilities are the "prob_means" variable, 95% is low_bound and upp_bound 

# Variable selection

varcount <- rbind(bart_fit1$varcount, bart_fit2$varcount)
varprob <- rbind(bart_fit1$varprob, bart_fit2$varprob)

varcount_mean <- colMeans(varcount)
varcount_sd <- apply(varcount, FUN = sd, MARGIN = 2)

sort(colMeans(varcount), decreasing = TRUE)[1:10]
sort(colMeans(varprob), decreasing = TRUE)[1:10]

#target_weight       dist_def1 
#0.13143302          0.11261055 
#angle_diff          qb_speed 
#0.10300300          0.08246918 
#dist_qb             yards_from_sideline 
#0.05959188          0.05283012 
#time_to_throw       roof4 
#0.04678602          0.03771001 
#qb_hit1             qb_hit2 
#0.03543694          0.03298055 





