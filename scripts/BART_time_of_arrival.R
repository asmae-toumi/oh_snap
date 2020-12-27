source("scripts/cp_time_of_arrival.R")
# df: df_cp_arrival_filt

library(skimr)
library(BART)

str(df_cp_arrival_filt)

df_cp_arrival_filt <- df_cp_arrival_filt %>% 
  as.data.frame() %>% 
  select(-c(penalty_codes, penalty_jersey_numbers, offense_formation, 
            defenders_in_the_box, type_dropback, pre_snap_home_score,
            pre_snap_visitor_score, n_scores, game_clock, absolute_yardline_number,
            yardline_side)) 

# changing outcome to 0-1
# changing possession team to factor
# changing variables to numeric, logical or factor
# dropping all NA's
df_cp_arrival <- df_cp_arrival_filt %>% 
  mutate(
    pass_result = case_when(
      pass_result == "C" ~ 1, 
      pass_result == "I" ~ 0),
    target_height = as.numeric(target_height), 
    qb_hit = as.factor(qb_hit),
    is_leading = as.factor(is_leading),
    roof = as.factor(roof)
    ) %>% 
  distinct_at(vars(game_id, play_id), .keep_all = T) %>% # remove duplicated rows 
  drop_na()

# prepping data for BART
drop_cols <- drop_cols <- c("game_id", "play_id", "target_nfl_id", "pass_result", "cp", 
                            "possession_team", "home_team_abbr", "visitor_team_abbr", 
                            "air_yards", "play_description", "play_type", "yardline_side", 
                            "yardline_number", "personnel_o", "personnel_d", 
                            "offense_play_result", "play_result", "epa", 
                            "is_defensive_pi", "game_date", "game_time_eastern", "week" )

y <- df_cp_arrival$pass_result
x <- df_cp_arrival[,!colnames(df_cp_arrival) %in% drop_cols]

# BART
bart_fit1 <- pbart(x.train = x, 
                   y.train = y, 
                   sparse = TRUE, 
                   ndpost = 500, 
                   nskip = 2500, 
                   keepevery = 5, 
                   printevery = 500)

save(bart_fit1, file = "data/BART_time_of_arrival/bart_fit1.RData")

bart_fit2 <- pbart(x.train = x, 
                   y.train = y, 
                   sparse = TRUE, 
                   ndpost = 500, 
                   nskip = 2500, 
                   keepevery = 5, 
                   printevery = 500)

save(bart_fit2, file = "data/BART_time_of_arrival/bart_fit2.RData")

# assess the convergence 
library(coda)
rhat <- gelman.diag(mcmc.list(mcmc(bart_fit1$prob.train), mcmc(bart_fit2$prob.train)), multivariate = FALSE)
ess <- effectiveSize(mcmc.list(mcmc(bart_fit1$prob.train), mcmc(bart_fit2$prob.train)))

save(rhat, ess, file = "data/BART_time_of_arrival/bart_fit_diags.RData")

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

trained_plus_phat_time_of_arrival <- cbind(df_cp_arrival, prob_means, low_bound, upp_bound)

save(trained_plus_phat_time_of_arrival, file = "data/BART_time_of_arrival/trained_plus_phat_time_of_arrival.RData")
# probabilities are the "prob_means" variable, 95% is low_bound and upp_bound

# Variable selection

varcount <- rbind(bart_fit1$varcount, bart_fit2$varcount)
varprob <- rbind(bart_fit1$varprob, bart_fit2$varprob)

varcount_mean <- colMeans(varcount)
varcount_sd <- apply(varcount, FUN = sd, MARGIN = 2)

sort(colMeans(varcount), decreasing = TRUE)[1:10]
sort(colMeans(varprob), decreasing = TRUE)[1:10]

# variable with largest posterior mean splitting probability is dist_def1 (23%)
# others: precipitation (9%), dist_traveled (7%), qb_hit (6%), yards_from_sideline (5%),
# time_ball_in_air (4%), dist_def2 (4%), roof (4%), target_height (3%)


