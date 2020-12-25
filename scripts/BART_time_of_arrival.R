source("scripts/cp_time_of_arrival.R")
# df: df_cp_arrival_filt

library(skimr)
library(BART)
library(tidymodels)

str(df_cp_arrival_filt)

df_cp_arrival_filt <- df_cp_arrival_filt %>% as.data.frame()

# changing outcome to 0-1
# changing possession team to factor
# changing variables to numeric, logical or factor
# dropping all NA's
df_cp_arrival <- df_cp_arrival_filt %>% 
  mutate(
    pass_result = case_when(
      pass_result == "C" ~ 1, 
      pass_result == "I" ~ 0),
    possession_team = as.factor(possession_team),
    target_height = as.numeric(target_height), 
    qb_hit = as.factor(qb_hit),
    roof = as.factor(roof)
    ) %>% 
  drop_na()

# splitting into training and test set 
# set.seed(1353)
# df_split <- initial_split(df_cp_arrival)
# train_data <- training(df_split)
# test_data <- testing(df_split)

# prepping data for BART
drop_cols <- c("game_id", "play_id", "target_nfl_id", "pass_result", "cp")

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

# Posterior means of probability 

prob_train1 <- bart_fit1$prob.train
prob_train2 <- bart_fit2$prob.train
prob_train <- rbind(prob_train1, prob_train2)

prob_means <- apply(prob_train, mean, MAR=2)

trained_plus_phat_time_of_arrival <- cbind(df_cp_arrival, prob_means)

save(trained_plus_phat_time_of_arrival, file = "data/BART_time_of_arrival/trained_plus_phat_time_of_arrival.RData")
# probabilities are the "prob_means" variable 

# Variable selection

varcount <- rbind(bart_fit1$varcount, bart_fit2$varcount)
varprob <- rbind(bart_fit1$varprob, bart_fit2$varprob)

varcount_mean <- colMeans(varcount)
varcount_sd <- apply(varcount, FUN = sd, MARGIN = 2)

sort(colMeans(varcount), decreasing = TRUE)[1:10]
sort(colMeans(varprob), decreasing = TRUE)[1:10]

## without team variable: 
# variable with largest posterior mean splitting probability is dist_def1 (22.3%)
# others: roof (7%), yards_from_sideline (6%), precipitation (6%), time_ball_in_air (6%),
# target_weight (5%), dist_def2 (5%), yards_from_los (4%), dist_qb (4%), qb_hit2 (4%) 
