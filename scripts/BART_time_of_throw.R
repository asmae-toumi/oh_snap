source("scripts/cp_time_of_throw.R")
# df: df_cp_throw_filt

library(skimr)
library(BART)

str(df_cp_throw_filt)

df_cp_throw_filt <- df_cp_throw_filt %>% as.data.frame()

# changing outcome to 0-1
# changing possession team to factor
# changing variables to numeric, logical or factor
# dropping all NA's
df_cp_throw <- df_cp_throw_filt %>% 
  mutate(
    pass_result = case_when(
      pass_result == "C" ~ 0, 
      pass_result == "I" ~ 1),
    possession_team = as.factor(possession_team),
    target_height = as.numeric(target_height), 
    roof = as.factor(roof)) %>% 
  drop_na()

# prepping data for BART
drop_cols <- c("game_id", "play_id", "target_nfl_id", "pass_result", "cp")

y <- df_cp_throw$pass_result
x <- df_cp_throw[,!colnames(df_cp_throw) %in% drop_cols]

# BART
bart_fit1 <- pbart(x.train = x, 
                   y.train = y, 
                   sparse = TRUE, 
                   ndpost = 500, 
                   nskip = 2500, 
                   keepevery = 5, 
                   printevery = 500)

saveRDS(bart_fit1, file = "data/BART_time_of_throw/bart_fit1.RDS")


bart_fit2 <- pbart(x.train = x, 
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

# Posterior means of probability 

prob_train1 <- bart_fit1$prob.train
prob_train2 <- bart_fit2$prob.train
prob_train <- rbind(prob_train1, prob_train2)

prob_means <- apply(prob_train, mean, MAR=2)

trained_plus_phat <- cbind(df_cp_throw, prob_means)

save(trained_plus_phat, file = "data/BART_time_of_throw/trained_plus_phat_time_of_throw.RData")
# probabilities are the "prob_means" variable 


