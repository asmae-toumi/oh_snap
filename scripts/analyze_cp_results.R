library(tidyverse)
library(janitor)
library(arrow)
library(gt)
library(kableExtra)

## load data from BDB
plays <- read_csv("data/plays.csv") %>% 
  clean_names() %>% 
  filter(!is.na(pass_result))

players <- read_csv("data/players.csv") %>%
  clean_names()

targeted <- read_csv("data/targetedReciever.csv") %>% 
  clean_names()

targeted <- targeted %>% 
  left_join(players %>% 
              select(target_nfl_id = nfl_id)
  ) 

## read tracking data
all_weeks <- read_parquet("data/all_weeks.parquet") %>%
  clean_names() %>%
  select(-week)

## create copy of tracking data
all_merged <- all_weeks

## merge tracking with plays and targets to use for receiver covariates
all_merged <- all_merged %>% inner_join(plays)
all_merged <- all_merged %>% inner_join(targeted)


## load results from bothBART models
load("data/BART_time_of_throw/trained_plus_phat_time_of_throw.Rdata")
load("data/BART_time_of_arrival/trained_plus_phat_time_of_arrival.Rdata")

## join CP predictions by game and play ids
cp_joined <- 
  inner_join(
    trained_plus_phat_time_of_throw %>% 
      select(game_id, play_id, pass_result, cp, throw_prob = prob_means), 
    trained_plus_phat_time_of_arrival %>% 
      select(game_id, play_id, arrival_prob = prob_means)) %>% 
  mutate(gameplay_id = paste(game_id, play_id, sep='_'))

## remove a few duplicate plays - literally no idea how they got here
cp_joined_nodups <- cp_joined %>% distinct(game_id, play_id, .keep_all=T)


## merge QB for each play
plays_qb <- all_merged %>% 
  filter(position=='QB', event=='ball_snap') %>% 
  select(game_id, play_id, nfl_id) %>% 
  left_join(players) %>% 
  select(game_id, play_id, display_name)

## merge target receiver for each play
plays_target <- all_merged %>% 
  filter(nfl_id == target_nfl_id, event=='ball_snap') %>% 
  select(game_id, play_id, nfl_id) %>% 
  left_join(players) %>% 
  select(game_id, play_id, display_name)


## cpoe for QBs
left_join(cp_joined_nodups, plays_qb) %>% 
  group_by(display_name) %>% 
  summarise(npasses = n(), comp_pct = mean(pass_result == 1),
            cpoe_qb_throw = sum(pass_result - throw_prob), 
            cpoe_qb_arr = sum(pass_result - arrival_prob)) %>% 
  arrange(-cpoe_qb_throw)

## cpoe for targeted receivers
left_join(cp_joined_nodups, plays_target) %>% 
  group_by(display_name) %>% 
  summarise(ncatches = n(), catch_pct = mean(pass_result == 1),
            cpoe_target_throw = sum(pass_result - throw_prob), 
            cpoe_target_arr = sum(pass_result - arrival_prob)) %>% 
  arrange(-cpoe_target_arr)



## find offensive team (home or away) for each play
offense <- all_merged %>%
  filter(position=='QB', event=='ball_snap') %>% 
  select(game_id,play_id, offense=team) %>% 
  distinct(game_id, play_id, .keep_all=T)


## play ids for CP dataset (n=12806)
has_target <- which(plays$gameplay_id %in% cp_joined_nodups$gameplay_id)


## filter tracking data to prep for distance to receiver calc
dist_df_arr <- all_merged %>% 
  filter(event == 'pass_arrived', team!='football') %>% 
  mutate(gameplay_id = paste(game_id, play_id, sep='_')) %>% 
  filter(gameplay_id %in% cp_joined_nodups$gameplay_id) %>% 
  left_join(offense) %>% 
  mutate(side_of_ball= ifelse(team==offense, 'offense','defense')) %>% 
  filter(nfl_id == target_nfl_id | side_of_ball=='defense') %>% 
  select(game_id, play_id, nfl_id, x, y, side_of_ball)

## fix duplicate frames
dist_df_arr <- dist_df_arr %>% 
  group_by(game_id, play_id, nfl_id) %>% 
  summarise(x=mean(x), y=mean(y), side_of_ball = side_of_ball[1])


dist_list_arrival <- vector("list", length(has_target))

## loop through each play, calculate distance from defenders
for(j in 1:length(has_target)){
  
  ## subset data
  playdf <- dist_df_arr %>% 
    filter(game_id == plays$game_id[has_target[j]],
           play_id == plays$play_id[has_target[j]]) %>% 
    arrange(desc(side_of_ball))
  
  ## count defenders
  ndef <- playdf %>% 
    filter(side_of_ball=='defense') %>% nrow
  
  ## calculate all euclidean distances
  dist_vec <- fields::rdist(playdf[1,c("x","y")],
                            playdf[-1,c("x","y")]) %>% as.vector
  
  ## use IDs as labels
  names(dist_vec) <- playdf$nfl_id[-1]
  
  ## inverse squared distance, normalized by total
  dist_list_arrival[[j]] <- (1/(dist_vec^2)) / sum(1/(dist_vec^2))
  
  ## print updates every 100 plays
  if(j %% 100 ==0){
    print(j)
  }
  
}


## dCPOE at time of arrival
dist_list_tmp <- vector("list", length(has_target))

for(k in 1:length(has_target)){
  
  dist_list_tmp[[k]] <- dist_list_arrival[[k]] * 
    (cp_joined_nodups[k,'pass_result'] - 
       cp_joined_nodups[k,"arrival_prob"])

}

## weighted_res_arrival w_arr * (pass_result - cp_arr)
weighted_res_arrival <- 
  aggregate(unlist(dist_list_tmp), 
            by=list(names(unlist(dist_list_tmp))), 
            FUN=sum) %>% 
  rename(nfl_id = Group.1, dCPOE = x) %>% 
  mutate(nfl_id = as.numeric(nfl_id)) %>%
  left_join(players) %>% 
  select(nfl_id, display_name, position, dCPOE)


## define position groups
position_groups <- data.frame(
  position = c('DB','CB','S','SS','FS','ILB','OLB','MLB','LB'),
  grp = c('CB','CB','S','S','S','LB','LB','LB','LB')
)

## save rankings to csv
weighted_res_arrival %>% 
  mutate(overall_rank = dense_rank(dCPOE)) %>%
  left_join(position_groups) %>%
  group_by(grp) %>%
  mutate(group_rank = dense_rank(dCPOE)) %>%
  arrange(overall_rank) %>%
  write_csv(file = 'data/catch_prob/dCPOE_final_rankings.csv')


## model evaluation - accuracy, brier score
cp_joined_nodups %>% 
  summarise(brier_nflfastr = mean((pass_result - cp)^2),
            brier_throw = mean((pass_result - throw_prob)^2),
            brier_arrival = mean((pass_result - arrival_prob)^2),
  acc_nflfastr = mean(pass_result == round(cp)),
            acc_throw = mean(pass_result == round(throw_prob)),
            acc_arrival = mean(pass_result == round(arrival_prob))
  )

## roc_auc
bind_rows(
  cp_joined_nodups %>% 
   yardstick::roc_auc(factor(pass_result), cp) %>% mutate(.metric='auc_nfl'),
  cp_joined_nodups %>% 
    yardstick::roc_auc(factor(pass_result), throw_prob) %>%
    mutate(.metric='auc_throw'),
  cp_joined_nodups %>% 
    yardstick::roc_auc(factor(pass_result), arrival_prob) %>%
    mutate(.metric='auc_arrival')
) %>%
  mutate(.estimate = 1 - .estimate)



## dCPOE_arr, top 15 players by position group
weighted_res_arrival %>% 
  left_join(position_groups) %>% 
  filter(!is.na(grp)) %>%
  group_by(grp) %>% 
  top_n(n = 15, -impact) %>% 
  arrange(grp, impact) %>% 
  select(display_name, grp, dCPOE_arr = impact)
 #%>% filter(grp=='LB')


## create table df
table_df <- data.frame(
  model=c('nflfastR','TOT','TOA','nflfastR','TOT','TOA'),
  pass_result = c(0,0,0,1,1,1)
)

## compute density of CP predictions by model and pass outcome
table_df$density <- list(
  density(cp_joined_nodups %>% filter(pass_result==0) %>% pull(cp),from=0,to=1),
  density(cp_joined_nodups %>% filter(pass_result==0) %>% pull(throw_prob),from=0,to=1),
  density(cp_joined_nodups %>% filter(pass_result==0) %>% pull(arrival_prob),from=0,to=1),
  density(cp_joined_nodups %>% filter(pass_result==1) %>% pull(cp),from=0,to=1),
  density(cp_joined_nodups %>% filter(pass_result==1) %>% pull(throw_prob),from=0,to=1),
  density(cp_joined_nodups %>% filter(pass_result==1) %>% pull(arrival_prob),from=0,to=1)
)

## create long table with all density points
table_df2 <- data.frame()

for(i in 1:6){
  tmp <- table_df$density[[i]]
  
  table_df2 <- bind_rows(
    table_df2, 
    data.frame(model=table_df$model[i], 
               pass_result=table_df$pass_result[i],
               x=tmp$x, y=tmp$y)
  )
}

## CP model evaluation metrics
table_metrics <- data.frame(model=c('nflfastR','TOT','TOA'),
                            acc=c("76.4%","78.0%","81.3%"),
                            brier=c(0.166, 0.153, 0.133),
                            auc=c(0.708, 0.762, 0.807))

## merge and build table with plots
gtab <- left_join(table_metrics, table_df2 %>% 
            group_by(pass_result, model) %>%
            summarize(data = list(y), .groups = "drop") %>% 
            mutate(
              plot = map(data, 
                         ~spec_plot(.x,ylim=c(0,7.5),polymin=0,
                                    width = 300, height = 150, col='#ffa3af', 
                                    minmax=list(col="#132f3c",pch='.',cex=2))),
              plot = map(plot, "svg_text"),
              plot = map(plot, gt::html)
            ) %>% 
            select(-data) %>% 
            pivot_wider(
              names_from='pass_result', 
              names_prefix='outcome = ',values_from='plot')) %>% 
  rename(Model = model, Accuracy = acc, 'Brier \nScore' = brier, 
         'AUC'=auc, 'CP | Incomplete'='outcome = 0', 
         'CP | Completed' ='outcome = 1' ) %>% 
  gt(auto_align = 'c') 

gtab <- gtab %>% 
  tab_header(title = 'Catch Probability Model Performance') %>% 
  tab_source_note(
    source_note = 'Models evaluated on n = 12,806 matching plays') %>% 
  tab_style(style=cell_text(weight = 'bold'), 
            locations=cells_column_labels(1:6))

gtsave(gtab, file='figs/cp_model_evaluation_table.png')
