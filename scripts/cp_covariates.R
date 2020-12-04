library(tidyverse)
library(janitor)
library(arrow)
library(naniar)

plays <- 
  read_csv("data/plays.csv") %>% 
  clean_names() %>% 
  # There are 2 of these. Not sure what to do with them... drop them.
  filter(!is.na(pass_result))

players <- read_csv("data/players.csv") %>%
  clean_names()

targeted <- read_csv("data/targetedReciever.csv") %>% 
  clean_names()

targeted <- targeted %>% 
  left_join(players %>% 
              select(target_nfl_id = nfl_id, 
                     target_height = height, 
                     target_weight = weight)
  ) 

## some player heights still in "6-2" form
conv_height <- function(ht){
  
  if(is.na(ht)){
    return(NA)
  } else if(str_detect(ht,'-')){
    return(sum(as.numeric(str_split(ht,"-",simplify = T))*c(12,1)))
  } else {
    return(as.numeric(ht))
  }
  
}

targeted$target_height <- unlist(sapply(targeted$target_height, conv_height), 
                                 use.names = F)

## read tracking data
all_weeks <-
  read_parquet("data/all_weeks.parquet") %>%
  clean_names() %>%
  select(-week)

## create copy of tracking data
all_merged <- all_weeks


## read in tony's closest defender data 
min_dist <- read_parquet('data/min_dists_features.parquet') %>% clean_names()

## read 2018 nflfastR data from API
pbp18 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds'))


## merge tracking with plays and targets to use for receiver covariates
all_merged <- all_merged %>% inner_join(plays)
all_merged <- all_merged %>% inner_join(targeted)


## initialize data frame with game situation features
df_cp <- plays %>% 
  select(game_id, play_id, pass_result,
         down, yards_to_go, number_of_pass_rushers,
         possession_team, quarter)

## add target id, height, and weight
df_cp <- left_join(df_cp, targeted) 

## subset nflfastR data
pbp18_cp <- pbp18 %>% 
  drop_na(complete_pass) %>% 
  filter(pass_attempt == 1) %>%
  select(game_id = old_game_id, play_id, complete_pass, 
         half_seconds_remaining, qb_hit, air_yards, cp) %>% 
  mutate(game_id = as.numeric(game_id))

# merge nflfastR with BDB data
df_cp <- left_join(df_cp, pbp18_cp)


## add checks for spikes and tipped passes
tracking_filters <- all_merged %>% 
  filter(team=='football') %>% 
  group_by(game_id, play_id) %>% 
  summarise(is_spike = "qb_spike" %in% event,
            is_tip = "pass_tipped" %in% event) 

## add check for penalties
tracking_filters <- left_join(tracking_filters,
            plays %>% 
              mutate(is_penalty = !is.na(penalty_codes)) %>% 
              select(game_id, play_id, is_penalty))

## add check for throw away passes
throw_away <- plays %>% 
  mutate(is_throw_away = str_detect(tolower(play_description),'thrown away') | 
           str_detect(tolower(play_description),'ball away')) %>% 
  select(game_id, play_id, is_throw_away)

tracking_filters <- left_join(tracking_filters, throw_away)

df_cp <- left_join(df_cp, tracking_filters)

## distance to 2 closest defenders
dist_to_closest_def <- min_dist %>% 
  left_join(targeted) %>% 
  filter(nfl_id_o == target_nfl_id, event == 'pass_arrived') %>%
  select(game_id, play_id, dist_def1 = dist_d1_naive, dist_def2 = dist_d2_naive) %>%
  ## have to average over plays with duplicate 'pass_arrived' frames
  group_by(game_id, play_id) %>% 
  summarise(dist_def1 = mean(dist_def1), dist_def2 = mean(dist_def2))

dist_to_closest_def <- min_dist %>% 
  filter(is_target==1, event == 'pass_arrived') %>% 
  select(game_id, play_id, dist_def1 = dist_d1_naive, dist_def2 = dist_d2_naive) %>% 
  ## average over duplicates
  group_by(game_id, play_id) %>% 
  summarise(dist_def1 = mean(dist_def1), dist_def2 = mean(dist_def2))


df_cp <- left_join(df_cp, dist_to_closest_def)
#df_cp <- left_join(df_cp, dist_to_second_closest_def)

## receiver distance from los, sideline at time of pass
receiver_dist <- all_merged %>%
      filter(event=='pass_forward', nfl_id == target_nfl_id) %>% 
      mutate(
        yards_from_los = case_when(
          play_direction == "right" ~ x - absolute_yardline_number,
          play_direction == "left" ~ absolute_yardline_number - x),
      yards_from_sideline = min(abs(c(y - 0, y - 53.33)))) %>%
  select(game_id, play_id, yards_from_los, yards_from_sideline) %>%
  group_by(game_id, play_id) %>% 
  summarise(yards_from_los = mean(yards_from_los),
            yards_from_sideline = mean(yards_from_sideline))

df_cp <- left_join(df_cp, receiver_dist)

## time ball is in the air
pass_events <- c("pass_forward","pass_shovel","pass_arrived")

## add to data frame
time_in_air <- all_merged %>%
      filter(team=='football', event %in% pass_events) %>%
      group_by(game_id, play_id) %>%
      summarise(time_ball_in_air = diff(frame_id)/10) %>% 
      ## can't figure out a way around this
      summarise(time_ball_in_air = max(time_ball_in_air))
          
df_cp <- left_join(df_cp, time_in_air)


## calculate clockwise angle difference
# .angle_diff_cw <- function(a1, a2){
#   (a1 - a2) %% 360
# }

## angle difference between QB and target at time of throw
ang_diff <- all_merged %>% 
  drop_na(target_nfl_id) %>% 
  filter(position == "QB" | nfl_id == target_nfl_id, 
         event %in% c("pass_forward","pass_shovel")) %>% 
  arrange(game_id, play_id, !(position == "QB")) %>%
  group_by(game_id, play_id) %>% 
  summarise(angle_diff = diff(o) %% 360) %>% 
  ## average over duplicate frames...
  summarise(angle_diff = mean(angle_diff))

df_cp <- left_join(df_cp, ang_diff)

## only 9k plays left after dropping all missing - not ideal
df_cp %>% drop_na

## filter out tricky plays
df_cp_filt <- df_cp %>%
  filter(pass_result %in% c("C","I","IN"), is_spike==F, is_penalty==F, 
         air_yards != 0, is_throw_away==F, is_tip==F)

## visualize missing patterns 
df_cp %>%
  vis_miss(sort_miss=F)

## visualize missing patterns after filtering
df_cp_filt %>% 
  select(target_nfl_id, target_height, target_weight, 
         cp, dist_def1, dist_def2, yards_from_los, yards_from_sideline,
         time_ball_in_air, angle_diff) %>% 
  vis_miss(sort_miss=F)



## convert shit to factors for modeling
df_cp$complete_pass <- factor(df_cp$complete_pass)
df_cp$down <- factor(df_cp$down)
df_cp$possession_team <- factor(df_cp$possession_team)

# notes:
## response variable can be complete_pass or pass_result
## make sure to drop cp (nflfastR model) before modeling

write_csv(df_cp, file='data/catch_prob_covariates.csv')


#################################################################
## Outdated code

## sometimes throw or arrival occurs 'between' two frames, 
### game_id == 2018091604, play_id == 2568, or game_id == 2018111106, play_id == 2650
## laterals and backwards passes can occur, even multiple times in a play
### e.g. game_id == 2018110410, play_id == 3640
## play action

## if pass arrival occurs twice but spaced apart, it could be a tip!
## e.g game_id == 2018110402, play_id == 1544

# dup_passes <- all_merged %>%
#   filter(team=='football', event %in% c('pass_forward','pass_shovel','pass_arrived')) %>%
#   group_by(game_id, play_id) %>%
#   summarise(time_ball_in_air = diff(frame_id)/10) %>% count(game_id, play_id) %>% filter(n > 1)
# 
# dup_passes$reason <- c("arrival takes two frames","tipped ball = 2 arrivals",
#                        "lateral = 2 arrivals", "throw takes two frames",
#                        "lateral = 2 arrivals","lateral = 2 arrivals",
#                        "arrival takes two frames","arrival takes 2 frames")

## (he said this is outdated and he will send us new stuff soon)
#closest_def <- read_rds('data/closest_defenders_at_events.rds')
## example of closest defender to target receiver at time of pass
# closest_def %>% 
#   filter(game_id == 2018090600, play_id == 75) %>% 
#   unnest(min_distances) %>% 
#   select(-is_bad) %>% 
#   filter(nfl_id_o == 2495454, event == 'pass_forward')

## lots of passes never "arrive", because they are inaccurate
## distance to closest defender at pass arrival
# dist_to_closest_def <- 
#   closest_def %>% left_join(targeted) %>% 
#   unnest(min_distances) %>% 
#   select(-is_bad) %>% 
#   filter(nfl_id_o == target_nfl_id, event == 'pass_arrived', 
#          idx_closest == 1) %>%
#   select(game_id, play_id, dist_def1 = dist)

## distance to 2nd closest defender at pass arrival
# dist_to_second_closest_def <- 
#   closest_def %>% left_join(targeted) %>% 
#   unnest(min_distances) %>% 
#   select(-is_bad) %>% 
#   filter(nfl_id_o == target_nfl_id, event == 'pass_arrived', 
#          idx_closest == 2) %>%
#   select(game_id, play_id, dist_def2 = dist)

# ang_data <- all_merged %>% drop_na(target_nfl_id) %>% 
#   filter(position == "QB" | nfl_id == target_nfl_id, 
#          event == "pass_forward")
# 
# ang_diff <- ang_data %>% 
#   arrange(game_id, play_id, !(position == "QB")) %>%
#   group_by(game_id, play_id) %>% 
#   summarise(angle_diff = diff(o) %% 360) 