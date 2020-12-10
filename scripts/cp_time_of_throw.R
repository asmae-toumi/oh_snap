library(tidyverse)
library(janitor)
library(arrow)
library(naniar)

plays <- read_csv("data/plays.csv") %>% 
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
all_weeks <- read_parquet("data/all_weeks.parquet") %>%
  clean_names() %>%
  select(-week)

## create copy of tracking data
all_merged <- all_weeks

## merge tracking with plays and targets to use for receiver covariates
all_merged <- all_merged %>% inner_join(plays)
all_merged <- all_merged %>% inner_join(targeted)


## read in tony's closest defender data 
min_dist <- read_parquet('data/min_dists_features.parquet') %>% 
            clean_names()

## read 2018 nflfastR data from API
pbp18 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds'))

## subset nflfastR data
pbp18_cp <- pbp18 %>% 
  drop_na(complete_pass) %>% 
  filter(pass_attempt == 1) %>%
  select(game_id = old_game_id, play_id, roof, 
         half_seconds_remaining, qb_hit, air_yards, cp) %>% 
  mutate(game_id = as.numeric(game_id))


## initialize data frame with game situation features
df_cp_throw <- plays %>% 
         select(game_id, play_id, pass_result,
         down, yards_to_go, number_of_pass_rushers,
         possession_team, quarter)

## add target id, height, and weight
df_cp_throw <- left_join(df_cp_throw, targeted) 

# merge nflfastR with BDB data
df_cp_throw <- left_join(df_cp_throw, pbp18_cp)


## define pass events
throw_events <- c("pass_forward","pass_shovel")

## distance to 2 closest defenders
dist_to_closest_def <- min_dist %>% 
  filter(is_target==1, event %in% throw_events) %>% 
  select(game_id, play_id, dist_def1 = dist_d1_naive, 
         dist_def2 = dist_d2_naive, dist_qb) %>% 
  ## average over duplicates
  group_by(game_id, play_id) %>% 
  summarise(dist_def1 = mean(dist_def1), 
            dist_def2 = mean(dist_def2), 
            dist_qb = mean(dist_qb))


df_cp_throw <- left_join(df_cp_throw, dist_to_closest_def)


## receiver distance from los
receiver_dist_los <- all_merged %>%
  filter(event %in% throw_events, nfl_id == target_nfl_id) %>% 
  mutate(
    yards_from_los = case_when(
      play_direction == "right" ~ x - absolute_yardline_number,
      play_direction == "left" ~ absolute_yardline_number - x)
  ) %>%
  select(game_id, play_id, yards_from_los) %>%
  ## average over duplicates
  group_by(game_id, play_id) %>% 
  summarise(yards_from_los = mean(yards_from_los))

df_cp_throw <- left_join(df_cp_throw, receiver_dist_los)

## receiver distance from sideline
receiver_dist_sideline <-  all_merged %>%
            filter(event %in% throw_events, 
                   nfl_id == target_nfl_id) %>% 
            group_by(game_id, play_id) %>% 
            summarise(yards_from_sideline = 
                        min(abs(c(y - 0, y - 53.33))))              

df_cp_throw <- left_join(df_cp_throw, receiver_dist_sideline)


## angle difference between QB and target at time of throw
ang_diff <- all_merged %>% 
  drop_na(target_nfl_id) %>% 
  filter(position == "QB" | nfl_id == target_nfl_id, 
         event %in% throw_events) %>% 
  arrange(game_id, play_id, !(position == "QB")) %>%
  group_by(game_id, play_id) %>% 
  summarise(angle_diff = diff(o) %% 360) %>% 
  ## average over duplicate frames...
  summarise(angle_diff = mean(angle_diff))

df_cp_throw <- left_join(df_cp_throw, ang_diff)


## add checks for spikes and tipped passes
tracking_filters <- all_merged %>% 
  filter(team=='football') %>% 
  group_by(game_id, play_id) %>% 
  summarise(is_spike = "qb_spike" %in% event,
            is_tip = "pass_tipped" %in% event) 

## add check for penalties (could probably keep some of these...)
penalty_filter <- plays %>% 
                    mutate(is_penalty = !is.na(penalty_codes)) %>% 
                    select(game_id, play_id, is_penalty)

tracking_filters <- left_join(tracking_filters, penalty_filter)

## add check for throw away passes
throw_away_filter <- plays %>% 
  mutate(is_throw_away = str_detect(tolower(play_description),'thrown away') | 
           str_detect(tolower(play_description),'ball away')) %>% 
  select(game_id, play_id, is_throw_away)

tracking_filters <- left_join(tracking_filters, throw_away_filter)

df_cp_throw <- left_join(df_cp_throw, tracking_filters)


## filter out tricky plays
df_cp_throw_filt <- df_cp_throw %>%
  filter(pass_result %in% c("C","I","IN"), 
         is_spike==F, is_penalty==F, 
         air_yards != 0, is_throw_away==F, is_tip==F) %>%
  ## remove any remaining plays with missing targets
  drop_na(target_nfl_id)


## visualize missingness
df_cp_throw_filt %>% 
  select(target_nfl_id, target_height, target_weight, number_of_pass_rushers,
         dist_def1, dist_def2, dist_qb, yards_from_los, yards_from_sideline,
          angle_diff, half_seconds_remaining) %>% 
  vis_miss(sort_miss=F)


## select variables for modeling (do some need to be factors?)
df_cp_throw_filt %>% 
  select(quarter, down, yards_to_go, number_of_pass_rushers, 
         target_height, target_weight, dist_def1, dist_def2, 
         dist_qb, yards_from_los, yards_from_sideline, 
         angle_diff, half_seconds_remaining, pass_result) %>%
  ## recode response variable
  mutate(pass_result = 
           recode_factor(pass_result, "C" = 1, "IN" = 0, "I" = 0)
         ) %>%
  ## still ~120 rows with missing data ¯\_(ツ)_/¯
  drop_na()

