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
## (he said this is outdated and he will send us new stuff soon)
closest_def <- read_rds('data/closest_defenders_at_events.rds')

## read 2018 nflfastR data from API
pbp18 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds'))


## merge tracking with plays and targets to use for receiver covariates
all_merged <- all_merged %>% inner_join(plays)
all_merged <- all_merged %>% inner_join(targeted)


pbp18_cp <-  pbp18 %>% 
  drop_na(complete_pass) %>% 
  filter(pass_attempt == 1) %>%
  select(game_id = old_game_id, play_id, complete_pass, 
         half_seconds_remaining, qb_hit, cp) %>% 
  mutate(game_id = as.numeric(game_id))


## initialize data frame with game situation features
df_cp <- plays %>% 
  select(game_id, play_id, pass_result,
         down, yards_to_go, number_of_pass_rushers,
         possession_team, quarter)

## add target id, height, and weight
df_cp <- left_join(df_cp, targeted) 

## merge nflfastR with BDB
df_cp <- left_join(df_cp, pbp18_cp)


## example of closest defender to target receiver at time of pass
closest_def %>% 
  filter(game_id == 2018090600, play_id == 75) %>% 
  unnest(min_distances) %>% 
  select(-is_bad) %>% 
  filter(nfl_id_o == 2495454, event == 'pass_forward')

## lots of passes never "arrive", because they are inaccurate
## distance to closest defender at pass arrival
dist_to_closest_def <- 
  closest_def %>% left_join(targeted) %>% 
  unnest(min_distances) %>% 
  select(-is_bad) %>% 
  filter(nfl_id_o == target_nfl_id, event == 'pass_arrived', 
         idx_closest == 1) %>%
  select(game_id, play_id, dist_def1 = dist)

## distance to 2nd closest defender at pass arrival
dist_to_second_closest_def <- 
  closest_def %>% left_join(targeted) %>% 
  unnest(min_distances) %>% 
  select(-is_bad) %>% 
  filter(nfl_id_o == target_nfl_id, event == 'pass_arrived', 
         idx_closest == 2) %>%
  select(game_id, play_id, dist_def2 = dist)

df_cp <- left_join(df_cp, dist_to_closest_def)
df_cp <- left_join(df_cp, dist_to_second_closest_def)

## receiver distance from los, sideline at time of pass
df_cp <- df_cp %>% 
  left_join(
    all_merged %>%
      filter(event=='pass_forward', nfl_id == target_nfl_id) %>% 
      mutate(
        yards_from_los = case_when(
          play_direction == "right" ~ x - absolute_yardline_number,
          play_direction == "left" ~ absolute_yardline_number - x),
      yards_from_sideline = min(abs(c(y - 0, y - 53.33)))) %>%
  select(game_id, play_id, yards_from_los, yards_from_sideline)
)

## time ball is in the air
pass_events <- c("pass_forward","pass_shovel","pass_arrived")

## add to data frame
df_cp <- df_cp %>% 
          left_join( 
            all_merged %>%
              filter(team=='football', event %in% pass_events) %>%
              group_by(game_id, play_id) %>%
              summarise(time_ball_in_air = diff(frame_id)/10) %>% 
              ## can't figure out a way around this
              summarise(time_ball_in_air = max(time_ball_in_air))
          )

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


## calculate clockwise angle difference
.angle_diff_cw <- function(a1, a2){
  (a1 - a2) %% 360
}

## angle difference
ang_data <- all_merged %>% drop_na(target_nfl_id) %>% 
  filter(position == "QB" | nfl_id == target_nfl_id, 
         event == "pass_forward")

ang_diff <- ang_data %>% 
  arrange(game_id, play_id, !(position == "QB")) %>%
  group_by(game_id, play_id) %>% 
  summarise(angle_diff = diff(o) %% 360) 

df_cp <- left_join(df_cp, ang_diff)

## visualize missing patterns (drop variables with no missing data)
df_cp %>% 
  select(-game_id, -play_id, -down, -pass_result,
         -yards_to_go, -possession_team, -quarter) %>% 
  vis_miss(sort_miss=F)

## only 5k plays left after dropping all missing - not good!!
df_cp %>% drop_na


## convert shit to factors for modeling
df_cp$complete_pass <- factor(df_cp$complete_pass)
df_cp$down <- factor(df_cp$down)
df_cp$possession_team <- factor(df_cp$possession_team)

# notes:
## response variable can be complete_pass or pass_result
## make sure to drop cp (nflfastR model) before modeling

write_csv(df_cp, file='data/catch_prob_covariates.csv',)