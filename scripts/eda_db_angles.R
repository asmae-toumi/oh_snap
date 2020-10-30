

library(tidyverse)
source('scripts/read_files.R')
source('scripts/animate_play.R')
source('scripts/pick_play.R')

games <- read_games()
plays <- read_plays()
positions <- read_positions()
tracking <- read_tracking(max_week = 1L, positions = positions, drop_cols = TRUE, cols = c('time', 'display_name'))
tracking
tracking %>% arrange(desc(a))
one_play <-
  tracking %>% 
  filter(game_id == 2018090901, play_id == 5369, nfl_id == 2555540)
one_play

events_pass_outcome <- sprintf('pass_outcome_%s', c('caught', 'incomplete', 'interception', 'touchdown'))
events_throw <- c('pass_forward', 'pass_shovel')
tracking_clipped_at_pass_outcome <- tracking %>% clip_tracking_at_events(events = events_pass_outcome)
tracking_clipped_at_pass_outcome

# 1 - 359 -> 2 # o - o_lag5 + 360
# 359 - 1 -> -2 # o - o_lag5 - 360
o_changes <-
  tracking_clipped_at_pass_outcome %>% 
  filter(game_id == dplyr::first(game_id)) %>% 
  # filter(play_id == dplyr::first(play_id)) %>% 
  arrange(game_id, play_id, nfl_id, frame_id) %>% 
  group_by(game_id, play_id, nfl_id) %>% 
  mutate(o_lag1 = dplyr::lag(o, n = 1L), dir_lag1 = dplyr::lag(dir, n = 1L)) %>% 
  ungroup() %>% 
  mutate(
    # o_diff1 = o - o_lag1,
    o_diff1 = case_when(
      o <= 90 & o_lag1 >= 270 ~ (o - o_lag1 + 360),
      o >= 270 & o_lag1 <= 90 ~ (o - o_lag1 - 360),
      TRUE ~ o - o_lag1
    ),
    dir_diff1 = case_when(
      dir <= 90 & dir_lag1 >= 270 ~ (dir - dir_lag1 + 360),
      dir >= 270 & dir_lag1 <= 90 ~ (dir - dir_lag1 - 360),
      TRUE ~ dir - dir_lag1
    )
  ) %>% 
  arrange(desc(abs(o_diff1))) %>% 
  relocate(o, o_lag1, o_diff1, dir, dir_lag1, dir_diff1)
o_changes

o_changes %>% 
  filter(!is.na(o_lag1)) %>% 
  select(o_diff1, dir_diff1) %>% 
  head(1000) %>% 
  # sample_frac(0.1) %>% 
  ggplot() +
  aes(x = o_diff1, y = dir_diff1) +
  geom_point()

o_changes %>% 
  filter(!is.na(o_lag1)) %>% 
  select(o_diff1, dir_diff1) %>% 
  corrr::correlate()
o_changes %>% select(dir) %>% skimr::skim()
o_changes %>% drop_na() %>% ggplot() + aes(x = o_diff1) + geom_histogram(binwidth = 30)
