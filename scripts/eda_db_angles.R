

library(tidyverse)
library(janitor)

positions <- read_csv(here::here("data/positions.csv"))
# tracking <- read_csv(here::here("data/highest_epa_example_data.csv"))
# tracking <- read_csv(here::here("data/week1.csv")) %>% clean_names()
n_week <- 6L
tracking <- 
  arrow::read_parquet(here::here("data/all_weeks.parquet")) %>% 
  janitor::clean_names()  %>%
  mutate(across(week, ~str_remove(.x, 'week') %>% as.integer())) %>% 
  filter(week <= n_week) %>% 
  left_join(positions %>% select(position, side))

games <-
  read_csv(here::here("data/games.csv")) %>%
  janitor::clean_names() %>%
  mutate(game_date = lubridate::mdy(game_date))

plays <-
  read_csv(here::here("data/plays.csv")) %>%
  janitor::clean_names() %>%
  filter(!is.na(pass_result))

ball <- tracking %>% filter(display_name == "Football")

tracking <-
  tracking %>%
  filter(display_name != "Football") %>%
  inner_join(ball %>% select(game_id, play_id, frame_id, ball_x = x, ball_y = y))
tracking

snap_frames <- tracking %>% filter(event == "ball_snap")
# Reference: https://github.com/danichusfu/RouteIdentification/blob/master/R/read_routes_from_csv.R
play_direction <-
  snap_frames %>%
  group_by(game_id, play_id, team) %>%
  summarise(mean_team = mean(x)) %>%
  filter(mean_team == max(mean_team)) %>%
  select(game_id, play_id, direction_left = team, -mean_team) %>% 
  ungroup()

line_of_scrimmage <-
  snap_frames %>%
  group_by(game_id, play_id, team) %>%
  summarise(right_scrim = max(x), left_scrim = min(x)) %>%
  ungroup()

possession <-
  plays %>%
  select(game_id, play_id, possession_team) %>%
  left_join(games, by = "game_id") %>%
  mutate(possession = if_else(possession_team == home_team_abbr, "home", "away")) %>%
  select(game_id, play_id, possession)

tracking <-
  tracking %>%
  # head(1000) %>% 
  left_join(play_direction, by = c("game_id", "play_id")) %>%
  left_join(line_of_scrimmage, by = c("team", "game_id", "play_id")) %>%
  left_join(possession, by = c("game_id", "play_id")) %>% 
  # filter(team == possession) %>% # To get the offense only
  mutate(los = if_else(team == direction_left, left_scrim, right_scrim)) %>% 
  select(-matches('_scrim$'), possession)
tracking

# Filter out stuff before the snap and after the pass has arrived.
# My attempt at making a full list of events where the route should be considered over.
# This is like https://github.com/danichusfu/RouteIdentification/blob/master/R/cut_plays.R.
events_end_route <- c(
  # "None",
  # "ball_snap",
  # "pass_forward",
  # "pass_arrived",
  "pass_outcome_caught",
  # "first_contact",
  # "tackle",
  "pass_outcome_incomplete",
  # "play_action",
  # "out_of_bounds",
  # "line_set",
  "qb_sack",
  # "man_in_motion",
  "pass_outcome_interception",
  # "touchdown",
  # "pass_tipped",
  # "fumble",
  "pass_outcome_touchdown",
  "qb_strip_sack",
  # "fumble_defense_recovered",
  # "fumble_offense_recovered",
  # "shift",
  # "handoff",
  # "pass_shovel",
  # "penalty_flag",
  # "run",
  "qb_spike" # ,
  # "touchback",
  # "field_goal_blocked",
  # "penalty_accepted"
)

# Probably should make this a function to be used anywhere.
compute_dist <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# hashmark_constant <- (70 * 12 + 9) / 36
x_max <- 120
y_max <- 160 / 3
# When `group = 0`, it's pre-snap.
# When `group = 2`, it's post-catch.
tracking_cleaned <-
  tracking %>%
  # head(300) %>% 
  group_by(game_id, play_id, nfl_id) %>%
  mutate(
    group = case_when(
      event == "ball_snap" ~ 1L,
      lag(event) %in% events_end_route ~ 1L,
      TRUE ~ 0L
    ),
    group = cumsum(group)
  ) %>%
  ungroup() %>%
  filter(group == 1L) %>%
  select(-group) %>% 
  mutate(
    direction = team == direction_left,
    across(c(x, ball_x), ~if_else(direction, !!x_max - .x, .x)),
    across(c(x, ball_x), ~if_else(direction, .x - (!!x_max - los), .x - los)),
    across(c(y, ball_y), ~if_else(direction, !!y_max - .x, .x)),
    y_relative = y - ball_y,
    dist = compute_dist(x, y, ball_x, ball_y),
    # dist_relative = compute_dist(x, y_relative, ball_x, 0),
    # y_half = case_when(y <= y_max / 2 ~ "bottom", TRUE ~ "top"),
    # y_third = case_when(y <= !!hashmark_constant ~ "bottom", y > (!!y_max - !!hashmark_constant) ~ "top", TRUE ~ "mid")
    y_side = case_when(y < ball_y ~ 'below', TRUE ~ 'above')
  ) %>% 
  select(-direction)
tracking_cleaned
# tracking %>% head(10)
snap_frames_cleaned <- tracking_cleaned %>% filter(event == "ball_snap")
snap_frames_cleaned
