## ----setup, include=FALSE
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)
# Run this with the cursor in the script window to convert to an Rmd
# rstudioapi::getActiveDocumentContext()$path %>% styler::style_file() %>% knitr::spin()

## ----packages
library(tidyverse)
library(janitor)
# source(here::here("scripts", "cleaning.R"))

# # Choose single play (highest EPA) for example.
# example_play <-
#   plays %>%
#   inner_join(games) %>%
#   # arrange(desc(epa)) %>%
#   slice_max(epa) %>% # just found out about this black magic
#   select(week, game_id, play_id, epa, play_description)
# example_play


## ----imports, cache=FALSE
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

## ---------------
ball <- tracking %>% filter(display_name == "Football")

tracking <-
  tracking %>%
  filter(display_name != "Football") %>%
  inner_join(ball %>% select(game_id, play_id, frame_id, ball_x = x, ball_y = y))
tracking

## ---------------
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

## ---------------
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

receivers_at_snap <-
  snap_frames_cleaned %>%
  # QBs get filtered out by `is.na(route)` anyways.
  # filter(position != 'QB' & side == 'O') %>%
  filter(side == 'O') %>%
  inner_join(plays, by = c("game_id", "play_id")) %>%
  group_by(game_id, play_id) %>%
  mutate(idx_y = row_number(y), n = n()) %>%
  ungroup() %>%
  filter(!is.na(route)) %>% 
  group_by(game_id, play_id, y_side) %>% 
  mutate(idx_y_side = case_when(y_side == 'below' ~ row_number(idx_y), TRUE ~ row_number(-idx_y))) %>% 
  ungroup() %>% 
  # Probably don't need all of these columns, but whatev
  select(
    game_id,
    play_id,
    nfl_id,
    display_name,
    position,
    x,
    y,
    y_relative,
    ball_x,
    ball_y,
    dist,
    y_side,
    idx_y_side,
    route,
    play_result
  )
receivers_at_snap

#' What are the most common route pairs?
#' 
#' 1. HITCH-FLAT
#' 2. GO-FLAT
#' 3. GO-OUT
#' 
## ----
# Figure out where TEs usually line up so that we can filter out players in the backfield
# receivers_at_snap %>% filter(position == 'TE') %>% mutate(across(y_relative, ~cut(.x, breaks = seq.int(-8, 8)))) %>% count(y_relative)
# receivers_at_snap <- receivers_at_snap %>% filter(abs(y_relative) > 3)
route_combos <-
  receivers_at_snap %>%
  select(game_id, play_id, y_side, idx_y_side, route) %>% 
  pivot_wider(names_from = idx_y_side, values_from = route, names_prefix = 'rec') %>% 
  unite('route_combo', matches('rec'), sep = '-') %>% 
  mutate(
    # Could sort of use `str_remove_all()` here as well, but this makes it more explicity about removing the "NA" at the front of the string afterwards.
    across(route_combo, ~str_replace_all(.x, c('-NA' = '', '^NA' = ''))),
    n_route = route_combo %>% str_count('-') + 1L
  )
route_combos

route_combos_n <-
  route_combos %>% 
  count(route_combo, n_route, sort = TRUE)
route_combos_n

# Only want the pairs.
# Should probably also consider 3, 4, and even 5 player combos.
route_combos_n_group <- 
  route_combos_n %>% 
  filter(n_route >= 2L, n_route <= 4L) %>% 
  mutate(rnk = row_number(desc(n)))
route_combos_n_group
route_combos_n_group %>% filter(route_combo == 'SLANT-OUT')

# There are plays with 5 receivers on the same side of the field?!?
route_combos_n %>% group_by(n_route) %>% summarize(across(n, sum)) %>% ungroup()
route_combos_n %>% filter(n_route == 5L)

#'
#'
#'
## ----
n_sample <- 20L
route_combos_n_group_sample <- route_combos_n_group %>% head(n_sample)
route_combos_n_group_sample

short_in_routes <- c('SLANT', 'IN')
short_out_routes <- c('FLAT', 'OUT')
short_pick_route_combos <-
  list(
    crossing(
      route1 = short_in_routes,
      route2 = short_out_routes
    ),
    crossing(
      route1 = short_in_routes,
      route2 = short_in_routes,
      route3 = short_out_routes
    ),
    crossing(
      route1 = short_in_routes,
      route2 = short_out_routes,
      route3 = short_in_routes
    )
  ) %>% 
  reduce(bind_rows) %>% 
  unite('route_combo', matches('route'), sep = '-') %>% 
  mutate(
    across(route_combo, ~str_replace_all(.x, c('-NA' = '', '^NA' = '')))
  )
short_pick_route_combos

route_combos_n_group %>% 
  inner_join(short_pick_route_combos)

route_combos_group <- 
  route_combos %>% 
  # filter(route_combo == 'HITCH-HITCH') %>% # debugging
  # only look at the top `n_sample` pairs (by descending count)
  inner_join(route_combos_n_group_sample) %>% 
  mutate(across(route_combo, ~fct_reorder(.x, desc(n)))) %>% 
  select(-n)
route_combos_group

# rejoin back to identify which players were involved in each pair
receivers_at_snap_group <-
  # data including `x`, `y` and `nfl_id`.
  receivers_at_snap %>% 
  # join data including `game_id`, `play_id`, and `y_side` for each pair
  inner_join(route_combos_group)
receivers_at_snap_group

identify_close_receivers <- function(data) {
  data <-
    tibble::tribble(
       ~nfl_id,  ~display_name, ~position,                 ~x,               ~y,            ~dist,  ~route, ~idx_y_side,
      2495454L,  "Julio Jones",      "WR", -0.579999999999998, 9.19333333333334, 17.3351261893301, "HITCH",          1L,
      2533040L, "Mohamed Sanu",      "WR",                  0, 17.1733333333333, 9.33407735129723, "HITCH",          2L
      )
  data <-
    
}

receivers_at_snap_group %>% 
  filter(n_route == 3L) %>% 
  select(game_id, play_id, y_side, route_combo, nfl_id, display_name, position, x, y, dist, route, idx_y_side) %>% 
  nest(data = c(nfl_id, display_name, position, x, y, dist, route, idx_y_side)) %>% 
  slice(1) %>% 
  select(data) %>% 
  unnest(data) %>% 
  clipr::write_clip()

receivers_at_snap_group %>% 
  select(game_id, play_id, y_side, idx_y_side, x, y) %>% 
  pivot_wider(names_from = idx_y_side, values_from = c(x, y), names_prefix = 'rec') %>% 
  

n_game <-
  games %>% 
  filter(week <= !!n_week) %>% 
  inner_join(plays) %>% 
  nrow()
n_game

route_combos_n_group %>% 
  mutate(pct = 100 * n / !!n_game) %>% 
  head(n_sample)


tracking_cleaned_group <-
  tracking_cleaned %>%
  inner_join(
    receivers_at_snap_group %>% 
      select(
        game_id,
        play_id,
        nfl_id,
        route,
        route_combo,
        y_side,
        idx_y_side,
        n_route
      )
  ) %>%
  mutate(across(c(y, ball_y), ~case_when(y_side == 'below' ~ !!y_max - .x, TRUE ~ .x)))
tracking_cleaned_group

tracking_cleaned_group_agg <-
  tracking_cleaned_group %>% 
  # Only first 5 seconds.
  filter(frame_id <= 50) %>% 
  group_by(route_combo, route, idx_y_side, n_route, frame_id) %>% 
  summarize(across(c(x, y), mean)) %>% 
  ungroup()
tracking_cleaned_group_agg


tracking_cleaned_pair_agg %>% 
  mutate(
    grp = sprintf('%s-%s-%s', route_combo, route, idx_y_side)
  ) %>% 
  ggplot() +
  aes(x = x, y = y, group = grp, color = route) +
  geom_path(size = 1.25) +
  coord_cartesian(ylim = c(0 - 5, y_max + 5)) +
  guides(color = FALSE) +
  facet_wrap(~route_combo)

tracking_cleaned_pair %>% 
  mutate(
    grp = sprintf('%s-%s-%s-%s-%s', game_id, play_id, route_combo, route, idx_y_side)
  ) %>% 
  # filter(route == 'GO') %>% 
  ggplot() +
  aes(x = x, y = y, group = grp, color = route) +
  geom_path() +
  facet_wrap(~route_combo)


#' Filter down to just a few of the route combos to make a plot.
#' Here we'll look at the combos with the max number of routes, and just a few cases of these.
## ---------------
# `max(n_route)` is 5 for weeks 1 - 6, but could be up to 6.
route_combos_n %>% filter(n_route == max(n_route))
tracking_max_routes <-
  tracking_cleaned %>% 
  semi_join(
    route_combos %>% 
      filter(n_route == max(n_route)) %>% 
      head(3)
  )
tracking_max_routes

snap_frames_max_routes <- tracking_max_routes %>% filter(event == "ball_snap")
throw_frames_max_routes <- tracking_max_routes %>% filter(event == "pass_forward")
end_frames_max_routes <- tracking_max_routes %>% filter(event %in% events_end_route)
frames_annotate_max_routes <- list(snap_frames_max_routes, throw_frames_max_routes, end_frames_max_routes) %>% reduce(bind_rows)
shapes <- c("ball_snap" = 21, "pass_forward" = 22, set_names(rep(23, length(events_end_route)), events_end_route))

viz_max_routes <-
  tracking_max_routes %>% 
  ggplot() +
  theme_minimal() +
  facet_wrap(~play_id, scales = 'free') %>% 
  aes(x = x, y = y, group = nfl_id, color = side) +
  geom_path(data = tracking_max_routes) +
  geom_point(
    data = frames_annotate_max_routes,
    aes(fill = side, shape = event),
    # shape = 21,
    size = 6,
    color = "black"
  ) +
  ggrepel::geom_label_repel(
    data = 
      end_frames_max_routes %>% 
      filter(!is.na(route)),
    aes(label = route),
    # shape = 21,
    # arrow = arrow(length = unit(0.2, 'npc')),
    hjust = -2,
    size = 5,
    color = "black"
  ) +
  scale_shape_manual(values = shapes) +
  geom_text(
    data = frames_annotate_max_routes,
    aes(label = jersey_number),
    size = 3,
    color = "white"
  )
viz_max_routes

## ---------------
throw_frame <- tracking_cleaned %>% filter(event == "pass_forward")
# Probably need a full list of events for pass arriving...
end_frame <- tracking_cleaned %>% filter(event %in% events_end_route)
frames_annotate <- list(snap_frame, throw_frame, end_frame) %>% reduce(bind_rows)


## ---------------
# field <- gg_field() # Couldn"t get this to work with the rest of the ggplot latyers...
shapes <- c("ball_snap" = 21, "pass_forward" = 22, set_names(rep(23, length(events_end_route)), events_end_route))
viz <-
  # field +
  ggplot() +
  theme_minimal() +
  aes(x = x, y = y, group = nfl_id, color = side) +
  geom_path(data = tracking_cleaned) +
  geom_point(
    data = frames_annotate,
    aes(fill = side, shape = event),
    # shape = 21,
    size = 6,
    color = "black"
  ) +
  ggrepel::geom_label_repel(
    data = end_frame %>% filter(!is.na(route)),
    aes(label = route),
    # shape = 21,
    # arrow = arrow(length = unit(0.2, 'npc')),
    hjust = -2,
    size = 5,
    color = "black"
  ) +
  scale_shape_manual(values = shapes) +
  geom_text(
    data = frames_annotate,
    aes(label = jersey_number),
    size = 3,
    color = "white"
  )
viz
