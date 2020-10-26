
library(tidyverse)
source("scripts/read_files.R")
source("scripts/animate_play.R")

positions <- read_positions()
games <- read_games()
plays <- read_plays()

# n_week <- 6L
tracking <- 
  # read_all_weeks() %>% 
  # mutate(across(week, ~str_remove(.x, 'week') %>% as.integer())) %>% 
  # filter(week <= n_week) %>% 
  read_week1() %>% 
  left_join(positions %>% select(position, side))

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

events_throw <- c(
  "pass_forward",
  "pass_shovel"
)

# Probably should make this a function to be used anywhere.
compute_dist <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

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
    # dist = compute_dist(x, y, ball_x, ball_y),
    # dist_relative = compute_dist(x, y_relative, ball_x, 0),
    # y_half = case_when(y <= y_max / 2 ~ "bottom", TRUE ~ "top"),
    # y_third = case_when(y <= !!hashmark_constant ~ "bottom", y > (!!y_max - !!hashmark_constant) ~ "top", TRUE ~ "mid")
    y_side = case_when(y < ball_y ~ 'below', TRUE ~ 'above')
  ) %>% 
  select(-direction)
tracking_cleaned

snap_frames_cleaned <- tracking_cleaned %>% filter(event == "ball_snap")
snap_frames_cleaned

receivers_at_snap <-
  snap_frames_cleaned %>%
  # QBs get filtered out by `is.na(route)` (and players who blocked).
  filter(side == 'O') %>%
  filter(!is.na(route)) %>% 
  inner_join(plays, by = c("game_id", "play_id")) %>%
  group_by(game_id, play_id) %>%
  mutate(idx_y = row_number(y), n = n()) %>%
  ungroup() %>%
  group_by(game_id, play_id, y_side) %>% 
  mutate(idx_y_side = case_when(y_side == 'below' ~ row_number(idx_y), TRUE ~ row_number(-idx_y))) %>% 
  ungroup() %>% 
  # Probably don't need all of these columns, but whatev
  select(
    game_id,
    play_id,
    nfl_id,
    event,
    display_name,
    position,
    x,
    y,
    y_relative,
    ball_x,
    ball_y,
    y_side,
    idx_y_side,
    route,
    play_result
  )
receivers_at_snap

# Figure out where TEs usually line up so that we can filter out players in the backfield.
# Seems like it's around ~3-5 yards from ball.
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

# Only want combos with 2 to 4 routes. 2 is a "well-defined" lower limit, but 4 is not.
# There are actually some plays with 5 receivers on one side of the field at snap!
route_combos_n_group <- 
  route_combos_n %>% 
  filter(n_route >= 2L, n_route <= 4L) %>% 
  mutate(rnk = row_number(desc(n)))
route_combos_n_group
route_combos_n_group

# There are plays with 5 receivers on the same side of the field?!?
route_combos_n %>% group_by(n_route) %>% summarize(across(n, sum)) %>% ungroup()
route_combos_n %>% filter(n_route == 5L)

# # Take just a sample of the route combos.
# n_sample <- 20L
# route_combos_n_group_sample <- route_combos_n_group %>% head(n_sample)
# route_combos_n_group_sample

# Create a data.frame with all possible pick play route combos.
short_in_routes <- c("SLANT", "IN")
short_out_routes <- c("FLAT", "OUT")
all_routes <- c("HITCH", "OUT", "FLAT", "CROSS", "GO", "SLANT", "SCREEN", "CORNER", "IN", "ANGLE", "POST", "WHEEL")
# `idx_y_out` marks the receiver doing the "out" part of the combo, which is typically the desired receiver.
short_pick_route_combos <-
  list(
    crossing(
      route1 = short_in_routes,
      route2 = short_out_routes
    ) %>% 
      mutate(idx_y_out = 2L, n_route = 2L),
    crossing(
      route1 = all_routes,
      route2 = short_in_routes,
      route3 = short_out_routes
    ) %>% 
      mutate(idx_y_out = 3L, n_route = 3L),
    crossing(
      route1 = short_in_routes,
      route2 = short_out_routes,
      route3 = all_routes
    ) %>% 
      mutate(idx_y_out = 2L, n_route = 3L),
    crossing(
      route1 = all_routes,
      route2 = all_routes,
      route3 = short_in_routes,
      route4 = short_out_routes
    ) %>% 
      mutate(idx_y_out = 4L, n_route = 4L),
    crossing(
      route1 = all_routes,
      route2 = short_in_routes,
      route3 = short_out_routes,
      route4 = all_routes
    ) %>% 
      mutate(idx_y_out = 3L, n_route = 4L),
    crossing(
      route1 = short_in_routes,
      route2 = short_out_routes,
      route3 = all_routes,
      route4 = all_routes
    ) %>% 
      mutate(idx_y_out = 2L, n_route = 4L)
  ) %>% 
  reduce(bind_rows) %>% 
  unite('route_combo', matches('^route'), sep = '-') %>% 
  mutate(
    across(route_combo, ~str_replace_all(.x, c('-NA' = '', '^NA' = '')))
  )
short_pick_route_combos

# Check to see which of the pre-defined combos are actually observed in the data.
route_combos_w_short_picks <-
  route_combos_n_group %>% 
  left_join(short_pick_route_combos) %>% 
  mutate(
    is_short_pick_combo = dplyr::if_else(!is.na(idx_y_out), TRUE, FALSE)
  )
route_combos_w_short_picks

# Summarize pick route combos vs. other route combos.
route_combos_w_short_picks %>% 
  group_by(is_short_pick_combo, n_route) %>% 
  summarize(across(n, sum)) %>% 
  ungroup() %>% 
  mutate(frac = n / sum(n))

# Add route combo counts back to just "raw" route combo data.
route_combos_group <- 
  route_combos %>% 
  # # Only look at the top `n_sample` pairs (by descending count).
  # inner_join(route_combos_n_group_sample) %>% 
  # # Factor to make `facet_wrap()` in order.
  # mutate(across(route_combo, ~fct_reorder(.x, desc(n)))) %>% 
  # select(-n)
  inner_join(route_combos_n_group)
route_combos_group

# Rejoin back to identify which players were involved in each pair.
receivers_at_snap_group <-
  # Data including `x`, `y` and `nfl_id`.
  receivers_at_snap %>% 
  # Join data including `game_id`, `play_id`, and `y_side` for each pair.
  inner_join(route_combos_group)
receivers_at_snap_group

# Get snap frames and throw frames, including players on both sides of the ball.
throw_framed_cleaned <- 
  tracking_cleaned %>% 
  filter(event %in% events_throw) %>% 
  # Re-define `event` to make it so that there is only one label, instead of the multiple labels defined by `events_throw`.
  # This makes `pivot_wider()` nicer later.
  mutate(event = "pass_throw")

# Going to be looking at the nearest defender at the time of snap vs. time of throw.
frames_group <-
  bind_rows(
    snap_frames_cleaned,
    throw_framed_cleaned
  ) %>% 
  # Filter down to just the plays were care about.
  # Don't want `frame_id` here.
  semi_join(
    route_combos_group %>% select(game_id, play_id),
    by = c("game_id", "play_id")
  )
frames_group

min_dists <-
  frames_group %>% 
  dplyr::filter(position != "QB") %>%
  dplyr::select(game_id, play_id, frame_id, event, nfl_id, side, x, y) %>%
  tidyr::nest(data = c(nfl_id, side, x, y)) %>%
  dplyr::mutate(data = purrr::map(data, compute_min_distances)) %>%
  tidyr::unnest(data)
min_dists

# Add `nfl_id*`s back.
# Add player names (`display_name*`) only for data checking. They're not actually necessary.
min_dists_w_players <-
  min_dists %>%
  inner_join(
    receivers_at_snap_group %>% 
      select(
        game_id,
        play_id,
        # frame_id,
        nfl_id_o = nfl_id,
        display_name_o = display_name,
        y_side,
        idx_y_side,
        route,
        route_combo,
        n_route # ,
        # rnk
      ),
    by = c("game_id", "play_id", "nfl_id_o")
  ) %>% 
  inner_join(
    snap_frames_cleaned %>% 
      filter(side == "D") %>% 
      select(game_id, play_id, nfl_id_d = nfl_id, display_name_d = display_name),
    by = c("game_id", "play_id", "nfl_id_d")
  ) %>% 
  arrange(game_id, play_id, y_side, idx_y_side, frame_id)
min_dists_w_players
min_dists_w_players %>% filter(!is.na(idx_y_out))

min_dists_def_tally <-
  min_dists_w_players %>%
  select(
    game_id,
    play_id,
    y_side,
    idx_y_side,
    route,
    route_combo,
    n_route,
    # rnk,
    event,
    nfl_id_o,
    nfl_id_d
  ) %>%
  # count(game_id, play_id, y_side, event, nfl_id_d, sort = TRUE)
  pivot_wider(
    names_from = c(event),
    values_from = c(nfl_id_d),
    names_prefix = "nfl_id_d_"
  ) %>%
  mutate(same_defender = dplyr::if_else(nfl_id_d_ball_snap == nfl_id_d_pass_throw, TRUE, FALSE))
min_dists_def_tally

min_dists_def_tally_n <-
  min_dists_def_tally %>% 
  # Add `idx_y_out` column. It will only be non-NA for `route_combo`s that count as pick combos.
  left_join(
    short_pick_route_combos,
    by = c("route_combo", "n_route")
  ) %>% 
  mutate(
    is_short_pick_route_combo = dplyr::if_else(!is.na(idx_y_out), TRUE, FALSE),
    is_out_rec = dplyr::if_else(idx_y_out == idx_y_side, TRUE, FALSE)
  ) %>% 
  # group_by(game_id, play_id, route, route_combo, n_route, is_short_pick_route_combo, is_out_rec) %>% 
  group_by(n_route, is_short_pick_route_combo, is_out_rec) %>% 
  summarize(n = n(), across(same_defender, sum)) %>% 
  ungroup() %>% 
  mutate(frac = same_defender / n)
min_dists_def_tally_n
min_dists_def_tally_n %>% filter(!is.na(is_out_rec))

min_dists_def_tally_n %>% 
  count(route_combo, n_route, is_short_pick_route_combo, same_defender) %>% 
  group_by(route_combo, n_route, is_short_pick_route_combo) %>% 
  mutate(
    total = n(),
    frac = n / total
  ) %>% 
  ungroup()
min_dists_def_tally_n

min_dists_def_tally_n %>% 
  filter(same_defender == 0L) %>% 
  arrange(desc(frac))

min_dists_def_tally %>% 
  filter(nfl_id_d_ball_snap == nfl_id_d_pass_throw)

# frames_group <-
#   list(
#     receivers_at_snap_group,
#     snap_frames_cleaned %>% 
#       filter(side == "D") %>% 
#       inner_join(
#         route_combos_group %>% select(game_id, play_id),
#         by = c("game_id", "play_id")
#       ) %>% 
#       select(any_of(names(receivers_at_snap_group))),
#     
#   )
# frames_group

# Count how frequent these actions are.
n_game <-
  games %>% 
  filter(week <= !!n_week) %>% 
  inner_join(plays) %>% 
  nrow()
n_game

route_combos_n_group %>% 
  mutate(pct = 100 * n / !!n_game) %>% 
  head(n_sample)

# Prep to make some plots.
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
  mutate(
    across(c(y, ball_y), ~case_when(y_side == 'below' ~ !!y_max - .x, TRUE ~ .x))
  )
tracking_cleaned_group


tracking_cleaned_group_agg <-
  tracking_cleaned_group %>% 
  # Only first 5 seconds.
  filter(frame_id <= 50) %>% 
  group_by(route_combo, route, idx_y_side, n_route, frame_id) %>% 
  summarize(across(c(x, y), mean)) %>% 
  ungroup()
tracking_cleaned_group_agg

# Plot average of each route combo
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

# Plot all routes in each route combo
tracking_cleaned_pair %>% 
  mutate(
    grp = sprintf('%s-%s-%s-%s-%s', game_id, play_id, route_combo, route, idx_y_side)
  ) %>% 
  ggplot() +
  aes(x = x, y = y, group = grp, color = route) +
  geom_path() +
  facet_wrap(~route_combo)
