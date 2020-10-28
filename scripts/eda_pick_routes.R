
library(tidyverse)
source("scripts/read_files.R")
source("scripts/animate_play.R")
source("scripts/pick_play.R")

positions <- read_positions()
games <- read_games()
plays <- read_plays()

# 2018 BDB did not have `a`, `o`, `position`, `play_direction`, or `route` in the tracking data.
n_week <- 17L
tracking <-
  read_all_weeks() %>%
  select(-a, -s, -o, -dis, -dir, -time, -jersey_number) %>% 
  mutate(across(week, ~str_remove(.x, "week") %>% as.integer())) %>%
  filter(week <= n_week) %>%
  # read_week1() %>%
  left_join(positions %>% select(position, position_category = category, side))

ball <- tracking %>% filter(display_name == "Football")

tracking <-
  tracking %>%
  filter(display_name != "Football") %>%
  select(-display_name) %>% 
  inner_join(ball %>% select(game_id, play_id, frame_id, ball_x = x, ball_y = y))
tracking

snap_frames <- tracking %>% filter(event == "ball_snap")

# plays %>% select(absolute_yardline_number)
line_of_scrimmage <-
  snap_frames %>%
  group_by(game_id, play_id) %>%
  filter(row_number() == 1L) %>% 
  ungroup() %>% 
  select(game_id, play_id, los = ball_x) %>%
  ungroup()
line_of_scrimmage

tracking <-
  tracking %>%
  left_join(line_of_scrimmage, by = c("game_id", "play_id"))
tracking

# Filter out stuff before the snap and after the pass has arrived.
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

x_max <- 120
y_max <- 160 / 3
# When `group = 0`, it"s pre-snap.
# When `group = 2`, it"s post-catch.
tracking_cleaned <-
  tracking %>%
  group_by(game_id, play_id, nfl_id) %>%
  mutate(
    group = case_when(
      event == "ball_snap" ~ 1L,
      # dplyr::lag(event) %in% events_end_route ~ 1L,
      dplyr::lag(event) %in% events_throw ~ 1L,
      TRUE ~ 0L
    ),
    group = cumsum(group)
  ) %>%
  ungroup() %>%
  filter(group == 1L) %>%
  select(-group) %>%
  mutate(
    across(c(x, ball_x), ~ if_else(play_direction == "left", !!x_max - .x, .x)),
    # Standardizing the x direction based on the los is best for ding general analysis,
    # but perhaps not for plotting.
    across(c(x, ball_x), ~if_else(play_direction == "left", .x - (!!x_max - los), .x - los)),
    across(c(y, ball_y), ~ if_else(play_direction == "left", !!y_max - .x, .x))
  ) %>% 
  add_side_cols()
tracking_cleaned

snap_frames_cleaned <- tracking_cleaned %>% filter(event == "ball_snap")
snap_frames_cleaned

receivers_at_snap <- snap_frames_cleaned %>% add_idx_y_col()
receivers_at_snap

# Note that there are plays without routes, including some completions and incompletions.
snap_frames_n <-
  snap_frames_cleaned %>%
  filter(side == "O") %>%
  filter(position != "QB") %>% 
  group_by(game_id, play_id, frame_id) %>% 
  summarize(n_route_na = sum(is.na(route)), n_route = n()) %>% 
  ungroup() %>% 
  inner_join(plays %>% select(game_id, play_id, pass_result, epa), by = c("game_id", "play_id"))
snap_frames_n

snap_frames_n %>% 
  count(pass_result, all_na_routes = ifelse(n_route_na == n_route, "no_routes", "has_routes")) %>% 
  pivot_wider(names_from = all_na_routes, values_from = n, values_fill = 0L) %>% 
  mutate(frac = no_routes / (no_routes + has_routes))

# Note the plays with NA `target_nfl_id`s
plays %>% filter(is.na(target_nfl_id)) %>% count(pass_result)

snap_frame_ids <- snap_frames_cleaned %>% distinct(game_id, play_id, frame_id)
snap_frame_ids

seconds_frames <-
  snap_frame_ids %>% 
  mutate(n = 10) %>% 
  uncount(n) %>%
  group_by(game_id, play_id) %>%
  mutate(
    sec = (row_number() - 1L) / 2
  ) %>% 
  ungroup() %>% 
  mutate(
    frame_id = frame_id + sec * 10, 
    event = sprintf("%.1f_sec_post_snap", sec)
  ) %>% 
  # select(-sec) %>% 
  inner_join(
    tracking_cleaned %>% 
      select(-event), 
    by = c("frame_id", "game_id", "play_id")
  )
seconds_frames

receivers_at_seconds <-
  seconds_frames %>%
  select(-x_side, -y_side) %>%
  # Use the `y_side` from the snap time, not at each seconds' time.
  inner_join(
    seconds_frames %>%
      filter(sec == 0) %>%
      filter(side == "O" & !is.na(route) & y_side != "mid" & x_side != "backfield") %>%
      group_by(game_id, play_id, y_side, x_side, frame_id) %>%
      mutate(n_route = n()) %>%
      ungroup() %>%
      select(game_id, play_id, nfl_id, x_side, y_side, n_route),
    by = c("game_id", "play_id", "nfl_id")
  ) %>%
  add_idx_y_col() %>%
  rename(x_side_init = x_side, y_side_init = y_side)
receivers_at_seconds

# Identify pick plays by crosses in routes (i.e. intersections).
identify_intersection_safely <- possibly(identify_intersection, otherwise = NULL)
receiver_intersections_init <-
  receivers_at_seconds %>% 
  # Can't be an intersection if there are less than 2 routes on the side of the field.
  filter(n_route >= 2L) %>% 
  # 1.5 is sort of an arbitrary cut off for how many seconds after the snap we consider.
  filter(sec <= 1.5) %>% 
  select(game_id, play_id, y_side_init, n_route, nfl_id, frame_id, x, y) %>% 
  nest(data = c(nfl_id, frame_id, x, y)) %>% 
  # head(20) %>% 
  # filter(game_id == 2018090909, play_id == 2618) %>% 
  mutate(
    intersection = map(data, identify_intersection_safely)
  )
receiver_intersections_init

is_bad <-
  receiver_intersections_init %>% 
  mutate(
    is_bad = map_lgl(intersection, ~is.null(.x))
  ) %>% 
  filter(is_bad) %>% 
  select(-intersection)

receiver_intersections <-
  receiver_intersections_init %>% 
  mutate(
    is_bad = map_lgl(intersection, ~is.null(.x))
  ) %>% 
  filter(!is_bad) %>% 
  select(-is_bad) %>% 
  mutate(
    has_intersection = map_lgl(intersection, ~nrow(.x) > 0),
  ) %>% 
  filter(has_intersection) %>% 
  select(-data, -has_intersection) %>% 
  mutate(n_intersection = map_dbl(intersection, ~nrow(.x) / 2L)) %>% 
  unnest(intersection) # %>% 
  # select(game_id, play_id, y_side_init, n_route, nfl_id, nfl_id_intersect, n_intersection, intersection) %>% 
  # mutate(
  #   x_intersect = map_dbl(intersection, ~.x[[1]]),
  #   y_intersect = map_dbl(intersection, ~.x[[2]])
  # ) %>% 
  # select(-intersection)
receiver_intersections

# Were plays more successful when one of the intersection receivers was targeted?
receiver_intersections_meta <-
  receiver_intersections %>% 
  distinct(game_id, play_id, y_side_init, nfl_id) %>% 
  inner_join(plays %>% select(game_id, play_id, target_nfl_id), by = c("game_id", "play_id")) %>% 
  group_by(game_id, play_id) %>% 
  summarize(
    target_is_intersect = sum(target_nfl_id == nfl_id)
  ) %>% 
  ungroup() %>% 
  inner_join(plays %>% select(game_id, play_id, pass_result, epa), by = c("game_id", "play_id"))
receiver_intersections_meta

# receiver_intersections_meta %>% skimr::skim()
receiver_intersections_meta %>% 
  # Drop the plays where the targeted receiver is NA.
  drop_na() %>% 
  count(target_is_intersect, pass_result) %>% 
  group_by(target_is_intersect) %>% 
  mutate(pct = 100 * n / sum(n)) %>% 
  ungroup()

# How many intersections are there, and how many routes were involved?
receiver_intersections_filtered <-
  receiver_intersections %>% 
  group_by(game_id, play_id, y_side_init) %>% 
  filter(nfl_id < nfl_id_intersect) %>% 
  ungroup()
receiver_intersections_filtered

receiver_intersections_filtered %>% 
  count(n_route, n_intersection)

snap_frames_split <-
  bind_rows(
    snap_frames_n %>% anti_join(receiver_intersections_meta %>% select(game_id, play_id)) %>% mutate(has_intersection = FALSE),
    snap_frames_n %>% inner_join(receiver_intersections_meta %>% select(game_id, play_id, target_is_intersect)) %>% mutate(has_intersection = TRUE)
  )
snap_frames_split
snap_frames_split %>% filter(has_intersection)

snap_frames_split_agg <-
  snap_frames_split %>% 
  group_by(has_intersection, target_is_intersect, pass_result) %>% 
  summarize(n = n(), across(c(n_route_na, n_route, epa), mean)) %>% 
  ungroup() %>% 
  group_by(has_intersection, target_is_intersect) %>% 
  mutate(frac = n / sum(n)) %>% 
  ungroup() %>% 
  arrange(has_intersection, target_is_intersect)
snap_frames_split_agg
snap_frames_split_agg %>% summarize(across(n, sum))

snap_frames_split_agg_pretty <-
  snap_frames_split %>% 
  mutate(
    across(
      pass_result,
      ~case_when(.x == "C" ~ "Complete", TRUE ~ "Not Complete")
    ),
    across(target_is_intersect, ~case_when(.x == 1L ~ TRUE, .x == 0 ~ FALSE, TRUE ~ NA))
  ) %>% 
  group_by(has_intersection, target_is_intersect, pass_result) %>% 
  summarize(n = n(), across(c(epa), mean)) %>% 
  ungroup() %>% 
  group_by(has_intersection, target_is_intersect) %>% 
  mutate(pct = 100 * n / sum(n)) %>% 
  ungroup() %>% 
  arrange(has_intersection, target_is_intersect) %>% 
  filter(pass_result == "Complete") %>% 
  select(has_pick_route = has_intersection, pass_result, target_is_pick = target_is_intersect, n, pct, epa)
snap_frames_split_agg_pretty




# # Visual inspection
# set.seed(42)
# res_intersections <-
#   receiver_intersections_filtered %>% 
#   group_by(n_route, n_intersection) %>% 
#   mutate(n = n(), n_sample = ifelse(n <= 3L, n, 3L)) %>% 
#   sample_n(size = n_sample) %>% 
#   ungroup() %>% 
#   mutate(
#     lab = sprintf("Pick route example with %d receivers on one side of field and %d intersection%s", n_route, n_intersection, ifelse(n_intersection > 1L, "s", "")),
#     path = file.path("figs", sprintf("pick_example-%d_receivers-%d_pick%s-%s-%s.png", n_route, n_intersection, ifelse(n_intersection > 1L, "s", ""), game_id, play_id))
#   ) %>%
#   mutate(
#     plot =
#       pmap(
#         list(game_id, play_id, lab),
#         ~ plot_pick_route_play(game_id = ..1, play_id = ..2, save = FALSE) +
#           labs(subtitle = ..3) +
#           theme(
#             plot.title = ggtext::element_markdown(size = 12),
#             plot.subtitle = element_text(size = 12),
#             plot.caption = ggtext::element_markdown(size = 12)
#           )
#       ),
#     # plot = map2(plot, lab, ~..1 + labs(subtitle = ..2) ),
#     path = map2(plot, path, ~ ggsave(plot = ..1, filename = ..2, height = 10, width = 10, type = "cairo"))
#   )
# res_intersections

# # Sensitivity test
# intersections_after <- function(sec) {
#   data <-
#     receivers_at_seconds %>% 
#     filter(n_route >= 2L) %>% 
#     filter(sec <= !!sec) %>% 
#     group_by(game_id, play_id) %>% 
#     filter(max(sec) >= !!sec) %>% 
#     ungroup() %>% 
#     select(game_id, play_id, y_side_init, nfl_id, frame_id, x, y) %>% 
#     nest(data = c(nfl_id, frame_id, x, y))
#   n_row <- data %>% nrow()
#   n_intersection <-
#     data %>% 
#     mutate(
#       intersection = map(data, identify_intersection),
#       has_intersection = map_lgl(intersection, ~nrow(.x) > 0),
#     ) %>% 
#     filter(has_intersection) %>% 
#     select(-data, -has_intersection) %>% 
#     mutate(n_intersection = map_dbl(intersection, ~nrow(.x) / 2L)) %>% 
#     summarize(across(n_intersection, sum)) %>% 
#     pull(n_intersection)
#   tibble(n_max = n_row, n_intersection = n_intersection)
# }
# 
# # Tallying how many intersections happen up through x seconds after the ball has been snapped.
# intersections_n <-
#   tibble(
#     sec = seq(0.5, 3, by = 0.5)
#   ) %>% 
#   mutate(data = map(sec, intersections_after)) %>% 
#   unnest(data)
# intersections_n
# 
# intersections_n %>% 
#   filter(sec >= 1) %>% 
#   mutate(pct_intersection = 100 * n_intersection / n_max)
