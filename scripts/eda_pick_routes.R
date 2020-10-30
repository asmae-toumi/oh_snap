
library(tidyverse)
source('scripts/read_files.R')
source('scripts/animate_play.R')
source('scripts/pick_play.R')

games <- read_games()
plays <- read_plays()

tracking <-
  read_tracking(max_week = 1L, minimal = TRUE) %>% 
  add_side_cols()

# Filter out stuff before the snap and after the pass has arrived.
# This is like https://github.com/danichusfu/RouteIdentification/blob/master/R/cut_plays.R.
events_end_route <- c(
  # 'None',
  # 'ball_snap',
  # 'pass_forward',
  # 'pass_arrived',
  'pass_outcome_caught',
  # 'first_contact',
  # 'tackle',
  'pass_outcome_incomplete',
  # 'play_action',
  # 'out_of_bounds',
  # 'line_set',
  'qb_sack',
  # 'man_in_motion',
  'pass_outcome_interception',
  # 'touchdown',
  # 'pass_tipped',
  # 'fumble',
  'pass_outcome_touchdown',
  'qb_strip_sack',
  # 'fumble_defense_recovered',
  # 'fumble_offense_recovered',
  # 'shift',
  # 'handoff',
  # 'pass_shovel',
  # 'penalty_flag',
  # 'run',
  'qb_spike' # ,
  # 'touchback',
  # 'field_goal_blocked',
  # 'penalty_accepted'
)

events_throw <- c('pass_forward', 'pass_shovel')
events_arrived <- c('pass_arrived')

# # Not sure what to do with more obscure things like this.
# plays %>% semi_join(tracking %>% filter(event == 'run')) %>% select(play_description)
# plays %>% semi_join(tracking %>% filter(event == 'field_goal_blocked')) %>% select(play_description) # janky
# plays %>% semi_join(tracking %>% filter(event == 'touchback')) %>% select(play_description) # DB intercepts and stays in endzone
# plays %>% semi_join(tracking %>% filter(event == 'penalty_accepted')) %>% select(play_description) # janky
# plays %>% semi_join(tracking %>% filter(event == 'shift')) %>% select(play_description)
tracking_events_frames <-
  tracking %>%
  filter(event != 'None') %>%
  distinct(game_id, play_id, event, frame_id) %>%
  pivot_wider(names_from = event, values_from = frame_id, values_fn = dplyr::first)

# Checking how long average time between arrival and throw is.
tracking_events_frames %>%
  filter(!is.na(pass_arrived)) %>%
  select(game_id, play_id, pass_arrived, matches('^pass_outcome_'), -pass_outcome_touchdown) %>%
  janitor::remove_empty(which = c('cols')) %>%
  # inner_join(plays %>% select(game_id, play_id, play_description))
  pivot_longer(-c(game_id, play_id, pass_arrived)) %>%
  drop_na() %>%
  mutate(diff = value - pass_arrived) %>%
  group_by(name) %>%
  summarize(across(diff, list(mean = mean, min = min, max = max), .names = '{fn}'), n = n()) %>%
  ungroup()

# Checking weird things.
pass_thrown_but_no_outcome <-
  tracking_events_frames %>%
  filter((!is.na(pass_forward) | !is.na(pass_shovel)) & (is.na(pass_outcome_caught) & is.na(pass_outcome_incomplete) & is.na(pass_outcome_interception) & is.na(pass_outcome_touchdown))) %>% 
  janitor::remove_empty(which = c('cols'))
pass_thrown_but_no_outcome

pass_thrown_but_no_outcome_or_arrival <-
  tracking_events_frames %>%
  filter((!is.na(pass_forward) | !is.na(pass_shovel)) & (is.na(pass_arrived) & is.na(pass_outcome_caught) & is.na(pass_outcome_incomplete) & is.na(pass_outcome_interception) & is.na(pass_outcome_touchdown))) %>% 
  janitor::remove_empty(which = c('cols'))
pass_thrown_but_no_outcome_or_arrival

# TODO: Update this clipping stuff based on above knowledge
tracking_clipped_at_throw <- tracking %>% clip_tracking_at_events(events = events_throw)
tracking_clipped_at_end_route <- tracking %>% clip_tracking_at_events(events = events_end_route)
rm('tracking') # Helpful for my 8 GB RAM laptop when working with all 17 weeks.
snap_frames <- tracking_clipped_at_throw %>% filter(event == 'ball_snap')
# snap_frames %>% filter(position == 'QB') %>% mutate(x1 = x - ball_x, x2 = x - los) %>% group_by(play_direction) %>% summarize(across(c(x1, x2), c(min = min, mean = mean, max = max)))
throw_frames <- tracking_clipped_at_throw %>% filter(event %in% events_throw)
end_route_frames <- tracking_clipped_at_end_route %>% filter(event %in% events_end_route)
throw_frames_2 <- tracking_clipped_at_end_route %>% filter(event %in% events_throw)

snap_ids <- snap_frames %>% distinct(game_id, play_id)
throw_ids <- throw_frames %>% distinct(game_id, play_id)
end_route_ids <- end_route_frames %>% distinct(game_id, play_id)
throw_ids_2 <- throw_frames_2 %>% distinct(game_id, play_id)
throw_ids %>% anti_join(snap_ids)
end_route_ids %>% anti_join(snap_ids)
throw_ids %>% anti_join(end_route_ids)
throw_ids %>% anti_join(throw_ids_2) # There's a small discrepancy here because of plays where `!is.na(qb_strip_sack) & !is.na(pass_outcome_interception)`.
throw_ids_2 %>% anti_join(end_route_ids)
# tracking_events_frames %>%
#   inner_join(throw_ids %>% anti_join(throw_ids_2)) %>%
#   janitor::remove_empty(which = c('cols')) %>%
#   inner_join(plays %>% select(game_id, play_id, play_description))
# tracking_events_frames %>% 
#   filter(!is.na(qb_strip_sack) & !is.na(pass_outcome_interception))
tracking_events_frames %>% 
  inner_join(throw_ids_2 %>% anti_join(end_route_ids)) %>%
  select(
    -out_of_bounds,
    -first_contact,
    -tackle,
    -man_in_motion,
    -play_action,
    -fumble,
    -fumble_offense_recovered,
    -fumble_defense_recovered,
    -handoff,
    -line_set,
    -shift,
    -touchdown,
    -touchback,
    -penalty_flag,
    -penalty_accepted,
    -field_goal_blocked
  ) %>% 
  janitor::remove_empty(which = c('cols')) %>% 
  # inner_join(plays %>% select(game_id, play_id, play_description))
  # group_by_at(vars(-c(game_id, play_id))) %>% 
  select(-game_id, -play_id) %>% 
  summarize_all(~sum(!is.na(.x)))

end_route_w_pass_ids <-
  tracking_clipped_at_end_route %>%
  # filter(game_id == 2018092301, play_id == 453) %>% 
  filter(side == 'O' & position != 'QB') %>% 
  filter(event %>% str_detect('^pass_outcome_')) %>%
  group_by(game_id, play_id) %>% 
  summarize(n_na_route = sum(is.na(route)), n_route = n()) %>% 
  ungroup() %>% 
  filter(n_na_route < n_route) %>% 
  # filter(event %in% events_end_route) %>%
  distinct(game_id, play_id)
end_route_w_pass_ids

summarize_events <- function(x1, x2) {
  bind_rows(
    x1 %>% 
      mutate(event = 'start'),
    x2 %>% 
      mutate(event = 'end')
  ) %>% 
    semi_join(end_route_w_pass_ids, by = c('game_id', 'play_id')) %>% 
    distinct(game_id, play_id, event, frame_id, ball_x, ball_y) %>% 
    pivot_wider(names_from = event, values_from = c(ball_x, ball_y, frame_id)) %>% 
    mutate(
      sec_diff = (frame_id_end - frame_id_start) * 0.1,
      x_diff = ball_x_end - ball_x_start,
      y_diff = ball_y_end - ball_y_start,
      dist = sqrt(x_diff^2 + y_diff^2)
    )
}

summarize_events_debug <- function(x1, x2) {
  bind_rows(
    x1 %>% 
      mutate(event = 'start'),
    x2 %>% 
      mutate(event = 'end')
  ) %>% 
    semi_join(end_route_w_pass_ids, by = c('game_id', 'play_id')) %>% 
    distinct(game_id, play_id, event, frame_id, ball_x, ball_y) %>% 
    pivot_wider(names_from = event, values_from = c(ball_x, ball_y, frame_id), values_fn = length)
}

# Probably should use the 'pass_arrived' event instead of the various `events_end_route` events to get a more accurate pass distance. I experimented with it but didn't like how the ids matched up.
pass_dists <-
  summarize_events(
    tracking_clipped_at_throw %>% 
      filter(event %in% events_throw),
    tracking_clipped_at_end_route %>% 
      # filter(event %>% str_detect('^pass_outcome_')) %>% 
      filter(event %in% events_end_route)
  )
pass_dists

pass_dists %>% 
  select(-game_id, -play_id) %>% 
  summarize_all(mean)

time_till_throw <-
  summarize_events(
    tracking_clipped_at_throw %>% 
      filter(event == 'ball_snap'),
    tracking_clipped_at_throw %>% 
      filter(event %in% events_throw)
  )
time_till_throw

receivers_at_snap <- snap_frames %>% add_idx_y_col()
receivers_at_snap

# Note that there are plays without routes, including some completions and incompletions.
# Not sure what `pass_result = 'R'` is...
snap_frames_n <-
  snap_frames %>%
  # semi_join(end_route_w_pass_ids, by = c('game_id', 'play_id'))
  # Probably should use `end_route_w_pass_ids`?
  filter(side == 'O' & position != 'QB') %>% 
  group_by(game_id, play_id, frame_id) %>% 
  summarize(n_route_na = sum(is.na(route)), n_route = n()) %>% 
  ungroup() %>% 
  # Don't really need `epa` here, but it's a nice add. Because I add it here, I don't need to add it later when joining on `snap_frames_n`.
  inner_join(plays %>% select(game_id, play_id, pass_result, epa), by = c('game_id', 'play_id'))
snap_frames_n

# Seeing what fraction of each pass result is missing routes.
snap_frames_n %>% 
  count(pass_result, all_na_routes = ifelse(n_route_na == n_route, 'no_routes', 'has_routes')) %>% 
  pivot_wider(names_from = all_na_routes, values_from = n, values_fill = 0L) %>% 
  mutate(frac = no_routes / (no_routes + has_routes))

# Note the plays with NA `target_nfl_id`s
plays %>% filter(is.na(target_nfl_id)) %>% count(pass_result)

snap_frame_ids <- snap_frames %>% distinct(game_id, play_id, frame_id)
snap_frame_ids

seconds_frames <-
  snap_frame_ids %>% 
  # Only need up to 1.5 seconds (0, 0.5, 1, 1.5) since that is my arbitrary cutoff.
  mutate(n = 4) %>% 
  uncount(n) %>%
  group_by(game_id, play_id) %>%
  mutate(
    sec = (row_number() - 1L) / 2
  ) %>% 
  ungroup() %>% 
  mutate(
    # Need this `event` column for something... I think with the `add_idx_y_col()` function.
    # event = sprintf('%.1f_sec_post_snap', sec)
    frame_id = frame_id + sec * 10, 
  ) %>% 
  # select(-sec) %>% 
  inner_join(
    tracking_clipped_at_throw %>% 
      select(-event), 
    by = c('frame_id', 'game_id', 'play_id')
  )
seconds_frames

receivers_at_seconds <-
  seconds_frames %>%
  select(-x_side, -y_side) %>%
  # Use the `y_side` from the snap time, not at each seconds' time.
  inner_join(
    seconds_frames %>%
      filter(sec == 0) %>%
      filter(side == 'O' & !is.na(route) & y_side != 'mid' & x_side != 'backfield') %>%
      group_by(game_id, play_id, y_side, x_side, frame_id) %>%
      mutate(n_route = n()) %>%
      ungroup() %>%
      select(game_id, play_id, nfl_id, x_side, y_side, n_route),
    by = c('game_id', 'play_id', 'nfl_id')
  ) %>%
  # add_idx_y_col() %>%
  rename(x_side_init = x_side, y_side_init = y_side)
receivers_at_seconds

# Identify pick plays by crosses in routes (i.e. intersections).
identify_intersection_xy_possibly <- possibly(identify_intersection_xy, otherwise = NULL)

path_receiver_intersections <- file.path('data', 'receiver_intersections.csv')
if(!file.exists(path_receiver_intersections)) {
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
      intersection = map(data, identify_intersection_xy_possibly)
    )
  receiver_intersections_init
  
  is_bad <-
    receiver_intersections_init %>% 
    mutate(
      is_bad = map_lgl(intersection, ~is.null(.x))
    ) %>% 
    filter(is_bad) %>% 
    select(-intersection)
  is_bad
  
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
    unnest(intersection)
  receiver_intersections
  receiver_intersections %>% write_csv(path_receiver_intersections)
} else {
  receiver_intersections <- path_receiver_intersections %>% read_csv()
}
# Prepping to answer the question "Were plays more successful when one of the intersection receivers was targeted?"
receiver_intersections_meta <-
  receiver_intersections %>% 
  distinct(game_id, play_id, y_side_init, nfl_id) %>% 
  # Just get the `target_nfl_id` first.
  inner_join(plays %>% select(game_id, play_id, target_nfl_id), by = c('game_id', 'play_id')) %>% 
  group_by(game_id, play_id) %>% 
  summarize(
    target_is_intersect = sum(target_nfl_id == nfl_id)
  ) %>% 
  ungroup() %>% 
  # Now get other numbers from `plays`.
  inner_join(plays %>% select(game_id, play_id, pass_result, epa, possession_team, quarter, down, yards_to_go), by = c('game_id', 'play_id'))
receiver_intersections_meta

receiver_intersections_meta %>% 
  count(team = possession_team, sort = TRUE)

# Preliminary answer to the question.
receiver_intersections_meta %>% 
  # Drop the plays where the targeted receiver is NA.
  drop_na() %>% 
  count(target_is_intersect, pass_result) %>% 
  group_by(target_is_intersect) %>% 
  mutate(pct = 100 * n / sum(n)) %>% 
  ungroup()

# Prepping to answer the questions "How many intersections are there? How many routes were involved?"
receiver_intersections_filtered <-
  receiver_intersections %>% 
  group_by(game_id, play_id, y_side_init) %>% 
  filter(nfl_id < nfl_id_intersect) %>% 
  ungroup()
# This will have more rows than `receiver_interactions_meta` because a play can have more than 1 pick. (`receiver_intersections_meta` is 1 row per pick play.)
receiver_intersections_filtered

# Answer to the question.
receiver_intersections_filtered %>% count(n_route, n_intersection)

snap_frames_split <-
  bind_rows(
    snap_frames_n %>% 
      anti_join(
        receiver_intersections_meta %>% 
          select(game_id, play_id)
      ) %>%
      mutate(has_intersection = FALSE),
    snap_frames_n %>% 
      inner_join(
        receiver_intersections_meta %>% 
          select(game_id, play_id, target_is_intersect)
      ) %>%
      mutate(has_intersection = TRUE)
  ) %>% 
  left_join(pass_dists %>% select(game_id, play_id, dist_throw = dist, sec_throw = sec_diff)) %>% 
  left_join(time_till_throw %>% select(game_id, play_id, dist_dropback = dist, sec_dropback = sec_diff)) %>% 
  inner_join(plays %>% select(game_id, play_id, possession_team, quarter, down, yards_to_go), by = c('game_id', 'play_id'))
snap_frames_split
# snap_frames_split %>% filter(is.na(x_throw) | is.na(x_dropback)) %>% count(pass_result)
# snap_frames_split %>% filter(is.na(possession_team))

# snap_frames_split_agg <-
#   snap_frames_split %>% 
#   group_by(has_intersection, target_is_intersect, pass_result) %>% 
#   summarize(n = n(), across(c(n_route_na, n_route, epa, matches('^(x|dist|sec)_')), mean, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   group_by(has_intersection, target_is_intersect) %>% 
#   mutate(frac = n / sum(n)) %>% 
#   ungroup() %>% 
#   arrange(has_intersection, target_is_intersect)
# snap_frames_split_agg

snap_frames_split_agg <-
  snap_frames_split %>% 
  mutate(
    across(
      pass_result,
      ~case_when(.x == 'C' ~ 'Complete', TRUE ~ 'Not Complete')
    ),
    across(target_is_intersect, ~case_when(.x == 1L ~ TRUE, .x == 0 ~ FALSE, TRUE ~ NA))
  ) %>% 
  select(-game_id, -play_id, -frame_id) %>% 
  group_by(has_intersection, target_is_intersect, pass_result) %>% 
  summarize(across(where(is.numeric), mean, na.rm = TRUE), n = n()) %>% 
  ungroup()  %>% 
  group_by(has_intersection, target_is_intersect) %>% 
  mutate(pct = 100 * n / sum(n)) %>% 
  ungroup() %>% 
  arrange(has_intersection, target_is_intersect) %>% 
  filter(pass_result == 'Complete') %>% 
  rename(has_pick_route = has_intersection, target_is_pick = target_is_intersect)
snap_frames_split_agg

snap_frames_split %>% 
  count(team = possession_team, has_intersection) %>% 
  mutate(across(has_intersection, ~if_else(.x == TRUE, 'has_pick', 'not_has_pick'))) %>% 
  pivot_wider(names_from = has_intersection, values_from = n) %>% 
  mutate(has_pick_frac = has_pick / (has_pick + not_has_pick)) %>% 
  arrange(desc(has_pick_frac)) %>% 
  mutate(across(team, ~fct_reorder(., has_pick_frac))) %>% 
  ggplot() +
  aes(y = team, x = has_pick_frac) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  labs(title = '% of Pass Plays with Pick Routes', y = NULL, x = '%')

# # Visual inspection
# set.seed(42)
# res_intersections <-
#   receiver_intersections_filtered %>% 
#   group_by(n_route, n_intersection) %>% 
#   mutate(n = n(), n_sample = ifelse(n <= 3L, n, 3L)) %>% 
#   sample_n(size = n_sample) %>% 
#   ungroup() %>% 
#   mutate(
#     lab = sprintf('Pick route example with %d receivers on one side of field and %d intersection%s', n_route, n_intersection, ifelse(n_intersection > 1L, 's', '')),
#     path = file.path('figs', sprintf('pick_example-%d_receivers-%d_pick%s-%s-%s.png', n_route, n_intersection, ifelse(n_intersection > 1L, 's', ''), game_id, play_id))
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
#     path = map2(plot, path, ~ ggsave(plot = ..1, filename = ..2, height = 10, width = 10, type = 'cairo'))
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
