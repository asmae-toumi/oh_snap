
library(tidyverse)
source("scripts/read_files.R")
source("scripts/animate_play.R")
source("scripts/pick_play.R")

positions <- read_positions()
games <- read_games()
plays <- read_plays()

# 2018 BDB did not have `a`, `o`, `position`, `play_direction`, or `route` in the tracking data.
# n_week <- 6L
tracking <-
  # read_all_weeks() %>%
  # mutate(across(week, ~str_remove(.x, "week") %>% as.integer())) %>%
  # filter(week <= n_week) %>%
  read_week1() %>%
  left_join(positions %>% select(position, position_category = category, side))

ball <- tracking %>% filter(display_name == "Football")

tracking <-
  tracking %>%
  filter(display_name != "Football") %>%
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
snap_frames_cleaned %>% distinct(game_id, play_id)

receivers_at_snap <- snap_frames_cleaned %>% add_idx_y_col()
receivers_at_snap
# receivers_at_snap %>% distinct(game_id, play_id)
# dropped_ids <-
#   snap_frames_cleaned %>% 
#   distinct(game_id, play_id) %>% 
#   anti_join(
#     receivers_at_snap %>% 
#       distinct(game_id, play_id)
#   )
# dropped_ids
# dropped_ids %>% 
#   head(3) %>% 
#   mutate(
#     path = fs::path("figs", sprintf("%s-%s.png", game_id, play_id)),
#     plot =
#       pmap(
#         list(game_id, play_id),
#         ~ plot_play(game_id = ..1, play_id = ..2, save = FALSE) +
#           theme(
#             plot.title = ggtext::element_markdown(size = 12),
#             plot.caption = ggtext::element_markdown(size = 14)
#           )
#       ),
#     path = map2(plot, path, ~ ggsave(plot = ..1, filename = ..2, height = 10, width = 10, type = "cairo"))
#   )

# Note that there are plays without routes, including some completions and incompletions.
snap_frames_cleaned %>%
  filter(side == "O") %>%
  filter(position != "QB") %>% 
  group_by(game_id, play_id, frame_id) %>% 
  summarize(n_na = sum(is.na(route)), n = n()) %>% 
  ungroup() %>% 
  inner_join(plays) %>% 
  count(pass_result, all_na_routes = ifelse(n_na == n, "no_routes", "has_routes")) %>% 
  pivot_wider(names_from = all_na_routes, values_from = n, values_fill = 0L) %>% 
  mutate(frac = no_routes / (no_routes + has_routes))

# # These are candidates for pick plays due to close alignment at the snap.
# potential_picks_based_on_snap <-
#   receivers_at_snap %>% 
#   select(game_id, play_id, y_side, nfl_id, x, y) %>% 
#   nest(data = c(nfl_id, x, y)) %>% 
#   mutate(
#     data = map(data, identify_nearby_players),
#     n_row = map_int(data, nrow)
#   ) %>% 
#   filter(n_row > 0L) %>% 
#   select(-n_row) %>% 
#   unnest(data)
# potential_picks_based_on_snap
# # These are instances where more than 1 route runner lines up near a given route runner, e.g. a trips bunch
# potential_picks_based_on_snap %>% filter(idx_match > 1L)

# Prepping data for another way to capture pick plays: change in order of receivers (relative to ball/sideline) x seconds after the snap.
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
      filter(y_side != "mid") %>%
      filter(x_side != "backfield") %>%
      filter(side == "O") %>%
      filter(position != "QB") %>%
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
receiver_intersections <-
  receivers_at_seconds %>% 
  # Can't be an intersection s if there are less tha
  filter(n_route >= 2L) %>% 
  # 1.5 is sort of an arbitrary cut off for how many seconds after the snap we consider.
  filter(sec <= 1.5) %>% 
  select(game_id, play_id, y_side_init, n_route, nfl_id, frame_id, x, y) %>% 
  nest(data = c(nfl_id, frame_id, x, y)) %>% 
  # head(20) %>% 
  # filter(game_id == 2018090909, play_id == 2618) %>% 
  mutate(
    intersection = map(data, identify_intersection),
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
receiver_intersections %>% distinct(game_id, play_id)
ids2 <- receivers_at_seconds %>% filter(sec == 0) %>% distinct(game_id, play_id)
ids1 <- plays %>% inner_join(games) %>% filter(week == 1) %>% distinct(game_id, play_id)

ids2 %>% anti_join(ids1)
ids1 %>% 
  anti_join(ids2) %>% 
  inner_join(plays) %>% 
  select(personnel_o, offense_formation)


receiver_intersections_meta <-
  receiver_intersections %>% 
  distinct(game_id, play_id, y_side_init, nfl_id) %>% 
  inner_join(plays %>% select(game_id, play_id, target_nfl_id), by = c("game_id", "play_id")) %>% 
  group_by(game_id, play_id) %>% 
  summarize(
    target_is_intersect = sum(target_nfl_id == nfl_id)
  ) %>% 
  ungroup() %>% 
  inner_join(plays %>% select(game_id, play_id, pass_result, play_result, epa), by = c("game_id", "play_id"))
receiver_intersections_meta

receiver_intersections_meta %>% 
  drop_na() %>% 
  count(target_is_intersect, pass_result) %>% 
  group_by(target_is_intersect) %>% 
  mutate(pct = 100 * n / sum(n)) %>% 
  ungroup()

# How many intersections are there?
receiver_intersections_filtered <-
  receiver_intersections %>% 
  group_by(game_id, play_id, y_side_init) %>% 
  filter(nfl_id < nfl_id_intersect) %>% 
  ungroup()
receiver_intersections_filtered

receiver_intersections_filtered %>% 
  count(n_route, n_intersection)

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


# ----
# Trying to track where changes happen in relative y position of a receiver compared to their position at the snap.
receivers_at_seconds_changes <-
  receivers_at_seconds %>% 
  arrange(game_id, play_id, y_side, nfl_id, frame_id, sec) %>% 
  group_by(game_id, play_id, y_side, nfl_id) %>% 
  mutate(
    idx_y_first = dplyr::first(idx_y),
    is_idx_y_same = idx_y == idx_y_first
  ) %>% 
  ungroup() %>% 
  filter(sec != 0) %>% 
  select(game_id, play_id, nfl_id, y_side, sec, event, route, idx_y, idx_y_first, is_idx_y_same)
receivers_at_seconds_changes
receivers_at_seconds_changes %>% filter(!is_idx_y_same)
receivers_at_seconds_changes %>% 
  filter(game_id == 2018090901, play_id == 2841) %>% 
  inner_join(snap_frames_cleaned %>% select(-event))

receivers_at_seconds_changes %>% 
  count(sec, is_idx_y_same) %>% 
  group_by(sec) %>% 
  mutate(frac = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = is_idx_y_same, values_from = c(frac, n))

receivers_at_seconds_filtered <-
  receivers_at_seconds_changes %>% 
  filter(sec == 1 & !is_idx_y_same)
receivers_at_seconds_filtered

receivers_at_seconds_filtered %>% 
  count(game_id, play_id) %>% 
  count(n, name = "nn")

# week1 <- read_week1()
set.seed(42)
receivers_at_seconds_filtered %>% 
  count(game_id, play_id) %>% 
  group_by(n) %>% 
  sample_n(size = 1) %>% 
  ungroup() %>% 
  mutate(
    lab = sprintf("Pick route example where %d receivers change relative positions", n),
    path = file.path("figs", sprintf("pick_example-%d_receivers-%s-%s.png", n, game_id, play_id))
  ) %>%
  # slice(1) %>%
  mutate(
    plot =
      pmap(
        list(game_id, play_id, lab),
        ~ plot_pick_route_play(game_id = ..1, play_id = ..2, save = FALSE) +
          labs(subtitle = ..3) +
          theme(
            plot.title = ggtext::element_markdown(size = 12),
            plot.subtitle = element_text(size = 12),
            plot.caption = ggtext::element_markdown(size = 12)
          )
      ),
    # plot = map2(plot, lab, ~..1 + labs(subtitle = ..2) ),
    path = map2(plot, path, ~ ggsave(plot = ..1, filename = ..2, height = 10, width = 10, type = "cairo"))
  )

# ----
route_combos <-
  receivers_at_snap %>%
  select(game_id, play_id, y_side, idx_y, route) %>%
  pivot_wider(names_from = idx_y, values_from = route, names_prefix = "rec") %>%
  unite("route_combo", matches("rec"), sep = "-") %>%
  mutate(
    # Could sort of use `str_remove_all()` here as well, but this makes it more explicity about removing the "NA" at the front of the string afterwards.
    across(route_combo, ~ str_replace_all(.x, c("-NA" = "", "^NA" = ""))),
    n_route = route_combo %>% str_count("-") + 1L
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
route_combos_n %>%
  group_by(n_route) %>%
  summarize(across(n, sum)) %>%
  ungroup()
route_combos_n %>% filter(n_route == 5L)

short_pick_route_combos <- generate_short_pick_route_combos()

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
  # Don"t want `frame_id` here.
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
# Add player names (`display_name*`) only for data checking. They"re not actually necessary.
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
        idx_y,
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
  arrange(game_id, play_id, y_side, idx_y, frame_id)
min_dists_w_players
min_dists_w_players %>% filter(!is.na(idx_y_out))

min_dists_def_tally <-
  min_dists_w_players %>%
  select(
    game_id,
    play_id,
    y_side,
    idx_y,
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
  # Add `idx_y_out` column. It will only be non-NA for `route_combo`s that count as pick combos.
  left_join(
    short_pick_route_combos,
    by = c("route_combo", "n_route")
  ) %>%
  mutate(
    is_same_defender = dplyr::if_else(nfl_id_d_ball_snap == nfl_id_d_pass_throw, TRUE, FALSE),
    is_short_pick_route_combo = dplyr::if_else(!is.na(idx_y_out), TRUE, FALSE),
    is_out_rec = dplyr::if_else(idx_y_out == idx_y, TRUE, FALSE)
  )
min_dists_def_tally

min_dists_def_tally_n <-
  min_dists_def_tally %>%
  # group_by(game_id, play_id, route, route_combo, n_route, is_short_pick_route_combo, is_out_rec) %>%
  group_by(n_route, is_short_pick_route_combo, is_out_rec) %>%
  summarize(n = n(), across(is_same_defender, sum)) %>%
  ungroup() %>%
  mutate(frac = is_same_defender / n)
min_dists_def_tally_n

set.seed(42)
meta_ex <-
  min_dists_def_tally %>%
  group_by(n_route, is_short_pick_route_combo, is_out_rec, is_same_defender) %>%
  sample_n(size = 1) %>%
  ungroup() %>%
  select(game_id, play_id, n_route, is_short_pick_route_combo, is_out_rec, is_same_defender)
meta_ex

week1 <- read_week1()
res <-
  meta_ex %>%
  filter(!is.na(is_out_rec)) %>%
  mutate(
    lab1 = dplyr::if_else(is_short_pick_route_combo, " pick route", "out pick route"),
    lab2 = dplyr::case_when(
      is_out_rec & is_same_defender ~ "a single defender stays with the out receiver",
      !is.na(is_out_rec) & !is_out_rec & is_same_defender ~ "a single defender stays with the in receiver",
      # is.na(is_out_rec) & is_same_defender ~ "a single defender stays with a receiver",
      is_out_rec & !is_same_defender ~ "the defender covering the out receiver changes between the snap and throw",
      !is.na(is_out_rec) & !is_same_defender ~ "the defender covering the in receiver changes between the snap and throw",
      # is.na(is_out_rec) & !is_same_defender ~ "the defender covering a receiver changes between the snap the throw"
      TRUE ~ ""
    ),
    lab = sprintf("%s-route example with%s where %s", n_route, lab1, lab2),
    path = file.path("figs", sprintf("pick_example-%d_receivers-%s_route-%s.png", n_route, ifelse(is_out_rec, "out", "other"), ifelse(is_same_defender, "same_defender", "defender_changes")))
  ) %>%
  # slice(1) %>%
  mutate(
    plot =
      pmap(
        list(game_id, play_id, lab),
        ~ plot_pick_route_play(game_id = ..1, play_id = ..2, tracking = week1, save = FALSE) +
          labs(subtitle = ..3) +
          theme(
            plot.title = ggtext::element_markdown(size = 12),
            plot.subtitle = element_text(size = 12),
            plot.caption = ggtext::element_markdown(size = 12)
          )
      ),
    # plot = map2(plot, lab, ~..1 + labs(subtitle = ..2) ),
    path = map2(plot, path, ~ ggsave(plot = ..1, filename = ..2, height = 10, width = 10, type = "cairo"))
  )
res

min_dists_def_tally_n %>% filter(!is.na(is_out_rec))

min_dists_def_tally_n %>%
  count(route_combo, n_route, is_short_pick_route_combo, is_same_defender) %>%
  group_by(route_combo, n_route, is_short_pick_route_combo) %>%
  mutate(
    total = n(),
    frac = n / total
  ) %>%
  ungroup()
min_dists_def_tally_n

min_dists_def_tally_n %>%
  filter(is_same_defender == 0L) %>%
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
        idx_y,
        n_route
      )
  ) %>%
  mutate(
    across(c(y, ball_y), ~ case_when(y_side == "below" ~ !!y_max - .x, TRUE ~ .x))
  )
tracking_cleaned_group


tracking_cleaned_group_agg <-
  tracking_cleaned_group %>%
  # Only first 5 seconds.
  filter(frame_id <= 50) %>%
  group_by(route_combo, route, idx_y, n_route, frame_id) %>%
  summarize(across(c(x, y), mean)) %>%
  ungroup()
tracking_cleaned_group_agg

# Plot average of each route combo
tracking_cleaned_pair_agg %>%
  mutate(
    grp = sprintf("%s-%s-%s", route_combo, route, idx_y)
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
    grp = sprintf("%s-%s-%s-%s-%s", game_id, play_id, route_combo, route, idx_y)
  ) %>%
  ggplot() +
  aes(x = x, y = y, group = grp, color = route) +
  geom_path() +
  facet_wrap(~route_combo)
