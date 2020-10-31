

library(tidyverse)
source('scripts/read_files.R')

games <- read_games()
plays <- read_plays()
positions <- read_positions()


.dist <- function(x1, x2, y1, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

.angle_diff <- function(a1, a2) {
  diff <- a2 - a1
  (diff + 180) %% 360 - 180
}

.join_and_filter <- function(x1, x2, side = c('o', 'd')) {
  side_upp <- side %>% toupper()
  side_low <- side %>% tolower()
  col_x_sym <- sprintf('x_%s', side_low) %>% sym()
  col_y_sym <- sprintf('y_%s', side_low) %>% sym()
  x1 %>% 
    left_join(
      x2 %>% 
        filter(side == !!side_upp) %>% 
        rename_with(~sprintf('%s_%s', .x, side_low), -c(game_id, play_id, frame_id, event))
    ) %>% 
    mutate(dist = .dist(x, !!col_x_sym, y, !!col_y_sym)) %>% 
    group_by(game_id, play_id, frame_id, nfl_id) %>% 
    filter(dist == min(dist)) %>% 
    ungroup() %>% 
    select(-dist)
}


weeks <- 1:17

tracking <-
  read_week(w) %>% 
  left_join(positions %>% select(position, position_category = category))
events_throw <- c('pass_forward', 'pass_shovel')
throw_ids <- tracking %>% filter(event %in% events_throw) %>% distinct(game_id, play_id)
# Add `pass_arrived` and clip on the first of arrived or outcome.
events_pass_outcome <- c('pass_arrived', sprintf('pass_outcome_%s', c('caught', 'incomplete', 'interception', 'touchdown')))
pass_outcome_ids <- tracking %>% filter(event %in% events_pass_outcome) %>% distinct(game_id, play_id)
dplyr::all_equal(throw_ids, pass_outcome_ids)

tracking_clipped_at_pass_outcome <- 
  tracking %>% 
  semi_join(throw_ids) %>% 
  clip_tracking_at_events(events = events_pass_outcome)
tracking_clipped_at_pass_outcome

frame_id_ranges <- 
  tracking_clipped_at_pass_outcome %>% 
  # filter(event != 'None') %>% 
  filter(event %in% c('ball_snap', events_throw, events_pass_outcome)) %>% 
  distinct(game_id, play_id, frame_id, event) %>% 
  mutate(
    across(
      event,
      ~case_when(
        .x %in% events_throw ~ 'posthrow-preoutcome',
        .x == 'ball_snap' ~ 'postsnap-prethrow',
        # .x %in% events_pass_outcome ~ 'pass_outcome',
        TRUE ~ 'outcome'
      )
    )
  ) %>% 
  group_by(game_id, play_id) %>% 
  mutate(frame_id_last = frame_id %>% dplyr::lead()) %>% 
  ungroup() %>% 
  # filter(!is.na(frame_id_last)) %>% 
  rename(frame_id_first = frame_id)
frame_id_ranges  
frame_id_ranges %>% count(event)

tracking_clipped_at_pass_outcome_minimal <-
  tracking_clipped_at_pass_outcome %>% 
  filter(position != 'QB') %>% 
  filter(position_category != 'DL') %>% 
  select(game_id, play_id, frame_id, event, nfl_id, position, side, x, y, s, o, dir)

tracking_clipped_at_pass_outcome_wide <-
  tracking_clipped_at_pass_outcome_minimal %>% 
  # filter(game_id == dplyr::first(game_id)) %>% 
  filter(side == 'D') %>% 
  .join_and_filter(tracking_clipped_at_pass_outcome_minimal, 'o') %>% 
  .join_and_filter(tracking_clipped_at_pass_outcome_minimal, 'd') %>% 
  mutate(
    dist_o = .dist(x, x_o, y, y_o),
    dist_d = .dist(x, x_d, y, y_d),
    dist_od = .dist(x_o, x_d, y_o, y_d),
    dist_rat = dist_o / (dist_od),
    # s_o_diff = (s - s_o),
    # s_d_diff = (s - s_d)
    dir_o_diff = .angle_diff(dir, dir_o),
    o_o_diff = .angle_diff(o, o_o)
  )
tracking_clipped_at_pass_outcome_wide

dt1 <- tracking_clipped_at_pass_outcome_wide %>% select(-event) %>% data.table::as.data.table()
dt2 <- frame_id_ranges %>% filter(!is.na(frame_id_last)) %>% data.table::as.data.table()

tracking_clipped_at_pass_outcome_wide <-
  dt1[dt2, on=.(game_id = game_id, play_id = play_id, frame_id >= frame_id_first, frame_id < frame_id_last)] %>% 
  as_tibble() %>% 
  select(-frame_id.1) %>% 
  relocate(event)
tracking_clipped_at_pass_outcome_wide

rm(list('dt1', 'dt2'))

tracking_clipped_at_pass_outcome_agg <-
  tracking_clipped_at_pass_outcome_wide %>% 
  semi_join(frame_id_ranges %>% select(game_id, play_id, frame_id = frame_id_first)) %>% 
  group_by(game_id, play_id, nfl_id) %>% 
  summarize(
    across(c(x, y, s, dist_o, dist_d, dist_rat, dir_o_diff, o_o_diff), list(mean = mean, var = var))
  ) %>% 
  ungroup() %>% 
  rename(
    off_mean = dist_o_mean,
    def_mean = dist_d_mean,
    off_dir_mean = dir_o_diff_mean,
    off_o_mean = o_o_diff_mean,
    off_var = dist_o_var,
    def_var = dist_d_var,
    off_dir_var = dir_o_diff_var,
    off_o_var = o_o_diff_var,
    rat_mean = dist_rat_mean,
    rat_var = dist_rat_var
  )

tracking_clipped_at_pass_outcome_agg

# tracking_at_pass_outcome <-
#   tracking %>% 
#   filter(event %in% events_throw)
# tracking_at_pass_outcome

play <- tracking_clipped_at_pass_outcome %>% filter(play_id == dplyr::first(play_id))
play


derive_features_at_frame <- function(play, frame_id = NULL) {

  if(is.null(frame_id)) {
    frame <- play %>% filter(frame_id == max(frame_id))
    frame_id <- frame$frame_id[[1]]
  } else {
    frame <- play %>% filter(frame_id == !!frame_id)
  }
  
  play_minimal <-
    play %>% 
    select(frame_id, nfl_id, x, y, s, dir, o, position, position_category, side) %>% 
    mutate(
      dir_rad = dir * pi / 180,
      v_x = sin(dir_rad) * s,
      v_y = cos(dir_rad) * s,
      v_theta = atan(v_y / v_x),
      v_theta = if_else(is.nan(v_theta), 0, v_theta)
    )

  eligible_player_frames <-
    play_minimal %>% 
    filter(position != 'QB' & position_category != 'DL')
  
  coverage_players <- 
    play_minimal %>% 
    filter(side == 'D' & position_category != 'DL') %>% 
    distinct(nfl_id)

  coverage_player_frames <-
    play_minimal %>% 
    semi_join(coverage_players)
  
  easy_features <-
    coverage_player_frames %>% 
    group_by(nfl_id) %>% 
    summarize(across(c(x, y, s), list(var = var), .names = '{fn}_{col}')) %>% 
    ungroup() %>% 
    rename(speed_var = var_s)
  easy_features
  
  .f <- function(frame_id, nfl_id, side) {
    eligible_player_frames %>% 
      filter(frame_id == !!frame_id) %>% 
      identify_nearest_player(nfl_id = nfl_id, side = side)
  }
  
  nearest_players <-
    coverage_player_frames %>% 
    select(frame_id, nfl_id) %>% 
    mutate(
      nfl_id_o = map2_dbl(frame_id, nfl_id, .f, side = 'O'),
      nfl_id_d = map2_dbl(frame_id, nfl_id, .f, side = 'D')
    )

  .select <- function(data, ...) {
    data %>% 
      select(frame_id, nfl_id, x, y, s, dir, o, v_theta, ...)
  }
  .select_rename <- function(data, suffix = c('o', 'd'), ...) {
    data %>% 
      .select(...) %>% 
      rename_with(~sprintf('%s_%s', .x, suffix), -frame_id)
  }

  nearest_players_details <-
    nearest_players %>% 
    inner_join(coverage_player_frames %>% .select()) %>% 
    inner_join(eligible_player_frames %>% .select_rename('o')) %>% 
    inner_join(eligible_player_frames %>% .select_rename('d')) %>% 
    mutate(
      dist_o = .dist(x, x_o, y, y_o),
      dist_d = .dist(x, x_d, y, y_d),
      dist_od = .dist(x_o, x_d, y_o, y_d),
      dist_rat = dist_o / (dist_od),
      # s_o_diff = (s - s_o),
      # s_d_diff = (s - s_d)
      dir_o_diff = .angle_diff(dir, dir_o),
      o_o_diff = .angle_diff(o, o_o)
    )
  nearest_players_details

  other_features <-
    nearest_players_details %>% 
    group_by(nfl_id) %>% 
    summarize(
      across(c(dist_o, dist_d, dist_rat, dir_o_diff, o_o_diff), list(mean = mean, var = var))
    ) %>% 
    ungroup() %>% 
    rename(
      off_mean = dist_o_mean,
      def_mean = dist_d_mean,
      off_dir_mean = dir_o_diff_mean,
      off_o_mean = o_o_diff_mean,
      off_var = dist_o_var,
      def_var = dist_d_var,
      off_dir_var = dir_o_diff_var,
      off_o_var = o_o_diff_var,
      rat_mean = dist_rat_mean,
      rat_var = dist_rat_var
    )
  other_features
  res <-
    full_join(easy_features, other_features) %>% 
    mutate(frame_id = !!frame_id) %>% 
    relocate(frame_id)
  res
}

features <- play %>% derive_features_at_frame()
features

do_derive_features_at_frame_timed <- function(data) {
  res <-
    data %>% 
    nest(data = -c(game_id, play_id))
  
  n <- res %>% nrow()
  dig <- (n %% 10) + 1
  fmt <- sprintf('%%d', dig)
  i <- 0
  .f <- function(game_id, play_id, ...) {
    i <<- i + 1
    msg <- glue::glue('{sprintf("%d%%d", dig, i)} of {n}: Computing for {game_id}, {play_id} at {Sys.time()}.')
    cat(msg)
    suppressMessages(derive_features_at_frame(...))
  }

  res <-
    res_init %>% 
    mutate(
      data = pmap(list(game_id, play_id, data), .f),
    ) %>% 
    unnest(data)
  res
}

features <-
  tracking_clipped_at_pass_outcome %>% 
  do_derive_features_at_frame_timed()

features %>% write_csv(file.path('coverage_identification_features_week1.csv'))
