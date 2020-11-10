
library(tidyverse)
source('scripts/read_files.R')

games <- read_games()
plays <- read_plays()
positions <- read_positions()

# Distance calculation.
.dist <- function(x1, x2, y1, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

# Signed angle difference calculation.
# Reference: https://stackoverflow.com/questions/1878907/the-smallest-difference-between-2-angles
.angle_diff <- function(a1, a2) {
  diff <- a2 - a1
  (diff + 180) %% 360 - 180
}

# `x1` is tracking data (pre-filtered for defenders who aren't DLs).
# `x2` is also tracking data, but it's filtered to either the offense or defense, depending on whether we are trying to find the nearest offensive/defensive player.
# `side` indicates if we are trying to find the nearest offensive player ('o') or defensive player ('d')
.join_and_filter <- function(x1, x2, side = c('o', 'd')) {
  side <- match.arg(side)
  side_upp <- side %>% toupper()
  side_low <- side %>% tolower()
  col_x_sym <- sprintf('x_%s', side_low) %>% sym()
  col_y_sym <- sprintf('y_%s', side_low) %>% sym()

  x1 %>% 
    left_join(
      x2 %>% 
        filter(side == !!side_upp) %>% 
        # Add `_o` or `_d` suffix to variables that aren't being used to join.
        rename_with(~sprintf('%s_%s', .x, side_low), -c(game_id, play_id, frame_id, event))
    ) %>% 
    # Add a temporary distance column to filter down to other player who is closest.
    mutate(dist = .dist(x, !!col_x_sym, y, !!col_y_sym)) %>% 
    # If re-joining on defensive players, make sure not to choose the same player!
    filter(dist > 0) %>% 
    group_by(game_id, play_id, frame_id, nfl_id) %>% 
    # Filter for just the player in `x2` who is closest to the player in `x1`.
    filter(dist == min(dist)) %>% 
    ungroup() %>% 
    # Drop the temporary column.
    select(-dist)
}


# Big ass function to compute the features on week at a time.
do_derive_and_export_coverage_identification_features <- function(week) {
  
  events_throw <- c('pass_forward', 'pass_shovel')
  # Add `pass_arrived` and clip on the first of arrived or outcome.
  events_pass_outcome <- c('pass_arrived', sprintf('pass_outcome_%s', c('caught', 'incomplete', 'interception', 'touchdown')))
  
  tracking <-
    read_week(week) %>% 
    left_join(positions %>% select(position, side, position_category = category))
  
  # Typical pre-processing stuff. Don't really need to do all of this since we won't be using the ball, but whatever.
  ball <- tracking %>% filter(display_name == 'Football')
  
  tracking <-
    tracking %>%
    filter(display_name != 'Football')
  
  tracking <-
    tracking %>% 
    inner_join(
      ball %>% 
        select(game_id, play_id, frame_id, ball_x = x, ball_y = y),
      by = c('frame_id', 'game_id', 'play_id')
    )
  
  line_of_scrimmage <-
    tracking %>%
    filter(event == 'ball_snap') %>% 
    group_by(game_id, play_id) %>%
    filter(row_number() == 1L) %>% 
    ungroup() %>% 
    select(game_id, play_id, los = ball_x) %>%
    ungroup()
  
  # Do some standardizing.
  x_max <- 120
  y_max <- 160 / 3
  tracking <-
    tracking %>%
    left_join(line_of_scrimmage, by = c('game_id', 'play_id')) %>%
    mutate(
      across(c(x, ball_x, los), ~ if_else(play_direction == 'left', !!x_max - .x, .x)),
      # Standardizing the x direction based on the los is best for doing general analysis,
      # but perhaps not for plotting.
      across(c(x, ball_x), ~.x - los),
      across(c(y, ball_y), ~ if_else(play_direction == 'left', !!y_max - .x, .x))
    )

  # We only want plays with throws (no sacks).
  throw_ids <- 
    tracking %>% 
    filter(event %in% events_throw) %>% 
    distinct(game_id, play_id)
  
  # pass_outcome_ids <- 
  #   tracking %>% 
  #   filter(event %in% events_pass_outcome) %>% 
  #   distinct(game_id, play_id)
  # dplyr::all_equal(throw_ids, pass_outcome_ids)
  
  tracking_clipped_at_pass_outcome <- 
    tracking %>% 
    # Use `semi_join` like a filter, reducing the data to just `game_id`-`play_id`s where there are throws.
    semi_join(throw_ids) %>% 
    # Remove frames before the snap (even though these are included in the paper, they were not among the features found to be significant) and frames after the catch.
    clip_tracking_at_events(events = events_pass_outcome) %>% 
    # Don't need QB.
    filter(position != 'QB') %>% 
    # Don't need to assign coverages for DLs, even though they may play some coverage on some plays.
    filter(position_category != 'DL') %>% 
    select(game_id, play_id, frame_id, event, nfl_id, position, side, x, y, s, o, dir)
  tracking_clipped_at_pass_outcome
  
  # Create a df to be used with a non-equi join later for "ranges" of frames, marked by whether the frames comes before/after the snap/throw/pass outcome.
  frame_id_ranges <- 
    tracking_clipped_at_pass_outcome %>% 
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
  # frame_id_ranges %>% count(event)
  
  features_all_frames <-
    tracking_clipped_at_pass_outcome %>% 
    # We only care about defenders.
    filter(side == 'D') %>% 
    # Join and filter down to the closest offensive player.
    .join_and_filter(tracking_clipped_at_pass_outcome, 'o') %>% 
    # ... to the closest defensive players.
    .join_and_filter(tracking_clipped_at_pass_outcome, 'd') %>% 
    # Calculations for the features specified in the paper.
    mutate(
      dist_o = .dist(x, x_o, y, y_o),
      dist_d = .dist(x, x_d, y, y_d),
      dist_od = .dist(x_o, x_d, y_o, y_d),
      dist_rat = dist_o / (dist_od),
      dir_o_diff = .angle_diff(dir, dir_o),
      # Orientation difference with the offensive player. This isn't used in the paper since they didn't have an orientation varaible, but it recommends doing something like this in further analysis.
      o_o_diff = .angle_diff(o, o_o)
    )
  features_all_frames
  
  # Prep for a non-equi join to group frames by the events 'postsnap-prethrow' and 'postthrow-preoutcome'
  dt1 <- features_all_frames %>% select(-event) %>% data.table::as.data.table()
  dt2 <- frame_id_ranges %>% filter(!is.na(frame_id_last)) %>% data.table::as.data.table()
  
  # Do the non-equi join.
  features_all_frames <-
    dt1[dt2, on=.(game_id = game_id, play_id = play_id, frame_id >= frame_id_first, frame_id < frame_id_last)] %>% 
    as_tibble() %>% 
    select(-frame_id.1) %>% 
    # Move `event` to the first column just to make it easy to see.
    relocate(event)
  features_all_frames

  # Aggregate to the "important" frames, i.e. the snap, throw, and outcome.
  features <-
    features_all_frames %>% 
    # Only keeping the 
    inner_join(frame_id_ranges %>% select(game_id, play_id, frame_id = frame_id_first)) %>% 
    group_by(game_id, play_id, event, nfl_id, position) %>% 
    summarize(
      n = n(),
      across(
        c(x, y, s, dist_o, dist_d, dist_rat, dir_o_diff, o_o_diff), 
        list(mean = mean, var = var)
      )
    ) %>% 
    ungroup() %>% 
    # Rename columns to match up with paper's names.
    rename(
      off_mean = dist_o_mean,
      def_mean = dist_d_mean,
      off_dir_mean = dir_o_diff_mean,
      off_o_mean = o_o_diff_mean, # New orientation feature.
      off_var = dist_o_var,
      def_var = dist_d_var,
      off_dir_var = dir_o_diff_var,
      off_o_var = o_o_diff_var, # New orientation feature.
      rat_mean = dist_rat_mean,
      rat_var = dist_rat_var
    ) %>% 
    drop_na()
  features
  # plot_play(game_id = 2018091001, play_id = 604)
  bad_plays <-
    features %>% 
    count(game_id, play_id, nfl_id) %>% 
    # Identify plays with only one event. Probably need to move this cleaning to prior script.
    filter(n == 1L) %>% 
    distinct(game_id, play_id)
  features <- features %>% anti_join(bad_plays)
  features %>% write_csv(file.path('data', sprintf('coverage_identification_features_week%d.csv', week)))
}

weeks <- 1:17
# weeks <- 1
# Call the function for each week, one at a time. (No need to save the results to a variable, hence `walk`.)
weeks %>% walk(do_derive_and_export_coverage_identification_features)

