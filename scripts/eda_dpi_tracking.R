
library(tidyverse)
source('scripts/read_files.R')
source('scripts/animate_play.R')
source('scripts/pick_play.R')

games <- read_games()
plays <- read_plays()
positions <- read_positions()
pens <- file.path('data', 'penalties.csv') %>% read_csv()
tracking <- read_tracking(max_week = 1L, positions = positions, minimal = TRUE, drop_cols = TRUE)
events_throw <- c('pass_forward', 'pass_shovel')
throw_ids <- tracking %>% filter(event %in% events_throw) %>% distinct(game_id, play_id)
# Add `pass_arrived` and clip on the first of arrived or outcome.
events_pass_outcome <- c('pass_arrived', sprintf('pass_outcome_%s', c('caught', 'incomplete', 'interception', 'touchdown')))

tracking_clipped_at_pass_outcome <- 
  tracking %>% 
  semi_join(throw_ids) %>% 
  clip_tracking_at_events(events = events_pass_outcome)

# To be used for finding frames till pass outcome.
frame_id_ranges <- 
  tracking_clipped_at_pass_outcome %>% 
  group_by(game_id, play_id) %>% 
  summarize(across(frame_id, list(min = min, max = max))) %>%
  ungroup() %>% 
  mutate(frame_id_diff = frame_id_max - frame_id_min)

# pens_tracking_clipped_at_pass_outcome <-
#   pens %>% 
#   rename(pen_route = route) %>% 
#   inner_join(tracking_clipped_at_pass_outcome)
# pens_tracking_clipped_at_pass_outcome

compute_min_distances_possibly <- quietly(possibly(compute_min_distances, otherwise = NULL))
min_dists_init <-
  # pens_tracking_clipped_at_pass_outcome %>%
  tracking_clipped_at_pass_outcome %>% 
  inner_join(positions %>% select(side, position_category = category, position = position)) %>%
  filter(position != 'QB') %>%
  filter(position_category != 'DL') %>%
  select(-position_category) %>% 
  select(game_id, play_id, frame_id, nfl_id, side, x, y) %>%
  left_join(frame_id_ranges) %>% 
  mutate(
    frames_till_outcome = frame_id_max - frame_id,
    frames_after_snap = frame_id - frame_id_min,
    keep_till_outcome = (frames_till_outcome %% 10 == 0),
    keep_after_snap = (frames_after_snap %% 10 == 0)
  ) %>% 
  filter(keep_till_outcome | keep_after_snap) %>% 
  select(game_id, play_id, frame_id, frames_till_outcome, frames_after_snap, frame_id_diff, matches('^keep_'), nfl_id, side, x, y) %>% 
  nest(data = c(nfl_id, side, x, y)) %>%
  # slice(1) %>% 
  mutate(
    data = map(data, ~compute_min_distances_possibly(.x) %>% pluck('result')),
    # data = map(data, ~compute_min_distances(.x)),
    is_bad = map_lgl(data, is.null)
  )
is_bad <- min_dists_init %>% filter(is_bad)
is_bad

min_dists <-
  min_dists_init %>% 
  filter(!is_bad) %>% 
  select(-is_bad) %>% 
  unnest(data) %>% 
  left_join(pens %>% mutate(nfl_id_o = target_nfl_id)) %>% 
  mutate(is_target = if_else(!is.na(target_nfl_id), TRUE, FALSE)) %>% 
  left_join(frame_id_ranges %>% select(game_id, play_id, frame_id_max, frame_id_diff)) %>% 
  group_by(game_id, play_id, frame_id) %>% 
  mutate(idx_dist = row_number(dist)) %>% 
  ungroup()

min_dists_first <-
  min_dists %>% 
  group_by(game_id, play_id) %>% 
  filter(frame_id == min(frame_id)) %>% 
  ungroup() %>% 
  select(game_id, play_id, nfl_id_o, idx_dist_first = idx_dist)
min_dists_first

min_dists <-
  min_dists %>% 
  inner_join(min_dists_first)
min_dists

min_dists %>% 
  group_by(game_id, play_id) %>% 
  mutate(across(frames_till_outcome, list(max = max))) %>% 
  ungroup() %>% 
  filter(frames_till_outcome_max < 60) %>% 
  filter(frames_till_outcome <= 50) %>% 
  filter(keep_till_outcome) %>% 
  filter(idx_dist_first <= 5L) %>% 
  group_by(frames_till_outcome, idx_dist_first) %>% 
  summarize(n = n(), across(dist, mean)) %>% 
  ungroup() %>% 
  mutate(across(idx_dist_first, factor)) %>% 
  ggplot() +
  aes(x = frames_till_outcome * 0.1, y = dist, group = idx_dist_first, color = idx_dist_first) +
  geom_line() +
  geom_point(aes(size = n)) +
  scale_x_reverse() +
  guides(
    color = guide_legend(title = 'nth most tightly covered receiver at the snap', title.position = 'top'), 
    size = guide_legend('# of plays with x seconds till the pass outcome', title.position = 'top')
  ) +
  theme_classic() +
  theme(legend.position = 'bottom') +
  labs(
    title = 'Distance between defender and nth receiver x-frames till the pass outcome',
    subtitle = 'Week 1, plays with less than 6 seconds between snap and pass outcome',
    y = 'Distance',
    x = 'Seconds till pass outcome'
  )

min_dists %>% 
  group_by(game_id, play_id) %>% 
  mutate(across(frames_after_snap, list(max = max))) %>% 
  ungroup() %>% 
  filter(frames_after_snap_max < 60) %>% 
  filter(frames_after_snap <= 50) %>% 
  filter(keep_after_snap) %>% 
  filter(idx_dist_first <= 5L) %>% 
  group_by(frames_after_snap, idx_dist_first) %>% 
  summarize(n = n(), across(dist, mean)) %>% 
  ungroup() %>% 
  mutate(across(idx_dist_first, factor)) %>% 
  ggplot() +
  aes(x = frames_after_snap * 0.1, y = dist, group = idx_dist_first, color = idx_dist_first) +
  geom_line() +
  geom_point(aes(size = n)) +
  guides(
    color = guide_legend(title = 'n\'th most tightly covered receiver at the snap', title.position = 'top'), 
    size = guide_legend('# of plays with x seconds after snap', title.position = 'top')
  ) +
  theme_classic() +
  theme(legend.position = 'bottom') +
  labs(
    title = 'Distance between defender and nth receiver x-frames after the snap',
    subtitle = 'Week 1, plays with less than 6 seconds between snap and pass outcome',
    y = 'Distance',
    x = 'Seconds after snap'
  )
min_dists
min_dists

pens_ids <- pens %>% filter(pen == 'DPI') %>% distinct(game_id, play_id)
min_dists_ids <- min_dists %>% distinct(game_id, play_id)
pens_ids %>% inner_join(games %>% filter(week == 1))
min_dists_ids

min_dists_ids %>% 
  # slice(1) %>% 
  inner_join(plays %>% select(game_id, play_id, play_description)) %>% 
  mutate(
    lab = play_description,
    path = file.path('figs', sprintf('dpi-week1-%s-%s.png', game_id, play_id))
  ) %>%
  mutate(
    plot =
      pmap(
        list(game_id, play_id, lab),
        ~ plot_play(game_id = ..1, play_id = ..2, save = FALSE) +
          labs(subtitle = str_wrap(..3, 80), caption = NULL) +
          theme(
            plot.title = ggtext::element_markdown(size = 12),
            plot.subtitle = element_text(size = 12)
          )
      ),
    # plot = map2(plot, lab, ~..1 + labs(subtitle = ..2) ),
    path = map2(plot, path, ~ ggsave(plot = ..1, filename = ..2, height = 10, width = 10, type = 'cairo'))
  )

min_dists

min_dists %>% 
  left_join(
    pens_tracking_clipped_at_pass_outcome %>% 
      filter(side == 'O') %>% 
      select(game_id, play_id, nfl_id_o = nfl_id)
  )

tracking %>% arrange(desc(a))
one_play <-
  tracking %>% 
  filter(game_id == 2018090901, play_id == 5369, nfl_id == 2555540)
one_play