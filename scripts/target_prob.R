
source('scripts/target_prob_setup.R')

# data processing ---- 
if(FALSE) {
weeks <- 1:17L
personnel_and_rushers <- 
  weeks %>%
  do_by_week(
    f = do_identify_personnel_and_rushers, 
    # overwrite = TRUE,
    path = .path_data_big('personnel_and_rushers', ext = 'rds')
  )

routes <- 
  weeks %>% 
  do_by_week(
    f = do_import_routes, 
    # overwrite = TRUE,
    path = .path_data_big_parquet('routes')
  )

players_from_tracking <- 
  weeks %>% 
  do_by_week(
    f = do_import_players_from_tracking, 
    # overwrite = TRUE,
    path = .path_data_big_parquet('players_from_tracking')
  )

features <-
  weeks %>% 
  do_by_week(
    f = do_generate_features_at_events,
    overwrite = TRUE,
    path = .path_data_big_parquet('target_prob_features_all')
  )

import_target_prob_features(suffix = 'all') %>% 
  distinct(game_id, play_id, frame_id, nfl_id, .keep_all = TRUE) %>% 
  group_by(game_id, play_id, frame_id) %>% 
  mutate(idx_o = row_number(idx_o)) %>% 
  ungroup() %>% 
  arrow::write_parquet(.path_data_big_parquet('target_prob_features_all'))

# Have to manually do this since it's not done by the `do_by_week` function
fs::dir_ls(
    file.path('data', 'target_prob', 'big'),
    regexp = 'min_dists_naive_all_week*'
  ) %>% 
  map(arrow::read_parquet) %>% 
  reduce(bind_rows) %>% 
  distinct(game_id, play_id, frame_id, nfl_id, nfl_id_d, .keep_all = TRUE) %>% 
  arrow::write_parquet(.path_data_big_parquet('min_dists_naive_all'))

do_fit_target_prob_model()
do_fit_catch_prob_model()
}
# eval stuff ----
positions <-
  import_positions() %>% 
  mutate(
    across(position_label, ~case_when(.x == 'DB' ~ 'CB', TRUE ~ .x))
  )
plays <- import_plays()

# Only need to import this if didn't run stuff above in the same session.
players_from_tracking <- import_players_from_tracking()

# Since this is a big boy, we'll check if it exists before re-generating it.
# This is how the data processing functions work, but it was too lazy to write a function for this.
path_probs_dists <- .path_data_big_parquet('probs_dists')

if(!file.exists(path_probs_dists)) {
# if(TRUE) {
  catch_probs <- 
    import_catch_probs() %>% 
    select(game_id, play_id, frame_id, nfl_id, catch_prob = .prob_1)
  
  probs <-
    crossing(
      cnd = 'all',
      # which = c(NA_character_, 'oob')
      which = NA_character_
    ) %>% 
    mutate(
      data = map2(cnd, which, import_target_probs)
    ) %>% 
    select(cnd, which, data) %>% 
    unnest(data) %>% 
    distinct() %>% 
    select(-matches('^[.]prob_0'), -.prob_1, -.prob_class) %>% 
    rename(prob = .prob_1_norm) %>% 
    left_join(catch_probs)
  probs
  
  probs_grp <- 
    probs %>% 
    group_by(cnd, which, game_id, play_id, nfl_id)
  
  probs_start <-
    probs_grp %>% 
    slice_min(frame_id, with_ties = FALSE) %>% 
    ungroup()
  
  probs_end <-
    probs_grp %>%
    slice_max(frame_id, with_ties = FALSE) %>%
    ungroup()
  
  secs <-
    full_join(
      probs_start %>% distinct(game_id, play_id, frame_id_start = frame_id),
      probs_end %>% distinct(game_id, play_id, frame_id_end = frame_id)
    ) %>%
    mutate(
      frame_id_diff = frame_id_end - frame_id_start
    )
  
  probs_aug <-
    probs %>% 
    # Drop the first frame
    # anti_join(probs_start %>% select(which, game_id, play_id, nfl_id, frame_id)) %>% 
    # Get the prob of the first frame
    left_join(
      probs_start %>% 
        select(which, game_id, play_id, nfl_id, catch_prob, prob) %>% 
        rename_with(~sprintf('%s_%s', .x, 'start'), -c(which, game_id, play_id, nfl_id))
    ) %>% 
    # Add how many frames are in the play
    left_join(secs %>% select(game_id, play_id, frame_id_start, frame_id_diff)) %>% 
    mutate(frame_id_frac = (frame_id - frame_id_start) / frame_id_diff)
  probs_aug
  rm('catch_probs', 'probs_grp', 'probs_start', 'probs_end', 'secs')
  
  # Could change this.
  .power <- 2
  ids <- probs_aug %>% distinct(game_id, play_id)
  
  min_dists_init <- 
    import_min_dists_naive() %>%
    semi_join(ids) %>% 
    distinct(game_id, play_id, frame_id, nfl_id, nfl_id_d, dist_d)
  min_dists_init
  
  # min_dists_init %>% filter(game_id == first(game_id), play_id == first(play_id), frame_id == first(frame_id))
  min_dists <-
    min_dists_init %>% 
    group_by(game_id, play_id, frame_id, nfl_id_d) %>%
    mutate(
      dist_d_total = sum(1 / dist_d^.power),
      wt = (1 / dist_d^.power) / dist_d_total
    ) %>%
    ungroup() %>% 
    # filter(dist_d <= 20) %>% 
    # Zero out some weights, per James' feedback.
    mutate(across(wt, ~if_else(dist_d >= 20, 0, .x))) %>% 
    select(-dist_d_total)
  min_dists
  
  min_dists_start <-
    min_dists %>% 
    group_by(game_id, play_id, nfl_id, nfl_id_d) %>% 
    slice_min(frame_id, with_ties = FALSE) %>% 
    ungroup()
  
  min_dists_aug <-
    min_dists %>% 
    left_join(
      min_dists_start %>% 
        select(game_id, play_id, nfl_id, nfl_id_d, wt, dist_d) %>% 
        rename_with(~sprintf('%s_%s', .x, 'start'), -c(game_id, play_id, nfl_id, nfl_id_d))
    )
  rm('min_dists', 'min_dists_init', 'min_dists_start')
  rm('ids', 'probs')
  
  probs_dists <-
    probs_aug %>%
    inner_join(min_dists_aug) %>% 
    arrange(game_id, play_id, frame_id, nfl_id) %>%
    select(
      which,
      game_id,
      play_id,
      nfl_id,
      nfl_id_d,
      is_target,
      frame_id,
      frame_id_diff,
      frame_id_frac,
      matches('dist_d'),
      matches('wt'),
      matches('prob')
    ) %>%
    mutate(
      across(is_target, binary_fct_to_lgl)
    )
  probs_dists
  
  # No frames anymore after changes to data processing.
  # if(!file.exists(path_probs_dists_bad)) {
  #   probs_dists_n <-
  #     probs_dists %>% 
  #     count(cnd, which, game_id, play_id, nfl_id, frame_id, nfl_id_d)
  #   probs_dists_n
  #   
  #   probs_dists_bad <-
  #     probs_dists_n %>% 
  #     filter(n > 1L)
  #   probs_dists_bad
  #   rm('probs_dists_n')
  #   probs_dists_bad %>% arrow::write_parquet(path_probs_dists_bad)
  # } else {
  #   probs_dists_bad <- path_probs_dists_bad %>% arrow::read_parquet()
  # }
  # rm('probs_aug', 'probs')
  probs_dists %>% arrow::write_parquet(path_probs_dists)
} else {
  probs_dists <- path_probs_dists %>% arrow::read_parquet()
}

# Gilmore's id is 2533062
# Per `players_from_tracking <- import_players_from_tracking(); players_from_tracking %>% filter(display_name == 'Stephon Gilmore')`
probs_dists <-
  probs_dists %>%
  mutate(
    prob_wt = wt * prob * catch_prob,
    # prob_diff = prob - prob_start,
    # prob_diff_wt_old = (wt * prob) - (wt_start * prob_start),
    prob_diff_wt = (wt * catch_prob * prob) - (wt_start * catch_prob_start * prob_start)
  )
probs_dists %>% arrow::write_parquet(.path_data_big_parquet('weighted_target_probs'))

probs_dists_end <-
  probs_dists %>% 
  group_by(
    which,
    game_id,
    play_id,
    is_target,
    nfl_id,
    nfl_id_d # ,
    # position_label_d,
    # display_name_d,
    # jersey_number_d
  ) %>% 
  slice_max(frame_id, with_ties = FALSE) %>%
  ungroup()

if(FALSE) {
# Browsing for a good example play.
probs_dists_end %>% 
  filter(nfl_id == 2533062) %>% 
  arrange(prob_diff_wt) %>% 
  select(game_id, play_id, frame_id_diff, is_target, wt, wt_start, prob_diff_wt, prob, prob_start) %>% 
  inner_join(plays %>% select(game_id, play_id, epa, pass_result, play_description)) %>% 
  filter(wt_start < 0.8, wt >= 0.9, !is_target) %>% 
  filter(epa < 0) %>% 
  head(20)

game_id_example <- 2018120212
play_id_example <- 536
animate_play(game_id = game_id_example, play_id = play_id_example, target_prob = TRUE)

path_play <- .path_figs_gif(sprintf('%s-%s', game_id_example, play_id_example))
path_tp <- .path_figs_gif(sprintf('%s-%s-tp', game_id_example, play_id_example))
path_res <- .path_figs_gif(sprintf('%s-%s-combined', game_id_example, play_id_example))
play_mgif <- magick::image_read(path_play)
tp_mgif <- magick::image_read(path_tp)

n_frame <- play_mgif %>% magick::image_info() %>% nrow()
stopifnot(n_frame == (tp_mgif %>% magick::image_info() %>% nrow()))
res_gif <- magick::image_append(c(play_mgif[1], tp_mgif[1]), stack = TRUE)
for(i in 2:n_frame){
  combo_gif <- magick::image_append(c(play_mgif[i], tp_mgif[i]), stack = TRUE)
  res_gif <- c(res_gif, combo_gif)
}
magick::image_write(res_gif, path = path_res)

players_from_tracking_slim <-
  players_from_tracking %>% 
  select(game_id, play_id, nfl_id, display_name, jersey_number)

prob_dists_example <-
  probs_dists %>% 
  filter(which %>% is.na()) %>% 
  filter(game_id == !!game_id_example, play_id == !!play_id_example) %>% 
  select(game_id, play_id, frame_id, nfl_id, nfl_id_d, wt, cp = catch_prob, tp = prob, `wt * cp * tp` = prob_wt) %>% 
  pivot_longer(
    -matches('^(game|play|nfl|frame)_id')
  ) %>% 
  left_join(
    players_from_tracking_slim
  ) %>%
  left_join(
    players_from_tracking_slim %>% 
      rename_with(
        ~ sprintf('%s_d', .x),
        c(nfl_id, display_name, jersey_number)
      )
  )

# TODO: Put this on a separate plot, then bind with patchwork
generic_trace_example <-
  prob_dists_example %>% 
  filter(name %in% c('cp', 'tp')) %>% 
  distinct(game_id, play_id, frame_id, nfl_id, display_name, jersey_number, name, value) %>% 
  mutate(lab = sprintf('%s (%s)', display_name, jersey_number))

prob_dists_example_adj <-
  prob_dists_example %>%
  filter(!(name %in% c('cp', 'tp'))) %>% 
  mutate(
    across(value, ~coalesce(.x, 0)),
    lab = sprintf('%s (%s)', display_name, jersey_number),
    lab_d = sprintf('%s (%s)', display_name_d, jersey_number_d)
  ) %>% 
  bind_rows(
    generic_trace_example %>% 
      mutate(lab_d = ' Generic')
  ) %>% 
  mutate(
    across(c(lab, lab_d), factor),
    across(name, ~ordered(.x, levels = c('wt', 'cp', 'tp', 'wt * cp * tp')))
  )

viz_tp_example <-
  prob_dists_example_adj %>% 
  filter(is.na(display_name_d) | display_name_d != 'Stephon Gilmore') %>% 
  ggplot() +
  aes(x = frame_id, y = value, group = lab_d, color = lab_d) +
  geom_line(size = 1) +
  geom_line(
    data = prob_dists_example_adj %>% filter(display_name_d == 'Stephon Gilmore'),
    inherit.aes = TRUE,
    size = 3,
  ) +
  facet_grid(name ~ lab, scales = 'free_y', switch = 'y') +
  # scale_y_discrete(labels = function(x) str_wrap(x, width = 12)) +
  scico::scale_color_scico_d(palette = 'berlin') +
  guides(color = guide_legend(title = '', override.aes = list(size = 3))) +
  # theme_minimal() +
  theme(
    plot.title.position = 'plot',
    plot.caption.position = 'plot',
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.caption = ggtext::element_markdown(
      # size = 12,
      hjust = 0,
      lineheight = 0
    ),
    legend.position = 'top'
  ) +
  labs(
    title = 'dTPOE factors',
    x = 'frame', y = NULL
  )
viz_tp_example

# Adjust width such that all player names are visible.
save_plot(
  viz_tp_example, 
  path = .path_figs_png(sprintf('target_prob_factors-%s-%s', game_id_example, play_id_example)), 
  width = 11, 
  height = 11
)
}

diffs_by_play <-
  probs_dists_end %>%
  group_by(
    which,
    game_id,
    play_id,
    is_target,
    nfl_id = nfl_id_d
  ) %>% 
  summarize(
    n_receiver = n(),
    across(c(matches('^prob')), sum, na.rm = TRUE)
  ) %>%
  ungroup()


roster <- nflfastR::fast_scraper_roster(seasons = 2018)
roster %>% filter(team == 'NE', position %in% c('SS', 'FS'))
player_grps <-
  roster %>% 
  filter(position %in% c('CB', 'DB', 'S', 'FS', 'SS')) %>% 
  select(display_name = full_name, team, position) %>% 
  left_join(positions %>% select(position, grp = position_label))

diffs_by_player <-
  diffs_by_play %>%
  left_join(
    players_from_tracking %>%
      select(game_id, play_id, nfl_id, display_name)
  ) %>%
  # left_join(
  #   positions %>% rename(grp = position_label)
  # ) %>% 
  inner_join(player_grps) %>% 
  # probs_dists_end %>% 
  group_by(which, nfl_id, display_name) %>% 
  mutate(
    n = sum(!is_target)
  ) %>%
  ungroup() %>% 
  group_by(which, is_target, nfl_id, grp, display_name, n) %>%
  summarize(
    # n = n(),
    # across(matches('^prob_diff'), list(sum = sum))
    across(prob_diff_wt, list(sum = sum))
  ) %>%
  ungroup() %>%
  # filter(n > 200) %>% 
  group_by(which, is_target) %>%
  mutate(
    # rnk_sum = row_number(prob_diff_sum),
    # rnk_wt_old_sum = row_number(prob_diff_wt_old_sum),
    rnk = row_number(prob_diff_wt_sum)
  ) %>%
  ungroup() %>%
  group_by(which, is_target, grp) %>%
  mutate(
    # rnk_position_sum = row_number(prob_diff_sum),
    # rnk_wt_old_position_sum = row_number(prob_diff_wt_old_sum),
    rnk_grp = row_number(prob_diff_wt_sum)
  ) %>%
  ungroup() %>%
  rename(tpoe = prob_diff_wt_sum) %>% 
  left_join(player_grps) %>% 
  arrange(rnk)
diffs_by_player %>% filter(display_name == 'Stephon Gilmore')
# diffs_by_player %>% count(grp, nfl_id, display_name) %>% filter(n > 2)

# diffs_by_player %>% filter(nfl_id == 2560725)
# players_from_tracking %>% filter(nfl_id == 2560725) %>% count(position)

# diffs_by_player %>% filter(display_name == 'Ha Ha Clinton-Dix')
diffs_by_player_wide <-
  diffs_by_player %>% 
  filter(which %>% is.na()) %>% 
  select(
    nfl_id,
    display_name,
    team,
    grp,
    is_target,
    n,
    tpoe,
    # rnk,
    rnk_grp
  ) %>% 
  mutate(across(is_target, ~if_else(.x, 'target', 'not_target'))) %>% 
  pivot_wider(
    names_from = is_target,
    values_from = c(tpoe, rnk_grp),
    values_fill = 0
  ) %>% 
  mutate(
    tpoe = tpoe_target + tpoe_not_target
  ) %>% 
  group_by(grp) %>% 
  mutate(
    rnk_grp = row_number(tpoe)
  ) %>% 
  ungroup() %>% 
  relocate(nfl_id, display_name, team, grp, n, tpoe, rnk_grp) %>% 
  arrange(grp, rnk_grp)
diffs_by_player_wide %>% filter(grp == 'CB')
write_csv(diffs_by_player_wide, .path_data_small_csv('tpoe_player_rankings'), na = '')
