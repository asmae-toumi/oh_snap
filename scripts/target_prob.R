
source('scripts/target_prob_setup.R')

# 98. main ---- 
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
# Have to manually do this since it's not done by the `do_by_week function
fs::dir_ls(
    file.path('data', 'target_prob', 'big'),
    regexp = 'min_dists_naive_target_all_week*'
  ) %>% 
  map(arrow::read_parquet) %>% 
  reduce(bind_rows) %>% 
  arrow::write_parquet(.path_data_big_parquet('min_dists_naive_target_all'))

do_fit_target_prob_model()
do_fit_catch_prob_model()

# 99. players stuff ----
positions <-
  import_positions() %>% 
  mutate(
    across(position_label, ~case_when(.x == 'DB' ~ 'CB', TRUE ~ .x))
  )
plays <- import_plays()

path_probs_dists_bad <- path_ohsnap_data('probs_dists_bad.parquet')
path_probs_dists_wt <- path_ohsnap_data('probs_dists_wt.parquet')

if(!file.exists(path_probs_dists_wt)) {
# if(TRUE) {
  catch_probs <- 
    import_catch_probs() %>% 
    select(game_id, play_id, frame_id, nfl_id, catch_prob = .prob_1)
  
  probs <-
    crossing(
      cnd = 'all',
      which = c(NA_character_, 'oob')
    ) %>% 
    mutate(
      data = map2(cnd, which, import_target_probs)
    ) %>% 
    select(cnd, which, data) %>% 
    unnest(data) %>% 
    distinct() %>% 
    # select(-matches('^[.]prob_0'), -.prob_1, -.prob_class) %>% 
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
    left_join(secs %>% select(game_id, play_id, frame_id_diff))
  probs_aug
  rm('probs_grp', 'probs_start', 'probs_end', 'secs')
  
  .power <- 2
  ids <- probs_aug %>% distinct(game_id, play_id)
  
  min_dists_init <- 
    import_min_dists_naive_target() %>%
    semi_join(ids) %>% 
    distinct(game_id, play_id, frame_id, nfl_id, nfl_id_d, dist_d)
  min_dists_init
  
  min_dists <-
    min_dists_init %>% 
    group_by(game_id, play_id, frame_id, nfl_id_d) %>%
    mutate(
      dist_d_total = sum(1 / dist_d^.power),
      wt = (1 / dist_d^.power) / dist_d_total
    ) %>%
    ungroup() %>% 
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
  
  probs_dists <-
    probs_aug %>%
    select(
      cnd,
      which,
      game_id,
      play_id,
      frame_id,
      frame_id_diff,
      nfl_id,
      is_target,
      matches('prob')
    ) %>%
    inner_join(min_dists_aug)
  probs_dists
  
  if(!file.exists(path_probs_dists_bad)) {
    probs_dists_n <-
      probs_dists %>% 
      count(cnd, which, game_id, play_id, nfl_id, frame_id, nfl_id_d)
    probs_dists_n
    
    probs_dists_bad <-
      probs_dists_n %>% 
      filter(n > 1L)
    probs_dists_bad
    rm('probs_dists_n')
    probs_dists_bad %>% arrow::write_parquet(path_probs_dists_bad)
  } else {
    probs_dists_bad <- path_probs_dists_bad %>% arrow::read_parquet()
  }
  rm('probs_aug', 'probs')
  
  probs_dists_wt <-
    probs_dists %>%
    anti_join(probs_dists_bad) %>%
    semi_join(plays) %>% 
    arrange(game_id, play_id, frame_id, nfl_id) %>%
    select(-matches('^[.]prob')) %>% 
    select(
      which,
      game_id,
      play_id,
      nfl_id,
      nfl_id_d,
      is_target,
      frame_id,
      frame_id_diff,
      matches('dist_d'),
      matches('wt'),
      matches('prob')
    ) %>%
    mutate(
      across(is_target, binary_fct_to_lgl)
    )
  probs_dists_wt %>% arrow::write_parquet(path_probs_dists_wt)
} else {
  probs_dists_wt <- path_probs_dists_wt %>% arrow::read_parquet()
}

probs_dists_wt <-
  probs_dists_wt %>%
  mutate(
    prob_wt = wt * prob * catch_prob,
    prob_diff = prob - prob_start,
    prob_diff_wt_old = (wt * prob) - (wt_start * prob_start),
    prob_diff_wt = (wt * catch_prob * prob) - (wt_start * catch_prob_start * prob_start)
  ) %>% 
  left_join(
    players_from_tracking %>%
      select(-week) %>% 
      rename_with(
        ~ sprintf('%s_d', .x),
        c(nfl_id, position, display_name, jersey_number)
      )
  ) %>%
  left_join(
    positions %>%
      select(position, position_label) %>%
      rename_with(
        ~ sprintf('%s_d', .x), c(position, position_label)
      )
  )

probs_dists_wt_end <-
  probs_dists_wt %>% 
  group_by(
    which,
    game_id,
    play_id,
    is_target,
    nfl_id,
    nfl_id_d,
    position_label_d,
    display_name_d,
    jersey_number_d
  ) %>% 
  slice_max(frame_id, with_ties = FALSE) %>%
  ungroup()

if(FALSE) {
  game_id_example <- 2018123010
  play_id_example <- 3585
  animate_play(game_id = game_id_example, play_id = play_id_example, target_prob = TRUE)
  
  path_play <- path_ohsnap_figs(sprintf('%s-%s.gif', game_id_example, play_id_example))
  path_tp <- path_ohsnap_figs(sprintf('%s-%s-tp.gif', game_id_example, play_id_example))
  path_res <- path_ohsnap_figs(sprintf('%s-%s-combined.gif', game_id_example, play_id_example))
  play_mgif <- magick::image_read(path_play)
  tp_mgif <- magick::image_read(path_tp)
  
  res_gif <- magick::image_append(c(play_mgif[1], tp_mgif[1]), stack = FALSE)
  for(i in 2:54){
    combo_gif <- magick::image_append(c(play_mgif[i], tp_mgif[i]), stack = FALSE)
    res_gif <- c(res_gif, combo_gif)
  }
  magick::image_write(res_gif, path = path_res)
  
  players_from_tracking_slim <-
    players_from_tracking %>% 
    select(game_id, play_id, nfl_id, display_name, jersey_number)
  
  viz_tp_example <-
    probs_dists_wt %>% 
    filter(which %>% is.na()) %>% 
    filter(game_id == !!game_id_example, play_id == !!play_id_example) %>% 
    select(frame_id, nfl_id, nfl_id_d, dist = dist_d, wt, catch_prob, target_prob_wt = prob_wt) %>% 
    pivot_longer(
      -matches('^(nfl|frame)_id')
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
    ) %>%
    mutate(
      across(value, ~coalesce(.x, 0)),
      lab = sprintf('%s (%s)', display_name, jersey_number),
      lab_d = sprintf('%s (%s)', display_name_d, jersey_number_d),
      across(c(lab, lab_d), factor),
      across(name, ~ordered(.x, levels = c('dist', 'wt', 'catch_prob', 'target_prob_wt')))
    ) %>% 
    ggplot() +
    aes(x = frame_id, y = value, group = lab_d, color = lab_d) +
    geom_line(size = 1) +
    # guides(color = FALSE) +
    guides(color = guide_legend(title = '', override.aes = list(size = 3))) +
    # theme_minimal() +
    theme(
      plot.title.position = 'plot',
      plot.caption.position = 'plot',
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 14),
      plot.caption = ggtext::element_markdown(
        # size = 12,
        hjust = 0,
        lineheight = 0
      ),
      legend.position = 'top'
    ) +
    labs(
      title  = 'Coverage skill factors',
      x = NULL, y = NULL
    ) +
    facet_grid(name ~ lab, scales = 'free_y')
  viz_tp_example
  
  save_plot(
    viz_tp_example, 
    path = path_ohsnap_figs(sprintf('target_prob_factors-%s-%s.png'), game_id_example, play_id_exaxmple), 
    width = 10, 
    height = 10
  )
}

diffs_by_play <-
  probs_dists_wt_end %>%
  group_by(
    which,
    game_id,
    play_id,
    is_target,
    nfl_id_d,
    position_label_d,
    display_name_d,
    jersey_number_d
  ) %>% 
  summarize(
    n_receiver = n(),
    across(c(matches('^prob')), sum, na.rm = TRUE)
  ) %>%
  ungroup()

diffs_by_player <-
  diffs_by_play %>%
  # probs_dists_wt_end %>% 
  group_by(which, nfl_id_d, display_name_d) %>% 
  mutate(
    n = sum(!is_target)
  ) %>%
  ungroup() %>% 
  group_by(which, is_target, nfl_id = nfl_id_d, grp = position_label_d, display_name = display_name_d, n) %>%
  summarize(
    # n = n(),
    across(matches('^prob_diff'), list(sum = sum))
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
  arrange(rnk)
diffs_by_player

# diffs_by_player %>% filter(display_name == 'Ha Ha Clinton-Dix')
diffs_by_player_wide <-
  diffs_by_player %>% 
  filter(which %>% is.na()) %>% 
  select(
    nfl_id,
    display_name,
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
    tpoe = tpoe_target + tpoe_not_target,
    rnk = row_number(tpoe)
  ) %>% 
  relocate(nfl_id, display_name, grp, n, tpoe, rnk)
diffs_by_player_wide
write_csv(diffs_by_player_wide, .path_data_small_csv('tpoe_player_rankings.csv'), na = '')
