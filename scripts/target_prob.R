
source('scripts/target_prob_setup.R')

# data processing ---- 
if(FALSE) {
weeks <- 1:17L
personnel_and_rushers <- 
  weeks %>%
  do_by_week(
    f = do_identify_personnel_and_rushers, 
    overwrite = TRUE,
    path = .path_data_big('personnel_and_rushers', ext = 'rds')
  )

routes <- 
  weeks %>% 
  do_by_week(
    f = do_identify_routes, 
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

# hot fix for bad plays. should add logic to `do_generate_features_at_events` for this
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
plays <- import_plays()

# Only need to import this if didn't run stuff above in the same session.
players_from_tracking <- import_players_from_tracking()

probs_dists <- do_combine_target_probs_and_dists()

# Gilmore's id is 2533062, per `import_players_from_tracking() %>% filter(display_name == 'Stephon Gilmore')`
nfl_id_example <- 2533062
probs_dists <-
  probs_dists %>%
  mutate(
    prob_wt = wt * prob * catch_prob,
    prob_diff_wt = (wt * catch_prob * prob) - (wt_start * catch_prob_start * prob_start)
  )

probs_dists_end <-
  probs_dists %>% 
  group_by(
    which,
    game_id,
    play_id,
    is_target,
    nfl_id,
    nfl_id_d
  ) %>% 
  slice_max(frame_id, with_ties = FALSE) %>%
  ungroup()

# diffs_by_play_cont <-
#   probs_dists %>%
#   group_by(which, game_id, play_id, nfl_id, nfl_id_d) %>%
#   mutate(across(frame_id_frac, ~.x / sum(.x))) %>%
#   ungroup() %>%
#   group_by(
#     which,
#     game_id,
#     play_id,
#     is_target,
#     nfl_id = nfl_id_d
#   ) %>%
#   summarize(
#     across(c(matches('^prob')), ~sum(.x * frame_id_frac, na.rm = TRUE))
#   ) %>%
#   ungroup()

diffs_by_play_minmax <-
  probs_dists %>%
  # head(1000) %>% 
  group_by(which, game_id, play_id, nfl_id, nfl_id_d) %>%
  arrange(frame_id, .by_group = TRUE) %>% 
  mutate(across(prob_diff_wt, ~slider::slide_dbl(.x, mean, .before = 10, .complete = FALSE))) %>% 
  # mutate(across(frame_id_frac, ~.x / sum(.x))) %>%
  ungroup() %>%
  group_by(
    which,
    game_id,
    play_id,
    is_target,
    nfl_id,
    nfl_id_d
  ) %>% 
  summarize(
    across(c(prob_diff_wt), list(min = min, max = max), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(prob_diff_wt_ma = prob_diff_wt_max + prob_diff_wt_min) %>% 
  group_by(
    which,
    game_id,
    play_id,
    is_target,
    nfl_id = nfl_id_d
  ) %>% 
  summarize(
    # n_receiver = n(),
    # across(c(matches('^prob')), ~sum(.x / n_receiver, na.rm = TRUE))
    across(c(matches('^prob')), ~sum(.x, na.rm = TRUE))
  ) %>%
  ungroup() 

diffs_by_play_minmax <-
  probs_dists %>%
  group_by(
    which,
    game_id,
    play_id,
    is_target,
    nfl_id,
    nfl_id_d
  ) %>% 
  summarize(
    across(prob_diff_wt, list(min = min, max = max), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(prob_diff_wt_ma = prob_diff_wt_max + prob_diff_wt_min) %>% 
  group_by(
    which,
    game_id,
    play_id,
    is_target,
    nfl_id = nfl_id_d
  ) %>% 
  summarize(
    # n_receiver = n(),
    # across(c(matches('^prob')), ~sum(.x / n_receiver, na.rm = TRUE))
    across(c(matches('^prob')), ~sum(.x, na.rm = TRUE))
  ) %>%
  ungroup() 

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
    across(c(matches('^prob')), sum, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  # left_join(diffs_by_play_cont %>% select(which, game_id, play_id, nfl_id, prob_diff_twt = prob_diff_wt))
  left_join(diffs_by_play_minmax)
diffs_by_play

diffs_by_play %>% 
  select(matches('^prob_diff_')) %>% 
  corrr::correlate()

diffs_by_play %>% 
  filter(nfl_id == 2539653) %>% 
  mutate(diff_abs = abs(prob_diff_wt - prob_diff_wt_ma)) %>% 
  arrange(-diff_abs)

diffs_by_play %>% 
  filter(nfl_id == 2539653) %>% 
  arrange(prob_diff_wt) %>% 
  ggplot() +
  aes(x = prob_diff_wt_ma) +
  geom_histogram(aes(fill = is_target)) +
  facet_wrap(~is_target)

.filter_example <- function(data) {
  data %>% 
    filter(which %>% is.na(), game_id == !!game_id_example, play_id == !!play_id_example)
}

probs_dists %>% 
  .filter_example() %>% 
  filter(nfl_id_d == !!nfl_id_example, nfl_id == 2552608)

probs_dists %>% 
  .filter_example() %>% 
  filter(nfl_id_d == !!nfl_id_example, nfl_id == 2552608) %>% 
  mutate(
    across(frame_id_frac, list(log = log, exp = exp)),
    across(c(prob_wt, prob_diff_wt), list(cumu = ~cumsum(.x))),
    prob_diff_wt_cumu_t = prob_wt_cumu * (frame_id_frac_exp / sum(frame_id_frac_exp))
  ) %>% 
  select(frame_id, prob_wt, matches('prob_diff_wt'), matches('frame_id_frac')) %>% 
  pivot_longer(-frame_id) %>% 
  ggplot() +
  aes(x = frame_id, y = value) +
  geom_line(aes(color = name), show.legend = FALSE, size = 1) +
  facet_wrap(~name, scales = 'free')

probs_dists_end %>% 
  .filter_example() %>% 
  # filter(nfl_id_d == !!nfl_id_example) %>% 
  inner_join(players_from_tracking %>% select(game_id, play_id, nfl_id, display_name)) %>% 
  inner_join(players_from_tracking %>% select(game_id, play_id, nfl_id_d = nfl_id, display_name_d = display_name)) %>% 
  arrange(prob_diff_wt)

diffs_by_play %>% 
  filter(is_target) %>% 
  filter(prob_diff_wt_ma != 0) %>% 
  count(prob_diff_wt_ma == prob_diff_wt_min)

diffs_by_play %>% 
  .filter_example() %>% 
  # filter(nfl_id == !!nfl_id_example)
  inner_join(players_from_tracking %>% select(game_id, play_id, nfl_id, display_name))

diffs_by_play_cont %>% 
  .filter_example() %>% 
  filter(nfl_id == !!nfl_id_example) %>% 
  arrange(prob_diff_wt)
# Harmon is 2541243
diffs_by_play %>% 
  .filter_example() %>% 
  filter(nfl_id == 2541243)
diffs_by_play %>% 
  group_by(is_target) %>% 
  summarize(across(prob_diff_wt, list(mean = mean, p90 = ~quantile(.x, 0.1))))

# Do this on-demand/ad-hoc
if(FALSE) {
# Browsing for a good example play.
  probs_dists_end %>%
    filter(nfl_id_d == !!nfl_id_example) %>%
    arrange(prob_diff_wt) %>%
    select(
      game_id,
      play_id,
      frame_id_diff,
      is_target,
      wt,
      wt_start,
      prob_diff_wt,
      prob,
      prob_start
    ) %>%
    inner_join(plays %>% select(game_id, play_id, epa, pass_result, play_description)) %>%
    filter(wt_start < 0.8, wt >= 0.9, !is_target) %>%
    filter(epa < 0) %>%
    head(20)
  
# Play chosen based on above criteria. It also happens to be interesting cuz of the blitz component.
game_id_example <- 2018123010 # 2018120212
play_id_example <- 516 # 536
animate_play(
  game_id = game_id_example, 
  play_id = play_id_example, 
  target_prob = TRUE
)

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
  .filter_example() %>% 
  select(game_id, play_id, frame_id, nfl_id, nfl_id_d, w = wt, cp = catch_prob, tp = prob, `w * cp * tp` = prob_wt) %>% 
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
generic_traces_example <-
  prob_dists_example %>% 
  filter(name %in% c('cp', 'tp')) %>% 
  distinct(game_id, play_id, frame_id, nfl_id, display_name, jersey_number, name, value) %>% 
  mutate(
    lab = sprintf('%s (%s)', display_name, jersey_number),
    lab_d = 'Generic',
    across(c(lab, lab_d), factor),
    across(name, ~ordered(.x, levels = c('cp', 'tp')))
  )
generic_traces_example

specific_traces_example <-
  prob_dists_example %>%
  filter(!(name %in% c('cp', 'tp'))) %>% 
  mutate(
    across(value, ~coalesce(.x, 0)),
    lab = sprintf('%s (%s)', display_name, jersey_number),
    lab_d = sprintf('%s (%s)', display_name_d, jersey_number_d)
  ) %>% 
  mutate(
    across(c(lab, lab_d), factor),
    across(name, ~ordered(.x, levels = c('w', 'w * cp * tp')))
  )
specific_traces_example

.commont_example_theme_layers <- function(...) {
  list(
    ...,
    geom_line(size = 1),
    facet_grid(name ~ lab, scales = 'free_y', switch = 'y'),
    theme(
      plot.title.position = 'plot',
      plot.caption.position = 'plot',
      plot.caption = element_text(size = 14, hjust = 0),
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 12, hjust = 1),
      legend.position = 'top'
    ),
    labs(
      x = NULL, y = NULL
    )
  )
}

viz_tp_generic_example <-
  generic_traces_example %>% 
  ggplot() +
  aes(x = frame_id, y = value) +
  .commont_example_theme_layers() +
  labs(
    title = 'dTPOE factors'
  )
viz_tp_generic_example

display_name_pri <- 'Robert Alford' # 'Stephon Gilmore'
display_name_pri_opp <- 'Mike Evans' # 'Stefon Diggs'
specific_traces_example_sec <-
  specific_traces_example %>% 
  filter(display_name_d != !!display_name_pri)

specific_traces_example_pri <-
  specific_traces_example %>% 
  filter(display_name_d == !!display_name_pri)

specific_traces_example_pri_opp <-
  specific_traces_example_pri %>% 
  filter(display_name == !!display_name_pri_opp)

specific_traces_example_pri_opp_wtp_first <- specific_traces_example_pri_opp %>% filter(frame_id == first(frame_id) & name != 'w')
specific_traces_example_pri_opp_wtp_last <- specific_traces_example_pri_opp %>% filter(frame_id == last(frame_id) & name != 'w')
tpoe_pri_opp_example <- specific_traces_example_pri_opp_wtp_last$value - specific_traces_example_pri_opp_wtp_first$value

viz_tp_specific_example <-
  specific_traces_example_sec %>% 
  ggplot() +
  aes(x = frame_id, y = value, group = lab_d, color = lab_d) +
  .commont_example_theme_layers() +
  geom_line(
    data = specific_traces_example_pri,
    inherit.aes = TRUE,
    size = 3,
  ) +
  geom_text(
    data = specific_traces_example_pri_opp_wtp_first,
    aes(y = value + 0.03, label = scales::number(value, accuracy = 0.001)),
    # vjust = 1,
    color = 'black',
    hjust = -0.1,
    size = pts(14)
  ) +
  geom_text(
    data = specific_traces_example_pri_opp_wtp_last,
    aes(y = value + 0.01, label = scales::number(value, accuracy = 0.001)),
    # vjust = 1,
    color = 'black',
    hjust = 1.4,
    size = pts(14)
  ) +
  geom_text(
    data = specific_traces_example_pri_opp_wtp_last,
    aes(y = 0.2, label = sprintf('dTPOE = %s', scales::number(!!tpoe_pri_opp_example, accuracy = 0.001))),
    # vjust = 1,
    color = 'black',
    hjust = 1.5,
    size = pts(11)
  ) +
  # scale_y_discrete(labels = function(x) str_wrap(x, width = 12)) +
  scico::scale_color_scico_d(palette = 'berlin') +
  guides(color = guide_legend(title = '', override.aes = list(size = 3))) +
  theme(strip.text.x = element_blank()) +
  labs(
    caption = 'Annotated: Gilmore\'s initial and final weighted target probabilities, used to compute dTPOE, for covering Diggs.',
    x = 'frame'
  )
viz_tp_specific_example

library(patchwork)
viz_tp_example <- viz_tp_generic_example / viz_tp_specific_example

# Adjust width such that all player names are visible.
save_plot(
  viz_tp_example, 
  path = .path_figs_png(sprintf('target_prob_factors-%s-%s', game_id_example, play_id_example)), 
  width = 11, 
  height = 11
)
}

db_grps <- import_nflfastr_db_groups()

diffs_by_player <-
  diffs_by_play %>%
  left_join(
    players_from_tracking %>%
      select(game_id, play_id, nfl_id, display_name)
  ) %>%
  inner_join(db_grps) %>% 
  group_by(which, is_target, nfl_id, grp, display_name) %>%
  summarize(
    n = n(),
    across(c(prob_diff_wt, prob_diff_wt_ma), list(sum = sum))
  ) %>%
  ungroup() %>%
  group_by(which, is_target, grp) %>%
  mutate(
    rnk_grp = row_number(prob_diff_wt_sum),
    rnk_grp_ma = row_number(prob_diff_wt_ma_sum)
  ) %>%
  ungroup() %>%
  rename(tpoe = prob_diff_wt_sum, tpoe_ma = prob_diff_wt_ma_sum) %>% 
  left_join(db_grps) %>% 
  arrange(grp, is_target, rnk_grp)
diffs_by_player
diffs_by_player %>% filter(display_name == 'Robert Alford')

diffs_by_player %>% 
  select(is_target, matches('^rnk_grp')) %>% 
  nest(data = -c(is_target)) %>% 
  mutate(
    res = map(data, ~corrr::correlate(.x, method = 'spearman', ))
  ) %>% 
  select(is_target, res) %>% 
  unnest(res) %>% 
  filter(!is.na(rnk_grp)) %>% 
  select(is_target, cor = rnk_grp)

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
    tpoe_ma,
    rnk_grp,
    rnk_grp_ma
  ) %>% 
  mutate(across(is_target, ~if_else(.x, 'target', 'not_target'))) %>% 
  pivot_wider(
    names_from = is_target,
    values_from = c(n, tpoe, tpoe_ma, rnk_grp, rnk_grp_ma),
    values_fill = 0
  ) %>% 
  mutate(
    n = n_target + n_not_target,
    tpoe = tpoe_target + tpoe_not_target,
    tpoe_ma = tpoe_ma_target + tpoe_ma_not_target
  ) %>% 
  group_by(grp) %>% 
  mutate(
    rnk_grp = row_number(tpoe),
    rnk_grp_ma = row_number(tpoe_ma)
  ) %>% 
  ungroup() %>% 
  relocate(nfl_id, display_name, team, grp, n, tpoe, rnk_grp) %>% 
  arrange(grp, rnk_grp)

diffs_by_player_wide %>% filter(team == 'CHI')
defenders_top_pff <-
  tibble(
    display_name = c('Stephon Gilmore', 'Desmond King', 'Chris Harris', 'Kareem Jackson', 'Byron Jones', 'Jason McCourty', 'Kyle Fuller', 'Patrick Peterson', 'Bryce Callahan', 'Johnathan Joseph', 'Prince Amukamara', 'Denzel Ward', 'Marlon Humphrey', 'Casey Hayward', 'Pierre Desir', 'Xavien Howard', 'A.J. Bouye', 'Darius Slay', 'Trumaine Johnson', 'Marshon Lattimore', 'Steven Nelson', 'William Jackson', 'Adoree\' Jackson', 'Jalen Ramsey', 'Jaire Alexander')
  ) %>% 
  mutate(rnk_pff = row_number())

diffs_by_player_wide %>% 
  # filter(tpoe_target < 0) %>% 
  # filter(grp %in% c('CB', 'S')) %>% 
  inner_join(defenders_top_pff %>% head(25)) %>% 
  select(matches('^rnk_')) %>% 
  arrange(rnk_pff) %>% 
  corrr::correlate(method = 'spearman')

diffs_by_player_wide %>% 
  # filter(tpoe_target < 0) %>% 
  filter(grp %in% c('CB', 'S')) %>% 
  filter(grp %in% c('CB')) %>%
  group_by(grp) %>% 
  slice_min(n = 10, order_by = rnk_grp_ma) %>% 
  ungroup() %>% 
  left_join(defenders_top_pff)

diffs_by_player_wide %>% 
  ggplot() +
  aes(x = tpoe_not_target, y = tpoe_target) +
  geom_point(aes(color = grp, size = n))
diffs_by_player_wide %>% filter(grp == 'CB')
write_csv(diffs_by_player_wide, .path_data_small_csv('tpoe_player_rankings'), na = '')
