
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
    # overwrite = TRUE,
    path = .path_data_big_parquet('target_prob_features_all')
  )

# Hot fix for bad plays. should add logic to `do_generate_features_at_events` for this
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
  a
  arrow::write_parquet(.path_data_big_parquet('min_dists_naive_all'))
# min_dists_naive <- import_min_dists_naive()
# min_dists_naive %>% semi_join(rushers %>% rename(nfl_id_d = nfl_id))
do_fit_target_prob_model()
do_fit_catch_prob_model()
}

# eval stuff ----
probs_dists <- do_combine_target_probs_and_dists()

plays <- import_plays()

# Only need to import this if didn't run stuff above in the same session.
players_from_tracking <- import_players_from_tracking()

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
  ungroup() %>% 
  filter(prob_wt > 0)

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
    across(prob_diff_wt, sum, na.rm = TRUE)
  ) %>%
  ungroup()
diffs_by_play

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
game_id_example <- 2018112505
play_id_example <- 2716

.filter_example <- function(data) {
  data %>% 
    filter(game_id == !!game_id_example, play_id == !!play_id_example)
}

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

diffs_by_play %>% 
  group_by(is_target) %>% 
  summarize(across(prob_diff_wt, list(p50 = median, p75 = ~quantile(.x, 0.3), p90 = ~quantile(.x, 0.1))))
diffs_by_play %>% 
  .filter_example() %>% 
  filter(nfl_id == !!nfl_id_example)
probs_dists_end %>% 
  .filter_example() %>% 
  filter(nfl_id_d == !!nfl_id_example)

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
    # geom_line(size = 1),
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
  geom_line(size = 1) +
  .commont_example_theme_layers() +
  labs(
    title = 'dTPOE factors'
  )
viz_tp_generic_example

display_name_pri <- 'Stephon Gilmore'
display_name_pri_opp <- 'Robby Anderson' # 'Stefon Diggs'
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
  filter(value != 0) %>% 
  ggplot() +
  aes(x = frame_id, y = value, group = lab_d, color = lab_d) +
  geom_line(size = 1) +
  .commont_example_theme_layers() +
  geom_line(
    data = specific_traces_example_pri %>% filter(value != 0),
    inherit.aes = TRUE,
    size = 3,
  ) +
  geom_text(
    data = specific_traces_example_pri_opp_wtp_first %>% filter(value != 0),
    aes(y = value + 0.01, label = scales::number(value, accuracy = 0.001)),
    # vjust = 1,
    color = 'black',
    hjust = -0.1,
    size = pts(14)
  ) +
  geom_text(
    data = specific_traces_example_pri_opp_wtp_last %>% filter(value != 0),
    aes(y = value + 0.03, label = scales::number(value, accuracy = 0.001)),
    # vjust = 1,
    color = 'black',
    hjust = 1,
    size = pts(14)
  ) +
  geom_text(
    data = specific_traces_example_pri_opp_wtp_last %>% filter(value != 0),
    aes(y = 0.15, label = sprintf('dTPOE = %s', scales::number(!!tpoe_pri_opp_example, accuracy = 0.001))),
    # vjust = 1,
    color = 'black',
    hjust = 1.1,
    size = pts(14)
  ) +
  # scale_y_discrete(labels = function(x) str_wrap(x, width = 12)) +
  scico::scale_color_scico_d(palette = 'berlin') +
  guides(color = guide_legend(title = '', override.aes = list(size = 3))) +
  theme(strip.text.x = element_blank()) +
  labs(
    caption = 'Annotated: Gilmore\'s initial and final weighted target probabilities, used to compute dTPOE, for covering Anderson.',
    x = 'frame'
  )
viz_tp_specific_example

library(patchwork)
viz_tp_example <- viz_tp_generic_example / viz_tp_specific_example

# Adjust width such that all player names are visible.
save_plot(
  viz_tp_example, 
  path = .path_figs_png(sprintf('target_prob_factors-%s-%s', game_id_example, play_id_example)), 
  width = 12, 
  height = 12
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
    across(c(prob_diff_wt), list(sum = sum))
  ) %>%
  ungroup() %>%
  group_by(which, is_target, grp) %>%
  mutate(
    rnk_grp = row_number(prob_diff_wt_sum)
  ) %>%
  ungroup() %>%
  rename(tpoe = prob_diff_wt_sum) %>% 
  left_join(db_grps) %>% 
  arrange(grp, is_target, rnk_grp)

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
    rnk_grp
  ) %>% 
  mutate(across(is_target, ~if_else(.x, 'target', 'not_target'))) %>% 
  pivot_wider(
    names_from = is_target,
    values_from = c(n, tpoe, rnk_grp),
    values_fill = 0
  ) %>% 
  mutate(
    n = n_target + n_not_target,
    tpoe = tpoe_target + tpoe_not_target
  ) %>% 
  group_by(grp) %>% 
  mutate(
    rnk_grp = row_number(tpoe)
  ) %>% 
  ungroup() %>% 
  relocate(nfl_id, display_name, team, grp, n, tpoe, rnk_grp) %>% 
  arrange(grp, rnk_grp)

# defenders_top_pff <-
#   tibble(
#     display_name = c('Stephon Gilmore', 'Desmond King', 'Chris Harris', 'Kareem Jackson', 'Byron Jones', 'Jason McCourty', 'Kyle Fuller', 'Patrick Peterson', 'Bryce Callahan', 'Johnathan Joseph', 'Prince Amukamara', 'Denzel Ward', 'Marlon Humphrey', 'Casey Hayward', 'Pierre Desir', 'Xavien Howard', 'A.J. Bouye', 'Darius Slay', 'Trumaine Johnson', 'Marshon Lattimore', 'Steven Nelson', 'William Jackson', 'Adoree\' Jackson', 'Jalen Ramsey', 'Jaire Alexander')
#   ) %>% 
#   mutate(rnk_pff = row_number())

# diffs_by_player_wide_old <- .path_data_small_csv('tpoe_player_rankings') %>% read_csv()
# 
# 
# bind_rows(
#   diffs_by_player_wide_old %>% mutate(src = 'old'),
#   diffs_by_player_wide %>% mutate(src = 'new')
# ) %>% 
#   select(nfl_id, src, rnk_grp, grp) %>% 
#   pivot_wider(names_from = src, values_from = rnk_grp) %>% 
#   select(-nfl_id) %>% 
#   nest(data = -grp) %>% 
#   mutate(res = map(data, ~corrr::correlate(.x, method = 'spearman'))) %>% 
#   select(grp, res) %>% 
#   unnest(res)

write_csv(diffs_by_player_wide, .path_data_small_csv('tpoe_player_rankings'), na = '')
