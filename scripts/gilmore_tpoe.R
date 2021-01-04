
source('scripts/target_prob_setup.R')
probs_dists <- do_combine_target_probs_and_dists()
plays <- import_plays()
# games <- import_games()
# players_from_tracking <- import_players_from_tracking()

features <- 
  import_target_prob_features(suffix = 'all') %>%
  # filter(event != 'ball_snap') %>% 
  semi_join(plays %>% select(game_id, play_id), by = c('game_id', 'play_id')) %>%
  group_by(game_id, play_id, nfl_id) %>% 
  slice_max(frame_id) %>% 
  ungroup() %>% 
  distinct(game_id, play_id, frame_id, nfl_id, .keep_all = TRUE) %>% 
  mutate(idx = row_number()) %>% 
  relocate(idx) %>% 
  drop_na(is_target)
features

gilmore_id <- 2533062
probs_dists <- import_probs_dists()

probs_dists_viz_init1 <-
  probs_dists %>% 
  mutate(is_gilmore = nfl_id_d == !!gilmore_id) %>% 
  select(game_id, play_id, frame_id, nfl_id, nfl_id_d, is_target, is_gilmore, prob_diff_wt) %>% 
  inner_join(features %>% select(game_id, play_id, frame_id, nfl_id, matches('nfl_id_d[12]_naive'))) %>% 
  filter(nfl_id_d == nfl_id_d1_naive | nfl_id_d == nfl_id_d2_naive)

probs_dists_viz_init2 <-
  probs_dists_viz_init1 %>% 
  group_by(is_gilmore, is_target) %>% 
  summarize(
    n = n(),
    n_player = n_distinct(nfl_id_d),
    across(prob_diff_wt, sum)
  ) %>% 
  ungroup()

probs_dists_viz <-
  probs_dists_viz_init2 %>% 
  left_join(probs_dists_viz_init2 %>% filter(is_gilmore) %>% select(is_target, n_gilmore = n)) %>% 
  mutate(
    n_scaler = n_gilmore / n,
    prob_diff_wt_scaled = prob_diff_wt * n_scaler
  )
probs_dists_viz

.filter <- function(.is_gilmore, .is_target) {
  probs_dists_viz %>% 
    filter(is_gilmore == .is_gilmore) %>% 
    filter(is_target == .is_target) %>% 
    pull(prob_diff_wt_scaled) %>% 
    scales::number(accuracy = 0.01)
}

color_gilmore <- '#ffa3af'
color_league <- '#132f3c'
# .mutate_is_target <- function(data) {
#   data %>% mutate(is_target = sprintf('Is Covering Target? %s', ifelse(is_target, 'Yes', 'No'))) 
# }
viz_tpoe_gilmore <-
  probs_dists_viz_init1 %>% 
  # filter(!is_gilmore) %>% 
  mutate(is_target = sprintf('Is Covering Target? %s', ifelse(is_target, 'Yes', 'No'))) %>% 
  ggplot() +
  aes(x = prob_diff_wt) +
  # geom_col(alpha = 0.7, fill = color_league) +
  # geom_col(data = probs_dists_viz %>% filter(is_gilmore), fill = color_gilmore, alpha = 0.7) + 
  geom_density(aes(fill = is_gilmore), alpha = 0.7) +
  # scale_y_log10() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c(`TRUE` = color_gilmore, `FALSE` = color_league)) +
  geom_text(
    inherit.aes = FALSE,
    size = pts(14),
    data = tibble(is_target = 'Is Covering Target? No'),
    hjust = 0,
    aes(x = 0.02, y = 12, label = sprintf('Gilmore: %s', .filter(T,F)))
  ) +
  geom_text(
    inherit.aes = FALSE,
    size = pts(14),
    data = tibble(is_target = 'Is Covering Target? No'),
    hjust = 0,
    aes(x = 0.02, y = 11, label = sprintf('League: %s', .filter(F,F)))
  ) +
  geom_text(
    inherit.aes = FALSE,
    size = pts(14),
    data = tibble(is_target = 'Is Covering Target? Yes'),
    hjust = 0,
    aes(x = 0.02, y = 8, label = sprintf('Gilmore: %s', .filter(T,T)))
  ) +
  geom_text(
    inherit.aes = FALSE,
    size = pts(14),
    data = tibble(is_target = 'Is Covering Target? Yes'),
    hjust = 0,
    aes(x = 0.02, y = 7, label = sprintf('League: %s', .filter(F,T)))
  ) +
  guides(fill = FALSE) +
  facet_wrap(~is_target) +
  theme(
    plot.title = ggtext::element_markdown(size = 16),
    plot.title.position = 'plot',
    plot.caption = element_text(
      size = 12,
      hjust = 0
    ),
    plot.caption.position = 'plot'
  ) +
  labs(
    title = glue::glue("dTPOE of <b><span style='color:{color_gilmore};'>Gilmore</span></b> vs. <b><span style='color:{color_league};'>League</span></b> when closest or second closest defender"),
    # caption = glue::glue("<b><span style='color:{color_gilmore};'>Gilmore</span></b> on non-targeted plays: {.filter(T, F)}
    #                                on     targeted plays: {.filter(T, T)}"),
    caption = glue::glue('Gilmore numbers are season-long sums.
                         "League" numbers represent the averages if all other players had the same number of plays as Gilmore.'),
    x = 'dTPOE', y = '% of all observations'
  )
viz_tpoe_gilmore
save_plot(
  viz_tpoe_gilmore,
  path = .path_figs_png('viz_tpoe_gilmore'),
  width = 10,
  height = 6
)

probs_dists_viz %>% 
  filter(nfl_id_d != gilmore_id) %>% 
  mutate(across(prob_diff_wt, ~round(.x, 2))) %>% 
  count(prob_diff_wt) %>% 
  mutate(frac = n / sum(n))
  ggplot() +
  aes(x = prob_diff_wt) + 
  geom_histogram(aes(fill = is_gilmore), position = 'dodge', alpha = 0.5) +

  ggplot() +
  aes(x = prob_diff_wt) + 
  geom_density(aes(fill = is_gilmore), position = 'dodge', alpha = 0.5) +
  # scale_y_log10() +
  facet_wrap(~is_target, labeller = label_both) +
  labs(x = 'TP', y = 'log(Density)')

features_probs_dists <-
  features %>% 
  inner_join(
    probs_dists
  ) %>% 
  select(idx, game_id, play_id, frame_id, frame_id_frac, frame_id_diff, nfl_id, nfl_id_d, nfl_id_d1_naive, x, y, is_target, prob_wt, prob_diff_wt) %>% 
  mutate(is_gilmore = nfl_id_d == !!gilmore_id) %>% 
  relocate(is_gilmore)

features_probs_dists %>% 
  # filter(nfl_id_d == nfl_id_d1_naive) %>% 
  # filter(is.na(prob_diff_wt))
  # sample_frac(0.1) %>% 
  mutate(across(c(x, y), floor)) %>% 
  ggplot() +
  # geom_histogram(aes(x = prob_diff_wt)) + facet_wrap(~is_gilmore)
  aes(y, x) +
  geom_raster(aes(fill = prob_diff_wt)) +
  facet_wrap(~is_gilmore, labeller = label_both)
  # geom_point(aes(size = prob_wt, color = prob_wt))
  geom_contour(aes(z = prob_wt)) +
  # add a fill gradient from low (blue) to high (red) 
  # with white as the zero midpoint
  scale_fill_gradient2(low="blue",mid="black", high="red", midpoint=0) +
  scale_color_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  # drop the legends
  # guides(color=FALSE, fill = FALSE) +
  # add contour polygon lines around the most dense points
  # stat_contour(aes(color=..level.., z = prob_diff_wt))
  # ggplot() +
  # aes(x = x, y = y) +
  # geom_density_2d_filled() +
  # facet_wrap(~is_gilmore) +
  coord_flip()

features %>% 
  drop_na(is_target) %>% 
  # filter(is_target == '1') %>% 
  inner_join(probs_dists %>% filter(nfl_id_d == !!gilmore_id)) %>% 
  ggplot() +
  aes(x = x, y = y) +
  geom_density_2d_filled() +
  coord_flip()

# play_ids_ne_def <-
#   plays %>% 
#   left_join(games) %>% 
#   filter(home_team_abbr == 'NE' | visitor_team_abbr == 'NE') %>% 
#   filter(possession_team != 'NE')
# play_ids_ne_def %>% 
#   anti_join(
#     players_from_tracking %>% filter(nfl_id == !!gilmore_id)
#   )

