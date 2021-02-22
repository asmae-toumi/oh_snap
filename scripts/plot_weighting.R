
source('plot_weighting_helpers.R')
# For interactive usae of `plot_defensive_allocation()`
game_id = 2018121600
play_id = 1125
plays = import_plays()
games = import_games()
positions = import_positions()
team_colors = TRUE
yardmin = NULL
yardmax = NULL
field_color = 'white'
line_color = 'black'
sideline_color = 'white'
endzone_color = NULL
buffer = NULL
colors = import_colors()
probs_dists = do_combine_target_probs_and_dists(overwrite = F)
snapshot = 'snap'
save = TRUE
dir = file.path('figs', 'target_prob')
ext = 'png'
filename = sprintf('%s-%s-%s.%s', game_id, play_id, snapshot, ext)
path = file.path(dir, filename)
title = NULL
subtitle = NULL
caption = NULL
width = 10
height = 10

probs_dists <- do_combine_target_probs_and_dists(overwrite = F)
crossing(
  snapshot = c('snap', 'throw'),
  n_defender = c(1, 3)
) %>% 
  mutate(
    res = walk2(
      snapshot,
      n_defender,
      ~ plot_defensive_allocation(
        snapshot = ..1,
        n_defender = ..2,
        probs_dists = probs_dists,
        subtitle = 'Defensive credit allocation for covering Julio Jones (11)',
        yardmin = 20,
        yardmax = 55
      )
    )
  )
