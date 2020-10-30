
library(tidyverse)
source('scripts/read_files.R')
source('scripts/animate_play.R')
source('scripts/pick_play.R')

games <- read_games()
plays <- read_plays()
positions <- read_positions()
players <- read_players(positions = positions)
tracking <-
  read_all_weeks() %>% 
  filter(display_name != 'Football') %>% 
  select(-x, -y, -o, -a, -s, -dis, -dir, -time)

players_from_tracking <-
  tracking %>% 
  distinct(game_id, play_id, nfl_id, display_name, jersey_number, team, position) %>%
  mutate(across(jersey_number, as.integer))

players_w_teams <-
  players %>%
  select(nfl_id, position, display_name, position, side) %>% 
  left_join(
    players_from_tracking %>%
      inner_join(games %>% select(game_id, ends_with('_abbr'))) %>%
      mutate(team = if_else(team == 'away', visitor_team_abbr, home_team_abbr)) %>% 
      select(game_id, play_id, nfl_id, team, position_tracking = position, display_name_tracking = display_name,  jersey_number)
  ) %>% 
  select(game_id, play_id, nfl_id, display_name, display_name_tracking, jersey_number, team, position, position_tracking, side)
players_w_teams
# players_w_teams %>% 
#   filter(display_name != display_name_tracking) %>% 
#   distinct(display_name, display_name_tracking)
defensive_players <- players_w_teams %>% filter(side == 'D')
offensive_players <- players_w_teams %>% filter(side == 'O')
 
dpi_plays <-
  plays %>%
  filter(is_defensive_pi) %>%
  left_join(tracking %>% distinct(game_id, play_id, nfl_id, route)) %>%
  filter(target_nfl_id == nfl_id) %>%
  select(game_id, play_id, pass_result, target_nfl_id, route, matches('^penalty_'))
dpi_plays
dpi_plays %>% count(pass_result, route)
dpi_plays %>% filter(route == 'GO')

select_players <- function(side = c('O', 'D')) {
  side <- match.arg(side)
  prefix <- switch(side, 'O' = 'target', 'D' = 'pen')
  players_w_teams %>% 
    filter(side == !!side) %>% 
    select(game_id, play_id, nfl_id, display_name, jersey_number, team, position) %>% 
    rename_with(~sprintf('%s_%s', prefix, .x), nfl_id:position)
}

pbp <- read_nflfastr_pbp()
pbp_slim <-
  pbp %>% 
  select(game_id = old_game_id, play_id, wp, wpa, epa) %>% 
  mutate(across(game_id, as.double))

pens <-
  dpi_plays %>%
  separate(penalty_codes, sprintf('pen_%s', 1:3)) %>%
  separate(penalty_jersey_numbers, sprintf('pen_%s_%s', rep(c('team', 'jersey'), 3), sort(rep(1:3, 2)))) %>%
  pivot_longer(matches('pen_[123]'), names_to = 'pen_idx', values_to = 'pen') %>%
  pivot_longer(matches('pen_team_[123]'), names_to = 'pen_team_idx', values_to = 'pen_team') %>%
  pivot_longer(matches('pen_jersey_[123]'), names_to = 'pen_jersey_idx', values_to = 'pen_jersey_number') %>%
  mutate(across(matches('^pen.*idx'), ~ str_replace_all(.x, '(^.*_)([123]$)', '\\2') %>% as.integer())) %>%
  filter(pen_idx == pen_team_idx, pen_idx == pen_jersey_idx, pen_team_idx == pen_jersey_idx) %>%
  filter(!is.na(pen)) %>% 
  select(-pen_team_idx, -pen_jersey_idx) %>%
  mutate(across(pen_jersey_number, as.integer)) %>% 
  filter(pen == 'DPI') %>% 
  left_join(select_players('D')) %>% 
  left_join(select_players('O')) %>% 
  left_join(pbp_slim)
pens

# Check against https://www.nflpenalties.com/penalty/defensive-pass-interference?year=2018&view=players
pens %>% count(pen_team, pen_jersey_number, pen_display_name, pen_position, sort = TRUE)
pens %>% filter(is.na(pen_display_name))

pens %>% count(target_team, target_jersey_number, target_display_name, target_position, sort = TRUE)
pens %>% count(target_team, sort = TRUE)
pens %>% 
  ggplot() +
  aes(x = wpa) +
  geom_histogram(binwidth = 0.01) +
  labs(title = 'Win Probability Added Due to DPIs')
pens %>% 
  summarize(across(c(wpa, epa), list(min = min, max = max, mean = mean))) +
write_csv(pens, file.path('data', 'penalties.csv'))



