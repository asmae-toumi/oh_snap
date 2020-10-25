
library(tidyverse)
source("scripts/gg_field.R")

positions <- read_csv(here::here("data/positions.csv"))

tracking <- 
  # arrow::read_parquet(here::here("data/all_weeks.parquet")) %>% 
  read_csv(here::here("data/week1.csv")) %>% 
  mutate(week = "1") %>% 
  janitor::clean_names()  %>%
  mutate(across(week, ~str_remove(.x, "week") %>% as.integer())) %>% 
  filter(week <= n_week) %>% 
  left_join(positions %>% select(position, side))

games <-
  read_csv(here::here("data/games.csv")) %>%
  janitor::clean_names() %>%
  mutate(game_date = lubridate::mdy(game_date))

plays <-
  read_csv(here::here("data/plays.csv")) %>%
  janitor::clean_names() %>%
  filter(!is.na(pass_result))

ball <- tracking %>% filter(display_name == "Football")

tracking <-
  tracking %>%
  filter(display_name != "Football") %>%
  inner_join(ball %>% select(game_id, play_id, frame_id, ball_x = x, ball_y = y))
tracking

snap_frames <- tracking %>% filter(event == "ball_snap")
# Reference: https://github.com/danichusfu/RouteIdentification/blob/master/R/read_routes_from_csv.R
play_direction <-
  snap_frames %>%
  group_by(game_id, play_id, team) %>%
  summarise(mean_team = mean(x)) %>%
  filter(mean_team == max(mean_team)) %>%
  select(game_id, play_id, direction_left = team, -mean_team) %>% 
  ungroup()

line_of_scrimmage <-
  snap_frames %>%
  group_by(game_id, play_id, team) %>%
  summarise(right_scrim = max(x), left_scrim = min(x)) %>%
  ungroup()

possession <-
  plays %>%
  select(game_id, play_id, possession_team) %>%
  left_join(games, by = "game_id") %>%
  mutate(possession = if_else(possession_team == home_team_abbr, "home", "away")) %>%
  select(game_id, play_id, possession)

tracking <-
  tracking %>%
  # head(1000) %>% 
  left_join(play_direction, by = c("game_id", "play_id")) %>%
  left_join(line_of_scrimmage, by = c("team", "game_id", "play_id")) %>%
  left_join(possession, by = c("game_id", "play_id")) %>% 
  # filter(team == possession) %>% # To get the offense only
  mutate(los = if_else(team == direction_left, left_scrim, right_scrim)) %>% 
  select(-matches("_scrim$"), possession)
tracking

# Filter out stuff before the snap and after the pass has arrived.
# My attempt at making a full list of events where the route should be considered over.
# This is like https://github.com/danichusfu/RouteIdentification/blob/master/R/cut_plays.R.
events_end_route <- c(
  "pass_outcome_caught",
  "pass_outcome_incomplete",
  "qb_sack",
  "pass_outcome_interception",
  "pass_outcome_touchdown",
  "qb_strip_sack",
  "qb_spike"
)

tracking_cleaned <-
  tracking %>%
  # head(300) %>% 
  group_by(game_id, play_id, nfl_id) %>%
  mutate(
    group = case_when(
      event == "ball_snap" ~ 1L,
      lag(event) %in% events_end_route ~ 1L,
      TRUE ~ 0L
    ),
    group = cumsum(group)
  ) %>%
  ungroup() %>%
  filter(group == 1L) %>%
  select(-group)
tracking_cleaned

tracking_ex <- tracking_cleaned %>% filter(play_id == dplyr::first(play_id))
tracking_ex

# # # Generate the example tribble.
# snap_frames_ex <- tracking_ex %>% filter(event == "ball_snap")
# snap_frames_ex
# snap_frame_nested_ex <-
#   snap_frames_ex %>%
#   select(game_id, play_id, nfl_id, side, x, y) %>%
#   mutate(across(c(x, y), ~round(.x, 1))) %>%
#   clipr::write_clip()

coerce_to_mat <- function(data) {
  res <- data %>% select(x, y) %>% as.matrix()
  rownames(res) <- data$nfl_id
  res
}

compute_min_distances <- function(data) {
#   data <-
# tibble::tribble(
#           ~game_id, ~play_id,  ~nfl_id, ~side,    ~x,   ~y,
#        2018090600L,      75L,     310L,   "O",    -1, 26.7,
#        2018090600L,      75L,   79848L,   "D",     0, 36.5,
#        2018090600L,      75L, 2495454L,   "O",  -0.6,  9.2,
#        2018090600L,      75L, 2495613L,   "D",  -2.4,   22,
#        2018090600L,      75L, 2533040L,   "O",     0, 17.2,
#        2018090600L,      75L, 2534832L,   "D", -15.3, 28.7,
#        2018090600L,      75L, 2543583L,   "O",  -7.5, 26.7,
#        2018090600L,      75L, 2552315L,   "D",  -2.3, 31.1,
#        2018090600L,      75L, 2552689L,   "D",  -6.3, 20.5,
#        2018090600L,      75L, 2555383L,   "D",    -5, 43.4,
#        2018090600L,      75L, 2555415L,   "O",  -0.5, 31.5,
#        2018090600L,      75L, 2558175L,   "D",  -3.2, 26.8,
#        2018090600L,      75L, 2559033L,   "O",  -4.3, 26.6
#        )

  o <- data %>% filter(side == "O") %>% coerce_to_mat()
  d <- data %>% filter(side == "D") %>% coerce_to_mat()
  dists <- fields::rdist(o, d)
  rows <- rownames(o)
  cols <- rownames(d)
  rownames(dists) <- rows
  colnames(dists) <- cols
  idx_min <- clue::solve_LSAP(dists, maximum = FALSE)
  cols_min <- cols[idx_min]
  pairs <- 
    tibble(nfl_id_d = rows, nfl_id_o = cols_min) %>% 
    mutate(across(starts_with("nfl_id"), as.integer)) 
  dists_tidy <- 
    dists %>% 
    as_tibble(rownames = "nfl_id_d") %>% 
    pivot_longer(-c(nfl_id_d), names_to = "nfl_id_o", values_to = "dist") %>% 
    relocate(nfl_id_o, nfl_id_d) %>% 
    mutate(across(starts_with("nfl_id"), as.integer)) %>% 
    inner_join(data %>% select(nfl_id_o = nfl_id, x_o = x, y_o = y), by = "nfl_id_o") %>% 
    inner_join(data %>% select(nfl_id_d = nfl_id, x_d = x, y_d = y), by = "nfl_id_d")
  dists_tidy
  res <-
    dists_tidy %>% 
    inner_join(pairs, by = c("nfl_id_o", "nfl_id_d"))
  res
}

min_dists <-
  tracking_ex %>% 
  select(game_id, play_id, nfl_id, frame_id, side, x, y) %>% 
  nest(data = -c(frame_id)) %>% 
  mutate(data = map(data, compute_min_distances)) %>% 
  unnest(data)
min_dists

los <- tracking_ex$los[[1]]
ball_ex <- tracking_ex %>% distinct(frame_id, x = ball_x, y = ball_y) %>% mutate(nfl_id = NA_real_)
pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

anim <-
  tracking_ex %>% 
  filter(frame_id == max(frame_id)) %>% 
  ggplot() +
  gg_field(yardmin = 60, yardmax = 100) +
  aes(x = x, y = y, group = nfl_id) +
  geom_segment(
    data = tibble(),
    inherit.aes = FALSE,
    aes(x = los, y = 0, xend = los, yend = 0 + 160/3),
    size = 2
  ) +
  geom_point(
    data = ball_ex %>% filter(frame_id == max(frame_id)),
    inherit.aes = TRUE, 
    size = 3, 
    # shape = 21,
    color = 'brown'
  ) +
  # geom_point(
  #   aes(fill = side), 
  #   size = 6, 
  #   shape = 21,
  #   color = 'black',
  #   show.legend = FALSE
  # ) +
  geom_text(
    aes(label = '\u25A0', color = side, angle = s),
    # size = 8,
    size = pts(24),
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = '\u0332', color = side, angle = s),
    # size = 8,
    size = pts(24),
    # vjust = 0.4,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = jersey_number, angle = s), 
    color = 'white',
    # size = 3,
    size = pts(8),
    vjust = 1,
  ) +
  scale_color_manual(values = c('O' = 'red', 'D' = 'blue')) +
  geom_segment(
    data = min_dists,
    inherit.aes = FALSE,
    aes(x = x_o, y = y_o, xend = x_d, yend = y_d),
    color = 'grey50'
  ) +
  # gganimate::transition_manual(frame_id) +
  labs(x = NULL, y = NULL)
anim

set.seed(42)
df <- data.frame(x = 1:10, value = cumsum(runif(10 , max = 10)))
p <- ggplot(df, aes(x=x, y=value)) 
p + geom_line() + 
  geom_text(data = tibble(x = 2, value = 5, label = '\u25A0', angle = 0), aes(label = label, angle = angle), color = 'red') +
  geom_text(data = tibble(x = 2, value = 5, label = '\u0332', angle = 0), aes(label = label, angle = angle), color = 'blue')
'203E'
path_gif <- fs::path('figs', 'min_dist_ex.gif')
n_sec <- ball_ex %>% nrow() %>% {. / 10}
fps <- 10
n_sec_end <- 3
height <- 600
width <- 600
n_frame <- (n_sec + n_sec_end) * fps

gganimate::animate(
  anim,
  nframe = n_frame,
  fps = fps,
  height = height,
  width = width,
  renderer = gganimate::gifski_renderer(path_gif),
  end_pause = n_sec_end * fps
)

