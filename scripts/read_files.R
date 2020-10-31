
library(tidyverse)

read_colors <- memoise::memoise({function() {
  file.path("data", "teamcolors.csv") %>% 
    readr::read_csv()
}})

read_positions <- memoise::memoise({function() {
  file.path("data", "positions.csv") %>% 
    readr::read_csv()
}})

read_plays <- memoise::memoise({function() {
  plays <-
    file.path("data", "plays.csv") %>% 
    readr::read_csv() %>%
    janitor::clean_names()
  target <-
    file.path("data", "targetedReciever.csv") %>% 
    readr::read_csv() %>% 
    janitor::clean_names()
  res <-
    plays %>% 
    inner_join(target)
  res
}})

read_games <- memoise::memoise({function() {
  file.path("data", "games.csv") %>% 
    readr::read_csv() %>%
    janitor::clean_names() %>% 
    dplyr::mutate(dplyr::across(game_date = lubridate::mdy))
}})

read_players <- memoise::memoise({function(positions = read_positions()) {
  file.path("data", "players.csv") %>% 
    readr::read_csv() %>%
    janitor::clean_names() %>% 
    dplyr::left_join(positions %>% dplyr::select(position, side)) %>% 
    dplyr::mutate(dplyr::across(birth_date, ~lubridate::parse_date_time(.x, order = c('y-m-d', 'm/d/y'), exact = FALSE)))
}})

read_week <- memoise::memoise({function(week = 1) {
  file.path("data", sprintf("week%d.csv", week)) %>% 
    vroom::vroom() %>%
    janitor::clean_names() %>% 
    dplyr::mutate(week = sprintf('week%d', !!week))
}})

# regenerate_all_weeks <- function(dir = file.path('data', 'nfl-big-data-bowl-2021')) {
#   paths <- fs::dir_ls(dir, regexp = 'week\\d+')
#   all_weeks <-
#     paths %>% 
#     purrr::map_df(vroom::vroom) %>% 
#     # purrr::map_df(readr::read_csv) %>% 
#     janitor::clean_names() %>% 
#     arrow::write_parquet(file.path('data', 'all_weeks.parquet'))
# }

read_nflfastr_pbp <- memoise::memoise({function(season = 2018) {
  sprintf('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_%s.rds', season) %>% 
    url() %>% 
    read_rds()
}})

read_all_weeks <- function() {
  file.path("data", "all_weeks.parquet") %>% 
    arrow::read_parquet() %>%
    janitor::clean_names()
}

# Use this to reduce data size.
regenerate_all_week_minimal <- function() {
  tracking <- read_all_weeks()
  tracking %>% 
    select(-a, -s, -dis, -dir, -time) %>% # , -jersey_number, -team) %>% 
    arrow::write_parquet(file.path('data', 'all_weeks_minimal.parquet'))
}

read_all_weeks_minimal <- function() {
  file.path("data", "all_weeks_minimal.parquet") %>% 
    arrow::read_parquet()
}

read_tracking <-
  function(max_week = 1,
           weeks = 1:max_week,
           positions = read_positions(),
           minimal = TRUE,
           drop_cols = TRUE,
           cols = c('a',
                    's',
                    'dis',
                    'dir',
                    'time',
                    'display_name',
                    'jersey_number',
                    'team'),
           standardize = TRUE) {
    
    if (max_week < 17 | length(weeks) < 17) {
      tracking <- 
        weeks %>% 
        map_dfr(read_week)
    } else {
      if (minimal) {
        tracking <- read_all_weeks_minimal()
      } else {
        tracking <- read_all_weeks()
      }
    }
    
    tracking <-
      tracking %>% 
      mutate(across(week, ~str_remove(.x, 'week') %>% as.integer())) %>% 
      filter(week <= max_week) %>% 
      select(-week)
    
    if(drop_cols) {
      cols_not_display_name <- setdiff(cols, 'display_name')
      tracking <-
        tracking %>% 
        select(-any_of(cols_not_display_name))
    }
    
    tracking <-
      tracking %>% 
      left_join(positions %>% select(position, side), by = 'position')
    
    ball <- tracking %>% filter(display_name == 'Football')
    
    tracking <-
      tracking %>%
      filter(display_name != 'Football')
    
    if(('display_name' %in% names(tracking)) & any('display_name' %in% cols)) {
      tracking <- tracking %>% select(-display_name)
    }
    
    tracking <-
      tracking %>% 
      inner_join(
        ball %>% 
          select(game_id, play_id, frame_id, ball_x = x, ball_y = y),
        by = c('frame_id', 'game_id', 'play_id')
      )
    
    if(!standardize) {
      return(tracking)
    }
    
    line_of_scrimmage <-
      tracking %>%
      filter(event == 'ball_snap') %>% 
      group_by(game_id, play_id) %>%
      filter(row_number() == 1L) %>% 
      ungroup() %>% 
      select(game_id, play_id, los = ball_x) %>%
      ungroup()
    
    x_max <- 120
    y_max <- 160 / 3
    tracking %>%
      left_join(line_of_scrimmage, by = c('game_id', 'play_id')) %>%
      mutate(
        across(c(x, ball_x, los), ~ if_else(play_direction == 'left', !!x_max - .x, .x)),
        # Standardizing the x direction based on the los is best for doing general analysis,
        # but perhaps not for plotting.
        # across(c(x, ball_x), ~if_else(play_direction == 'left', .x - (!!x_max - los), .x - los)),
        across(c(y, ball_y), ~ if_else(play_direction == 'left', !!y_max - .x, .x))
      )
  }

# Probably need to put this in some other file since it doesn't go perfectly with all these `read_*` functions.
# When `group = 0`, it's pre-snap. When `group = 2`, it's `events`.
clip_tracking_at_events <- function(tracking, events) {
  tracking %>% 
    group_by(game_id, play_id, nfl_id) %>%
    mutate(
      group = case_when(
        event == 'ball_snap' ~ 1L,
        dplyr::lag(event) %in% !!events ~ 1L,
        TRUE ~ 0L
      ),
      group = cumsum(group)
    ) %>%
    ungroup() %>%
    filter(group == 1L) %>%
    select(-group)
}

