

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

read_week1 <- memoise::memoise({function() {
  file.path("data", "week1.csv") %>% 
    vroom::vroom() %>%
    janitor::clean_names()
}})

read_all_weeks <- function() {
  file.path("data", "all_weeks.parquet") %>% 
    arrow::read_parquet() %>%
    janitor::clean_names()
}

read_tracking <- function(max_week = 1L, positions = read_positions()) {
  if(max_week == 1L) {
    tracking <- read_week1() %>% mutate(week = 1L)
  } else {
    tracking <-
      read_all_weeks() %>%
      mutate(across(week, ~str_remove(.x, 'week') %>% as.integer())) %>%
      filter(week <= n_week)
  }
  
  tracking <-
    tracking %>% 
    left_join(positions %>% select(position, side), by = 'position')
  
  ball <- tracking %>% filter(display_name == 'Football')
  
  tracking <-
    tracking %>%
    filter(display_name != 'Football') %>%
    select(-display_name) %>% 
    inner_join(
      ball %>% 
        select(game_id, play_id, frame_id, ball_x = x, ball_y = y),
      by = c('frame_id', 'game_id', 'play_id')
    )

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

