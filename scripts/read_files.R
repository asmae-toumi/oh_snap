

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
