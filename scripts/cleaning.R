library(tidyverse)
library(piggyback)
library(janitor)
library(arrow)


# Games -------------------------------------------------------------------

games <- 
  read_csv(here::here("data/games.csv")) %>% 
  clean_names() %>% 
  mutate(game_date = lubridate::mdy(game_date))


# Players -----------------------------------------------------------------

players <- 
  read_csv(here::here("data/players.csv")) %>% 
  clean_names() %>% 
  mutate(birth_date = lubridate::parse_date_time(birth_date, 
                                                 orders = c("y-m-d", "m/d/y"),
                                                 exact = F)) 



defense <- c("CB", "DB", "DE", "DT", "FS", "ILB", "LB", "MLB", "NT", "OLB", "S", "SS")
offense <- c("FB", "HB", "QB", "RB", "TE", "WR")
special <- c("K", "LS", "P")

players <- players %>% 
  mutate(
    category = case_when(
      position %in% defense ~ "defense", 
      position %in% offense ~ "offense", 
      position %in% special ~ "special"
    )
  )

# # Proposing this as an alternative to the above.
# positions <- read_csv("data/positions.csv")
# players <- players %>% 
#   left_join(positions %>% select(position, category = side))

# Plays -------------------------------------------------------------------


plays <- 
  read_csv(here::here("data/plays.csv")) %>% 
  clean_names() %>% 
  # There are 2 of these. Not sure what to do with them... drop them.
  filter(!is.na(pass_result))

plays <- plays %>% 
  separate(penalty_codes, c("pen_1", "pen_2", "pen_3")) %>% 
  separate(penalty_jersey_numbers, c("pen_team_1", "pen_jersey_1",
                                     "pen_team_2", "pen_jersey_2",
                                     "pen_team_3", "pen_jersey_3")) 

# TO DO: join player ID

plays <- plays %>% 
  separate(game_clock, into = c("minutes", "seconds")) %>% 
  mutate(minutes = as.numeric(minutes), 
         seconds = as.numeric(seconds),
         seconds_left_quarter = 60*minutes + seconds,
         seconds_left_game = seconds_left_quarter + (4-quarter)*15*60,
         seconds_into_game = 3600-seconds_left_game, 
         game_minute = floor(seconds_into_game/60)) 

plays <- plays %>% 
  left_join(games, by = "game_id")

plays <- plays %>% 
  mutate(
    score_diff = abs(pre_snap_home_score - pre_snap_visitor_score),
    game_close = case_when(
      score_diff <= 16 & game_minute <= 45 ~ "yes", 
      score_diff <= 8 & game_minute <= 15 ~ "no", 
      TRUE ~ "not close"
    ), 
    leading = case_when(
      pre_snap_home_score < pre_snap_visitor_score ~ "home",
      pre_snap_visitor_score < pre_snap_home_score ~ "away",
      TRUE ~ "tie"
    ),
    offensive_team_home = case_when(
      possession_team == home_team_abbr ~ "yes", 
      possession_team == visitor_team_abbr ~ "no", 
      TRUE ~ "other"
    ),
    offensive_team_leading = case_when(
      leading == "tie" ~ "no", 
      leading == "home" & offensive_team_home == "yes" ~ "yes", 
      leading == "home" & offensive_team_home == "no" ~ "no", 
      leading == "away" & offensive_team_home == "yes" ~ "no", 
      leading == "away" & offensive_team_home == "no" ~ "yes"
    )
  ) 

# All weeks ---------------------------------------------------------------

all_weeks <- 
  read_parquet(here::here("data/all_weeks.parquet")) %>% 
  clean_names()

# Standardizing tracking data so its always in direction of offense vs raw on-field coordinates:
all_weeks <- all_weeks %>%
  mutate(x = ifelse(play_direction == "left", 120-x, x),
         y = ifelse(play_direction == "left", 160/3 - y, y))





