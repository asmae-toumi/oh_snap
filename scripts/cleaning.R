library(tidyverse)
library(piggyback)
library(janitor)
library(arrow)


# Games -------------------------------------------------------------------

games <- 
  read_csv("data/games.csv") %>% 
  clean_names() %>% 
  mutate(game_date = lubridate::mdy(game_date))


# Players -----------------------------------------------------------------

players <- 
  read_csv("data/players.csv") %>% 
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


# Plays -------------------------------------------------------------------


plays <- 
  read_csv("data/plays.csv") %>% 
  clean_names()

plays <- plays %>% 
  separate(penalty_codes, c("pen_1", "pen_2", "pen_3")) %>% 
  separate(penalty_jersey_numbers, c("pen_team_1", "pen_jersey_1",
                                     "pen_team_2", "pen_jersey_2",
                                     "pen_team_3", "pen_jersey_3")) 

# TO DO: join player ID


# Players -----------------------------------------------------------------

players <- 
  read_csv("data/players.csv") %>% 
  clean_names()


# All weeks ---------------------------------------------------------------

all_weeks <- 
  read_parquet("data/all_weeks.parquet") %>% 
  clean_names()

# Standardizing tracking data so its always in direction of offense vs raw on-field coordinates:
all_weeks <- all_weeks %>%
  mutate(x = ifelse(play_direction == "left", 120-x, x),
         y = ifelse(play_direction == "left", 160/3 - y, y))


#TO DO: gg_football()




