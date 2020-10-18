library(tidyverse)
library(piggyback)
library(janitor)
library(arrow)


# Import ------------------------------------------------------------------


games <- 
  read_csv("data/games.csv") %>% 
  clean_names() %>% 
  mutate(game_date = lubridate::mdy(game_date))

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

all_weeks <- 
  read_parquet("data/all_weeks.parquet") %>% 
  clean_names()






