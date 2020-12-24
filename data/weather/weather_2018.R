library(tidyverse)

# data
games <- 
  read_csv("data/games.csv") %>% 
  filter(Season == 2018)

games_weather <- read_csv("data/games_weather.csv")

stadium_coordinates <- read_csv("data/stadium_coordinates.csv")

# join just the games from 2018

games_plus_weather <- games %>% 
  left_join(games_weather, by = "game_id")

games_plus_weather <- 
  games_plus_weather %>% 
  group_by(game_id) %>% 
  summarize(
    temperature = mean(Temperature, na.rm = T),
    humidity = mean(Humidity, na.rm = T), 
    precipitation = mean(Precipitation, na.rm = T),
    wind_speed = mean(WindSpeed, na.rm = T)
  )

write_csv(games_plus_weather, "weather_2018.csv")



