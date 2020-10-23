
## This code is intended to calculate the convex hull of the defensive team at ball snap.
## The convex hull is the outermost polygon connecting their positions.
## The area of the convex hull is basically a summary of how spread out the defense is, 
## which could be an interesting feature to look at in relation to different coverages.

library(tidyverse)
library(janitor)
library(arrow)
source("scripts/gg_field.R")

## load plays, games, and tracking data
games <- 
  read_csv("data/games.csv") %>% 
  clean_names() %>% 
  mutate(game_date = lubridate::mdy(game_date))

plays <- 
  read_csv("data/plays.csv") %>% 
  clean_names() %>% 
  # There are 2 of these. Not sure what to do with them... drop them.
  filter(!is.na(pass_result))

plays <- plays %>% 
  left_join(games, by = "game_id")


all_weeks <- 
  read_parquet("data/all_weeks.parquet") %>% 
  clean_names() 

# Standardizing tracking data so its always in direction of offense vs raw on-field coordinates:
all_weeks <- all_weeks %>%
  mutate(x = ifelse(play_direction == "left", 120-x, x),
         y = ifelse(play_direction == "left", 160/3 - y, y))



## read in coverage data for week 1
coverage <- read_csv("data/coverages_week1.csv") %>% 
  clean_names()

## subset tracking data to week 1
week1 <- all_weeks %>% filter(week=="week1")

week1 <- week1 %>% inner_join(plays, by=c("game_id","play_id"))
week1 <- week1 %>% inner_join(coverage, by=c("game_id","play_id"))

## create variable to check if player is on offense or defense
week1 <- week1 %>% 
  mutate(team_abbrev = case_when(
    team == "home" ~ home_team_abbr,
    team == "away" ~ visitor_team_abbr
  ),
  side_of_ball = case_when(
    team_abbrev == possession_team ~ "offense",
    team_abbrev != possession_team ~ "defense",
    TRUE ~ "football"
  )
)

## subset to single play
ex_game_id <- "2018090600"
ex_play_id <- 75

play1 <- week1 %>% filter(game_id == ex_game_id, play_id == ex_play_id)

## only use frame at time of snap
play1_snap <- play1 %>% filter(event == "ball_snap")

## order of defensive players needed to make polygon
def_chull_order <- play1_snap %>% 
                    filter(side_of_ball == "defense") %>% 
                    select(x, y) %>% 
                    chull

def_chull_order <- c(def_chull_order, def_chull_order[1])

def_chull_coords <- play1_snap %>% filter(side_of_ball == "defense") %>%
                      select(x,y) %>% slice(def_chull_order)

## polygon object to get area of chull
def_chull_poly <- sp::Polygon(def_chull_coords, hole=F)
def_chull_area <- def_chull_poly@area

## area of polygon spanned by defense
print(def_chull_area)

## plot player positions with defensive convex hull
gg_field() + 
  geom_point(data=play1_snap, aes(x=x, y=y, col=factor(side_of_ball)), cex=3) + 
  scale_color_manual(values=c('offense'='blue','defense'='red','football'='brown')) + 
  geom_polygon(data=def_chull_coords, aes(x=x,y=y), fill='red',alpha=0.2) + 
  labs(color='') + 
  ggtitle(paste0('GameID=', ex_game_id,', PlayID=',ex_play_id))


## function to compute area of convex hull of defensive setup
calc_chull_area <- function(playdf, gameid, playid){
  
  ## pull out locations of defenders at time of ball snap
  player_positions <- playdf %>% 
                        filter(game_id == gameid, play_id == playid,
                                event == "ball_snap", side_of_ball == "defense") %>% 
                        select(x, y)

  ## get connection order of players
  chull_order <- chull(player_positions)
  
  ## add last point to connect polygon
  chull_order <- c(chull_order, chull_order[1])
  
  ## order positions according to polygon
  chull_coords <- player_positions %>% slice(chull_order)
  
  ## define polygon and calculate area
  chull_poly <- sp::Polygon(chull_coords, hole=F)
  chull_area <- chull_poly@area
  
  return(chull_area)
}

## example of function for single play
calc_chull_area(playdf=week1, gameid = "2018090600", playid = 75)

## number of unique plays
nplays <- week1 %>% select(play_id, game_id) %>% n_distinct


## how to apply the calc_chull_area function to all week1 plays??
