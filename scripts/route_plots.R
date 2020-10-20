
library(ggplot2)

## these two only needed for heatmaps, not regular route plots
library(fields)
library(rayshader)

## read in data
plays <- 
  read_csv("data/plays.csv") %>% 
  clean_names() %>% 
  # There are 2 of these. Not sure what to do with them... drop them.
  filter(!is.na(pass_result))

games <- 
  read_csv("data/games.csv") %>% 
  clean_names() %>% 
  mutate(game_date = lubridate::mdy(game_date))


## tracking data - do not standardize positions!
all_weeks <- 
  read_parquet("data/all_weeks.parquet") %>% 
  clean_names() %>% 
  select(-week)

#merging plays and tracking data
all_merged <- inner_join(games,plays,
                        by = c("game_id" = "game_id"))


#merging games data to previously merged frame
all_merged <- inner_join(all_merged,
                        all_weeks,
                        by = c("game_id" = "game_id",
                               "play_id" = "play_id")) 

## subset to WHEEL routes
wheel <- all_merged %>% 
            filter(route=='WHEEL', week %in% 1:8) %>% 
            drop_na(route) %>% 
  
            ## standardize to LOS
            mutate(x_from_los = case_when(
              play_direction == "right" ~ x - absolute_yardline_number,
              play_direction == "left" ~ absolute_yardline_number - x
            )) 

## show all wheel routes
wheel %>% ggplot() + 
  geom_line(aes(x=x_from_los, y=y, group=interaction(play_id, nfl_id))) + 
  ggtitle("Wheel Routes from LOS, Week 1") + labs(x="Yards from LOS")


## wheel routes broken up by pass result (C=catch, I=incomplete, IN=intercepted)
wheel %>% ggplot() + 
  geom_line(aes(x=x_from_los, y=y, group=interaction(play_id, nfl_id))) + 
  facet_wrap(~pass_result) + 
  ggtitle("Wheel Routes from LOS, Week 1") + labs(x="Yards from LOS")



## make plot of all routes   
all_merged %>% 
  
  ## remove interceptions bc offense chases the defense
  filter(week==1, pass_result != "IN") %>% 
  
  ## na routes are for defenders or just missed labels
  drop_na(route) %>% 
  
  ## standardize to LOS
  mutate(x_from_los = case_when(
    play_direction == "right" ~ x - absolute_yardline_number,
    play_direction == "left" ~ absolute_yardline_number - x
  )) %>%

  ggplot() + 
  
  ## separate lines by play and player
  geom_line(aes(x=x_from_los, y=y, group=interaction(play_id, nfl_id))) + 
  
  ## split up different routes
  facet_wrap(~route) + 

  ggtitle("Routes run from Line of Scrimmage, Week 1" ) + 
  labs(x="Yards from LOS")


################################################################
## heatmap code below
## warning - it involves base R code!!
################################################################


## find extent of left-to-right positions
x_range <- wheel %>% 
            summarise(xmin=min(x_from_los), xmax=max(x_from_los)) %>% 
            unlist

## cover whole field y range, plus 2 yards out-of bounds
y_range <- c(-2, 56)

## make grid of 1-yard x 1-yard cells
heatgrid <- expand.grid(x=seq(x_range[1],x_range[2], by=1), 
                        y=seq(y_range[1], y_range[2],by=1))

## check which grid cell a player falls in
check_grid_cell <- function(xpos, ypos, grd){
  dm <- fields::rdist(matrix(c(xpos,ypos),1,2), grd)
  
  ind <- which.min(dm)
  return(ind)
}

## function to get counts for each grid cell 
## (avert your eyes if you dislike base R!!)
count_heatmap_cells <- function(df, grd){
  
  cell_ids <- unlist(sapply(1:nrow(df), function(a) {
                        check_grid_cell(df[a,"x_from_los"], df[a,"y"], grd)})
                     )
  
  grd$count <- sapply(1:nrow(grd), function(a) sum(cell_ids == a))
  
  return(grd)
}

heatgrid <- count_heatmap_cells(wheel, heatgrid)

## heatmap for wheel routes
gg_heat <- heatgrid %>% 
            ### remove cells with no plays in them
            filter(count > 0) %>%
            ggplot(aes(x=x, y=y)) +
            geom_vline(aes(xintercept=0)) + 
            geom_tile(aes(fill=count)) + 
            scale_fill_gradient(low="white", high="red") + 
            ggtitle("Wheel Route Heatmap") + labs(x="Yards beyond LOS")

gg_heat

## 3d heatmap if you wanna get fancy
rayshader::plot_gg(gg_heat)
