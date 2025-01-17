
# Load raster first to let dplyr select mask it
library(raster)
library(tidyverse)
library(janitor)
library(arrow)
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
all_weeks <- read_csv("data/week1.csv") %>% clean_names() #  for simplicity
# all_weeks <- 
#   read_parquet("data/all_weeks.parquet") %>% 
#   clean_names() %>% 
#   select(-week)

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
wheel %>% 
  ggplot() + 
  geom_path(aes(x=x_from_los, y=y, group=interaction(game_id, play_id, nfl_id))) + 
  ggtitle("Wheel Routes from LOS, Weeks 1-8") + labs(x="Yards from LOS")


## wheel routes broken up by pass result (C=catch, I=incomplete, IN=intercepted)
wheel %>% ggplot() + 
  geom_path(aes(x=x_from_los, y=y, group=interaction(game_id, play_id, nfl_id))) + 
  facet_wrap(~pass_result) + 
  ggtitle("Wheel Routes from LOS, Weeks 1-8, by Pass Result") + labs(x="Yards from LOS")



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
  geom_path(aes(x=x_from_los, y=y, group=interaction(game_id, play_id, nfl_id))) + 
  
  ## split up different routes
  facet_wrap(~route) + 

  ggtitle("Routes run from Line of Scrimmage, Week 1" ) + 
  labs(x="Yards from LOS")



## find extent of left-to-right positions
x_range <- wheel %>% 
  summarise(xmin=min(x_from_los), xmax=max(x_from_los)) %>% 
  unlist

## cover whole field y range, plus 2 yards out-of bounds
y_range <- c(-2, 56)

######################################################
## preferred heatmap code with raster package
######################################################

## 1-yard x 1-yard resolution
xres <- 1
yres <- 1

nxcells <- ceiling((x_range[2]-x_range[1])/xres)
nycells <- ceiling((y_range[2]-y_range[1])/yres)


heatgrid_raster <- raster::raster(xmn=x_range[1], xmx=x_range[2], 
                                ymn=y_range[1], ymx=y_range[2], 
                                ncols=nxcells, nrows=nycells)

## initialize all cell counts as 0
raster::values(heatgrid_raster) <- 0

## compute number of points (player positions) that fall in each cell
wheel_pos <- wheel %>% select(x_from_los, y) %>% as.matrix

cell_counts <- raster::cellFromXY(heatgrid_raster, wheel_pos) %>% table


## fill in cell counts to raster object
heatgrid_raster[as.numeric(names(cell_counts))] <- cell_counts


## use log-spaced so we can see patterns more easily
## these values could change a lot depending on data, grid resolution! 
range(cell_counts)

brks <- c(0, exp(seq(log(1), log(max(cell_counts)), length.out=9)))
cols <- length(brks) %>% heat.colors %>% rev

## base R version of plot (better resolution IMO)
plot(heatgrid_raster, col=cols, 
     breaks=brks, xlab='Yards from LOS',
     main="Heatmap of Wheel Routes, Weeks 1-8")

## ggplot version of plot(works fine but could be improved)
heatgrid_df <- data.frame(raster::rasterToPoints(heatgrid_raster))

gg_heat_raster <- ggplot() + 
  geom_raster(data=heatgrid_df, aes(x=x, y=y, fill=layer)) + 
  scale_fill_gradientn(colours=cols, values=brks/max(brks)) + 
  ggtitle("Heatmap of Wheel Routes, Weeks 1-8")

gg_heat_raster

rayshader::plot_gg(gg_heat_raster)

###############################################
## use same grid for GO routes
###############################################

go <- all_merged %>% 
  filter(route=='GO') %>% 
  drop_na(route) %>% 
  
  ## standardize to LOS
  mutate(x_from_los = case_when(
    play_direction == "right" ~ x - absolute_yardline_number,
    play_direction == "left" ~ absolute_yardline_number - x
  )) 

## compute number of points (player positions) that fall in each cell
go_pos <- go %>% select(x_from_los, y) %>% as.matrix

## reset all cell counts to 0
raster::values(heatgrid_raster) <- 0

## repopulate cells
cell_counts <- raster::cellFromXY(heatgrid_raster, go_pos) %>% table
heatgrid_raster[as.numeric(names(cell_counts))] <- cell_counts

brks <- c(0, exp(seq(log(1), log(max(cell_counts)), length.out=9)))
cols <- length(brks) %>% heat.colors %>% rev

plot(heatgrid_raster, col=cols, 
     breaks=brks, xlab='Yards from LOS',
     main="Heatmap of GO Routes, All weeks")

######################################################
## alternative heatmap method (less efficient)
######################################################
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
