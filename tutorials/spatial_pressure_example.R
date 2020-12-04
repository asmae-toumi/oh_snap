
library(tidyverse)
library(sampSurf)
library(sf)
library(dismo)

## play1: game_id == 2018090600, play_id == 75
## really only need a few variables: frame_id, event, team, position, x, y
play1 <- read_csv('../data/play1.csv') %>% rowid_to_column(var='rowid')

play1

## for this particular play, offense is away team, defense is home 
off_ind <- play1 %>% filter(frame_id==1, team =='away') %>% pull(rowid)
def_ind <- play1 %>% filter(frame_id==1, team =='home') %>% pull(rowid)

## borders for ggvoronoi rectangle
outline.df <- data.frame(x=c(0, 120, 120, 0), y=c(0, 0, 53.33, 53.33))

## voronoi for individual frame at pass frame
play1 %>% 
  filter(event == 'pass_forward', team != 'football') %>% 
  ggplot(aes(x=x,y=y)) + 
  ggvoronoi::geom_voronoi(aes(fill=factor(team)), 
                          outline=outline.df, color='black', alpha=0.3) + 
  geom_text(aes(label=position), size=2.5) + 
  scale_fill_manual(values=c('away'='blue', 'home'='red'), 
                    labels=c('offense','defense')) + 
  labs(fill='team') +
  theme_void() +
  ggtitle('Voronoi at time of pass')

## get football position at events
df_football <- play1 %>% filter(event != 'None', team=='football')

## voronoi for all event frames
play1 %>% 
  filter(event != 'None', team != 'football') %>% 
  ggplot(aes(x=x,y=y)) + 
  ggvoronoi::geom_voronoi(aes(fill=factor(team)), 
                          outline=outline.df, color='black', alpha=0.3) + 
  geom_text(aes(label=position), size=2.5) + 
  geom_point(data=df_football, aes(x=x,y=y), size=2, color='brown') +
  scale_fill_manual(values=c('away'='blue', 'home'='red'), 
                    labels=c('offense','defense')) + 
  labs(fill='team') +
  facet_wrap(~factor(event, unique(event))) +
  theme_void()

df_throw <-  play1 %>% 
  filter(event == 'pass_forward', team != 'football') %>% 
  select(x,y, team, position)

df_throw

## voronoi object at throw
vor <- df_throw %>% 
  dplyr::select(x,y) %>% 
  as.matrix %>% 
  dismo::voronoi(ext=c(0,120,0,53.33))

## make 5-yard circle around player (target WR is third row)
rad <- 5
circ_coords <- df_throw %>% 
                slice(3) %>% 
                dplyr::select(x,y) %>% unlist

circ <- sampSurf::spCircle(radius = rad, centerPoint = circ_coords)


inter <- st_intersection(st_as_sf(circ$spCircle), st_as_sf(vor))  
inter <- inter %>% mutate(team = df_throw$team[id])

inter

## plot circle intersected with voronoi
inter %>% 
  ggplot() + 
  geom_sf(aes(fill=factor(team)),col='black', alpha=0.3) + 
  scale_fill_manual(values=c('away'='blue', 'home'='red'), 
                    labels=c('offense','defense')) +
  labs(fill='team') + 
  ggtitle('5-yard Control Circle for target WR') +
  theme_void()
  
## % control for WR at time of throw
(inter %>% filter(id %in% off_ind) %>% st_area %>% sum) / 
  (inter %>% st_area %>% sum)

## @df_play: data frame from tracking merged with plays and filtered
## @frame: frame_id 
## @player_ind: which index of players in a play to calculate pressure for
## @team_vec: vector of which players are on the reference team

## clean version of pressure function
calc_pressure <- function(df_play, frame, player_ind, team_vec){
  
  ## subset play df to single frame, remove football
  df_frame <- df_play %>% 
    filter(frame_id == frame, team != 'football') %>% 
    dplyr::select(x,y)
  
  ## voronoi for frame
  vor_f <- df_frame %>% as.matrix %>% 
    dismo::voronoi(ext=c(0,120,0,53.33))
  
  ## circle around player for frame
  rad <- 5
  circ_coords <- df_frame %>% slice(player_ind) %>% unlist
  circ_f <- sampSurf::spCircle(radius = rad, centerPoint = circ_coords)
  
  ## convert to sf objects
  spcirc_f <- st_as_sf(circ_f$spCircle)
  spvor_f <- st_as_sf(vor_f)
  
  ## find spatial overlap of circle and voronoi polygons
  ## prints an annoying warning, so suppress it :p
  inter_f <- suppressWarnings( st_intersection(spcirc_f, spvor_f) )
  
  ## calculate % of area controlled by reference team
  pct_control <- (inter_f %>% filter(id %in% team_vec) %>% st_area %>% sum) / 
    (inter_f %>% st_area %>% sum)
  
  ## % of area controlled by opposite team
  pct_pressure <- 1 - pct_control
  
  return(pct_pressure)
}


## find pressure for QB at snap (frame 11) (team_vec = offense)
play1$position[off_ind]
calc_pressure(play1, frame = 11, player_ind = off_ind[1], team_vec = off_ind)

## find pressure for SS at frame 11 (team_vec = defense)
play1$position[def_ind]
calc_pressure(play1, frame = 11, player_ind = def_ind[1], team_vec = def_ind)

nframes <- max(play1$frame_id)

## calculate pressure for all offensive players, all frames
pressure_mat_off <- matrix(NA, nr=nframes, nc=length(off_ind))

for(t in 1:nframes){
  for(j in 1:length(off_ind)){
    pressure_mat_off[t, j] <- calc_pressure(play1, frame=t, 
                                    player_ind=off_ind[j], team_vec=off_ind)
  }
  print(t)
}

pressure_df_off <- data.frame(frame=1:59, pressure_mat_off)
names(pressure_df_off)[-1] <- c("QB","WR1","WR2","RB","TE","FB") #play1$position[off_ind]

## calculate pressure for all defensive players, all frames
pressure_mat_def <- matrix(NA, nr=nframes, nc=length(def_ind))

for(t in 1:nframes){
  for(k in 1:length(def_ind)){
    pressure_mat_def[t, k] <- calc_pressure(play1, frame=t, 
                                    player_ind=def_ind[k], team_vec=def_ind)
  }
  print(t)
}

pressure_df_def <- data.frame(pressure_mat_def)
names(pressure_df_def) <- c("SS","FS1","FS2","MLB","CB1","CB2","LB") #play1$position[def_ind]

## combine into one 
pressure_df <- pressure_df_off %>% 
  bind_cols(pressure_df_def) %>%
  pivot_longer(cols=QB:LB, names_to = "position", values_to='pressure') %>%
  mutate(control = 1 - pressure, 
         team = ifelse(position %in% c("QB","WR1","WR2","RB","TE","FB"), 
                       "offense", "defense")
         )
  

pressure_df

## annotations for play events
df_annotate <- data.frame(event = c("snap","pass","arrival","catch","run oob"), 
                          frame=c(11,36,47,52,58))

## plot offensive control over time
pressure_df %>%
  filter(team == 'offense') %>% 
  ggplot(aes(x=frame, y=control)) + 
  geom_vline(data=df_annotate, aes(xintercept=frame), linetype=2) +
  geom_text(data=df_annotate, aes(x=frame-1, y=0.1, label=event), angle=90) +
  geom_line(aes(col=factor(position)), lwd=1.2) +
  facet_wrap(~position) + 
  ylim(0,1) +
  ggtitle(label='Offensive Control', sub='Game ID: 2018090600, Play ID: 75') +
  theme(legend.position='none') +
  theme_bw()

## plot defensive pressure over time
pressure_df %>%
  filter(team == 'defense') %>% 
  ggplot(aes(x=frame, y=pressure)) + 
  geom_vline(data=df_annotate, aes(xintercept=frame), linetype=2) +
  geom_text(data=df_annotate, aes(x=frame-1, y=0.9, label=event), angle=90) +
  geom_line(aes(col=factor(position)), lwd=1.2) +
  facet_wrap(~position) + 
  ylim(0,1) +
  ggtitle(label='Defensive Pressure', sub='Game ID: 2018090600, Play ID: 75') +
  theme_bw() +
  theme(legend.position='none')
