## only needed for plotting random field designs
library(patchwork)

source("scripts/gg_field.R")

## reminders: 
### x-axis goes from 0 (back of left endzone) to 120 (back of right end zone)
### left goal line (x=10), 50-yard line (x=60), right goal line (x=110)
### y-axis goes from 0 to 53.33

##################################################
### Examples of gg_field 2.0 (current)
##################################################

## regular field (direction="horiz")
ggplot() + gg_field() 

## vertical field
ggplot() + gg_field(direction="vert") 


## zoomed in on half
ggplot() + gg_field(yardmin=60) 

## decrease/increase sideline buffer (default is 5 yards on each side)
ggplot() + gg_field(buffer=0) 
ggplot() + gg_field(buffer=15) 


## random dataset
df <- data.frame(x=runif(50, 10,110), y=runif(50, min=1, max=53), grp=rep(1:5,each=10))

## adding points to the field
ggplot(data=df, aes(x=x,y=y)) + 
  gg_field() + 
  geom_point(col='gold', cex=2)

## faceted fields! (look a little busy when small but fine when you expand image)
ggplot(data=df, aes(x=x,y=y)) + 
  gg_field() + 
  geom_point(col='gold', cex=2) +
  facet_wrap(~grp, labeller = label_both)


## change the field colors (sideline and endzone default to same color as field unless 
## otherwise specified)
ggplot() + gg_field(field_color='gray') 


## adjust sideline and endzone colors
ggplot() + gg_field(endzone_color = 'gold',sideline_color = 'gray') 


## realistic-looking field 
ggplot() + gg_field(field_color='forestgreen', line_color='white', endzone_color = 'darkgreen')


## (gg)cute field
fairy_cols <- ggcute:::fairyfloss_colours
ggplot() + gg_field(field_color=fairy_cols[6], 
                       line_color=fairy_cols[1], 
                       endzone_color = fairy_cols[4],
                       sideline_color = fairy_cols[2]) 

## generate 9 fields with random colors from grDevices::colors()
p <- vector("list", 9)
random_cols <- matrix(NA, nr=9, nc=4)

for(i in 1:9){
  random_cols[i,] <- sample(colors(), 4, replace=F)  
  p[[i]] <- ggplot() + gg_field(field_color=random_cols[i,1], 
                                   line_color=random_cols[i,2], 
                                   endzone_color = random_cols[i,3],
                                   sideline_color = random_cols[i,4]) 
  
}

## patchwork package allows for easy combining of separate ggplots :)
(p[[1]] + p[[2]] + p[[3]]) / (p[[4]] + p[[5]] + p[[6]]) / (p[[7]] + p[[8]] + p[[9]])


##################################################
### Examples of gg_field 1.0 (outdated)
##################################################

## example of full field
# field_full <- gg_field()
# field_full
# 
# ## example, clipped at 50 yardline
# field_clipped_at_50 <- gg_field(yardmin=60)
# field_clipped_at_50
# 
# 
# ## example, no sideline buffer (not recommended since players often go out-of-bounds)
# field_no_buffer <- gg_field(buffer=0)
# field_no_buffer
# 
# 
# ## example frame taken from a play in the dataset
# ex_frame <- readr::read_csv("data/gg_field_example_data.csv") %>%
#             janitor::clean_names()

# ex_frame <- ex_frame %>% mutate(team_colors = case_when(
#               team == "football" ~ "brown",
#               team == "away" ~ "blue",
#               team == "home" ~ "gold"
#             ))
# 
# ## add players to full field
# field_full + 
#   geom_point(data=ex_frame, aes(x=x, y=y, col=team_colors), cex=3) + 
#   scale_color_identity()
# 
# ## add players to half field (50 yl to end zone)
# field_clipped_at_50 + 
#   geom_point(data=ex_frame, aes(x=x, y=y, col=team_colors), cex=3) + 
#   scale_color_identity()
